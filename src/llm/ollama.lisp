(defpackage #:agent/llm/ollama
  (:use #:cl)
  (:export
   ;; Backend class
   #:ollama-backend
   #:make-ollama-backend
   ;; Original API (backward compatibility)
   #:*tools*
   #:tool
   #:tool-name
   #:tool-description
   #:tool-parameters
   #:tool-function
   #:define-tool
   #:find-tool
   #:register-tool
   #:gen-tools
   #:message
   #:message-role
   #:message-content
   #:message-thinking
   #:message-tool-calls
   #:session
   #:session-messages
   #:chat))
(in-package #:agent/llm/ollama)

(defmethod print-object ((object hash-table) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (alexandria:hash-table-alist object) stream)))

(defun hash (&rest keyword-and-values)
  (alexandria:plist-hash-table
   (loop :for (keyword value) :on keyword-and-values :by #'cddr
         :collect (string-downcase keyword)
         :collect value)
   :test 'equal))

(defmacro with-hash-bindings (bindings hash-table &body body)
  `(let ,(loop :for binding :in bindings
               :collect `(,binding (gethash ,(string-downcase binding) ,hash-table)))
     ,@body))

;;; tools
(defvar *tools* (make-hash-table :test 'equal))

(defstruct tool name description arguments parameters function)

(defun register-tool (tool)
  (setf (gethash (tool-name tool) *tools*) tool))

(defmacro define-tool (name (&rest parameters) description &body body)
  (alexandria:with-unique-names (params)
    (let ((arguments (mapcar #'car parameters)))
      `(register-tool
        (make-tool :name ,name
                   :description ,description
                   :arguments ',(mapcar #'string-downcase arguments)
                   :parameters ',parameters
                   :function (lambda (,params)
                               ,description
                               (with-hash-bindings ,arguments ,params
                                 (block nil
                                   ,@body))))))))

(defun find-tool (name)
  (gethash name *tools*))

(define-tool "read-file" ((file :type "string" :description "file path"))
    "Provides read-only access to local files.
Given a file path, this tool loads the file and returns its content as a UTF-8 text string.
Use this tool when the agent must examine or process data stored on disk."
  (unless (uiop:file-exists-p file)
    (return (hash :error (format nil "File ~A was not found" file))))
  (let ((text (uiop:read-file-string file)))
    text))

(define-tool "list" ((path :type "string" :description "directory path"))
    "Lists files and directories in the specified path.
Given a directory path, this tool returns a list of entries (files and subdirectories).
Use this tool when the agent needs to explore directory contents or find files."
  (unless (uiop:directory-exists-p path)
    (return (hash :error (format nil "Directory ~A was not found" path))))
  (let* ((dir (uiop:ensure-directory-pathname path))
         (entries (append (uiop:subdirectories dir)
                          (uiop:directory-files dir))))
    (mapcar #'namestring entries)))

(defun gen-tools ()
  (let ((tools '()))
    (maphash
     (lambda (name tool)
       (assert (equal name (tool-name tool)))
       (push (hash :type "function"
                   :function (hash :name name
                                   :description (tool-description tool)
                                   :parameters (hash
                                                :type "object"
                                                :required (mapcar #'string-downcase
                                                                  (tool-arguments tool))
                                                :properties (alexandria:alist-hash-table
                                                             (mapcar (lambda (parameter)
                                                                      (cons (string-downcase
                                                                             (first parameter))
                                                                            (apply #'hash
                                                                                   (rest
                                                                                    parameter))))
                                                                     (tool-parameters tool))
                                                             :test 'equal))))
             tools))
     *tools*)
    (coerce tools 'vector)))

;;;
(defstruct message
  role
  content
  thinking
  tool-calls)

(defun message-to-hash (message)
  (alexandria:alist-hash-table
   `(("role" . ,(message-role message))
     ("content" . ,(message-content message))
     ("thinking" . ,(message-thinking message)))
   :test 'equal))

(defun hash-to-message (message)
  (make-message :role (gethash "role" message)
                :content (gethash "content" message)
                :thinking (gethash "thinking" message)
                :tool-calls (gethash "tool_calls" message)))

(defstruct response
  stream
  done)

(defun chat-request (messages)
  (let ((stream
          (dex:post "http://localhost:11434/api/chat"
                    :read-timeout nil
                    :connect-timeout nil
                    :want-stream t
                    :force-string t
                    :headers '(("Content-Type" . "application/json"))
                    :content (with-output-to-string (out)
                               (yason:encode-alist
                                `(("model" . "qwen3:32b")
                                  ("messages" . ,(map 'vector
                                                      #'message-to-hash
                                                      messages))
                                  ("tools" . ,(gen-tools)))
                                out)))))
    (make-response :stream stream)))

(defun read-response (response)
  (unless (response-done response)
    (let ((json (yason:parse (response-stream response))))
      (when (gethash "done" json)
        (setf (response-done response) t))
      (unless (response-done response)
        (hash-to-message (gethash "message" json))))))

(defun process-message (message)
  (unless (alexandria:emptyp (message-thinking message))
    (write-string (cl-ansi-text:blue (message-thinking message))))
  (unless (alexandria:emptyp (message-content message))
    (write-string (cl-ansi-text:red (message-content message))))
  (let ((messages '()))
    (dolist (call (message-tool-calls message))
      (let* (;(id (gethash "id" call))
             (function (gethash "function" call))
             ;(index (gethash "index" function))
             (name (gethash "name" function))
             (arguments (gethash "arguments" function)))
        (let ((tool (find-tool name)))
          (when tool
            (write-line (cl-ansi-text:yellow (format nil "tool_call ~S" name)))
            (let ((result (funcall (tool-function tool) arguments)))
              (write-line (cl-ansi-text:yellow (format nil "tool_called ~S" result)))
              (push (make-message :role "tool"
                                  :content (com.inuoe.jzon:stringify result :pretty t))
                    messages))))))
    (nreverse messages)))

(defclass session ()
  ((messages :initform '()
             :accessor session-messages)))

(defmethod append-messages ((session session) messages)
  (alexandria:nconcf (session-messages session)
                     messages))

(defun has-tool-calls-p (messages)
  (some #'message-tool-calls messages))

(defmethod process-response ((session session) response)
  (let ((messages (loop :for message := (read-response response)
                        :while message
                        :collect message
                        :when (process-message message)
                        :append :it)))
    (append-messages session messages)
    messages))

(defmethod chat ((session session) content)
  (append-messages session
                   (list (make-message :role "user"
                                       :content content)))
  (loop
    (let* ((response (chat-request (session-messages session)))
           (messages (process-response session response)))
      (unless (has-tool-calls-p messages)
        (return nil)))))

;;; usage
(eval-when ()
  (defparameter *session* (make-instance 'session))
  (chat *session* "...text..."))

;;; ==========================================================================
;;; Interface Implementation (agent/interface protocol)
;;; ==========================================================================

(defclass ollama-backend (agent/llm/interface:backend)
  ((base-url :initarg :base-url
             :accessor ollama-backend-base-url
             :initform "http://localhost:11434"
             :documentation "Ollama server URL")
   (tools-registry :initarg :tools-registry
                   :accessor ollama-backend-tools-registry
                   :initform *tools*
                   :documentation "Hash table of registered tools"))
  (:default-initargs :model "qwen3:32b")
  (:documentation "Ollama local inference backend implementation."))

(defun make-ollama-backend (&key (model "qwen3:32b")
                                 (base-url "http://localhost:11434"))
  "Create a new Ollama backend instance."
  (make-instance 'ollama-backend :model model :base-url base-url))

;;; Message protocol implementation

(defmethod agent/llm/interface:make-user-message ((backend ollama-backend) content)
  (make-message :role "user" :content content))

(defmethod agent/llm/interface:make-assistant-message ((backend ollama-backend) content
                                                   &key tool-calls)
  (make-message :role "assistant" :content content :tool-calls tool-calls))

(defmethod agent/llm/interface:make-system-message ((backend ollama-backend) content)
  (make-message :role "system" :content content))

(defmethod agent/llm/interface:make-tool-result-message ((backend ollama-backend)
                                                     tool-call-id result)
  (declare (ignore tool-call-id))
  (make-message :role "tool"
                :content (if (stringp result)
                             result
                             (com.inuoe.jzon:stringify result :pretty t))))

(defmethod agent/llm/interface:message-to-api-format ((backend ollama-backend)
                                                  (msg agent/llm/interface:message))
  (message-to-hash (make-message :role (agent/llm/interface:message-role msg)
                                 :content (agent/llm/interface:message-content msg)
                                 :tool-calls (agent/llm/interface:message-tool-calls msg))))

;;; Backend protocol implementation

(defmethod agent/llm/interface:chat-completion ((backend ollama-backend) messages &key tools)
  (declare (ignore tools))
  (let* ((converted-messages
           (mapcar (lambda (msg)
                     (if (typep msg 'message)
                         msg
                         (make-message :role (agent/llm/interface:message-role msg)
                                       :content (agent/llm/interface:message-content msg)
                                       :tool-calls (agent/llm/interface:message-tool-calls msg))))
                   messages))
         (stream
           (dex:post (format nil "~A/api/chat" (ollama-backend-base-url backend))
                     :read-timeout nil
                     :connect-timeout nil
                     :want-stream t
                     :force-string t
                     :headers '(("Content-Type" . "application/json"))
                     :content (with-output-to-string (out)
                                (yason:encode-alist
                                 `(("model" . ,(agent/llm/interface:backend-model backend))
                                   ("messages" . ,(map 'vector
                                                       #'message-to-hash
                                                       converted-messages))
                                   ("tools" . ,(gen-tools)))
                                 out)))))
    ;; Return response object with accumulated messages
    (let ((response-messages '())
          (done nil))
      (loop :until done
            :do (let ((json (yason:parse stream)))
                  (when (gethash "done" json)
                    (setf done t))
                  (unless done
                    (push (hash-to-message (gethash "message" json))
                          response-messages))))
      ;; Return accumulated messages as the response
      (list :messages (nreverse response-messages)
            :done done))))

(defmethod agent/llm/interface:get-response-content ((backend ollama-backend) response)
  (let ((messages (getf response :messages)))
    (when messages
      (let ((contents (remove nil (mapcar #'message-content messages))))
        (when contents
          (apply #'concatenate 'string contents))))))

(defmethod agent/llm/interface:get-response-tool-calls ((backend ollama-backend) response)
  (let ((messages (getf response :messages)))
    (loop :for msg :in messages
          :append (message-tool-calls msg))))

(defmethod agent/llm/interface:response-finish-reason ((backend ollama-backend) response)
  (let ((tool-calls (agent/llm/interface:get-response-tool-calls backend response)))
    (if tool-calls
        agent/llm/interface:+finish-tool-calls+
        agent/llm/interface:+finish-stop+)))

(defmethod agent/llm/interface:get-response-message ((backend ollama-backend) response)
  (let ((messages (getf response :messages)))
    ;; Combine all messages into one assistant message for history
    (make-message :role "assistant"
                  :content (agent/llm/interface:get-response-content backend response)
                  :thinking (let ((thinkings (remove nil (mapcar #'message-thinking messages))))
                              (when thinkings
                                (apply #'concatenate 'string thinkings)))
                  :tool-calls (agent/llm/interface:get-response-tool-calls backend response))))

;;; Tool call protocol implementation

(defmethod agent/llm/interface:tool-call-id ((backend ollama-backend) tool-call)
  ;; Ollama doesn't use tool call IDs, generate one
  (or (gethash "id" tool-call)
      (format nil "call_~A" (random 100000))))

(defmethod agent/llm/interface:tool-call-name ((backend ollama-backend) tool-call)
  (let ((function (gethash "function" tool-call)))
    (gethash "name" function)))

(defmethod agent/llm/interface:tool-call-arguments ((backend ollama-backend) tool-call)
  (let ((function (gethash "function" tool-call)))
    (gethash "arguments" function)))

;;; Tool protocol implementation

(defmethod agent/llm/interface:execute-tool ((tool tool) arguments)
  (funcall (tool-function tool) arguments))

(defmethod agent/llm/interface:tool-to-api-format ((backend ollama-backend)
                                               (tool agent/llm/interface:tool))
  (hash :type "function"
        :function (hash :name (agent/llm/interface:tool-name tool)
                        :description (agent/llm/interface:tool-description tool)
                        :parameters (hash
                                     :type "object"
                                     :properties (or (agent/llm/interface:tool-parameters tool)
                                                     (make-hash-table :test 'equal))))))

;;; Ollama-specific tool executor using registered tools

(defun ollama-tool-executor (name args)
  "Execute a tool from the global *tools* registry."
  (let ((tool (find-tool name)))
    (when tool
      (funcall (tool-function tool) args))))
