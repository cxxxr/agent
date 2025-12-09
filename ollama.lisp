(defpackage #:agent/ollama
  (:use #:cl))
(in-package #:agent/ollama)

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
