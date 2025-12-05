(defpackage :agent/openrouter
  (:use :cl)
  (:export #:*api-key*
           #:*base-url*
           #:chat-completion
           #:list-models
           #:make-message
           #:make-tool-message
           #:make-tool
           #:make-function-tool
           #:get-response-content
           #:get-tool-calls
           #:tool-call-id
           #:tool-call-name
           #:tool-call-arguments
           #:finish-reason
           #:get-assistant-message
           #:make-read-file-tool
           #:run-agent
           #:make-conversation
           #:conversation-chat
           #:conversation-messages
           #:api-error
           #:api-error-status
           #:api-error-body))

(in-package :agent/openrouter)

;;; Configuration

(defvar *api-key* (uiop:getenv "OPENROUTER_API_KEY")
  "OpenRouter API key. Defaults to OPENROUTER_API_KEY environment variable.")

(defvar *base-url* "https://openrouter.ai/api/v1"
  "OpenRouter API base URL.")

;;; Conditions

(define-condition api-error (error)
  ((status :initarg :status :reader api-error-status)
   (body :initarg :body :reader api-error-body))
  (:report (lambda (c stream)
             (format stream "OpenRouter API error (status ~A): ~A"
                     (api-error-status c)
                     (api-error-body c)))))

;;; Internal utilities

(defun make-headers (&optional extra-headers)
  "Create HTTP headers for OpenRouter API requests."
  (append
   `(("Authorization" . ,(format nil "Bearer ~A" *api-key*))
     ("Content-Type" . "application/json")
     ("HTTP-Referer" . "https://github.com/cl-ai-project")
     ("X-Title" . "Common Lisp Agent"))
   extra-headers))

(defun api-request (endpoint &key (method :get) body)
  "Make a request to the OpenRouter API."
  (let ((url (format nil "~A~A" *base-url* endpoint)))
    (multiple-value-bind (response-body status)
        (handler-case
            (dex:request url
                         :method method
                         :headers (make-headers)
                         :content (when body
                                    (com.inuoe.jzon:stringify body))
                         :want-stream nil)
          (dex:http-request-failed (e)
            (error 'api-error
                   :status (dex:response-status e)
                   :body (dex:response-body e))))
      (if (<= 200 status 299)
          (com.inuoe.jzon:parse response-body)
          (error 'api-error :status status :body response-body)))))

;;; Public API

(defun make-message (role content &key tool-calls)
  "Create a message object for chat completion.
ROLE should be :system, :user, or :assistant.
CONTENT is the message text.
TOOL-CALLS is optional list of tool calls (for assistant messages)."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "role" table) (string-downcase (string role)))
    (when content
      (setf (gethash "content" table) content))
    (when tool-calls
      (setf (gethash "tool_calls" table) (coerce tool-calls 'vector)))
    table))

(defun make-tool-message (tool-call-id content)
  "Create a tool result message.
TOOL-CALL-ID is the ID from the tool call.
CONTENT is the result of the tool execution."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "role" table) "tool")
    (setf (gethash "tool_call_id" table) tool-call-id)
    (setf (gethash "content" table) content)
    table))

(defun make-tool (type function-def)
  "Create a tool definition.
TYPE should be :function.
FUNCTION-DEF is a hash table with name, description, and parameters."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "type" table) (string-downcase (string type)))
    (setf (gethash "function" table) function-def)
    table))

(defun make-function-tool (name description parameters)
  "Create a function tool definition.
NAME is the function name (string).
DESCRIPTION is what the function does.
PARAMETERS is a JSON Schema hash table for the function arguments."
  (let ((func (make-hash-table :test #'equal)))
    (setf (gethash "name" func) name)
    (setf (gethash "description" func) description)
    (setf (gethash "parameters" func) parameters)
    (make-tool :function func)))

(defun chat-completion (messages &key
                                   (model "openai/gpt-4o-mini")
                                   temperature
                                   max-tokens
                                   top-p
                                   tools
                                   tool-choice
                                   stream)
  "Send a chat completion request to OpenRouter.

MESSAGES is a list of message objects created with MAKE-MESSAGE.
MODEL is the model identifier (default: openai/gpt-4o-mini).
TEMPERATURE controls randomness (0.0-2.0).
MAX-TOKENS limits the response length.
TOP-P controls nucleus sampling.
TOOLS is a list of tool definitions created with MAKE-TOOL or MAKE-FUNCTION-TOOL.
TOOL-CHOICE controls tool selection: :auto, :none, :required, or a specific tool name.
STREAM enables streaming responses (not yet implemented).

Returns the parsed JSON response as a hash table."
  (declare (ignore stream)) ; Streaming not implemented yet
  (let ((body (make-hash-table :test #'equal)))
    (setf (gethash "model" body) model)
    (setf (gethash "messages" body) (coerce messages 'vector))
    (when temperature
      (setf (gethash "temperature" body) temperature))
    (when max-tokens
      (setf (gethash "max_tokens" body) max-tokens))
    (when top-p
      (setf (gethash "top_p" body) top-p))
    (when tools
      (setf (gethash "tools" body) (coerce tools 'vector)))
    (when tool-choice
      (setf (gethash "tool_choice" body)
            (if (keywordp tool-choice)
                (string-downcase (string tool-choice))
                tool-choice)))
    (api-request "/chat/completions" :method :post :body body)))

(defun list-models ()
  "List available models from OpenRouter.
Returns the parsed JSON response containing model information."
  (api-request "/models"))

;;; Convenience functions

(defun get-response-content (response)
  "Extract the content from a chat completion response."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0))
         (message (gethash "message" first-choice)))
    (gethash "content" message)))

(defun get-tool-calls (response)
  "Extract tool calls from a chat completion response.
Returns a list of tool call hash tables, or NIL if none."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0))
         (message (gethash "message" first-choice))
         (tool-calls (gethash "tool_calls" message)))
    (when tool-calls
      (coerce tool-calls 'list))))

(defun tool-call-id (tool-call)
  "Get the ID from a tool call."
  (gethash "id" tool-call))

(defun tool-call-name (tool-call)
  "Get the function name from a tool call."
  (let ((function (gethash "function" tool-call)))
    (gethash "name" function)))

(defun tool-call-arguments (tool-call)
  "Get the arguments from a tool call as a parsed hash table."
  (let* ((function (gethash "function" tool-call))
         (args-string (gethash "arguments" function)))
    (com.inuoe.jzon:parse args-string)))

(defun finish-reason (response)
  "Get the finish reason from a response.
Returns :stop, :tool-calls, :length, or :content-filter."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0))
         (reason (gethash "finish_reason" first-choice)))
    (cond
      ((string= reason "stop") :stop)
      ((string= reason "tool_calls") :tool-calls)
      ((string= reason "length") :length)
      ((string= reason "content_filter") :content-filter)
      (t (intern (string-upcase reason) :keyword)))))

(defun get-assistant-message (response)
  "Extract the full assistant message from response for conversation history."
  (let* ((choices (gethash "choices" response))
         (first-choice (aref choices 0)))
    (gethash "message" first-choice)))

;;; Built-in tools

(defun make-read-file-tool ()
  "Create a read_file tool definition."
  (let ((params (make-hash-table :test #'equal))
        (props (make-hash-table :test #'equal))
        (path-prop (make-hash-table :test #'equal)))
    (setf (gethash "type" path-prop) "string")
    (setf (gethash "description" path-prop) "The absolute path to the file to read")
    (setf (gethash "path" props) path-prop)
    (setf (gethash "type" params) "object")
    (setf (gethash "properties" params) props)
    (setf (gethash "required" params) #("path"))
    (make-function-tool
     "read_file"
     "Read the contents of a file at the specified path"
     params)))

(defun execute-builtin-tool (name args)
  "Execute a built-in tool by name with given arguments."
  (cond
    ((string= name "read_file")
     (handler-case
         (uiop:read-file-string (gethash "path" args))
       (error (e) (format nil "Error: ~A" e))))
    (t nil)))

;;; Agent loop

(defun run-agent (user-message &key
                                 (tools (list (make-read-file-tool)))
                                 (tool-executor #'execute-builtin-tool)
                                 (model "openai/gpt-4o-mini")
                                 system-prompt)
  "Run a single-turn agent with tool calling support.

USER-MESSAGE is the initial user message.
TOOLS is a list of tool definitions (default: read_file tool).
TOOL-EXECUTOR is a function (name args) -> result for executing tools.
MODEL is the model to use.
SYSTEM-PROMPT is an optional system message.

Returns the final text response from the model."
  (let ((messages (if system-prompt
                      (list (make-message :system system-prompt)
                            (make-message :user user-message))
                      (list (make-message :user user-message)))))
    (loop
      (let ((response (chat-completion messages :tools tools :model model)))
        (case (finish-reason response)
          (:stop
           (return (get-response-content response)))
          (:tool-calls
           (setf messages (append messages (list (get-assistant-message response))))
           (dolist (tc (get-tool-calls response))
             (let* ((id (tool-call-id tc))
                    (name (tool-call-name tc))
                    (args (tool-call-arguments tc))
                    (result (funcall tool-executor name args)))
               (setf messages (append messages (list (make-tool-message id (or result "Tool not found")))))))
           )
          (otherwise
           (return (format nil "Unexpected finish reason: ~A"
                           (finish-reason response)))))))))

;;; Multi-turn conversation

(defstruct (conversation (:constructor %make-conversation))
  "A multi-turn conversation with tool support."
  (messages nil :type list)
  (tools nil :type list)
  (tool-executor #'execute-builtin-tool :type function)
  (model "openai/gpt-4o-mini" :type string))

(defun make-conversation (&key
                            (tools (list (make-read-file-tool)))
                            (tool-executor #'execute-builtin-tool)
                            (model "openai/gpt-4o-mini")
                            system-prompt)
  "Create a new conversation.

TOOLS is a list of tool definitions.
TOOL-EXECUTOR is a function (name args) -> result for executing tools.
MODEL is the model to use.
SYSTEM-PROMPT is an optional system message to start the conversation."
  (let ((conv (%make-conversation
               :tools tools
               :tool-executor tool-executor
               :model model)))
    (when system-prompt
      (push (make-message :system system-prompt) (conversation-messages conv)))
    conv))

(defun conversation-chat (conversation user-message)
  "Send a message in an existing conversation.

CONVERSATION is the conversation object.
USER-MESSAGE is the user's message string.

Returns the assistant's response text.
The conversation history is updated in place."
  (setf (conversation-messages conversation)
        (append (conversation-messages conversation)
                (list (make-message :user user-message))))
  (loop
    (let ((response (chat-completion
                     (conversation-messages conversation)
                     :tools (conversation-tools conversation)
                     :model (conversation-model conversation))))
      (case (finish-reason response)
        (:stop
         (let ((content (get-response-content response)))
           (setf (conversation-messages conversation)
                 (append (conversation-messages conversation)
                         (list (make-message :assistant content))))
           (return content)))
        (:tool-calls
         (setf (conversation-messages conversation)
               (append (conversation-messages conversation)
                       (list (get-assistant-message response))))
         (dolist (tc (get-tool-calls response))
           (let* ((id (tool-call-id tc))
                  (name (tool-call-name tc))
                  (args (tool-call-arguments tc))
                  (result (funcall (conversation-tool-executor conversation) name args)))
             (setf (conversation-messages conversation)
                   (append (conversation-messages conversation)
                           (list (make-tool-message id (or result "Tool not found"))))))))
        (otherwise
         (return (format nil "Unexpected finish reason: ~A"
                         (finish-reason response))))))))
