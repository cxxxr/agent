;;;; agent/interface.lisp - Common interface for AI agent backends
;;;;
;;;; This file defines the common protocols that both openrouter.lisp and ollama.lisp
;;;; backends implement. Using CLOS generic functions allows polymorphic dispatch
;;;; across different backends.

(defpackage #:agent/interface
  (:use #:cl)
  (:export
   ;; Message protocol
   #:message
   #:message-role
   #:message-content
   #:message-tool-calls
   #:make-user-message
   #:make-assistant-message
   #:make-system-message
   #:make-tool-result-message

   ;; Tool protocol
   #:tool
   #:tool-name
   #:tool-description
   #:tool-parameters
   #:execute-tool
   #:tool-to-api-format

   ;; Backend protocol
   #:backend
   #:backend-model
   #:chat-completion
   #:get-response-content
   #:get-response-tool-calls
   #:get-response-message
   #:response-finish-reason

   ;; Tool call protocol
   #:tool-call-id
   #:tool-call-name
   #:tool-call-arguments

   ;; Message conversion
   #:message-to-api-format

   ;; Conversation protocol
   #:conversation
   #:conversation-backend
   #:conversation-messages
   #:conversation-tools
   #:conversation-add-message
   #:conversation-chat

   ;; Agent loop
   #:run-agent-loop

   ;; Utility functions
   #:find-tool-by-name

   ;; Finish reasons
   #:+finish-stop+
   #:+finish-tool-calls+
   #:+finish-length+
   #:+finish-error+))

(in-package #:agent/interface)

;;; ==========================================================================
;;; Finish Reason Constants
;;; ==========================================================================

(defconstant +finish-stop+ :stop
  "Normal completion - model finished generating.")

(defconstant +finish-tool-calls+ :tool-calls
  "Model is requesting tool execution.")

(defconstant +finish-length+ :length
  "Response was truncated due to max tokens.")

(defconstant +finish-error+ :error
  "An error occurred during generation.")

;;; ==========================================================================
;;; Message Protocol
;;; ==========================================================================

(defclass message ()
  ((role :initarg :role
         :accessor message-role
         :type string
         :documentation "Message role: user, assistant, system, or tool")
   (content :initarg :content
            :accessor message-content
            :initform nil
            :documentation "Text content of the message")
   (tool-calls :initarg :tool-calls
               :accessor message-tool-calls
               :initform nil
               :documentation "List of tool calls (for assistant messages)"))
  (:documentation "Base class for chat messages."))

(defgeneric make-user-message (backend content)
  (:documentation "Create a user message for the given backend."))

(defgeneric make-assistant-message (backend content &key tool-calls)
  (:documentation "Create an assistant message for the given backend."))

(defgeneric make-system-message (backend content)
  (:documentation "Create a system message for the given backend."))

(defgeneric make-tool-result-message (backend tool-call-id result)
  (:documentation "Create a tool result message for the given backend."))

(defgeneric message-to-api-format (backend message)
  (:documentation "Convert a message to the backend's API format."))

;;; ==========================================================================
;;; Tool Protocol
;;; ==========================================================================

(defclass tool ()
  ((name :initarg :name
         :accessor tool-name
         :type string
         :documentation "Unique name of the tool")
   (description :initarg :description
                :accessor tool-description
                :type string
                :documentation "Human-readable description of what the tool does")
   (parameters :initarg :parameters
               :accessor tool-parameters
               :initform nil
               :documentation "JSON Schema for tool parameters"))
  (:documentation "Base class for tool definitions."))

(defgeneric execute-tool (tool arguments)
  (:documentation "Execute a tool with the given arguments hash table.
Returns the result as a string or a hash table with :error key on failure."))

(defgeneric tool-to-api-format (backend tool)
  (:documentation "Convert a tool definition to the backend's API format."))

;;; ==========================================================================
;;; Backend Protocol
;;; ==========================================================================

(defclass backend ()
  ((model :initarg :model
          :accessor backend-model
          :type string
          :documentation "Model identifier for this backend"))
  (:documentation "Base class for LLM backends (OpenRouter, Ollama, etc.)."))

(defgeneric chat-completion (backend messages &key tools)
  (:documentation "Send messages to the backend and get a response.
MESSAGES is a list of message objects.
TOOLS is an optional list of tool objects.
Returns a backend-specific response object."))

(defgeneric get-response-content (backend response)
  (:documentation "Extract the text content from a backend response."))

(defgeneric get-response-tool-calls (backend response)
  (:documentation "Extract tool calls from a backend response.
Returns a list of tool call objects, or NIL if none."))

(defgeneric response-finish-reason (backend response)
  (:documentation "Get the finish reason from a response.
Returns one of: +finish-stop+, +finish-tool-calls+, +finish-length+, +finish-error+"))

(defgeneric get-response-message (backend response)
  (:documentation "Extract the full assistant message from response for history."))

;;; ==========================================================================
;;; Tool Call Protocol
;;; ==========================================================================

(defgeneric tool-call-id (backend tool-call)
  (:documentation "Get the unique ID from a tool call."))

(defgeneric tool-call-name (backend tool-call)
  (:documentation "Get the function/tool name from a tool call."))

(defgeneric tool-call-arguments (backend tool-call)
  (:documentation "Get the arguments from a tool call as a hash table."))

;;; ==========================================================================
;;; Conversation Protocol
;;; ==========================================================================

(defclass conversation ()
  ((backend :initarg :backend
            :accessor conversation-backend
            :documentation "The backend to use for this conversation")
   (messages :initarg :messages
             :accessor conversation-messages
             :initform nil
             :type list
             :documentation "List of messages in the conversation")
   (tools :initarg :tools
          :accessor conversation-tools
          :initform nil
          :type list
          :documentation "List of tools available in this conversation")
   (tool-executor :initarg :tool-executor
                  :accessor conversation-tool-executor
                  :initform nil
                  :documentation "Function to execute tools: (name args) -> result"))
  (:documentation "A multi-turn conversation with tool support."))

(defgeneric conversation-add-message (conversation message)
  (:documentation "Add a message to the conversation history."))

(defgeneric conversation-chat (conversation user-input)
  (:documentation "Send a user message and get the assistant's response.
Handles tool calls automatically. Returns the final text response."))

;;; ==========================================================================
;;; Default Implementations
;;; ==========================================================================

(defmethod conversation-add-message ((conv conversation) message)
  "Default implementation: append message to the end of the list."
  (setf (conversation-messages conv)
        (append (conversation-messages conv) (list message))))

;;; ==========================================================================
;;; Agent Loop
;;; ==========================================================================

(defgeneric run-agent-loop (backend messages tools tool-executor)
  (:documentation "Run the agent loop until completion.
Sends messages, handles tool calls, and returns the final response.

BACKEND is the LLM backend to use.
MESSAGES is the initial list of messages.
TOOLS is a list of available tools.
TOOL-EXECUTOR is a function (name args) -> result for executing tools.

Returns the final text response from the model."))

(defmethod run-agent-loop ((backend backend) messages tools tool-executor)
  "Default agent loop implementation using the backend protocol."
  (let ((current-messages (copy-list messages)))
    (loop
      (let ((response (chat-completion backend current-messages :tools tools)))
        (case (response-finish-reason backend response)
          (#.+finish-stop+
           (return (get-response-content backend response)))
          (#.+finish-tool-calls+
           ;; Add assistant message to history
           (setf current-messages
                 (append current-messages
                         (list (get-response-message backend response))))
           ;; Execute each tool call and add results
           (dolist (tc (get-response-tool-calls backend response))
             (let* ((id (tool-call-id backend tc))
                    (name (tool-call-name backend tc))
                    (args (tool-call-arguments backend tc))
                    (result (funcall tool-executor name args)))
               (setf current-messages
                     (append current-messages
                             (list (make-tool-result-message
                                    backend id
                                    (or result "Tool not found"))))))))
          (otherwise
           (return (format nil "Unexpected finish reason: ~A"
                           (response-finish-reason backend response)))))))))

;;; ==========================================================================
;;; Utility Functions
;;; ==========================================================================

(defun find-tool-by-name (tools name)
  "Find a tool by name in a list of tools."
  (find name tools :key #'tool-name :test #'string=))
