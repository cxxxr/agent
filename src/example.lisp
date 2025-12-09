;;;; agent/example.lisp - Usage examples for agent/llm/interface
;;;;
;;;; This file demonstrates how to use the common interface to work with
;;;; both OpenRouter and Ollama backends interchangeably.

(defpackage #:agent/example
  (:use #:cl)
  (:export #:run-examples))
(in-package #:agent/example)

;;; ============================================================
;;; Example 1: Creating and Switching Backends
;;; ============================================================

(defun example-backend-creation ()
  "Demonstrate backend creation and model access."
  (let ((openrouter (agent/llm/openrouter:make-openrouter-backend
                     :model "openai/gpt-4o-mini"))
        (ollama (agent/llm/ollama:make-ollama-backend
                 :model "qwen3:32b")))
    (values
     (agent/llm/interface:backend-model openrouter)
     (agent/llm/interface:backend-model ollama))))

(defun example-backend-switching ()
  "Demonstrate runtime backend switching."
  (let ((current-backend nil))
    (flet ((switch-backend (type)
             (setf current-backend
                   (ecase type
                     (:openrouter (agent/llm/openrouter:make-openrouter-backend))
                     (:ollama (agent/llm/ollama:make-ollama-backend))))
             (format nil "Switched to ~A (model: ~A)"
                     (type-of current-backend)
                     (agent/llm/interface:backend-model current-backend))))
      (list
       (switch-backend :openrouter)
       (switch-backend :ollama)))))

;;; ============================================================
;;; Example 2: Message Creation (Backend-agnostic)
;;; ============================================================

(defun create-chat-messages (backend user-input &optional system-prompt)
  "Create messages using the common interface.
Works with any backend that implements the protocol."
  (if system-prompt
      (list (agent/llm/interface:make-system-message backend system-prompt)
            (agent/llm/interface:make-user-message backend user-input))
      (list (agent/llm/interface:make-user-message backend user-input))))

(defun example-message-creation ()
  "Demonstrate message creation with both backends."
  (let ((openrouter (agent/llm/openrouter:make-openrouter-backend))
        (ollama (agent/llm/ollama:make-ollama-backend)))
    (values
     (create-chat-messages openrouter "Hello!" "Be concise.")
     (create-chat-messages ollama "Hello!" "Be concise."))))

;;; ============================================================
;;; Example 3: Custom Tool Definition
;;; ============================================================

(defclass calculator-tool (agent/llm/interface:tool)
  ()
  (:default-initargs
   :name "calculator"
   :description "Perform basic arithmetic operations on Lisp expressions"
   :parameters (alexandria:plist-hash-table
                '("expression" (:type "string"
                                :description "A Lisp arithmetic expression like (+ 2 3)"))
                :test 'equal))
  (:documentation "A simple calculator tool that evaluates Lisp expressions."))

(defmethod agent/llm/interface:execute-tool ((tool calculator-tool) arguments)
  "Execute the calculator tool with given arguments."
  (let ((expr (gethash "expression" arguments)))
    (handler-case
        (let ((result (eval (read-from-string expr))))
          (format nil "~A" result))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun example-custom-tool ()
  "Demonstrate custom tool creation and execution."
  (let ((calc (make-instance 'calculator-tool)))
    (list
     :name (agent/llm/interface:tool-name calc)
     :description (agent/llm/interface:tool-description calc)
     :test-add (agent/llm/interface:execute-tool
                calc
                (alexandria:plist-hash-table '("expression" "(+ 2 3)") :test 'equal))
     :test-mul (agent/llm/interface:execute-tool
                calc
                (alexandria:plist-hash-table '("expression" "(* 4 5)") :test 'equal)))))

;;; ============================================================
;;; Example 4: Tool API Format Conversion
;;; ============================================================

(defun example-tool-api-format ()
  "Demonstrate tool conversion to backend-specific API format."
  (let ((openrouter (agent/llm/openrouter:make-openrouter-backend))
        (ollama (agent/llm/ollama:make-ollama-backend))
        (tool (make-instance 'calculator-tool)))
    (flet ((describe-format (backend)
             (let ((api-format (agent/llm/interface:tool-to-api-format backend tool)))
               (list :backend (type-of backend)
                     :type (gethash "type" api-format)
                     :function-name (gethash "name" (gethash "function" api-format))))))
      (list
       (describe-format openrouter)
       (describe-format ollama)))))

;;; ============================================================
;;; Example 5: Read File Tool
;;; ============================================================

(defclass read-file-tool (agent/llm/interface:tool)
  ()
  (:default-initargs
   :name "read_file"
   :description "Read the contents of a file at the specified path"
   :parameters (alexandria:plist-hash-table
                '("path" (:type "string"
                          :description "The absolute path to the file to read"))
                :test 'equal))
  (:documentation "Tool for reading file contents."))

(defmethod agent/llm/interface:execute-tool ((tool read-file-tool) arguments)
  "Read file contents."
  (let ((path (gethash "path" arguments)))
    (handler-case
        (uiop:read-file-string path)
      (error (e)
        (format nil "Error reading file: ~A" e)))))

;;; ============================================================
;;; Example 6: Complete Agent Setup
;;; ============================================================

(defun make-default-tools ()
  "Create a list of default tools."
  (list (make-instance 'calculator-tool)
        (make-instance 'read-file-tool)))

(defun make-tool-executor (tools)
  "Create a tool executor function for the given tools."
  (lambda (name args)
    (let ((tool (agent/llm/interface:find-tool-by-name tools name)))
      (if tool
          (agent/llm/interface:execute-tool tool args)
          (format nil "Unknown tool: ~A" name)))))

(defun example-agent-setup ()
  "Demonstrate complete agent setup with tools."
  (let* ((backend (agent/llm/openrouter:make-openrouter-backend
                   :model "openai/gpt-4o-mini"))
         (tools (make-default-tools))
         (executor (make-tool-executor tools))
         (messages (list (agent/llm/interface:make-system-message
                          backend
                          "You are a helpful assistant with access to tools.")
                         (agent/llm/interface:make-user-message
                          backend
                          "What is 2 + 2?"))))
    (list :backend (type-of backend)
          :model (agent/llm/interface:backend-model backend)
          :tools (mapcar #'agent/llm/interface:tool-name tools)
          :message-count (length messages)
          :executor-test (funcall executor "calculator"
                                   (alexandria:plist-hash-table
                                    '("expression" "(+ 2 2)")
                                    :test 'equal)))))

;;; ============================================================
;;; Example 7: Agent Loop (requires API key)
;;; ============================================================

(defun run-agent (backend user-message &key system-prompt tools tool-executor)
  "Run a complete agent interaction using the common interface.

BACKEND - An instance of agent/llm/interface:backend
USER-MESSAGE - The user's input string
SYSTEM-PROMPT - Optional system prompt
TOOLS - List of agent/llm/interface:tool instances
TOOL-EXECUTOR - Function (name args) -> result

Returns the final response from the model."
  (let ((messages (if system-prompt
                      (list (agent/llm/interface:make-system-message backend system-prompt)
                            (agent/llm/interface:make-user-message backend user-message))
                      (list (agent/llm/interface:make-user-message backend user-message)))))
    (agent/llm/interface:run-agent-loop backend messages tools tool-executor)))

;;; ============================================================
;;; Run All Examples
;;; ============================================================

(defun run-examples ()
  "Run all examples and print results."
  (format t "~%=== Example 1: Backend Creation ===~%")
  (multiple-value-bind (or-model ollama-model) (example-backend-creation)
    (format t "OpenRouter model: ~A~%" or-model)
    (format t "Ollama model: ~A~%" ollama-model))

  (format t "~%=== Example 2: Backend Switching ===~%")
  (dolist (result (example-backend-switching))
    (format t "~A~%" result))

  (format t "~%=== Example 3: Custom Tool ===~%")
  (let ((result (example-custom-tool)))
    (format t "Tool: ~A~%" (getf result :name))
    (format t "2 + 3 = ~A~%" (getf result :test-add))
    (format t "4 * 5 = ~A~%" (getf result :test-mul)))

  (format t "~%=== Example 4: Tool API Format ===~%")
  (dolist (result (example-tool-api-format))
    (format t "~A: type=~A, name=~A~%"
            (getf result :backend)
            (getf result :type)
            (getf result :function-name)))

  (format t "~%=== Example 5: Agent Setup ===~%")
  (let ((result (example-agent-setup)))
    (format t "Backend: ~A~%" (getf result :backend))
    (format t "Model: ~A~%" (getf result :model))
    (format t "Tools: ~A~%" (getf result :tools))
    (format t "Executor test (2+2): ~A~%" (getf result :executor-test)))

  (format t "~%=== All examples completed ===~%")
  t)

;;; ============================================================
;;; Usage Documentation
;;; ============================================================

#|
Quick Start Guide:

1. Load the system:
   (ql:quickload :agent)

2. Run examples:
   (agent/example:run-examples)

3. Create a backend:
   (defvar *backend* (agent/llm/openrouter:make-openrouter-backend))
   ;; or
   (defvar *backend* (agent/llm/ollama:make-ollama-backend))

4. Use common interface:
   (agent/llm/interface:make-user-message *backend* "Hello!")
   (agent/llm/interface:chat-completion *backend* messages :tools tools)
   (agent/llm/interface:run-agent-loop *backend* messages tools executor)

5. Define custom tools:
   (defclass my-tool (agent/llm/interface:tool) ()
     (:default-initargs :name "my_tool" :description "..."))

   (defmethod agent/llm/interface:execute-tool ((tool my-tool) args)
     ...)

The key benefit is that code written against agent/interface works
with both OpenRouter and Ollama backends without modification.
|#
