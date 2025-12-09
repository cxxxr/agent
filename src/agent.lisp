(defpackage #:agent
  (:use #:cl)
  (:local-nicknames (#:llm #:agent/llm/interface)))
(in-package #:agent)

(defclass agent ()
  ((backend :initarg :backend 
            :reader agent-backend)
   (tools :initarg :tools
          :initform '()
          :reader agent-tools)
   (messages :initform '()
             :accessor agent-messages)))

(defmethod query ((agent agent) prompt)
  (alexandria:nconcf (agent-messages agent)
                     (list (llm:make-user-message (agent-backend agent)
                                                  prompt)))
  (let ((response
          (llm:run-agent-loop (agent-backend agent)
                              (agent-messages agent)
                              (agent-tools agent)
                              (lambda (name args)
                                (declare (ignore name args))))))
    (alexandria:nconcf (agent-messages agent)
                       (list (llm:make-assistant-message (agent-backend agent)
                                                         response)))
    response))
