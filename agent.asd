(defsystem "agent"
  :depends-on ("dexador" 
               "yason"
               "alexandria"
               "cl-ansi-text"
               "com.inuoe.jzon")
  :serial t
  :components ((:file "openrouter")
               (:file "ollama")))
