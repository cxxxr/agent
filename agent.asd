(defsystem "agent"
  :depends-on ("dexador"
               "yason"
               "alexandria"
               "cl-ansi-text"
               "com.inuoe.jzon")
  :serial t
  :pathname "src"
  :components ((:file "interface")
               (:file "openrouter")
               (:file "ollama")
               (:file "example")))
