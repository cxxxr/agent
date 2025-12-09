(defsystem "agent"
  :depends-on ("dexador"
               "yason"
               "alexandria"
               "cl-ansi-text"
               "com.inuoe.jzon")
  :serial t
  :components ((:module "llm"
                :pathname "src/llm"
                :serial t
                :components ((:file "interface")
                             (:file "openrouter")
                             (:file "ollama")))
               (:file "example" :pathname "src/example")))
