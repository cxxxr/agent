(:mallet-config
 (:extends :all)  ; or :all

 ;; Ignore files/directories (uses glob patterns)
 (:ignore ".direnv/**")

 ;; Enable rules with options
 (:enable :line-length :max 100)
 (:enable :consecutive-blank-lines :max 2)

 ;; Disable specific rules
 (:disable :constant-naming))
