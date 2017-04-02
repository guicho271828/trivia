(defsystem "trivia.level2.test"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of trivia.level2"
  :depends-on ("fiveam" "trivia.level2")
  :pathname "test/"
  :serial t
  :components ((:file "suite")
               (:file "level2"))
  :perform (test-op (o c) (eval (read-from-string "(5am:run! :trivia)"))))
