(defsystem "trivia.test"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of trivia"
  :depends-on ("fiveam" "trivia"
                        "trivia.ppcre"
                        "trivia.quasiquote"
                        "trivia.cffi"
                        "trivia.fset"
                        "optima")
  :pathname "test/"
  :components ((:file "base")
               (:file "level0")
               (:file "level1")
               (:file "level2")
               (:file "ppcre")
               (:file "quasiquote")
               (:file "cffi")
               (:file "fset"))
  :serial t
  :perform (test-op (o c) (eval (read-from-string "(5am:run! :trivia)"))))
