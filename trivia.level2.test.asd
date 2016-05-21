(defsystem :trivia.level2.test
    :author "Masataro Asai"
    #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
    :description "Test system of trivia.level2"
    :depends-on (:fiveam :trivia.level2)
    :pathname "test/"
    :serial t
    :components ((:file "suite")
                 (:file "level2"))
    :perform (test-op :after (op c)
                      (eval (read-from-string "(5am:run! :trivia)"))))
