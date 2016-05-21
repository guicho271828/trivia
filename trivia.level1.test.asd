(defsystem :trivia.level1.test
    :author "Masataro Asai"
    #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
    :description "Test system of trivia.level1"
    :depends-on (:fiveam :trivia.level1)
    :pathname "test/"
    :components ((:file "level1"))
    :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :trivia.level1)"))))
