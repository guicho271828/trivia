(defsystem :trivia.level0.test
    :author "Masataro Asai"
    #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
    :description "Test system of trivia.level0"
    :depends-on (:fiveam :trivia.level0)
    :pathname "test/"
    :components ((:file "level0"))
    :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :trivia.level0)"))))
