(defsystem :trivia.ppcre.test
    :author "Masataro Asai"
    #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
    :description "Test system of trivia.ppcre"
    :depends-on (:fiveam :trivia.ppcre)
    :pathname "test/"
    :serial t
    :components ((:file "ppcre"))
    :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :trivia.ppcre)"))))
