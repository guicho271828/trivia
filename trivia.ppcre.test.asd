(asdf:defsystem :trivia.ppcre.test
    :author "Masataro Asai"
    :mailto "guicho2.71828@gmail.com"
    :description "Test system of trivia.ppcre"
    :depends-on (:fiveam :trivia.ppcre)
    :pathname "test/"
    :serial t
    :components ((:file "ppcre")))
