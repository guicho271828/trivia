(asdf:defsystem :trivia.ppcre.test
  :description "Test system of trivia.ppcre"
  :depends-on (:fiveam :trivia.ppcre)
  :pathname "test/"
  :serial t
  :components ((:file "ppcre")))
