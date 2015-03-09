(asdf:defsystem :trivia.ppcre.test
  :depends-on (:fiveam :trivia.ppcre)
  :pathname "test/"
  :serial t
  :components ((:file "ppcre")))
