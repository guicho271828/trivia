(asdf:defsystem :trivia.level2.test
  :depends-on (:fiveam :trivia.level2)
  :pathname "test/"
  :components ((:file "level2")
               (:file "suite")))
