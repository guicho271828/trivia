(asdf:defsystem :trivia.level2.test
  :description "Test system of trivia.level2"
  :depends-on (:fiveam :trivia.level2)
  :pathname "test/"
  :components ((:file "level2")
               (:file "suite")))
