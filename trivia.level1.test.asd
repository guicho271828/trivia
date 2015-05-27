(asdf:defsystem :trivia.level1.test
  :description "Test system of trivia.level1"
  :depends-on (:fiveam :trivia.level1)
  :pathname "test/"
  :components ((:file "level1")))
