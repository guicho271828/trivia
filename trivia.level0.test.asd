(asdf:defsystem :trivia.level0.test
  :description "Test system of trivia.level0"
  :depends-on (:fiveam :trivia.level0)
  :pathname "test/"
  :components ((:file "level0")))
