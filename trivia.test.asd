(defsystem "trivia.test"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Runs the level2 test with various optimizers"
  :depends-on ("trivia.level2.test")
  :in-order-to ((test-op (test-op "trivia.trivial.test"))))
