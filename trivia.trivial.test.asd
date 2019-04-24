(defsystem "trivia.trivial.test"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Runs the level2 test with :trivial optimizer"
  :depends-on ("trivia.trivial" "trivia.level2.test")
  :in-order-to ((test-op (test-op "trivia.level2.test"))))
