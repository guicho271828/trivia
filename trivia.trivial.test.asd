(defsystem "trivia.trivial.test"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Runs the level2 test with :trivial optimizer"
  :depends-on ("trivia.level2.test")
  :perform (test-op (o c)
                    (eval (read-from-string "(trivia:in-optimizer :trivial t)"))
                    (asdf:test-system "trivia.level2.test")))
