(defsystem trivia.trivial
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia.level2)
  :description "Base level system of Trivia with a trivial optimizer.
 Systems that intend to enhance Trivia should depend on this package, not the TRIVIA system,
 in order to avoid the circular dependency.")
