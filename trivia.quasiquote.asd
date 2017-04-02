(defsystem "trivia.quasiquote"
  :description "fare-quasiquote extension for trivia"
  :version "0.1"
  :license "LLGPL"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :depends-on ("fare-quasiquote-readtable" "trivia")
  :pathname "quasiquote/"
  :components ((:file "quasiquote"))
  :in-order-to ((test-op (test-op "trivia.quasiquote.test"))))
