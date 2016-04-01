
(defsystem trivia.quasiquote.test
  :description "test trivia.quasiquote"
  :version "0.1"
  :license "LLGPL"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :depends-on (:fiveam :trivia.quasiquote)
  :pathname "test/"
  :components ((:file "quasiquote"))
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(every #'fiveam::TEST-PASSED-P
                         (5am:run! :trivia.quasiquote))"))))
