
(defsystem trivia.quasiquote.test
  :description "test trivia.quasiquote"
  :version "0.1"
  :license "LLGPL"
  :author "Masataro Asai"
  #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
  :depends-on (:fiveam :trivia.quasiquote)
  :pathname "test/"
  :components ((:file "quasiquote"))
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(5am:run! :trivia.quasiquote)"))))
