
(defsystem trivia.cffi.test
  :description "test trivia.cffi"
  :version "0.1"
  :license "LLGPL"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :depends-on (:fiveam :trivia.cffi)
  :pathname "test/"
  :components ((:file "cffi"))
  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(5am:run! :trivia.cffi)"))))
