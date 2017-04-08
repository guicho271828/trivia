
(defsystem trivia.cffi
  :description "cffi foreign slot access extension for trivia"
  :version "0.1"
  :license "LLGPL"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :depends-on (:trivia :cffi)
  :pathname "cffi/"
  :components ((:file "cffi"))
  :in-order-to ((test-op (test-op :trivia.cffi.test))))

