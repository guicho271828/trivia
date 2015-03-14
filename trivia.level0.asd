(asdf:defsystem :trivia.level0
  :description "Bootstrapping Pattern Matching Library for implementing trivia"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:alexandria)
  :pathname "level0/"
  :serial t
  :components ((:file "package")
               (:file "impl"))
  :in-order-to ((test-op (load-op :trivia.level0.test))))
