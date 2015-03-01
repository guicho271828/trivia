(asdf:defsystem :optima.level0
  :description "Bootstrapping Pattern Matching Library for implementing optima"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:alexandria)
  :pathname "level0/"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (load-op :optima.level0.test))))
