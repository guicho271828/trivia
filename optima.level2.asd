(asdf:defsystem :optima.level2
  :description "Optimized Pattern Matching Library"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:optima.level1)
  :pathname "level2/"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (load-op :optima.level2.test))))
