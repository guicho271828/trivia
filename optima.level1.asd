(asdf:defsystem :optima.level1
  :description "Core patterns"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:optima.level0)
  :pathname "level1/"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (load-op :optima.level1.test))))
