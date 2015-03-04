(asdf:defsystem :optima.ppcre
  :description "PPCRE extention of optima"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:optima.level2 :cl-ppcre)
  :pathname "ppcre/"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (load-op :optima.ppcre.test))))
