(defsystem trivia.balland2006
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia.trivial :type-i :iterate :alexandria)
  :pathname "balland2006/"
  :components ((:file "package")
               (:file "optimizer")
               (:file "column-swapping"))
  :serial t
  :description "Optimizer for Trivia based on (Balland 2006)"
  :in-order-to ((test-op (test-op trivia.test))))
