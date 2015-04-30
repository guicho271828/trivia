(asdf:defsystem :trivia.level2
  :description "NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.level1
               :lisp-namespace
               :closer-mop
               )
  :pathname "level2/"
  :serial t
  :components ((:file "package")
               (:file "impl")
               (:file "derived")
               (:file "derived2")
               (:file "derived-numbers"))
  :in-order-to ((test-op (load-op :trivia.level2.test))))

