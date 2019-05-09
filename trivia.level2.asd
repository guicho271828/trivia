(defsystem "trivia.level2"
  :description "NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase"
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("trivia.level1" "lisp-namespace" "closer-mop" "trivial-cltl2")
  :pathname "level2/"
  :serial t
  :components ((:file "package")
               (:file "impl")
               (:file "sugars")
               (:file "derived")
               (:file "derived-class")
               (:file "derived2")
               (:file "derived3")
               (:file "derived-numbers")
               (:file "arrays")
               (:file "inline-pattern")))
