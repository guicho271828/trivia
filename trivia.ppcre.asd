(asdf:defsystem :trivia.ppcre
  :description "PPCRE extention of trivia"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.level2 :cl-ppcre)
  :pathname "ppcre/"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (load-op :trivia.ppcre.test))))
