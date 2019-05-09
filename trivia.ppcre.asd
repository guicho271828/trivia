(defsystem "trivia.ppcre"
  :description "PPCRE extention of trivia"
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("trivia.trivial" "cl-ppcre")
  :pathname "ppcre/"
  :serial t
  :components ((:file "package")))
