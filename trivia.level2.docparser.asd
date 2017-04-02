(defsystem "trivia.level2.docparser"
  :description "docparser interface to trivia patterns"
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("trivia.level2" "docparser")
  :pathname "level2/"
  :serial t
  :components ((:file "docparser-interface")))
