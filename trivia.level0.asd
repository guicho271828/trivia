(defsystem "trivia.level0"
  :description "Bootstrapping Pattern Matching Library for implementing Trivia"
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("alexandria")
  :pathname "level0/"
  :serial t
  :components ((:file "package")
               (:file "impl")))
