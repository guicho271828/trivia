(defsystem "trivia.level1"
  :description "Core patterns of Trivia"
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("trivia.level0")
  :pathname "level1/"
  :serial t
  :components ((:file "package")
               (:file "impl")))
