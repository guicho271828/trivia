(defsystem "trivia.fset"
  :description "FSet extention of trivia"
  :version "0.1"
  :author "Manfred Bergmann"
  :mailto "mb@software-by-mabe.com"
  :license "LLGPL"
  :depends-on ("trivia.trivial" "fset")
  :pathname "fset/"
  :serial t
  :components ((:file "package")))
