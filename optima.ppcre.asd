(asdf:defsystem :optima.ppcre
  :description "(Legacy) CL-PPCRE support for optima --- Compatibility layer for Trivia"
  :version "1.0"
  :author "Tomohiro Matsuyama"
  :maintainer "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.ppcre)
  :pathname "optima-compat/"
  :components ((:file "ppcre")))
