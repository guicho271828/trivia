(defsystem optima
  :description "(Legacy) Optimized Pattern Matching Library --- Compatibility layer for Trivia"
  :version "1.0"
  :author "Tomohiro Matsuyama"
  :maintainer "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia)
  :pathname "optima-compat/"
  :components ((:file "package")))

