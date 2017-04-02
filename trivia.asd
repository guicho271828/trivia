(defsystem "trivia"
  :description "NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase"
  ;; :long-description #.(read-file-string (subpathname *load-pathname* "README.org"))
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on ("trivia.level2")
  :in-order-to ((test-op (test-op "trivia.level2"))))
