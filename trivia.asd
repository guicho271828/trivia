(defsystem :trivia
  :description "NON-optimized pattern matcher compatible with OPTIMA, with extensible optimizer interface and clean codebase"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"README.org"
                                               (or *load-pathname* *compile-file-pathname*)))
                        (let ((str (make-string (file-length stream))))
                          (read-sequence str stream)
                          str))
  :version "0.1"
  :author "Masataro Asai"
  #+asdf3 :mailto #+asdf3 "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia.level2)
  :in-order-to ((test-op (test-op :trivia.level2))))
