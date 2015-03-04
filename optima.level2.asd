(asdf:defsystem :optima.level2
  :description "Optimized Pattern Matching Library"
  :long-description #.(with-open-file (stream (merge-pathnames
                                               #p"optima.level2.md"
                                               (or *load-pathname* *compile-file-pathname*)))
                        (let ((str (make-string (file-length stream))))
                          (read-sequence str stream)
                          str))
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:optima.level1
               :lisp-namespace
               :closer-mop)
  :pathname "level2/"
  :serial t
  :components ((:file "package")
               (:file "derived")
               (:file "optimizer")
               (:file "typerel")
               (:file "patterns"))
  :in-order-to ((test-op (load-op :optima.level2.test))))
