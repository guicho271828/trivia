(asdf:defsystem :fivepm
  :description "Very Fast Pattern Matching Library"
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria
               :anaphora
               :closer-mop)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "equal")
                             (:file "pattern")
                             (:file "compiler")
                             (:file "match")))))
