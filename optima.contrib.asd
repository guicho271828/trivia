(asdf:defsystem :optima.contrib
  :description "Contribution library for optima"
  :long-description "Contribution library for optima.

Available Patterns
------------------

### PPCRE

    (ppcre REGEXP PATTERN*)

Matches REGEXP against the target string. Sub-PATTERNs will be used to
match the matched groups, if REGEXP matched.

Examples:

    (match \"2012-11-04\"
      ((ppcre \"^\\\\d{4}-\\\\d{2}-\\\\d{2}$\" year month day)
       (list year month day)))
    => (\"2012\" \"11\" \"04\")"
  :version "0.2"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:optima
               :cl-ppcre)
  :components ((:module "contrib"
                :serial t
                :components ((:file "package")
                             (:file "ppcre")))))
