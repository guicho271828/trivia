(asdf:defsystem :optima.ppcre
  :description "CL-PPCRE support for optima"
  :version "1.0"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:optima
               :alexandria
               :cl-ppcre)
  :components ((:file "lib/ppcre")))
