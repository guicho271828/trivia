(asdf:defsystem :optima.test
  :depends-on (:eos :optima :optima.ppcre)
  :pathname "test/"
  :components ((:file "suite")))
