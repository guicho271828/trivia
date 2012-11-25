(asdf:defsystem :optima.test
  :depends-on (:eos :optima :optima.ppcre)
  :components ((:file "test/suite")))
