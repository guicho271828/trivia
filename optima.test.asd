(asdf:defsystem :optima.test
  :depends-on (:eos :optima :optima.contrib)
  :components ((:file "test/suite")))
