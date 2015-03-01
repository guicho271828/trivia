
(defpackage :optima.level0.test
  (:use :cl :eos :optima.level0))
(in-package :optima.level0.test)

(def-suite :optima.level0)
(in-suite :optima.level0)

(defun testfn (x) 
  (match x
    ((list* :keyword _) :keyword)
    ((list* "string" _) "string")
    ((list* MOST-POSITIVE-FIXNUM _) MOST-POSITIVE-FIXNUM)
    ((list 'a b 2) b)
    ((list a b) (+ a b))
    ((list* _ b) b)))

(test level0
  (is (equal :keyword (testfn '(:keyword 3 2 1))))
  (is (equal "string" (testfn '("string" 3 2 1))))
  (is (equal MOST-POSITIVE-FIXNUM (testfn `(,MOST-POSITIVE-FIXNUM 3 2 1))))
  (is (= 3 (testfn '(a 3 2))))
  (is (= 5 (testfn '(3 2))))
  (is (equal '(3 2 1) (testfn '(a 3 2 1)))))

(eval-when (:load-toplevel :execute)
  (run! :optima.level0))

