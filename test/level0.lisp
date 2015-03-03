
(defpackage :optima.level0.test
  (:use :cl :fiveam :optima.level0))
(in-package :optima.level0.test)

(def-suite :optima.level0)
(in-suite :optima.level0)

(defun testfn (x) 
  (match0 x
    ((list) :null)
    ((list* :keyword _) :keyword)
    ((list* "string" _) "string")
    ((list* MOST-POSITIVE-FIXNUM _) MOST-POSITIVE-FIXNUM)
    ((list 'a b 2) b)
    ((list a b) (+ a b))
    ((list* _ b) b)))

(test level0
  (is (equal :null (testfn nil)))
  (is (equal :keyword (testfn '(:keyword 3 2 1))))
  (is (equal "string" (testfn '("string" 3 2 1))))
  (is (equal MOST-POSITIVE-FIXNUM (testfn `(,MOST-POSITIVE-FIXNUM 3 2 1))))
  (is (= 3 (testfn '(a 3 2))))
  (is (= 5 (testfn '(3 2))))
  (is (equal '(3 2 1) (testfn '(a 3 2 1)))))

(test list*
  (signals error
    (macroexpand '(match0 x ((list*) t))))) ;; invalid list* pattern: needs at least 1 arg

(eval-when (:load-toplevel :execute)
  (run! :optima.level0))

