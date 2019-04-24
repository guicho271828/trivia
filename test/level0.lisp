
(defpackage :trivia.level0.test
  (:use :cl :fiveam :trivia.level0))
(in-package :trivia.level0.test)

(in-suite :trivia.level0)

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
  ;; invalid list* pattern: needs at least 1 arg
  (signals error
    (macroexpand '(match0 x ((list*) t))))

  (match0 '(:a nil)
    ((list* a b c)
     (is (eq :a a))
     (is (eq nil b))
     (is (eq nil c)))
    (_ (fail))))



