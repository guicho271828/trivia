
(defpackage :optima.level2.test
  (:use :cl :fiveam :optima.level2 :alexandria))
(in-package :optima.level2.test)

(def-suite :optima.level2)
(in-suite :optima.level2)


(test defpattern
  (finishes (print (pattern-expand '(cons a b)))))

;; (test test=
;;   (is-true
;;    (test= '(guard1 it1 (consp it1))
;;           '(guard1 it2 (consp it2)))))

(defun exunion (union types)
  (set-equal (exhaustive-union types) union))

(test exhaustive-union
  (is (exunion '(list) '(cons null)))
  (is (exunion '(list array) '(cons null array)))
  (is (exunion '(sequence) '(cons null sequence)))
  (is (exunion '(sequence) '(cons null vector))) ; file:///usr/share/doc/hyperspec/Body/t_seq.htm
  (is (exunion '(array sequence)
               '(array simple-string sequence)))
  (is (exunion '(simple-array)
               '(simple-vector simple-array simple-string))))


(eval-when (:load-toplevel :execute)
  (run! :optima.level2))

