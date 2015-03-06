
(defpackage :optima.level2.test
  (:use :cl :fiveam :alexandria
        :optima.level0
        :optima.level1
        :optima.level2))
(in-package :optima.level2.test)

(def-suite :optima.level2)
(in-suite :optima.level2)


(test defpattern
  (finishes (print (pattern-expand '(cons a b)))))

(defun exunion (union types)
  (set-equal (exhaustive-union types) union))

(test exhaustive-union
  (is (exunion '(list) '(cons null)))
  (is (exunion '(list array) '(cons null array)))
  (is (exunion '(sequence) '(cons null sequence)))
  (is (exunion '(sequence) '(cons null vector))) ; file:///usr/share/doc/hyperspec/Body/t_seq.htm
  ;; these types may not necessarily a class in common lisp, but many implementations do.
  (is (exunion '(array sequence)
               '(array simple-string sequence)))
  (is (exunion '(simple-array)
               '(simple-vector simple-array simple-string))))

(defun subset (expected actual)
  (subsetp expected actual :test #'equal))

(test test-type
  (is (eq nil (test-type '(ababa ?))))
  (is (eq 'null (test-type '(eql nil ?))))
  (is (eq 'string (test-type '(stringp ?))))

  (is (subset '((TYPEP ? 'FIXNUM)) (type-tests 'fixnum)))

  (is (subset '((TYPEP ? 'integer)
                (integerp ?))
              (type-tests 'integer)))

  ;; more inference on integers, e.g., (< 0 ? 4), should be added
  (is (subset '((TYPEP ? '(mod 5))
                (TYPEP ? '(integer 0 4)))
              (type-tests '(mod 5))))
  (is (not (subset '((INTEGERP ?))
                   (type-tests '(mod 5))))))


(eval-when (:load-toplevel :execute)
  (run! :optima.level2))

