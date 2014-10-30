(defpackage :optima.test
  (:use :cl :eos :optima :optima.extra :optima.ppcre)
  (:import-from :optima.core #:parse-pattern #:unparse-pattern)
  (:shadowing-import-from :optima #:fail))
(in-package :optima.test)

(def-suite optima-test)
(in-suite optima-test)

;;; Pattern syntax

(test roundtrip
  (macrolet
      ((check-roundtrip (pattern)
         `(is (equal ',pattern
                     (unparse-pattern (parse-pattern ',pattern))))))
    ;; constant
    (check-roundtrip 1)
    (check-roundtrip t)
    (check-roundtrip nil)
    (check-roundtrip :foo)
    (check-roundtrip 3.14)
    (check-roundtrip "foo")
    (check-roundtrip 'x)
    (check-roundtrip '(x . y))
    (check-roundtrip '(x y))
    (check-roundtrip '(1 :foo))
    ;; variable
    ; (check-roundtrip _) does not roundtrip, but that's probably OK.
    (check-roundtrip x)))

;;; Pattern matching

(defmacro is-match (arg pattern)
  `(is-true (match ,arg (,pattern t))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

(test empty
  (is-true (match 1 (_ (fail)) (_ t))))

(test constant-pattern
  ;; integer
  (is-match 1 1)
  ;; t
  (is-match t t)
  ;; nil
  (is-match nil nil)
  ;; keyword
  (is-match :foo :foo)
  ;; float
  (is-match 3.14 3.14)
  (is-not-match 0.999 1)
  ;; string
  (is-match "foo" "foo")
  ;; complex
  (is-match '(1 t 3.14 :foo "foo") '(1 t 3.14 :foo "foo")))

(test variable-pattern
  ;; simple bind
  (is (eql (match 1 (x x)) 1))
  ;; anonymous bind
  (is-match 1 _)
  ;; complex bind
  (is (eql (match '(1 2 3)
             ((list x y z) (+ x y z)))
           6)))

(test place-pattern
  ;; level 0
  (let ((z 1))
    (match z ((place x) (incf x)))
    (is (eql z 2)))
  ;; level 1
  (let ((z (cons 1 2)))
    (match z
      ((cons (place x) y)
       (incf x)
       (incf y)))
    (is (equal z (cons 2 2))))
  ;; level 2
  (let ((z (list (vector 1))))
    (match z
      ((list (vector (place x)))
       (incf x)))
    (is (equalp z (list (vector 2))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass person ()
    ((name :initarg :name)
     (age :initarg :age)))

  (defstruct (point (:predicate point-p))
    x y))

(test constructor-pattern
  ;; cons
  (is-match (cons 1 2) (cons 1 2))
  ;; assoc
  (is-match '((1 . 2)) (assoc 1 2))
  (is-match '((1 . 2) (3 . 4)) (assoc 3 4))
  (is-match '(1 (2 . 3)) (assoc 2 3))
  (is-match '((a . 1)) (assoc a 1))
  (is-not-match 1 (assoc 1 2))
  (is-not-match '((1 . 2)) (assoc 3 4))
  (is-not-match '((1 . 2) (3 . 4)) (assoc 3 5))
  (is-match '(("a" . 1)) (assoc "A" 1 :test string-equal))
  ;; property
  (is-match '(:a 1) (property :a 1))
  (is-match '(:a 1 :b 2) (property :a 1))
  (is-match '(:a 1 2) (property :a 1))
  (is-match '(1 2 :b 3) (property :b 3))
  (is-match '(a 1) (property a 1))
  (is-not-match 1 (property :a 1))
  (is-not-match '(:a 1) (property :b 1))
  (is-not-match '(:a 1 :b 2) (property :b 3))
  ;; vector
  (is-match (vector 1 2) (vector 1 2))
  ;; simple-vector
  (is-match (vector 1 2) (simple-vector 1 2))
  ;; class
  (let ((person (make-instance 'person :name "Bob" :age 31)))
    (is (equal (match person
                 ((person name age) (list name age)))
               '("Bob" 31)))
    (is-match person (person))
    (is-match person (person (name "Bob") (age 31)))
    (is-match person (person (name "Bob")))
    (is-match person (person (age 31)))
    (is-not-match person (person (name "Alice")))
    (is-not-match person (person (age 49)))
    (is-not-match 1 (person))
    ;; make-instance style
    (is-match person (person :name "Bob" :age 31))
    (is-not-match person (person :name "Bob" :age 49)))
  ;; structure
  (let ((point (make-point :x 1 :y 2)))
    (is (equal (match point
                 ((point- x y) (list x y)))
               '(1 2)))
    (is-match point (point))
    (is-match point (point (x 1) (y 2)))
    (is-match point (point (x 1)))
    (is-match point (point (y 2)))
    (is-not-match point (point (x 2)))
    (is-not-match 1 (point-))
    (is-match point (point-))
    (is-match point (point- (x 1) (y 2)))
    (is-match point (point- (x 1)))
    (is-match point (point- (y 2)))
    (is-not-match point (point- (x 2)))
    (is-not-match 1 (point-))
    ;; make-instance style
    (is-match point (point- :x 1 :y 2))
    (is-not-match point (point- :x 2 :y 2))))

(test derived-pattern
  ;; list
  (is-match '() (list))
  (is-match '(1 2 3) (list 1 2 3))
  (is-not-match '() (list _))
  (is-not-match 5 (list _))
  ;; list*
  (is-match '() (list* _))
  (is-match '(1 2 3) (list* 1 2 (list 3)))
  (is-match '(1 2 3) (list* _))
  (is-not-match 5 (list* _))
  ;; alist
  (is-match '((1 . 2) (2 . 3) (3 . 4)) (alist (3 . 4) (1 . 2)))
  ;; plist
  (is-match '(:a 1 :b 2 :c 3) (plist :c 3 :a 1))
  ;; satisfies
  (is-match 1 (satisfies numberp))
  (is-not-match 1 (satisfies stringp))
  ;; eq, eql, equal, equalp
  (is-match :foo (eq :foo))
  (is-match 1 (eql 1))
  (is-match "foo" (equal "foo"))
  (is-match #(1) (equalp #(1)))
  ;; type
  (is-match 1 (type number))
  (is-match "foo" (type string))
  (is-match :foo (type (eql :foo)))
  (is-match 1 (type (or string number)))
  (is-not-match 1 (type (not t))))

(test guard-pattern
  (is-match 1 (guard _ t))
  (is-not-match 1 (guard _ nil))
  (is-match 1 (guard 1 t))
  (is-not-match 1 (guard 1 nil))
  (is-not-match 1 (guard 2 t))
  (is-match 1 (guard x (eql x 1)))
  ;; lift
  (is-match 1 (and x (guard y (eql x y))))
  (is-match 1 (and (guard x (eql x y)) y))
  (is-not-match 1 (and x (guard 2 (eql x 1))))
  (is-not-match 1 (and x (guard y (not (eql x y)))))
  (is-match '(1 1) (list x (guard y (eql x y))))
  (is-match '(1 1) (list (guard x (oddp x)) (guard y (eql x y))))
  (is-not-match '(1 2) (list (guard x (oddp x)) (guard y (eql x y))))
  (is-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 1))))
  (is-not-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 2))))
  (is-match 1 (or (list x) (guard x (oddp x))))
  (is-match '(1) (or (list x) (guard x (oddp x))))
  (is-not-match 1 (or (list x) (guard x (evenp x))))
  (is-match '(1) (list (or (list x) (guard x (oddp x)))))
  (is-not-match '(1) (or (list (or (list x) (guard x (evenp x))))
                         (list (guard x (eql x 2)))))
  (is-match '(2) (or (list (or (list x) (guard x (evenp x))))
                     (list (guard x (eql x 2)))))
  (is-match (list 2 2) (list (or (guard x (eql x 1))
                                 (guard x (eql x 2)))
                             (or (guard y (eql y 1))
                                 (guard y (eql y 2)))))
  (is-match 2 (or (guard x (eql x 1))
                  (guard x (eql x 2))))
  (is-match 5 (or (guard x (eql x 1))
                  (or (guard x (eql x 2))
                      (or (guard x (eql x 3))
                          (or (guard x (eql x 4))
                              (guard x (eql x 5)))))))
  (is-match '(((1)))
            (list (or (guard x (eql x 1))
                      (list (or (guard x (eql x 1))
                                (list (or (guard x (eql x 1))
                                          (list)))))))))

(test not-pattern
  (is-match 1 (not 2))
  (is-not-match 1 (not 1))
  ;; double negation
  (is-not-match 1 (not (not (not 1))))
  (is-match 1 (not (not (not (not 1)))))
  ;; complex
  (is-match 1 (not (guard it (consp it))))
  (is (equal (let ((it 1))
               (match 2 ((not (guard it (eql it 3))) it)))
             1)))

(test or-pattern
  (is-not-match 1 (or))
  (is-match 1 (or 1))
  (is-match 1 (or 1 1))
  (is-match 1 (or 1 2))
  (is-match 1 (or 2 1))
  (is-not-match 1 (or 2 3))
  (is-match 1 (or _))
  (is-match 1 (or _ _))
  (is-match 1 (or 2 (or 1)))
  (is-match 1 (or (or 1)))
  (is-not-match 1 (or (or (not 1))))
  ;; unshared variables
  (is (equal (match 1 ((or 1 (list x) y) (list x y)))
             '(nil nil)))
  (is (equal (match 2 ((or 1 (list x) y) (list x y)))
             '(nil 2)))
  (is (equal (match '(1) ((or 1 (list x) y) (list x y)))
             '(1 nil))))

(test and-pattern
  (is-match 1 (and))
  (is-match 1 (and 1))
  (is-match 1 (and 1 1))
  (is-not-match 1 (and 1 2))
  (is-match 1 (and _))
  (is-match 1 (and _ _))
  (is-match 1 (and 1 (and 1)))
  (is-match 1 (and (and 1)))
  (is (eql (match 1 ((and 1 x) x)) 1))
  (is-not-match 1 (and (and (not 1))))
  (is-match 1 (and (type number) (type integer)))
  ;; complex
  (is-true (match 1
             ((and 1 2) nil)
             (1 t)
             (2 nil)))
  (is (eql (match (list 1 2)
             ((list (and 1 x) 2) x))
           1))
  (is-true (match (list 1 2 3)
             ((list (and 1 2 3 4 5) 2))
             ((list (and 1 (type number)) 3 3))
             ((list (and 1 (type number)) 2 3) t)))
  (is-true (match (list 2 2)
             ((list (and 2 (type number)) 3))
             ((list (and 2 (type number)) 2 3))
             ((list (and 1 (type number)) 2))
             ((list (and 2 (type number)) 2) t))))

(test match
  ;; empty
  (is-false (match 1))
  ;; values
  (is (equal (multiple-value-list (match 1 (1 (values 1 2 3))))
             '(1 2 3)))
  ;; mixture
  (is-true (match (cons 1 2)
             ((cons 2 1) 2)
             (_ t)
             ((cons 1 2) nil)))
  ;; fail
  (is-false (match 1
              (1 (fail))))
  (is-true (match 1
              (1 (fail))
              (1 t)))
  (is-true (match 1
             (1 (fail))
             (1 t)
             (_ nil)))
  (is (eql (match (cons 1 2)
             ((cons x y)
              (if (eql x 1)
                  (fail)
                  y))
             ((cons x 2)
              x))
           1))
  ;; linear pattern
  (signals error
    (macroexpand
     '(match (cons 1 2)
       ((cons x x) t))))
  (signals error
    (macroexpand
     '(match (list 1 (list 2 3))
       ((list x (list y x)) t))))
  (signals error
    (macroexpand
     '(match (list 1 (list 2 3))
       ((list x (list y y)) t))))
  (signals error
    (macroexpand
     '(match 1
       ((and x x) t))))
  (is-match 1 (and _ _))
  (is-match 1 (or _ _))
  ;; declarations
  (is-true (match 1
             (1 (declare (ignore)) t)))
  (is-true (match 1
             (1 when t (declare (ignore)) t)))
  (is-true (match 1
             ((guard 1 t) (declare (ignore)) t)))
  ;; syntax sugar
  (is-true (match 1 (_ when t t)))
  (is-true (match 1 (_ unless nil t))))

(test multiple-value-match
  (is (eql (multiple-value-match (values 1 2)
             ((2) 1)
             ((1 y) y))
           2))
  ;; fail
  (is-true (multiple-value-match (values 1 2)
             ((1 2) (fail))
             ((_ _) t)))
  ;; linear pattern
  (signals error
    (macroexpand
     '(multiple-value-match (values 1 2)
       ((x x) t))))
  (is-true (multiple-value-match 1
             ((_ _) t))))

(test ematch
  (is-true (ematch 1 (1 t)))
  (signals match-error
    (ematch 1 (2 t)))
  ;; fail
  (is-true (ematch 1
             (1 (fail))
             (_ t)))
  (signals match-error
    (ematch 1
      (1 (fail))
      (1 (fail))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql (handler-case (ematch (f) (0 t))
                 (match-error (e)
                   (first (match-error-values e))))
               1)))))

(test multiple-value-ematch
  (signals match-error
    (multiple-value-ematch (values 1 2)
      ((2 1) t)))
  ;; fail
  (signals match-error
    (multiple-value-ematch (values 1 2)
      ((1 2) (fail))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql (handler-case (multiple-value-ematch (values (f)) ((0) t))
                 (match-error (e)
                   (first (match-error-values e))))
               1)))))

(test cmatch
  (is-true (cmatch 1 (1 t)))
  (is-false (handler-bind ((match-error #'continue))
              (cmatch 1 (2 t))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql (handler-case (cmatch (f) (0 t))
                 (match-error (e)
                   (first (match-error-values e))))
               1)))))

(test multiple-value-cmatch
  (is-false (handler-bind ((match-error #'continue))
              (multiple-value-cmatch (values 1 2)
                ((2 1) t))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql (handler-case (multiple-value-cmatch (values (f)) ((0) t))
                 (match-error (e)
                   (first (match-error-values e))))
               1)))))

;;; Contrib tests

(test ppcre
  (is-match "a" (ppcre "^a$"))
  (is-not-match "a" (ppcre "^b$"))
  (is-not-match 1 (ppcre "a"))
  (is-not-match :A (ppcre "A"))
  (is-true (match "a"
             ((ppcre "^(.)$")
              t)))
  (is (equal (match "a"
               ((ppcre "(a)" x y)
                (list x y)))
             '("a" nil)))
  (is (equal (match "2012-11-04"
               ((ppcre "^(\\d+)-(\\d+)-(\\d+)$" year month day)
                (list year month day)))
             '("2012" "11" "04"))))

;;; Regression tests

(test issue39
  (is (eql (match '(0) ((list x) x))
           0))
  (is (eql (match '(0) ((list (and x (guard it (numberp it)))) x))
           0)))

(test issue38
  (signals error
    (macroexpand '(match 1 ((or (place x) (place x)))))))

(test issue31
  (is (equal (match '(1 2 3 4)
               ((or (list* (and (type symbol) x) y z)
                    (list* y x z))
                (list x y z)))
             '(2 1 (3 4)))))

(test issue68
  (is (equal (match 1
               ((guard x (equal x 2)) (list :a x))
               (x (list :b x)))
             '(:b 1))))

(defun signal-error ()
  (error 'error))
(defun will-fail ()
  (match 1
    ((not 2)
     (signal-error))))
(test issue101
  (signals error (will-fail)))

(test issue105
  (is-match '(1) (list* (or 1 2) _)))
