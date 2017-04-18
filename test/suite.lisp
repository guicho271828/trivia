;;; integrated testing including derived patterns
;;;
;;; INCOMPATIBILITY NOTE: `fail' no longer effective by default: it is now exported
;;; separately from :trivia.fail to avoid the common conflict against fiveam.
;;; Also, :trivia.next and :trivia.skip exports `next' (conflicts with
;;; `iterate:next') and `skip', respectively. all 3 macros have the same meaning.
;;; 
;;; INCOMPATIBILITY NOTE: `match' no longer expanded in 1-pass through
;;; `macroexpand': some tests are now replaced with eval
(defpackage :trivia.test
  (:use :closer-common-lisp :fiveam
        :trivia.level2
        :trivia.level1
        :trivia.next
        :trivia.level2.impl))

(in-package :trivia.test)

;; for debugging purpose
;; (setf *trace-dispatching* t)

(def-suite :trivia)
(in-suite :trivia)

;;; Pattern matching

(defmacro is-match (arg &body pattern)
  `(is-true (locally
                (declare (optimize (safety 3) (debug 3) (speed 0)))
              (match ,arg (,@pattern t)))
            ,(format nil "~<pattern ~a did not match against arg ~s~:@>" (list pattern arg))))

(defmacro is-not-match (arg &body pattern)
  `(is-false (locally
                 (declare (optimize (safety 3) (debug 3) (speed 0)))
               (match ,arg (,@pattern t)))
             ,(format nil "~<pattern ~a matched against arg ~s~:@>" (list pattern arg))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass person ()
       ((name :initarg :name :reader name)
        (age :initarg :age)))
  (defun age (a b c)
    (format t "this function is meaningless ~a ~a ~a" a b c))

  (defstruct (point (:predicate point-p))
    x y)
  (defmethod make-load-form ((o point) &optional environment)
    (make-load-form-saving-slots o
                                 :slot-names '(x y)
                                 :environment environment))
  (defstruct (point3 (:include point))
    z)
  (defmethod make-load-form ((o point3) &optional environment)
    (make-load-form-saving-slots o
                                 :slot-names '(x y z)
                                 :environment environment))
  (declaim (inline x))
  (defun x (a b c)
    (format t "this function is meaningless ~a ~a ~a" a b c))
  (declaim (notinline x)))

(test (constant-pattern :compile-at :definition-time)
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
  (is-match '(1 t 3.14 :foo "foo") '(1 t 3.14 :foo "foo"))
  (is (eql 3 (match '(1 2 3) ('(1 _ a) a))))
  (is (eql 3 (match #(1 2 3) (#(1 _ a) a))))
  ;; ensure these are compared by char-wise eql
  (is-match "aaa" "aaa")
  (is-not-match "aaa" "AAA")
  (is-match #S(POINT :x "A" :y "B")
            #S(POINT :x "A" :y "B"))
  (is-not-match #S(POINT :x "A" :y "B")
                #S(POINT :x "a" :y "b"))
  ;; check if INCLUDEd slots are direct slots
  (is-match #S(POINT3 :x "A" :y "B" :z "C")
            #S(POINT3 :x "A" :y "B" :z "C"))
  (is-not-match #S(POINT3 :x "A" :y "B" :z "C")
                #S(POINT3 :x "a" :y "b" :z "c")))

(test variable-pattern
  ;; simple bind
  (is (eql 1 (match 1 (x x))))
  ;; anonymous bind
  (is-match 1 _)
  ;; complex bind
  (is (eql 6
           (match '(1 2 3)
             ((list x y z) (+ x y z))))))

(test place-pattern
  ;; level 0
  (let ((z 1))
    (match z ((place x) (incf x)))
    (is (eql 2 z)))
  ;; level 1
  (let ((z (cons 1 2)))
    (match z
      ((cons (place x) y)
       (incf x)
       (incf y)))
    (is (equal (cons 2 2) z)))
  ;; level 2
  (let ((z (list (vector 1))))
    (match z
      ((list (vector (place x)))
       (incf x)))
    (is (equalp (list (vector 2)) z))))

(test predicatep
  (is (null (predicatep 'point)))
  (is (eq 'point-p (predicate-p 'point))))


(test constructor-pattern
  ;; cons
  (is-match (cons 1 2) (cons 1 2)))

#-ecl ;; there is a magical error on ECL only on travis.
(test assoc
  (is-match '((1 . 2)) (assoc 1 2))
  (is-match '((1 . 2) (3 . 4)) (assoc 3 4))
  ;; NOTE: incompatibility --- this is not an association list, according to CLHS
  ;; (is-match '(1 (2 . 3)) (assoc 2 3))
  ;; NOTE: old incompatibility --- superceded by the following
  #+old
  (signals type-error
    (match '(1 (2 . 3)) ((assoc 2 3) t)))
  ;; NOTE: new incompatibility --- when it is not an assoc list, do not match.
  (is-not-match '(1 (2 . 3)) (assoc 2 3))
  ;; issue #54
  (is-not-match '(1) (assoc :foo val)) ; should not signal error
  (signals type-error
    (match '((:foo 1))
      ((assoc :foo val :test (lambda (x y) (declare (ignorable x y)) (error 'type-error)))
       val)))
  (signals type-error
    (match '((:foo 1))
      ((assoc :foo val :key (lambda (x) (declare (ignorable x)) (error 'type-error)))
       val)))
  (let ((counter 0))
    (is-false
     (handler-bind ((type-error #'continue))
       (match '((:foo 1) (:bar 1))
         ((assoc :baz val :key (lambda (x)
                                 (declare (ignorable x))
                                 (restart-case (error 'type-error)
                                   (continue ()
                                     (incf counter)
                                     (print :continue!)))
                                 x))
          val))))
    (is (= 2 counter)))

  ;; NOTE: incompatibility --- first argument to assoc should be quoted or constant
  ;; (is-match '((a . 1)) (assoc a 1))
  (is-match '((a . 1)) (assoc 'a 1))
  (is-not-match 1 (assoc 1 2))
  (is-not-match '((1 . 2)) (assoc 3 4))
  (is-not-match '((1 . 2) (3 . 4)) (assoc 3 5))
  ;; issue #52
  (is-match     '((:foo . 1))  (assoc :foo val))
  (is-not-match '((foo . 2))   (assoc :foo val))
  (is-not-match '(("foo" . 3)) (assoc :foo val))
  (is-not-match '((0 . 4))     (assoc :foo val))
  (is-not-match '(1)           (assoc :foo val))
  (is-not-match '((1 . 2) 2)   (assoc :foo val))
  ;; NOTE: incompatibility --- keyword arguments to assoc is evaluated
  ;; (is-match '(("a" . 1)) (assoc "A" 1 :test string-equal))
  (is-match '(("a" . 1)) (assoc "A" 1 :test #'string-equal)))

(test property
  (is-match '(:a 1) (property :a 1))
  (is-match '(:a 1 :b 2) (property :a 1))
  ;; NOTE: depends on SAFETY setting, it may signal type-error
  ;; (is-match '(:a 1 2) (property :a 1))
  (is-match '(1 2 :b 3) (property :b 3))
  ;; NOTE: incompatibility --- first argument to property should be quoted or constant
  ;; (is-match '(a 1) (property a 1))
  (is-match '(a 1) (property 'a 1))
  (is-not-match 1 (property :a 1))
  (is-not-match '(:a 1) (property :b 1))
  (is-not-match '(:a 1 :b 2) (property :b 3)))

(test property-default-foundp
  (is (= 3 (match '(:a 1 :b 2)
             ((property :c c 3)
              c))))
  (is-false (match '(:a 1 :b 2)
              ((property :c c 3 foundp)
               foundp)))
  (is (= 2 (match '(:a 1 :b 2)
             ((property :b b 3)
              b))))
  (is-true (match '(:a 1 :b 2)
              ((property :b b 3 foundp)
               foundp))))

(test vector
  (is-match (vector 1 2) (vector 1 2))
  (match (vector 1 2)
    ;; soft vector pattern
    ((vector* 1 2 a) (is (eq a nil)))))
(test simple-vector
  (is-match (vector 1 2) (simple-vector 1 2)))
(test class
  (let ((person (make-instance 'person :name "Bob" :age 31)))
    (is (equal '("Bob" 31)
               (match person
                 ((person name age) (list name age)))))
    (is-match person (person))
    (is-match person (person (name "Bob") (age 31)))
    (is-match person (person (name "Bob")))
    (is-match person (person (age 31)))
    (is-not-match person (person (name "Alice")))
    (is-not-match person (person (age 49)))
    (is-not-match 1 (person))))
(test make-instance
  (let ((person (make-instance 'person :name "Bob" :age 31)))
    (is-match person (person :name "Bob" :age 31))
    (is-not-match person (person :name "Bob" :age 49))))
(test structure
  (let ((point (make-point :x 1 :y 2)))
    (is (equal '(1 2)
               (match point
                 ((point- x y) (list x y)))))
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
    (is-not-match 1 (point-))))
(test structure-make-instance
  (let ((point (make-point :x 1 :y 2)))
    (is-match point (point- :x 1 :y 2))
    (is-not-match point (point- :x 2 :y 2))))

(test list
  (is-match '() (list))
  (is-match '(1 2 3) (list 1 2 3))
  (is-not-match '() (list _))
  (is-not-match 5 (list _)))
(test list*
  (is-match '() (list* _))
  (is-match '(1 2 3) (list* 1 2 (list 3)))
  (is-match '(1 2 3) (list* _))
  ;;;;; guicho271828 --- this is incorrect, should match because (list* 5) == 5
  ;; (is-not-match 5 (list* _))
  (is-match 5 (list* _)))
(test alist
  (is-match '((1 . 2) (2 . 3) (3 . 4)) (alist (3 . 4) (1 . 2))))
(test plist
  (is-match '(:a 1 :b 2 :c 3) (plist :c 3 :a 1)))
(test satisfies
  (is-match 1 (satisfies numberp))
  (is-not-match 1 (satisfies stringp)))
(test eq-family
  (is-match :foo (eq :foo))
  (is-match 1 (eql 1))
  (is-match "foo" (equal "foo"))
  (is-match #(1) (equalp #(1))))
(test type
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
  (is-match 1 (guard x (eql x 1))))
(test lift
  (is-match 1 (and x (guard y (eql x y))))
  (is-match 1 (and (guard x (eql x y)) y)) ;; forward-referencing guard
  (is-not-match 1 (and x (guard 2 (eql x 1))))
  (is-not-match 1 (and x (guard y (not (eql x y)))))
  (is-match '(1 1) (list x (guard y (eql x y))))
  (is-match '(1 1) (list (guard x (oddp x)) (guard y (eql x y)))))
(test lift1
  (is-not-match '(1 2) (list (guard x (oddp x)) (guard y (eql x y))))
  (is-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 1))))
  (is-not-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 2))))
  (is-match 1 (or (list x) (guard x (oddp x))))
  (is-match '(1) (or (list x) (guard x (oddp x))))
  (is-not-match 1 (or (list x) (guard x (evenp x)))))
(test lift2
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
  ;; variables in not pattern should not be bound
  (is (equal 1
             (let ((it 1))
               (match 2
                 ((not (guard it (eql it 3))) it)
                 (_ :fail))))))

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
  (is (equal '(nil nil)
             (match 1 ((or 1 (list x) y) (list x y)))))
  (is (equal '(nil 2)
             (match 2 ((or 1 (list x) y) (list x y)))))
  (is (equal '(1 nil)
             (match '(1) ((or 1 (list x) y) (list x y))))))

(test and-pattern
  (is-match 1 (and))
  (is-match 1 (and 1))
  (is-match 1 (and 1 1))
  (is-not-match 1 (and 1 2))
  (is-match 1 (and _))
  (is-match 1 (and _ _))
  (is-match 1 (and 1 (and 1)))
  (is-match 1 (and (and 1)))
  (is (eql 1 (match 1 ((and 1 x) x))))
  (is-not-match 1 (and (and (not 1))))
  (is-match 1 (and (type number) (type integer)))
  ;; complex
  (is-true (match 1
             ((and 1 2) nil)
             (1 t)
             (2 nil)))
  (is (eql 1
           (match (list 1 2)
             ((list (and 1 x) 2) x))))
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
  (is (equal '(1 2 3)
             (multiple-value-list (match 1 (1 (values 1 2 3))))))
  ;; mixture
  (is-true (match (cons 1 2)
             ((cons 2 1) 2)
             (_ t)
             ((cons 1 2) nil)))

  ;; linear pattern
  (signals error
    (eval
     '(match (cons 1 2)
       ((cons x x) t))))
  (signals error
    (eval
     '(match (list 1 (list 2 3))
       ((list x (list y x)) t))))
  (signals error
    (eval
     '(match (list 1 (list 2 3))
       ((list x (list y y)) t))))
  (signals error
    (eval
     '(match 1
       ((and x x) t))))
  (is-match 1 (and _ _))
  (is-match 1 (or _ _))
  ;; declarations
  (is-true (match 1
             (1 (declare (ignore)) t)))
  ;; when is not supported
  #+nil
  (is-true (match 1
             (1 when t (declare (ignore)) t)))
  (is-true (match 1
             ((guard 1 t) (declare (ignore)) t)))
  ;; syntax sugar
  #+nil
  (is-true (match 1 (_ when t t)))
  #+nil
  (is-true (match 1 (_ unless nil t))))

(test multiple-value-match
  (is (eql 2
           (multiple-value-match (values 1 2)
             ((2) 1)
             ((1 y) y))))

  ;; linear pattern
  (signals error
    (eval
     '(multiple-value-match (values 1 2)
       ((x x) t))))
  (is-true (multiple-value-match 1
             ((_ _) t))))

(test ematch
  (is-true (ematch 1 (1 t)))
  (signals match-error
    (ematch 1 (2 t)))

  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql 1
               (handler-case (ematch (f) (0 t))
                 (match-error (e)
                   (first (match-error-values e)))))))))

#-cmu ;; there is a magical error only on CMU on travis.
(test multiple-value-ematch
  (signals match-error
    (multiple-value-ematch (values 1 2)
      ((2 1) t)))

  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql 1
               (handler-case (multiple-value-ematch (values (f)) ((0) t))
                 (match-error (e)
                   (first (match-error-values e)))))))))

(test cmatch
  (is-true (cmatch 1 (1 t)))
  (is-false (handler-bind ((match-error #'continue))
              (cmatch 1 (2 t))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql 1
               (handler-case (cmatch (f) (0 t))
                 (match-error (e)
                   (first (match-error-values e)))))))))

(test multiple-value-cmatch
  (is-false (handler-bind ((match-error #'continue))
              (multiple-value-cmatch (values 1 2)
                ((2 1) t))))
  ;; only once
  (let ((count 0))
    (flet ((f () (incf count)))
      (is (eql 1
               (handler-case (multiple-value-cmatch (values (f)) ((0) t))
                 (match-error (e)
                   (first (match-error-values e)))))))))

;;; Regression tests

(test issue39
  (is (eql 0
           (match '(0) ((list x) x))))
  (is (eql 0
           (match '(0) ((list (and x (guard it (numberp it)))) x)))))

;; This is from https://github.com/m2ym/optima/issues/38 , but no description is given and it's not clear why
;; this should not be allowed.
#+nil
(test issue38
  (signals error
    (eval '(match 1 ((or (place x) (place x)))))))

(test issue31
  (is (equal '(2 1 (3 4))
             (match '(1 2 3 4)
               ((or (list* (and (type symbol) x) y z)
                    (list* y x z))
                (list x y z))))))

(test issue68
  (is (equal '(:b 1)
             (match 1
               ((guard x (equal x 2)) (list :a x))
               (x (list :b x))))))

(defun signal-error ()
  (error 'error))
(defun will-fail ()
  (match 1
    ((not 2)
     (signal-error))))
(test issue101
  (signals error (will-fail)))

#-cmu ;; there is a magical error on CMU only on travis.
(test issue105
  (is-match '(1) (list* (or 1 2) _)))

(defun ^2 (x) (* x x))

(test access
  (is-match '(2) (list (access #'^2 4))))

(test match*
  (match* ((list 2) (list 3) (list 5))
    (((list x) (list y) (list (guard z (= z (+ x y)))))
     (is (= 5 z)))
    (_ (fail))))

(test defun-match*
  (defun-match* myfunc (x y)
    ((1 2) 3)
    ((4 5) 6)
    ((_ _) nil))
  (is (= 3 (myfunc 1 2))) 
  (is (= 6 (myfunc 4 5))) 
  (is-false (myfunc 7 8)))


(test next
  ;; optima:fail --> trivia:next : it causes symbol conflict with fiveam
  ;; and not convenient but note that it you :use trivia.fail package, it
  ;; exports 'trivia:fail, same functionality as in optima
  (is-false (match 1 (1 (next))))
  (is-true (match 1
              (1 (next))
              (1 t)))
  (is-true (match 1
             (1 (next))
             (1 t)
             (_ nil)))
  (is (eql 1
           (match (cons 1 2)
             ((cons x y)
              (if (eql x 1)
                  (next)
                  y))
             ((cons x 2)
              x))))
  (is-true (multiple-value-match (values 1 2)
             ((1 2) (next))
             ((_ _) t)))
  (is-true (ematch 1
             (1 (next))
             (_ t)))
  (signals match-error
    (ematch 1
      (1 (next))
      (1 (next))))
  
  (signals match-error
    (multiple-value-ematch (values 1 2)
      ((1 2) (next))))
  (is-true (match 1 (_ (next)) (_ t)))

  (signals match-error
    (ematch 1
      (1 (match2 1 ;; this should be match2, since 
           (1 (next)) 
           (1 (next)) ;; <<--- this `next' jumps to the 
           ))
      (1  ;; <<--- next matching clause here
       (next))))

  (let ((x `(match2 1 (1 (next)))))
    (signals ERROR ;; not necessarily PROGRAM-ERROR
      ;; using `next' in the last clause of match2
      (eval x))))

(test hash-table
  (match (make-hash-table)
    ((hash-table count) (is (= count 0)))))


(test and-wildcard-bug
  ;; unintended unwinding by "wildcard" condition
  (is-true
   (match 3
     ((guard1 it t it (and (type number) a _))
      (eq a 3)))))

(test issue-23
  (is-match '(shader foo :fragment "")
            (guard (list shader name type value)
                   (string-equal (symbol-name shader) "shader"))))

;; on clisp, fixnum is not recognized as an instance of built-in-class
#-clisp
(progn
(defgeneric plus (a b))
(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))

(test issue-24
  (is-match #'plus
            (generic-function))
  (match #'plus
     ((generic-function
       methods
       method-combination
       lambda-list
       argument-precedence-order)
      (is (= 1 (length methods)))
      (is-true method-combination)
      (is (= 2 (length lambda-list)))
      (is (equal '(a b) argument-precedence-order)))))
)

(test issue-51
  ;; otherwise == _
  (signals unbound-variable
    (eval
     '(match :anything0
       (otherwise
        otherwise))))
  (is-match :anything1 otherwise)
  (is-match (list :anything2 :anything2)
    (list otherwise otherwise))
  (is-match (list :anything3 :anything4)
    (list otherwise otherwise))
  (is-match (list :anything5 :anything6)
    (list a b))
  ;; this is explicitly allowed.
  (is-true
   (match (list :anything7 :anything7)
     (otherwise t)
     (_ (error "should not match")))))
