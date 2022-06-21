;;; integrated testing including derived patterns
;;;
;;; INCOMPATIBILITY NOTE: `fail' no longer effective by default: it is now exported
;;; separately from :trivia.fail to avoid the common conflict against fiveam.
;;; Also, :trivia.next and :trivia.skip exports `next' (conflicts with
;;; `iterate:next') and `skip', respectively. all 3 macros have the same meaning.
;;;
;;; INCOMPATIBILITY NOTE: `match' no longer expanded in 1-pass through
;;; `macroexpand': some tests are now replaced with eval
(defpackage :trivia.level2.test
  (:use :closer-common-lisp :fiveam
        :trivia.level2
        :trivia.level1
        :trivia.next
        :trivia.level2.impl))

(in-package :trivia.level2.test)

;; for debugging purpose
;; (setf *trace-dispatching* t)

(in-suite :trivia.level2)

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
  (declaim (notinline x))

  (defun dont-call (x) (error "don't!")))

(test (constant-pattern :compile-at :run-time)
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
  ;; quoted conses should not expand to patterns (issue #86)
  #+(or)
  (is (eql 3 (match '(1 2 3) ('(1 _ a) a))))
  (is-not-match '(1 2 3) '(1 _ a))

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

(test (variable-pattern :compile-at :run-time)
  ;; simple bind
  (is (eql 1 (match 1 (x x))))
  ;; anonymous bind
  (is-match 1 _)
  ;; complex bind
  (is (eql 6
           (match '(1 2 3)
             ((list x y z) (+ x y z))))))

(test (place-pattern :compile-at :run-time)
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
    (is (equalp (list (vector 2)) z)))

  (finishes
    (match (make-point)
      ((point :dont-call (place x))
       (print :ok)))))

(test (predicatep :compile-at :run-time)
  (is (null (predicatep 'point)))
  (is (eq 'point-p (predicate-p 'point))))


(test (constructor-pattern :compile-at :run-time)
  ;; cons
  (is-match (cons 1 2) (cons 1 2)))

#-(or clasp ecl) ;; there is a magical error on ECL only on travis.
(test (assoc :compile-at :run-time)
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

(test (property :compile-at :run-time)
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

(test (property-default-foundp :compile-at :run-time)
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

(test (vector :compile-at :run-time)
  (is-match (vector 1 2) (vector 1 2))
  (match (vector 1 2)
    ;; soft vector pattern
    ((vector* 1 2 a) (is (eq a nil)))))
(test (simple-vector :compile-at :run-time)
  (is-match (vector 1 2) (simple-vector 1 2)))

(test (class :compile-at :run-time)
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
(test (make-instance :compile-at :run-time)
  (let ((person (make-instance 'person :name "Bob" :age 31)))
    (is-match person (person :name "Bob" :age 31))
    (is-not-match person (person :name "Bob" :age 49))))
(test (structure :compile-at :run-time)
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
(test (structure-make-instance :compile-at :run-time)
  (let ((point (make-point :x 1 :y 2)))
    (is-match point (point- :x 1 :y 2))
    (is-not-match point (point- :x 2 :y 2))))

(test (list :compile-at :run-time)
  (is-match '() (list))
  (is-match '(1 2 3) (list 1 2 3))
  (is-not-match '() (list _))
  (is-not-match 5 (list _)))
(test (list* :compile-at :run-time)
  (is-match '() (list* _))
  (is-match '(1 2 3) (list* 1 2 (list 3)))
  (is-match '(1 2 3) (list* _))
  ;;;;; guicho271828 --- this is incorrect, should match because (list* 5) == 5
  ;; (is-not-match 5 (list* _))
  (is-match 5 (list* _)))
(test (alist :compile-at :run-time)
  (is-match '((1 . 2) (2 . 3) (3 . 4)) (alist (3 . 4) (1 . 2))))
(test (plist :compile-at :run-time)
  (is-match '(:a 1 :b 2 :c 3) (plist :c 3 :a 1)))

(test (hash-table-entry :compile-at :run-time)
  (is-not-match nil (hash-table-entry :a 1))
  (let ((table (alexandria:plist-hash-table '(:a 1))))
    (is-match table (hash-table-entry :a 1))
    (is-match table (hash-table-entry :b nil))
    (is-not-match table (hash-table-entry :a 2))
    (is (eq 1 (match table ((hash-table-entry :b x 1) x))))
    (is-not-match table (hash-table-entry :b nil 1))))
(test (hash-table-entry! :compile-at :run-time)
  (let ((table (alexandria:plist-hash-table '(:a 1))))
    (is-match table (hash-table-entry! :a 1))
    (is-not-match table (hash-table-entry! :b nil))
    (is-not-match table (hash-table-entry! :b _))))
(test (hash-table-entries :compile-at :run-time)
  (let ((abc (alexandria:plist-hash-table '(:a 1 :b "2" :c nil)))
        (empty (alexandria:plist-hash-table nil)))
    (is-match abc (hash-table-entries :a 1))
    (is (eq 3 (match abc ((hash-table-entries :a a :b (read b)) (and a b (+ a b))))))
    (is-match abc (hash-table-entries :c nil))
    (is-not-match abc (hash-table-entries :c 1))
    (is-match empty (hash-table-entries :c nil))
    (flet ((count-gethash-calls (code)
             (let ((count 0))
               (subst-if t (constantly nil) (macroexpand code)
                         :key (lambda-match
                                ((list* 'gethash :a _)
                                 (incf count))))
               count)))
      (is (= 1 (count-gethash-calls '(match nil ((hash-table-entries :a 1)))))))))
(test (hash-table-entries! :compile-at :run-time)
  (let ((table (alexandria:plist-hash-table '(:a 1 :b "2"))))
    (is-not-match table (hash-table-entries! :c nil))
    (is (eq 3 (match table ((hash-table-entries! :a a :b (read b)) (+ a b)))))))
(test (hash-table-entries-warnings :compile-at :run-time)
  (signals trivia.level2.impl::hash-table-odd-number-of-entries-warning
    (macroexpand '(match nil ((hash-table-entries :a 1 :b))))))

(test (satisfies :compile-at :run-time)
  (is-match 1 (satisfies numberp))
  (is-not-match 1 (satisfies stringp)))
(test (eq-family :compile-at :run-time)
  (is-match :foo (eq :foo))
  (is-match 1 (eql 1))
  (is-match "foo" (equal "foo"))
  (is-match #(1) (equalp #(1))))
(test (type :compile-at :run-time)
  (is-match 1 (type number))
  (is-match "foo" (type string))
  (is-match :foo (type (eql :foo)))
  (is-match 1 (type (or string number)))
  (is-not-match 1 (type (not t))))

(test (guard-pattern :compile-at :run-time)
  (is-match 1 (guard _ t))
  (is-not-match 1 (guard _ nil))
  (is-match 1 (guard 1 t))
  (is-not-match 1 (guard 1 nil))
  (is-not-match 1 (guard 2 t))
  (is-match 1 (guard x (eql x 1))))
(test (lift :compile-at :run-time)
  (is-match 1 (and x (guard y (eql x y))))
  (is-match 1 (and (guard x (eql x y)) y)) ;; forward-referencing guard
  (is-not-match 1 (and x (guard 2 (eql x 1))))
  (is-not-match 1 (and x (guard y (not (eql x y)))))
  (is-match '(1 1) (list x (guard y (eql x y))))
  (is-match '(1 1) (list (guard x (oddp x)) (guard y (eql x y)))))
(test (lift1 :compile-at :run-time)
  (is-not-match '(1 2) (list (guard x (oddp x)) (guard y (eql x y))))
  (is-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 1))))
  (is-not-match '(1 (1)) (list x (guard (list (guard y (eql x y))) (eql x 2))))
  (is-match 1 (or (list x) (guard x (oddp x))))
  (is-match '(1) (or (list x) (guard x (oddp x))))
  (is-not-match 1 (or (list x) (guard x (evenp x)))))
(test (lift2 :compile-at :run-time)
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

(test (not-pattern :compile-at :run-time)
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

(test (or-pattern :compile-at :run-time)
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

(test (and-pattern :compile-at :run-time)
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

(test (match :compile-at :run-time)
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

(test (multiple-value-match :compile-at :run-time)
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

(test (ematch :compile-at :run-time)
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
(test (multiple-value-ematch :compile-at :run-time)
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

(test (cmatch :compile-at :run-time)
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

(test (multiple-value-cmatch :compile-at :run-time)
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

(test (issue39 :compile-at :run-time)
  (is (eql 0
           (match '(0) ((list x) x))))
  (is (eql 0
           (match '(0) ((list (and x (guard it (numberp it)))) x)))))

;; This is from https://github.com/m2ym/optima/issues/38 , but no description is given and it's not clear why
;; this should not be allowed.
#+nil
(test (issue38 :compile-at :run-time)
  (signals error
    (eval '(match 1 ((or (place x) (place x)))))))

(test (issue31 :compile-at :run-time)
  (is (equal '(2 1 (3 4))
             (match '(1 2 3 4)
               ((or (list* (and (type symbol) x) y z)
                    (list* y x z))
                (list x y z))))))

(test (issue68 :compile-at :run-time)
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
(test (issue101 :compile-at :run-time)
  (signals error (will-fail)))

#-cmu ;; there is a magical error on CMU only on travis.
(test (issue105 :compile-at :run-time)
  (is-match '(1) (list* (or 1 2) _)))

(defun ^2 (x) (* x x))

(test (access :compile-at :run-time)
  (is-match '(2) (list (access #'^2 4))))

(test (match* :compile-at :run-time)
  (match* ((list 2) (list 3) (list 5))
    (((list x) (list y) (list (guard z (= z (+ x y)))))
     (is (= 5 z)))
    (_ (fail))))

(test (defun-match* :compile-at :run-time)
  (defun-match* myfunc (x y)
    ((1 2) 3)
    ((4 5) 6)
    ((_ _) nil))
  (is (= 3 (myfunc 1 2)))
  (is (= 6 (myfunc 4 5)))
  (is-false (myfunc 7 8)))


(test (next :compile-at :run-time)
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

(test (hash-table :compile-at :run-time)
  (match (make-hash-table)
    ((hash-table count) (is (= count 0)))))


(test (and-wildcard-bug :compile-at :run-time)
  ;; unintended unwinding by "wildcard" condition
  (is-true
   (match 3
     ((guard1 it t it (and (type number) a _))
      (eq a 3)))))

(test (issue-23 :compile-at :run-time)
  (is-match '(shader foo :fragment "")
            (guard (list shader name type value)
                   (string-equal (symbol-name shader) "shader"))))

;; on clisp, fixnum is not recognized as an instance of built-in-class
#-clisp
(progn
(defgeneric plus (a b))
(defmethod plus ((a fixnum) (b fixnum))
  (+ a b))

(test (issue-24 :compile-at :run-time)
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

(test (issue-51 :compile-at :run-time)
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

(test (defpattern :compile-at :run-time)
  (finishes (print (pattern-expand '(cons a b)))))

(test (pad :compile-at :run-time)
  (is (= 1 (match* nil (() 1) (() 1)))))

(defmacro testmatcher (list)
  `(match ,list ((λlist a b &key (c -1)) (list a b c))))

(test (destructuring-key :compile-at :run-time)
  (is (equal '(1 2 3)   (testmatcher '(1 2 :c 3))))
  (is (equal '(1 2 -1)  (testmatcher '(1 2))))
  (is (equal nil        (testmatcher '(1 2 :c)))))

(defmacro testmatcher2 (list)
  `(match ,list
     ((λlist a b &optional c &rest rr &key (d -1) &allow-other-keys)
      (list a b c rr d))))

(test (destructuring-opt-key :compile-at :run-time)
  (is (equal '(1 2 3 (:c 4 :d 5) 5)
             (testmatcher2 '(1 2 3 :c 4 :d 5))))
  (is (equal nil
             ;; because &optional c consumes one value, it becomes an invalid plist
             (testmatcher2 '(1 2 :c 3 :d 4)))))

;;complex
(test (complex :compile-at :run-time)
  (is (= 0 (match #c(0 1) ((cl:complex r 1) r)))))

;; inline patterns

(test (vector-inline-patterns :compile-at :run-time)
  (is (equal '((vector 1 _ _ _ _ _ _ _ _ _ _ 5))
             (inline-pattern-expand '(vector 1 (@@ 10 _) 5))))
  (is-match (vector 1 2 3 4 5 6 7 8 9 10)
            (vector 1 (@@ 8 _) 10)))

(test (issue-21 :compile-at :run-time)
  ;; inline-pattern-expand is confused when the pattern contains non-pattern forms
  (finishes
    (inline-pattern-expand '(guard x (let ((y 1)) (= x y))))))

(test (issue-32 :compile-at :run-time)
  (match (list 1 2 3)
    ((lambda-list 1 2)
     (fail "should not match"))
    (_
     (pass)))
  (signals error
    (pattern-expand-1 `(lambda-list a &rest b &optional c)))
  (signals error
    (pattern-expand-1 `(lambda-list a &aux (c 2) &rest d))))

(test (issue-41 :compile-at :run-time)
  (match (list :x 1 :y 1)
    ((lambda-list &key &allow-other-keys)
     (pass))
    (_
     (fail "should match")))
  (signals error
    (pattern-expand-1 `(lambda-list &allow-other-keys))))

(test (lambda-list-nc :compile-at :run-time)
  (match (list :x 1 :y 1)
    ((lambda-list-nc &key x &allow-other-keys (list :y 1))
     (pass))
    (_
     (fail "should match")))
  (signals error
    (pattern-expand-1 `(lambda-list &key x &allow-other-keys (list :y 1)))))

(test (array :compile-at :run-time)
  (match #2A((0 1) (2 3))
    ((array :adjustable nil
            :has-fill-pointer nil
            :displaced-to nil
            :displaced-index-offset 0
            :dimensions '(2 2)
            :rank 2
            :total-size 4
            :contents ((a b) (c d)))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against array")))
  (match #2A((0 1) (2 3))
    ((simple-array :rank 2 :contents ((a b) (c d)))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against simple-array")))
  (signals error
    ;; rank is not determined
    (eval
     '(match #2A((0 1) (2 3))
       ((simple-array :contents ((a b) (c d)))))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '(2 2) :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions 2 :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '2 :contents ((a b) (c d))))
  ;; quoted conses should not expand to patterns (issue #86)
  #+(or)
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '(_ _) :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions (list _ _) :contents ((a b) (c d))))
  (match #2A((0 1) (2 3))
    ((row-major-array :contents (a b c d))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against row-major-array")))
  (match #2A((0 1) (2 3))
    ((row-major-array* :contents (a b c))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2)))
    (_
     (fail "failed to match against row-major-array*")))

  (let* ((a (make-array 10))
         (d (make-array 8 :displaced-to a :displaced-index-offset 2)))
    (is-match d (array))))

(test (last :compile-at :run-time)
      (is-match (alexandria:iota 5)
                (last (list 3 4) 2))
      (is-not-match (alexandria:iota 5)
                    (last (list 4) 2))
      (is-not-match 5 (last (list 3 4) 2))
      (signals error
               (macroexpand
                '(match x
                  ((last _ -1)
                   t)))))

(test (issue-81 :compile-at :run-time)
  (is-match 'x 'x))

(test (extensive-eq-test :compile-at :run-time)
  ;; note: whether eq returns t for read-time literal is implementation-defined
  ;; except symbols

  (is-match 'x 'x)
  (is-match 'x (eq 'x))
  (is-match 'x (eql 'x))
  (is-match 'x (equal 'x))
  (is-match 'x (equalp 'x))
  (is-not-match 'y 'x)

  (is-match 1 1)

  ;; http://clhs.lisp.se/Body/f_eq.htm , see Notes:
  ;;
  ;; An implementation is permitted to make ``copies'' of characters and numbers
  ;; at any time. The effect is that Common Lisp makes no guarantee that eq is
  ;; true even when both its arguments are ``the same thing'' if that thing is a
  ;; character or number.
  ;;
  #+undefined (is-match 1 (eq 1))

  (is-match 1 (eql 1))
  (is-match 1 (equal 1))
  (is-match 1 (equalp 1))

  (is-match #\a #\a)
  #+undefined (is-match #\a (eq #\a))
  (is-match #\a (eql #\a))
  (is-match #\a (equal #\a))
  (is-match #\a (equalp #\a))

  (is-not-match #\A #\a)
  (is-not-match #\A (eq #\a))
  (is-not-match #\A (eql #\a))
  (is-not-match #\A (equal #\a))
  (is-match #\A (equalp #\a))

  (is-match #c(0 1) #c(0 1))
  #+undefined (is-match #c(0 1) (eq #c(0 1)))
  (is-match #c(0 1) (eql #c(0 1)))
  (is-match #c(0 1) (equal #c(0 1)))
  (is-match #c(0 1) (equalp #c(0 1)))

  (is-match '(0 1) '(0 1))
  #+undefined (is-match '(0 1) (eq '(0 1)))
  #+undefined (is-match '(0 1) (eql '(0 1)))
  (is-match '(0 1) (equal '(0 1)))
  (is-match '(0 1) (equalp '(0 1)))

  (is-match '(0 . 1) '(0 . 1))
  #+undefined (is-match '(0 . 1) (eq '(0 . 1)))
  #+undefined (is-match '(0 . 1) (eql '(0 . 1)))
  (is-match '(0 . 1) (equal '(0 . 1)))
  (is-match '(0 . 1) (equalp '(0 . 1)))

  (is-match '(a b) '(a b))
  #+undefined (is-match '(a b) (eq '(a b)))
  #+undefined (is-match '(a b) (eql '(a b)))
  (is-match '(a b) (equal '(a b)))
  (is-match '(a b) (equalp '(a b)))

  ;; quoted conses should not expand to patterns (issue #86)
  #+(or)
  (is-match '(0 1) '(a b))
  (is-not-match '(0 1) '(a b))
  (is-not-match '(0 1) (eq '(a b)))
  (is-not-match '(0 1) (eql '(a b)))
  (is-not-match '(0 1) (equal '(a b)))
  (is-not-match '(0 1) (equalp '(a b)))

  (is-match #(0 1) #(0 1))
  #+undefined (is-match #(0 1) (eq #(0 1)))
  #+undefined (is-match #(0 1) (eql #(0 1)))
  #+undefined (is-match #(0 1) (equal #(0 1)))
  (is-match #(0 1) (equalp #(0 1)))
  (is-match #(0 1) #(_ _))

  (is-match "aaa" "aaa")
  #+undefined (is-match "aaa" (eq "aaa"))
  #+undefined (is-match "aaa" (eql "aaa"))
  (is-match "aaa" (equal "aaa"))
  (is-match "aaa" (equalp "aaa"))

  (is-match #*0010 #*0010)
  #+undefined (is-match #*0010 (eq #*0010))
  #+undefined (is-match #*0010 (eql #*0010))
  (is-match #*0010 (equal #*0010))
  (is-match #*0010 (equalp #*0010))

  (is-match #2A((0 1) (1 0)) #2A((0 1) (1 0)))
  #+undefined (is-match #2A((0 1) (1 0)) (eq #2A((0 1) (1 0))))
  #+undefined (is-match #2A((0 1) (1 0)) (eql #2A((0 1) (1 0))))
  #+undefined (is-match #2A((0 1) (1 0)) (equal #2A((0 1) (1 0))))
  (is-match #2A((0 1) (1 0)) (equalp #2A((0 1) (1 0))))

  (is-match #2A((#\a #\a) (#\a #\a)) #2A((#\a #\a) (#\a #\a)))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (eq #2A((#\a #\a) (#\a #\a))))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (eql #2A((#\a #\a) (#\a #\a))))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (equal #2A((#\a #\a) (#\a #\a))))
  (is-match #2A((#\a #\a) (#\a #\a)) (equalp #2A((#\a #\a) (#\a #\a))))

  (is-not-match #2A((#\a #\a) (#\a #\a)) #2A((#\A #\A) (#\A #\A)))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (eq #2A((#\A #\A) (#\A #\A))))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (eql #2A((#\A #\A) (#\A #\A))))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (equal #2A((#\A #\A) (#\A #\A))))
  (is-match #2A((#\a #\a) (#\a #\a)) (equalp #2A((#\A #\A) (#\A #\A))))

  (is-match #p"/usr/src/" #p"/usr/src/")
  #+undefined (is-match #p"/usr/src/" (eq #p"/usr/src/"))
  #+undefined (is-match #p"/usr/src/" (eql #p"/usr/src/"))
  (is-match #p"/usr/src/" (equal #p"/usr/src/"))
  (is-match #p"/usr/src/" (equalp #p"/usr/src/"))
  #+implementation-defined (is-match #p"/usr/src/" (equal #p"/USR/SRC/"))
  #+implementation-defined (is-match #p"/usr/src/" (equalp #p"/USR/SRC/"))

  (is-match #S(POINT :x "A" :y "B") #S(POINT :x "A" :y "B"))
  (is-not-match #S(POINT :x "A" :y "B") (eq #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") (eql #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") (equal #S(POINT :x "A" :y "B")))
  (is-match #S(POINT :x "A" :y "B") (equalp #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") #S(POINT :x "a" :y "b")))

(test (issue-86 :compile-at :run-time)
  (let ((a 0))
    (match `(1)
      ('(a)
        (fail "should not match")))))

(test (issue-89-property! :compile-at :run-time)
  (is (eq nil
          (match '(:y 88)
            ((property :x x) x)
            ((property :y y) y))))
  (is (eq 88
          (match '(:y 88)
            ((property :x (and x (not nil))) x)
            ((property :y y) y))))

  (is (eq nil
          (match '(:y 88 :x nil)
            ((property :x x nil t) x)  ; constant t matches the constant t.
            ((property :y y) y))))

  (is (eq 88
          (match '(:y 88)
            ((property :x x nil t) x)  ; t does not match nil, thus fail
            ((property :y y) y))))

  (is (eq nil
          (match '(:y 88 :x nil)
            ((property! :x x) x)
            ((property! :y y) y))))
  (is (eq 88
          (match '(:y 88)
            ((property! :x x) x)
            ((property! :y y) y)))))

(test (issue-93-eq-type-inference :compile-at :run-time)

  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form 42 t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form '42 t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form `(quote 42) t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form 42 nil)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form '42 nil)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form `(quote 42) nil)))

  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form #\c t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form '#\c t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form `(quote #\c) t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form #\c nil)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form '#\c nil)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form `(quote #\c) nil)))

  (is (equal t
             (trivia.level2.impl::type-of-form 'a t)))
  (is (equal '(eql a)
             (trivia.level2.impl::type-of-form `(quote a) t)))
  (is (equal t
             (trivia.level2.impl::type-of-form 'a nil)))
  (is (equal '(eql a)
             (trivia.level2.impl::type-of-form `(quote a) nil)))

  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form :keyword t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form ':keyword t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form `(quote :keyword) t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form :keyword nil)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form ':keyword nil)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form `(quote :keyword) nil)))

  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form "str" t)))
  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form '"str" t)))
  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form `(quote "str") t)))
  (let ((s "str"))
    (is (equal `(eql ,s)
               (trivia.level2.impl::type-of-form s nil)))
    (is (equal `(eql ,s)
               (trivia.level2.impl::type-of-form `(quote ,s) nil))))

  (is (equal 'person
             (trivia.level2.impl::type-of-form (make-instance 'person) t)))
  (is (equal 'person
             (trivia.level2.impl::type-of-form `(quote ,(make-instance 'person)) t)))
  (let ((p (make-instance 'person)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form p nil)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form `(quote ,p) nil))))

  (is (equal 'point
             (trivia.level2.impl::type-of-form (make-point) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form (quote #.(make-point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form `(quote ,(make-point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form #S(point) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form (quote #S(point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form `(quote #S(point)) t)))

  (let ((p #S(point)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form p nil)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form `(quote ,p) nil))))

  ;; structural (conses)
  (is (equal t
             (trivia.level2.impl::type-of-form `(unknownfunctionform a b) t)))
  (is (equal t
             (trivia.level2.impl::type-of-form `(unknownfunctionform a b) nil)))
  (defconstant +answer-to-everything+ 42)
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `+answer-to-everything+ t)))
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `+answer-to-everything+ nil)))
  (define-symbol-macro %%answer-to-everything%% +answer-to-everything+)
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `%%answer-to-everything%% t)))
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `%%answer-to-everything%% nil)))

  (is (equal '(cons point point)
             (trivia.level2.impl::type-of-form `(quote (,(make-point) . ,(make-point))) t)))

  (let* ((p #S(point))
         (c `(,p . ,p)))
    (is (equal '(cons point point)
               (trivia.level2.impl::type-of-form `(quote ,c) t)))
    (is (equal `(eql ,c)
               (trivia.level2.impl::type-of-form `(quote ,c) nil))))

  (let ((c '(x y z)))
    (is (equal '(cons (eql x) (cons (eql y) (cons (eql z) (eql nil))))
               (trivia.level2.impl::type-of-form `(quote ,c) t)))
    (is (equal `(eql ,c)
               (trivia.level2.impl::type-of-form `(quote ,c) nil)))))

(test (member :compile-at :run-time)
      (is-match 1 (member '(1 2 3)))
      (is-not-match 6 (member '(1 2 3)))
      (let ((list (list 1 2 3)))
        (is-match 1 (member '(1 2 3)))
        (is-not-match 6 (member '(1 2 3)))))

(test (more-guards :compile-at :run-time)
  (flet ((optima (x)
           (optima:match x
             ((list (optima:guard x (eql x y)) y)
              :ok)))
         (trivia (x)
           (match x
             ((list (guard x (eql x y)) y)
              :ok))))

    (is (eql :ok (optima '(1 1))))
    (is (eql nil (optima '(1 2))))
    (is (eql :ok (trivia '(1 1))))
    (is (eql nil (trivia '(1 2)))))

  (flet ((optima (x)
           (optima:match x
             ((not (optima:guard (list x y) (eql x y)))
              :ok)))
         ;; does not compile; variables in NOT are not captured
         #+(or)
         (optima2 (x)
           (optima:match x
             ((not (optima:guard (list x y) (eql x y)))
              (+ x y))))
         (trivia (x)
           (match x
             ((not (guard (list x y) (eql x y)))
              :ok))))

    ;; optima:
    ;; (or (guard (list #:x #:y) (not (eql #:x #:y)))
    ;;     (not (list #:x #:y)))
    ;; --- variables are renamed
    (is (eql nil (optima '(1 1))))
    (is (eql :ok (optima '(1 2))))
    (is (eql :ok (optima :a)))

    ;; currently:
    ;; (is (eql nil (trivia '(1 1))))
    ;; (is (eql nil (trivia '(1 2))))
    ;; (is (eql t   (trivia :a)))

    ;; should be:
    (is (eql nil (trivia '(1 1))))
    (is (eql :ok (trivia '(1 2))))
    (is (eql :ok (trivia :a)))))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct slice (start 0) stop (step 1)))

(test (test-call-on-macro-function :compile-at :run-time)
  ;; STEP is a macro function, thus symbol-function may return something
  ;; that is not a function
  (finishes
    (compile nil '(lambda () (match (make-slice :start 0 :stop 10) ((slice start stop step) (+ start stop step)))))))


(test (issue-134-optimizer-error :compile-at :run-time)
  (is (= 1
         (trivia:match "bar"
            ("bar" 1)
            ("foo" 2)
            (otherwise 3))))
  (is (= 2
         (trivia:match "foo"
            ("bar" 1)
            ("foo" 2)
            (otherwise 3))))
  (is (= 3
         (trivia:match "baz"
            ("bar" 1)
            ("foo" 2)
            (otherwise 3))))
  (is (= 1
         (trivia:match "bar"
            ((equal "bar") 1)
            ((equal "foo") 2)
            (otherwise 3))))
  (is (= 2
         (trivia:match "foo"
            ((equal "bar") 1)
            ((equal "foo") 2)
            (otherwise 3))))
  (is (= 3
         (trivia:match "baz"
            ((equal "bar") 1)
            ((equal "foo") 2)
            (otherwise 3)))))

