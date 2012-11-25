(in-package :optima)

(define-condition match-error (error)
  ((values :initarg :values
           :initform nil
           :reader match-error-values)
   (patterns :initarg :patterns
             :initform nil
             :reader match-error-patterns))
  (:report (lambda (condition stream)
             (format stream "Can't match ~S with ~{~S~^ or ~}."
                     (match-error-values condition)
                     (match-error-patterns condition)))))

(defmacro match (arg &body clauses)
  "Matches ARG with CLAUSES. CLAUSES is a list of the form of (PATTERN
. BODY) where PATTERN is a pattern specifier and BODY is an implicit
progn. If ARG is matched with some PATTERN, then evaluates
corresponding BODY and returns the evaluated value. Otherwise, returns
NIL.

Evaluating a form (FAIL) in the clause body causes the latest pattern
matching be failed. For example,

    (match 1
      (x (if (eql x 1)
             (fail)
             x))
      (_ 'ok))

returns OK, because the form (FAIL) in the first clause is
evaluated.

If BODY starts with the symbols WHEN or UNLESS, then the next form
will be used to introduce (FAIL). That is,

    (match list ((list x) when (oddp x) x))
    (match list ((list x) unless (evenp x) x))

will be translated to

    (match list ((list x) (if (oddp x) x (fail))))
    (match list ((list x) (if (evenp x) (fail) x)))

Examples:

    (match 1 (1 1))
    => 1
    (match 1 (2 2))
    => 2
    (match 1 (x x))
    => 1
    (match (list 1 2 3)
      (list x y z) (+ x y z))
    => 6"
  (compile-match-1 arg clauses nil))

(defmacro match* (args &body clauses)
  (once-only* args
    (compile-match args clauses nil)))

(defmacro multiple-value-match (values-form &body clauses)
  "Matches the multiple values of VALUES-FORM with CLAUSES. Unlike
MATCH, CLAUSES have to have the form of (PATTERNS . BODY), where
PATTERNS is a list of patterns. The number of values that will be used
to match is determined by the maximum arity of PATTERNS among CLAUSES.

Examples:

    (multiple-value-match (values 1 2)
     ((2) 1)
     ((1 y) y))
    => 2"
  (compile-multiple-value-match values-form clauses nil))

(defmacro ematch (arg &body clauses)
  "Same as MATCH, except MATCH-ERROR will be raised if not matched."
  (once-only (arg)
    (let ((else `(error 'match-error
                        :values (list ,arg)
                        :patterns ',(mapcar (lambda (c) (list (car c))) clauses))))
      (compile-match-1 arg clauses else))))

(defmacro ematch* (args &body clauses)
  (once-only* args
    (let ((else `(error 'match-error
                        :values (list ,@args)
                        :patterns ',(mapcar #'car clauses))))
      (compile-match args clauses else))))

(defmacro multiple-value-ematch (values-form &body clauses)
  "Same as MULTIPLE-VALUE-MATCH, except MATCH-ERROR will be raised if
not matched."
  (let* ((values (gensym "VALUES"))
         (else `(error 'match-error
                       :values ,values
                       :patterns ',(mapcar #'car clauses))))
    ;; FIXME: remove allocations
    `(let ((,values (multiple-value-list ,values-form)))
       ,(compile-multiple-value-match `(values-list ,values) clauses else))))

(defmacro cmatch (arg &body clauses)
  "Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched."
  (once-only (arg)
    (let ((else `(cerror "Continue."
                         'match-error
                         :values (list ,arg)
                         :patterns ',(mapcar (lambda (c) (list (car c))) clauses))))
      (compile-match-1 arg clauses else))))

(defmacro cmatch* (args &body clauses)
  (once-only* args
    (let ((else `(cerror "Continue."
                         'match-error
                         :values (list ,@args)
                         :patterns ',(mapcar #'car clauses))))
      (compile-match args clauses else))))

(defmacro multiple-value-cmatch (values-form &body clauses)
  "Same as MULTIPLE-VALUE-MATCH, except continuable MATCH-ERROR will
be raised if not matched."
  (let* ((values (gensym "VALUES"))
         (else `(cerror "Continue."
                        'match-error
                        :values ,values
                        :patterns ',(mapcar #'car clauses))))
    ;; FIXME: remove allocations
    `(let ((,values (multiple-value-list ,values-form)))
       ,(compile-multiple-value-match `(values-list ,values) clauses else))))
