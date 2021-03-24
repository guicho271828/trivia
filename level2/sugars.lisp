(in-package :trivia.level2.impl)

;;;; external apis

(define-condition match-error (error)
  ((pattern :initarg :pattern
            :reader match-error-pattern
            ;; optima compatibility
            :reader match-error-patterns)
   (values :initarg :values :reader match-error-values))
  (:report (lambda (c s)
             (format s "Pattern: ~s ~& Values: ~s ~&"
                     (match-error-pattern c)
                     (match-error-values c)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +match-doc+ "
Syntax:
    [e|c]?match argument &body {clause}* -> result

- argument : a form, evaluated.
- clause   : ( pattern &body {bodyform}* )
- pattern  : a pattern language.
- bodyform : an implicit progn.

Matches argument against the patterns provided in clauses. Evaluate the bodyform
of the first clause whose pattern matches against argument. bodyform is treated
as an implicit progn.

- In MATCH, if no clauses have matched the given argument, returns nil.
- In EMATCH, if no clauses have matched the given argument, ematch signals an error MATCH-ERROR.
- In CMATCH, if no clauses have matched the given argument, cmatch signals a correctable MATCH-ERROR.
"))

(defmacro match (what &body clauses)
  #.+match-doc+
  `(match2 ,what
     ,@clauses
     (_ nil)))

(defmacro ematch (what &body clauses)
  #.+match-doc+
  (with-gensyms (otherwise)
    `(match2 ,what
       ,@clauses
       (,otherwise (error 'match-error :pattern ',clauses :values (list ,otherwise))))))

(defmacro cmatch (what &body clauses)
  #.+match-doc+
  (with-gensyms (otherwise)
    `(match2 ,what
       ,@clauses
       (,otherwise (cerror "continue" 'match-error :pattern ',clauses :values (list ,otherwise))))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +match*-doc+ "
Syntax:
    [e|c]?match* (&rest arguments) &body {multiclause}* -> result

- arguments :  a list of forms, evaluated.
- multiclause : ((&rest patterns) &body {bodyform}* )
- patterns :    a list of pattern languages.
- bodyform :    an implicit progn.

Arguments are evaluated in left-to-right manner, and the patterns are matched
against the results of evaluation of arguments in left-to-right manner. Evaluate
the bodyform of the first clause whose patterns match successfully.

When the number of patterns in a clause is insufficient, it is padded with
wildcard patterns, i.e., no check is conducted. In contrast, excessive number of
patterns will signal a compile-time error.

- In MATCH*,  if no clauses have matched the given argument, returns nil.
- In EMATCH*, if no clauses have matched the given argument, ematch signals an error MATCH-ERROR.
- In CMATCH*, if no clauses have matched the given argument, cmatch signals a correctable MATCH-ERROR.
"))

(defmacro match* (whats &body clauses)
  #.+match*-doc+  
  `(match2* ,whats
     ,@clauses
     (_ nil)))

(defmacro ematch* (whats &body clauses)
  #.+match*-doc+
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match2* ,whats
       ,@clauses
       (,temps
        (error 'match-error :pattern ',clauses :values (list ,@temps))))))

(defmacro cmatch* (whats &body clauses)
  #.+match*-doc+  
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match2* ,whats
       ,@clauses
       (,temps
        (cerror "continue" 'match-error :pattern ',clauses :values (list ,@temps))))))


(defun call-with-mvb-temp-vars (clauses fn)
  (let* ((max (reduce #'max (mapcar (compose #'length #'ensure-list #'car) clauses)))
         (clauses (mapcar (curry #'pad max) clauses))
         (temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (iota max))))
    (funcall fn clauses temps)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +multiple-value-match-doc+ "
Syntax:
    multiple-value-[e|c]?match values-form &body {multiclause}* -> result

- values-form : a form producing multiple values, evaluated.
- multiclause : ((&rest patterns) &body {bodyform}* )
- patterns :    a list of pattern languages.
- bodyform :    an implicit progn.

Similar to match*, but this is for multiple values. values-form is evaluated, and
each of the returned values are matched against the patterns provided
in multiclauses in left-to-right manner.
Evaluate the bodyform of the first clause whose patterns match successfully.

When the form returned more values than the number of patterns, excessive values are ignored.
When the form returned less values than the number of patterns, excessive patterns are matched against nil.

- In multiple-value-match,  if no clauses have matched the given argument, returns nil.
- In multiple-value-ematch, if no clauses have matched the given argument, ematch signals an error MATCH-ERROR.
- In multiple-value-cmatch, if no clauses have matched the given argument, cmatch signals a correctable MATCH-ERROR.
"))

(defmacro multiple-value-match (values-form &body clauses)
  #.+multiple-value-match-doc+
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind  ,temps ,values-form
        (match* ,temps
          ,@clauses)))))

(defmacro multiple-value-ematch (values-form &body clauses)
  #.+multiple-value-match-doc+
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind ,temps ,values-form
        (match* ,temps
          ,@clauses
          (,temps
           (error 'match-error :pattern ',clauses :values (list ,@temps))))))))

(defmacro multiple-value-cmatch (values-form &body clauses)
  #.+multiple-value-match-doc+
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind ,temps ,values-form
        (match* ,temps
          ,@clauses
          (,temps
           (cerror "continue" 'match-error :pattern ',clauses :values (list ,@temps))))))))



;;;; lambda-match family
(defun parse-matcher-body (body)
  (multiple-value-bind (clauses declarations documentation)
      (parse-body body :documentation t)
    (values `(,@(when documentation
                  (list documentation))
              ,@declarations)
            clauses)))

;; originally from metabang-bind's lambda-bind, also in fare-matcher, optima etc
(defmacro lambda-match (&body body)
  "Equivalent to (lambda (arg) (match arg BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (with-gensyms (clause)
      `(lambda (,clause)
         ,@preamble
         (match ,clause
           ,@clauses)))))

(defmacro lambda-ematch (&body body)
  "Equivalent to (lambda (arg) (ematch arg BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (with-gensyms (clause)
      `(lambda (,clause)
         ,@preamble
         (ematch ,clause
           ,@clauses)))))

(defmacro lambda-cmatch (&body body)
  "Equivalent to (lambda (arg) (cmatch arg BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (with-gensyms (clause)
      `(lambda (,clause)
         ,@preamble
         (cmatch ,clause
           ,@clauses)))))

(defmacro lambda-match* (&body body)
  "Equivalent to (lambda (args...) (match* (args...) BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (let ((gensyms (make-gensyms (caar clauses))))
      `(lambda ,gensyms
         ,@preamble
         (match* ,gensyms
           ,@clauses)))))

(defmacro lambda-ematch* (&body body)
  "Equivalent to (lambda (args...) (ematch* (args...) BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (let ((gensyms (make-gensyms (caar clauses))))
      `(lambda ,gensyms
         ,@preamble
         (ematch* ,gensyms
           ,@clauses)))))

(defmacro lambda-cmatch* (&body body)
  "Equivalent to (lambda (args...) (cmatch* (args...) BODY...))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    (let ((gensyms (make-gensyms (caar clauses))))
      `(lambda ,gensyms
         ,@preamble
         (cmatch* ,gensyms
           ,@clauses)))))

;; from optima extras
(defmacro lambda-match1 (pattern &body body)
  "Equivalent to (lambda (arg) (match arg (PATTERN BODY...)))."
  `(lambda-match (,pattern ,@body)))

(defmacro lambda-ematch1 (pattern &body body)
  "Equivalent to (lambda (arg) (ematch arg (PATTERN BODY...)))."
  `(lambda-ematch (,pattern ,@body)))

(defmacro lambda-cmatch1 (pattern &body body)
  "Equivalent to (lambda (arg) (cmatch arg (PATTERN BODY...)))."
  `(lambda-cmatch (,pattern ,@body)))

;;;; defun-match family
(defmacro defun-match (name (arg) &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (match arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name (,arg)
       ,@preamble
       (match ,arg
         ,@clauses))))

(defmacro defun-ematch (name (arg) &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (ematch arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name (,arg)
       ,@preamble
       (ematch ,arg
         ,@clauses))))

(defmacro defun-cmatch (name (arg) &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (cmatch arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name (,arg)
       ,@preamble
       (cmatch ,arg
         ,@clauses))))

(defmacro defun-match* (name args &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (match arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name ,args
       ,@preamble
       (match* ,args
         ,@clauses))))

(defmacro defun-ematch* (name args &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (ematch arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name ,args
       ,@preamble
       (ematch* ,args
         ,@clauses))))

(defmacro defun-cmatch* (name args &body body)
  "Equivalent to (defun (arg) [decl-and-docstring] (cmatch arg (PATTERN BODY...)))."
  (multiple-value-bind (preamble clauses) (parse-matcher-body body)
    `(defun ,name ,args
       ,@preamble
       (cmatch* ,args
         ,@clauses))))

;;;; extras

(defmacro if-match (pattern arg &body (then &optional else))
  "Equivalent to (match ARG (PATTERN THEN) (_ ELSE))."
  `(match ,arg
     (,pattern ,then)
     (_ ,else)))

(defmacro when-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN BODY...))."
  `(match ,arg (,pattern ,.body)))

(defmacro unless-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN) (_ BODY...))."
  `(match ,arg
     (,pattern)
     (_ ,.body)))

(defmacro let-match (bindings &body body)
  "Similar to LET, except not only a variable but also a pattern can
be used in BINDINGS."
  `(ematch* ,(mapcar #'cadr bindings)
     (,(mapcar #'car bindings) ,.body)))

(defmacro let-match* (bindings &body body)
  "Similar to LET-MATCH but matches sequentially."
  (reduce (lambda (binding form) `(let-match (,binding) ,form))
          bindings
          :from-end t
          :initial-value `(locally ,.body)))

(defmacro let-match1 (pattern arg &body body)
  "Equivalent to (let-match ((PATTERN ARG)) BODY...)."
  `(let-match ((,pattern ,arg)) ,.body))
