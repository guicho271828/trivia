(in-package :fivepm)

(define-condition match-error (error)
  ((argument :initarg :argument
             :initform nil
             :reader match-argument)
   (patterns :initarg :patterns
             :initform nil
             :reader match-patterns))
  (:report (lambda (condition stream)
             (format stream "Can't match ~A with ~{~S~^ or ~}."
                     (match-argument condition)
                     (match-patterns condition)))))

(defmacro match (arg &body clauses)
  "Matches ARG with CLAUSES. CLAUSES is a list of the form
of (PATTERN . BODY) where PATTERN is a pattern specifier and BODY is
an implicit progn. If ARG is matched with some PATTERN, then evaluates
BODY and returns the evaluated value. Otherwise, returns NIL.

If BODY starts with a symbol WHEN, then the next form will be used to
introduce a guard for PATTERN. That is,

    (match list ((list x) when (oddp x) x))

will be translated to

    (match list ((guard (list x) (oddpx)) x))"
  (once-only (arg)
    `(%match-1 ,arg ,clauses nil)))

(defun %ematch-else (&optional arg patterns)
  (error 'match-error
         :argument arg
         :patterns patterns))

(defmacro ematch (arg &body clauses)
  "Same as MATCH, except MATCH-ERROR will be raised if not matched."
  (once-only (arg)
    (let ((else `(%ematch-else ,arg ',(mapcar #'car clauses))))
      `(%match-1 ,arg ,clauses ,else))))

(defun %cmatch-else (&optional arg patterns)
  (cerror "Continue."
         'match-error
         :argument arg
         :patterns patterns))

(defmacro cmatch (arg &body clauses)
  "Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched."
  (once-only (arg)
    (let ((else `(%cmatch-else ,arg ',(mapcar #'car clauses))))
      `(%match-1 ,arg ,clauses ,else))))
