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
  (once-only (arg)
    `(%match-1 ,arg ,clauses nil)))

(defun %ematch-else (&optional arg patterns)
  (error 'match-error
         :argument arg
         :patterns patterns))

(defmacro ematch (arg &body clauses)
  (once-only (arg)
    (let ((else `(%ematch-else ,arg ',(mapcar #'car clauses))))
      `(%match-1 ,arg ,clauses ,else))))

(defun %cmatch-else (&optional arg patterns)
  (cerror "Continue."
         'match-error
         :argument arg
         :patterns patterns))

(defmacro cmatch (arg &body clauses)
  (once-only (arg)
    (let ((else `(%cmatch-else ,arg ',(mapcar #'car clauses))))
      `(%match-1 ,arg ,clauses ,else))))
