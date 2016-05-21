(in-package :trivia.level0)

(defvar *what*)
(defvar *bindings*)
(defvar *env*)
(defmacro match0 (*what* &body clauses &environment *env*)
  (once-only (*what*)
    (parse-patterns clauses)))

(defmacro ematch0 (what &body clauses)
  `(match0 ,what
     ,@clauses
     (_ (error "level0 match error!"))))

(defmacro lambda-match0 (&body clauses)
  (alexandria:with-gensyms (arg)
    `(lambda (,arg)
       (match0 ,arg
         ,@clauses))))

(defmacro lambda-ematch0 (&body clauses)
  (alexandria:with-gensyms (arg)
    `(lambda (,arg)
       (ematch0 ,arg
         ,@clauses))))

(defun parse-patterns (clauses)
  (if (null clauses)
      nil
      (destructuring-bind ((pattern &rest body) . rest) clauses
        (multiple-value-bind (condition bindings)
            (let ((*bindings* nil))
              (values (make-pattern-predicate pattern)
                      *bindings*))
          `(if ,condition
               (let* ,(reverse bindings)
                 (declare (ignorable ,@(mapcar #'first bindings)))
                 ,@body)
               ,(parse-patterns rest))))))



(defun make-pattern-predicate (pattern)
  (if (atom pattern)
      (cond
        ((constantp pattern *env*) `(equal ,*what* ,pattern))
        ((symbolp pattern)
         (unless (string= "_" (symbol-name pattern))
           (push `(,pattern ,*what*) *bindings*))
         t)
        (t (error "what is this? ~a" pattern)))
      (destructuring-bind (name . args) pattern
        (ecase name
          (quote `(equal ,*what* ',@args))
          (cons
           (destructuring-bind (car cdr) args
             `(and (consp ,*what*)
                   ,(let* ((what `(car ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate car)))
                   ,(let* ((what `(cdr ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate cdr))))))
          (list
           (if args
               (destructuring-bind (car . cdr) args
                 (make-pattern-predicate
                  (if cdr
                      `(cons ,car (list ,@cdr))
                      `(cons ,car nil))))
               `(null ,*what*)))
          (list*
           (assert (not (null args)) nil "invalid list* pattern: needs at least 1 arg")
           ;; FIXME most lisps allow destructuring-bind on NIL ; ABCL does not. bug?
           (destructuring-bind (car . cdr) args
             (make-pattern-predicate
              (if cdr
                  `(cons ,car (list* ,@cdr))
                  car))))))))

