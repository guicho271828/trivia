(defpackage :optima.level2
  (:export :match :match*
           :ematch :ematch*
           :cmatch :cmatch*
           :match-error
           :pattern
           :match-error-pattern
           :match-error-values
           ;; 
           :multiple-value-cmatch
           :multiple-value-ematch
           :multiple-value-match
           ;; 
           :guard
           ;; 
           :defpattern
           :pattern-expand
           :pattern-expand-1
           :pattern-expand-all
           :defoptimizer
           ;; pattern utils
           :pattern-compatible-p
           ;; test utils
           :define-inference-rule
           :test-type
           :type-tests
           :test-compatible-p
           ;; type utils
           :disjointp
           :all-subclasses
           :exhaustive-partitions
           :exhaustive-union
           ;; optimizer
           :optimizer
           :*optimizer*
           :in-optimizer
           :defoptimizer))

(defpackage :optima.level2.impl
  (:use :cl :alexandria
        :introspect-environment
        :optima.level0
        :optima.level1
        :optima.level2))

(in-package :optima.level2.impl)




;;;; inference database

(lispn:define-namespace pattern function)

(define-condition wildcard () ())

(defun pattern-expand-1 (p)
  "expand the given pattern once, just like macroexpand-1"
  (if (atom p)
      (values (cond
                ((constantp p) `(eq ,p))
                ((symbolp p)
                 ;; there is no _ pattern / variable pattern in level1
                 (if (string= "_" (symbol-name p))
                     (progn (signal 'wildcard) ;; upper pattern-expand would handle this
                            (with-gensyms (it) `(guard1 ,it t)))
                     `(guard1 ,p t)))
                (t (error "what is this? ~a" p)))
              t)
  (handler-case
      (values (apply (symbol-pattern (car p)) (cdr p)) t)
    (unbound-pattern (c)
      (declare (ignore c))
          p))))

(defun pattern-expand (p)
  "expand the given pattern once, just like macroexpand"
  (let (expanded)
    (do () (nil)
      (multiple-value-bind (new expanded1) (pattern-expand-1 p)
        (if expanded1
            (setf p new expanded expanded1)
            (return (values new expanded)))))))

(defun pattern-expand-all (p)
  "expand the given pattern once, just like macroexpand-all"
  ;; should start by guard1 or or1
  (match0 (pattern-expand p)
    ((list* 'guard1 sym test more-patterns)
     (list* 'guard1 sym test
            (alist-plist
             (mappend
              (lambda-match0
                       ((cons generator subpattern)
                 (handler-case
                     (list (cons generator (pattern-expand-all subpattern)))
                   (wildcard () ;; remove wildcard pattern
                     nil))))
                     (plist-alist more-patterns)))))
    ((list* 'or1 subpatterns)
     (list* 'or1 (mapcar #'pattern-expand-all subpatterns)))))

  
(defmacro defpattern (name args &body body)
  `(setf (symbol-pattern ',name)
         #+sbcl
         (sb-int:named-lambda ',name ,args ,@body)
         #-sbcl
         (lambda ,args ,@body)))




;;;; optimizer database

(lispn:define-namespace optimizer function)
(defvar *optimizer* :trivial)
(defmacro in-optimizer (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *optimizer* ',name)))

(defmacro defoptimizer (name (types clauses) &body body)
  `(setf (symbol-optimizer ',name)
         #+sbcl
         (sb-int:named-lambda ',name (,types ,clauses) ,@body)
         #-sbcl
         (lambda (,types ,clauses) ,@body)))

(defoptimizer :trivial (types clauses)
  (declare (ignore types))
  clauses)




;;;; external apis

(defun ensure-multipattern (clauses)
  (mapcar (lambda-match0
                 ((list* pattern body)
                  (list* (list pattern) body)))
          clauses))

(defmacro match (what &body clauses)
  `(match* (,what) ,@(ensure-multipattern clauses)))

(defmacro match* (whats &body clauses)
  `(match+ ,whats
           ,(make-list (length whats) :initial-element t)
           ;; ^^^^ this part can surely be improved by using &environment intensively!
           ,@clauses))

(defmacro multiple-value-match (values-form &body clauses)
  (let* ((max (reduce #'max (mapcar (compose #'length #'ensure-list #'car) clauses)))
         (temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (iota max)))
         (padded (mapcar (curry #'pad max) clauses)))
    `(multiple-value-bind ,temps ,values-form
       (match* ,temps
         ,@padded))))

(defun pad (max clause)
  (match0 clause
    ((list* patterns body)
     (list* (append patterns
                    (make-list (- max (length patterns)) :initial-element '_)) 
            body))))

(defmacro match+ (whats types &body clauses)
  "Variant of match* : can specify the inferred types of each argument"
  (%match whats types clauses))

(defun %match (args types clauses)
  `(match1* ,args
     ,@(funcall (symbol-optimizer *optimizer*)
                types
                (mapcar (lambda-match0
                          ((list* patterns body)
                           (list* (mapcar #'pattern-expand-all patterns) body)))
                        clauses))))

;;;; more external apis

(define-condition match-error (error)
  ((pattern :initarg :pattern :reader match-error-pattern)
   (values :initarg :values :reader match-error-values)))

(defmacro ematch (what &body clauses)
  `(match ,what
     ,@clauses
     (_ (error 'match-error :pattern ',clauses :values ',what))))

(defmacro ematch* (whats &body clauses)
  `(match* ,whats
           ,@clauses
           (,(mapcar (constantly '_) whats)
       (error 'match-error :pattern ',clauses :values ',what))))

(defmacro multiple-value-ematch (values-form &body clauses)
  (once-only (values-form)
    `(multiple-value-match ,values-form
       ,@clauses
       (_ (error 'match-error :pattern ',clauses :values ,values-form)))))

(defmacro cmatch (what &body clauses)
  `(match ,what
     ,@clauses
     (_ (cerror "continue" 'match-error :pattern ',clauses :values ',what))))

(defmacro cmatch* (whats &body clauses)
  `(match* ,whats
           ,@clauses
           (,(mapcar (constantly '_) whats)
       (cerror "continue" 'match-error :pattern ',clauses :values ',what))))

(defmacro multiple-value-cmatch (values-form &body clauses)
  (once-only (values-form)
    `(multiple-value-match ,values-form
       ,@clauses
       (_ (cerror "continue" 'match-error :pattern ',clauses :values ',values-form)))))
