(defpackage :optima.level2
  (:export :match
           :match*
           :guard
           :defpattern
           :define-pattern-transformer
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

(defun pattern-expand-1 (p)
  "expand the given pattern once, just like macroexpand-1"
  (assert (listp p))
  (handler-case
      (values (apply (symbol-pattern (car p)) (cdr p)) t)
    (unbound-pattern (c)
      (declare (ignore c))
      p)))

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
             (mapcar (lambda-match0
                       ((cons generator subpattern)
                        (cons generator (pattern-expand-all subpattern))))
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
         (sb-int:named-lambda ',name (,clauses) ,@body)
         #-sbcl
         (lambda ,args ,@body)))

(defoptimizer :trivial (types clauses)
  (declare (ignore types))
  clauses)




;;;; external apis

(defmacro match (what &body clauses)
  `(match* (,what)
     ,@(mapcar (lambda-match0
                 ((list* pattern body)
                  (list* (list pattern) body)))
               clauses)))

(defmacro match* (whats &body clauses)
  `(match+ ,whats
           ,(make-list (length whats) :initial-element t)
           ;; ^^^^ this part can surely be improved by using &environment intensively!
           ,@clauses))

(defmacro match+ (whats types &body clauses)
  "Variant of match* : can specify the inferred types of each argument"
  (%match whats types clauses))

(defun %match (args types clauses)
  `(match1* ,args
     ,@(funcall (symbol-optimizer *optimizer*)
                types
                (mapcar (lambda-match0
                          ((list* patterns body)
                           (list* (mapcar #'pattern-expand patterns) body)))
                        clauses))))

