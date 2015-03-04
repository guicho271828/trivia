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
           :test-type
           :type-tests
           :add-type-test
           :define-type-tests
           :test-compatible-p
           ;; type utils
           :disjointp
           :all-subclasses
           :exhaustive-partitions
           :exhaustive-union
           ))

(defpackage :optima.level2.impl
  (:use :cl :alexandria
        :optima.level0
        :optima.level1
        :optima.level2))

(in-package :optima.level2.impl)

;;;; api


(defmacro match (what &body clauses)
  `(match* (,what)
     ,@(mapcar (lambda-match0
                 ((list* pattern body)
                  (list* (list pattern) body)))
               clauses)))

(defmacro match* (whats &body clauses)
  (%match whats clauses))

(defun %match (args clauses)
  ;; first step: expand all subpatterns
  (let* ((bodies (mapcar #'cdr clauses))
         (multi-patterns (mapcar #'car clauses))
         (expanded-level1 (mapcar (lambda (mp)
                                    (mapcar #'pattern-expand mp))
                                  multi-patterns)))
    (optimize-level1 expanded-level1 body)))


;;;; inference database

(lispn:define-namespace pattern function)
(lispn:define-namespace pattern-transformer function)
(lispn:define-namespace optimizer function)

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

(defmacro define-pattern-transformer (name args &body body)
  `(setf (symbol-transformer ',name)
         #+sbcl
         (sb-int:named-lambda ',name ,args ,@body)
         #-sbcl
         (lambda ,args ,@body)))

(defmacro defoptimizer (name args &body body)
  `(setf (symbol-optimizer ',name)
         #+sbcl
         (sb-int:named-lambda ',name ,args ,@body)
         #-sbcl
         (lambda ,args ,@body)))
