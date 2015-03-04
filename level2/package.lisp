(defpackage :optima.level2
  (:export :match
           :match*
           :guard
           :defpattern
           :define-pattern-transformer))

(defpackage :optima.level2.impl
  (:use :cl :alexandria
        :optima.level0
        :optima.level1
        :optima.level2))

(in-package :optima.level2.impl)

(defmacro match (what &body clauses)
  `(match* (,what)
     ,@(mapcar (lambda-match0
                 ((list* pattern body)
                  (list* (list pattern) body)))
               clauses)))

(defmacro match* (whats &body clauses)
  (%match whats clauses))

(defun %match (args clauses)
  

  )

(lispn:define-namespace pattern function)
(lispn:define-namespace pattern-transformer function)
(lispn:define-namespace optimizer function)

