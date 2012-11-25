(defpackage :optima.ppcre
  (:use :cl :optima)
  (:export #:ppcre)
  (:documentation "
### [Pattern] ppcre

Syntax:

    (ppcre REGEXP PATTERN*)

Matches REGEXP against the target string. Sub-PATTERNs will be used to
match the matched groups, if REGEXP matched.

Examples:

    (match \"2012-11-04\"
      ((ppcre \"^\\\\d{4}-\\\\d{2}-\\\\d{2}$\" year month day)
       (list year month day)))
    => (\"2012\" \"11\" \"04\")"))
(in-package :optima.ppcre)

(defstruct (ppcre-pattern (:include optima::constructor-pattern)
                          (:constructor make-ppcre-pattern (regex &rest subpatterns)))
  regex)

(defmethod optima::destructor-equal ((x ppcre-pattern) (y ppcre-pattern))
  (equal (ppcre-pattern-regex x) (ppcre-pattern-regex y)))

(defmethod optima::destructor-predicate-form ((pattern ppcre-pattern) var)
  (values `(nth-value 1 (ppcre:scan-to-strings ,(ppcre-pattern-regex pattern) ,var)) t))

(defmethod optima::destructor-forms ((pattern ppcre-pattern) var)
  (loop for i from 0 below (optima::constructor-pattern-arity pattern)
        collect `(optima::%svref ,var ,i)))

(defmethod optima::parse-constructor-pattern ((name (eql 'ppcre)) &rest args)
  (apply #'make-ppcre-pattern (first args)
         (mapcar #'optima::parse-pattern (rest args))))

(defmethod optima::unparse-pattern ((pattern ppcre-pattern))
  `(ppcre ,(ppcre-pattern-regex pattern) ,@(ppcre-pattern-subpatterns pattern)))
