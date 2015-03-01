(defpackage :optima.ppcre
  (:use :cl :optima.core)
  (:import-from :alexandria
                #:with-unique-names)
  (:export #:ppcre)
  (:documentation "
### [Pattern] ppcre

Syntax:

    (ppcre REGEXP PATTERN*)

Matches REGEXP against the target string. Sub-PATTERNs will be used to
match the matched groups, if the REGEXP matched.

Examples:

    (match \"2012-11-04\"
      ((ppcre \"^(\\\\d{4})-(\\\\d{2})-(\\\\d{2})$\" year month day)
       (list year month day)))
    => (\"2012\" \"11\" \"04\")"))
(in-package :optima.ppcre)

(defstruct (ppcre-pattern (:include constructor-pattern)
                          (:constructor make-ppcre-pattern (regex &rest subpatterns)))
  regex)

(defmethod constructor-pattern-destructor-sharable-p ((x ppcre-pattern) (y ppcre-pattern))
  (equal (ppcre-pattern-regex x) (ppcre-pattern-regex y)))

(defmethod constructor-pattern-make-destructor ((pattern ppcre-pattern) var)
  (with-unique-names (it)
    (make-destructor :bindings `((,it (and (stringp ,var)
                                           (nth-value 1 (ppcre:scan-to-strings ,(ppcre-pattern-regex pattern) ,var)))))
                     :predicate-form it
                     :accessor-forms (loop for i from 0 below (constructor-pattern-arity pattern)
                                           collect `(%svref ,it ,i)))))

(defmethod parse-constructor-pattern ((name (eql 'ppcre)) &rest args)
  (apply #'make-ppcre-pattern (first args)
         (mapcar #'parse-pattern (rest args))))

(defmethod unparse-pattern ((pattern ppcre-pattern))
  `(ppcre ,(ppcre-pattern-regex pattern) ,@(ppcre-pattern-subpatterns pattern)))
