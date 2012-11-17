(in-package :optima.contrib)

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
