(in-package :optima.contrib)

(defmethod optima::parse-constructor-pattern ((name (eql 'ppcre)) &rest args)
  (destructuring-bind (re . patterns) args
    (optima::make-constructor-pattern
     :specifier `(ppcre ,@args)
     :signature `(ppcre ,re)
     :arguments (mapcar #'optima::parse-pattern patterns)
     :predicate (lambda (var) (values `(nth-value 1 (ppcre:scan-to-strings ,re ,var)) t))
     :accessor (lambda (var i) `(svref ,var ,i)))))
