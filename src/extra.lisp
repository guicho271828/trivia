(in-package :optima.extra)

(defmethod optima::parse-constructor-pattern ((name (eql 'plist)) &rest args)
  (let (keys values)
    (loop for (key value) on args by #'cddr
       do (push key keys)
       do (push value values))
    (unless (= (length keys)
               (length values))
      (error "plist pattern has an extra key: ~A" args))
    (optima::make-constructor-pattern
     :signature `(plist ,@keys)
     :arguments (mapcar #'optima::parse-pattern values)
     :predicate (lambda (var) `(zerop (rem (length ,var) 2))) ; Optimize/simplify to just consp?
     :accessor (lambda (var i) `(getf ,var (nth ,i ',keys))))))

(defmethod optima::parse-constructor-pattern ((name (eql 'alist)) &rest args)
  (let (keys values)
    (loop for (key . value) in args
       do (push key keys)
       do (push value values))
    (optima::make-constructor-pattern
     :signature `(alist ,@keys)
     :arguments (mapcar #'optima::parse-pattern values)
     :predicate (lambda (var) `(every #'consp ,var)) ; Optimize/simplify to just consp?
     :accessor (lambda (var i) `(cdr (assoc (nth ,i ',keys) ,var))))))
