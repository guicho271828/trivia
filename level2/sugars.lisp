(in-package :trivia.level2.impl)

;;;; external apis

(defmacro match (what &body clauses)
  `(match2 ,what
     ,@clauses
     (_ nil)))

(defmacro match* (whats &body clauses)
  `(match2* ,whats
     ,@clauses
     (_ nil)))



(define-condition match-error (error)
  ((pattern :initarg :pattern :reader match-error-pattern)
   (values :initarg :values :reader match-error-values))
  (:report (lambda (c s)
             (format s "Pattern: ~s ~& Values: ~s ~&"
                     (match-error-pattern c)
                     (match-error-values c)))))

(defmacro ematch (what &body clauses)
  (with-gensyms (otherwise)
    `(match2 ,what
       ,@clauses
       (,otherwise (error 'match-error :pattern ',clauses :values (list ,otherwise))))))

(defmacro ematch* (whats &body clauses)
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match2* ,whats
       ,@clauses
       (,temps
        (error 'match-error :pattern ',clauses :values (list ,@temps))))))

(defmacro cmatch (what &body clauses)
  (with-gensyms (otherwise)
    `(match2 ,what
       ,@clauses
       (,otherwise (cerror "continue" 'match-error :pattern ',clauses :values (list ,otherwise))))))

(defmacro cmatch* (whats &body clauses)
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match2* ,whats
       ,@clauses
       (,temps
        (cerror "continue" 'match-error :pattern ',clauses :values (list ,@temps))))))


(defun call-with-mvb-temp-vars (clauses fn)
  (let* ((max (reduce #'max (mapcar (compose #'length #'ensure-list #'car) clauses)))
         (clauses (mapcar (curry #'pad max) clauses))
         (temps (mapcar (lambda (x) (declare (ignore x)) (gensym)) (iota max))))
    (funcall fn clauses temps)))

(defmacro multiple-value-match (values-form &body clauses)
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind  ,temps ,values-form
        (match* ,temps
          ,@clauses)))))

(defmacro multiple-value-ematch (values-form &body clauses)
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind ,temps ,values-form
        (match* ,temps
          ,@clauses
          (,temps
           (error 'match-error :pattern ',clauses :values (list ,@temps))))))))

(defmacro multiple-value-cmatch (values-form &body clauses)
  (call-with-mvb-temp-vars
   clauses
   (lambda (clauses temps)
     `(multiple-value-bind ,temps ,values-form
        (match* ,temps
          ,@clauses
          (,temps
           (cerror "continue" 'match-error :pattern ',clauses :values (list ,@temps))))))))



;;;; lambda-match family

(defmacro lambda-match (&body clauses)
  (with-gensyms (clause)
    `(lambda (,clause)
       (match ,clause
         ,@clauses))))
(defmacro lambda-ematch (&body clauses)
  (with-gensyms (clause)
    `(lambda (,clause)
       (ematch ,clause
         ,@clauses))))
(defmacro lambda-cmatch (&body clauses)
  (with-gensyms (clause)
    `(lambda (,clause)
       (cmatch ,clause
         ,@clauses))))

(defmacro lambda-match* (&body clauses)
  (let ((gensyms (make-gensyms (caar clauses))))
    `(lambda ,gensyms
       (match* ,gensyms
         ,@clauses))))
(defmacro lambda-ematch* (&body clauses)
  (let ((gensyms (make-gensyms (caar clauses))))
    `(lambda ,gensyms
       (ematch* ,gensyms
         ,@clauses))))
(defmacro lambda-cmatch* (&body clauses)
  (let ((gensyms (make-gensyms (caar clauses))))
    `(lambda ,gensyms
       (cmatch* ,gensyms
         ,@clauses))))

;;;; defun-match family
(defmacro defun-match (name (arg) &body clauses)
  `(defun ,name (,arg)
     (match ,arg
       ,@clauses)))
(defmacro defun-ematch (name (arg) &body clauses)
  `(defun ,name (,arg)
     (ematch ,arg
       ,@clauses)))
(defmacro defun-cmatch (name (arg) &body clauses)
  `(defun ,name (,arg)
     (cmatch ,arg
       ,@clauses)))

(defmacro defun-match* (name args &body clauses)
  `(defun ,name ,args
     (match* ,args
       ,@clauses)))
(defmacro defun-ematch* (name args &body clauses)
  `(defun ,name ,args
     (ematch* ,args
       ,@clauses)))
(defmacro defun-cmatch* (name args &body clauses)
  `(defun ,name ,args
     (cmatch* ,args
       ,@clauses)))

;;;; extras

(defmacro if-match (pattern arg &body (then &optional else))
  "Equivalent to (match ARG (PATTERN THEN) (_ ELSE))."
  `(match ,arg
     (,pattern ,then)
     (_ ,else)))

(defmacro when-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN BODY...))."
  `(match ,arg (,pattern ,.body)))

(defmacro unless-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN) (_ BODY...))."
  `(match ,arg
     (,pattern)
     (_ ,.body)))

(defmacro let-match (bindings &body body)
  "Similar to LET, except not only a variable but also a pattern can
be used in BINDINGS."
  `(ematch* ,(mapcar #'cadr bindings)
     (,(mapcar #'car bindings) ,.body)))

(defmacro let-match* (bindings &body body)
  "Similar to LET-MATCH but matches sequentially."
  (reduce (lambda (binding form) `(let-match (,binding) ,form))
          bindings
          :from-end t
          :initial-value `(locally ,.body)))

(defmacro let-match1 (pattern arg &body body)
  "Equivalent to (let-match ((PATTERN ARG)) BODY...)."
  `(let-match ((,pattern ,arg)) ,.body))
