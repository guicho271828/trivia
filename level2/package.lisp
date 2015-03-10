;;;; trivia level2
(defpackage :trivia.level2
  (:use :trivia.level1)
  (:export :match :match*
           :ematch :ematch*
           :cmatch :cmatch*
           :match+
           :match-error
           :match-error-pattern
           :match-error-values
           :multiple-value-cmatch
           :multiple-value-ematch
           :multiple-value-match
           ;; level1
           :match1* :match1 :or1 :guard1 :variables
           :*or-pattern-allow-unshared-variables*
           :or1-pattern-inconsistency
           :guard1-pattern-nonlinear
           :conflicts :pattern :repair-pattern
           :correct-pattern
           :preprocess-symopts
           ;; 
           :guard
           :alist
           :plist
           :property
           :access
           :$guard1
           :$or1
           ;; 
           :defpattern
           :pattern-expand
           :pattern-expand-1
           :pattern-expand-all
           :defoptimizer
           ;; optimizer
           :optimizer
           :*optimizer*
           :in-optimizer
           :defoptimizer)
  (:nicknames :trivia))

(defpackage :trivia.level2.impl
  (:use :cl :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.level2)
  (:export :predicatep :predicate-p))

(in-package :trivia.level2.impl)




;;;; derived pattern database

(lispn:define-namespace pattern function)

(define-condition wildcard () ())

(defun wildcardp (pattern)
  (and (symbolp pattern)
       (string= "_" (symbol-name pattern))))


(defun variablep (pattern)
  (and (symbolp pattern)
       (not (string= "_" (symbol-name pattern)))))


(defun pattern-expand-1 (p)
  "expand the given pattern once, just like macroexpand-1"
  (if (atom p)
      (values (cond
                ((typep p 'sequence) `(equal ,p))
                ((constantp p) `(eq ,p))
                ((wildcardp p) 
                 (signal 'wildcard) ;; upper pattern-expand would handle this
                 (with-gensyms (it) `(guard1 ,it t)))
                ((variablep p)
                 `(guard1 ,p t))
                (t (error "what is this? ~a" p)))
              t)
      (ematch0 p
        ((list* 'guard1 _) p)
        ((list* 'or1 _)    p)
        ((list* name args)
         ;; handle implicit patterns
         (values 
          (handler-case
              (apply (symbol-pattern name) args)
            (unbound-pattern (c)
              (if (or (find-class name nil)
                      (fboundp (predicatep name))
                      (fboundp (predicate-p name)))
                  `(structure ,name ,@args)
                  (error c))))
          t)))))

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
  (ematch0 (pattern-expand p)
    ((list* 'guard1 sym test more-patterns)
     (list* 'guard1 sym test
            (alist-plist
             (mappend
              (lambda-ematch0
                ((cons generator subpattern)
                 (handler-case
                     (list (cons generator (pattern-expand-all subpattern)))
                   (wildcard () ;; remove wildcard pattern
                     nil))))
                     (plist-alist more-patterns)))))
    ((list* 'or1 subpatterns)
     (list* 'or1 (mapcar #'pattern-expand-all subpatterns)))))

  
(defmacro defpattern (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-pattern ',name)
           #+sbcl
           (sb-int:named-lambda ',name ,args ,@body)
           #-sbcl
           (lambda ,args ,@body))))




;;;; optimizer database

(lispn:define-namespace optimizer (function (list list &key &allow-other-keys) list))
(defvar *optimizer* :trivial)
(defmacro in-optimizer (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *optimizer* ',name)))

(defmacro defoptimizer (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-optimizer ',name)
           #+sbcl
           (sb-int:named-lambda ',name ,args ,@body)
           #-sbcl
           (lambda ,args ,@body))))

(defoptimizer :trivial (types clauses)
  (declare (ignore types))
  clauses)

;;;; external apis

(defun ensure-multipattern (clauses)
  (mapcar (lambda-ematch0
                 ((list* pattern body)
                  (list* (list pattern) body)))
          clauses))

(defmacro match (what &body clauses)
  `(match1 ,what
     ,@(funcall (symbol-optimizer *optimizer*)
                `(t)
                (mapcar (lambda-ematch0
                          ((list* pattern body)
                           (list* (correct-pattern
                                   (pattern-expand-all pattern)) body)))
                        clauses))))

(defmacro match* (whats &body clauses)
  `(match+ ,whats
       ,(make-list (length whats) :initial-element t)
     ;; ^^^^ this part can surely be improved by using &environment intensively!
     ,@(mapcar (lambda (clause)
                 (pad (length whats) clause))
               clauses)))

(defmacro match+ ((&rest whats) (&rest types) &body clauses)
  "Variant of match* : can specify the inferred types of each argument"
  `(match1* ,whats
     ,@(funcall (symbol-optimizer *optimizer*)
                types
                (mapcar (lambda-ematch0
                          ((list* patterns body)
                           (list* (mapcar (compose #'correct-pattern
                                                   #'pattern-expand-all)
                                          patterns) body)))
                        clauses))))

;;;; more external apis

(defun make-gensyms (list &optional (name "G"))
  (mapcar (lambda (x) (declare (ignore x)) (gensym name)) list))

(define-condition match-error (error)
  ((pattern :initarg :pattern :reader match-error-pattern)
   (values :initarg :values :reader match-error-values)))

(defmacro ematch (what &body clauses)
  (with-gensyms (otherwise)
    `(match ,what
       ,@clauses
       (,otherwise (error 'match-error :pattern ',clauses :values (list ,otherwise))))))

(defmacro ematch* (whats &body clauses)
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match* ,whats
       ,@clauses
       (,temps
        (error 'match-error :pattern ',clauses :values (list ,@temps))))))

(defmacro cmatch (what &body clauses)
  (with-gensyms (otherwise)
    `(match ,what
       ,@clauses
       (,otherwise (cerror "continue" 'match-error :pattern ',clauses :values (list ,otherwise))))))

(defmacro cmatch* (whats &body clauses)
  (let ((temps (make-gensyms whats "OTHERWISE")))
    `(match* ,whats
       ,@clauses
       (,temps
        (cerror "continue" 'match-error :pattern ',clauses :values (list ,@temps))))))



;;;; multiple values

(defun pad (max clause)
  (ematch0 clause
    ((list* patterns body)
     (let ((patterns (ensure-list patterns)))
       (list* (append patterns
                      (make-list (- max (length patterns)) :initial-element '_)) 
              body)))))

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
