
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
                ((constantp p) `(constant ,p)) ;; see derived
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
  "Adds a new derived pattern. &optional arguments are, when the default
value is not supplied, defaulted to '_, instead of nil."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-pattern ',name)
           #+sbcl
           (sb-int:named-lambda ',name ,(process-lambda-args args) ,@body)
           #-sbcl
           (lambda ,(process-lambda-args args) ,@body))))

(defun process-lambda-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&optional rest)
     (list* '&optional (process-optional-args rest)))
    ((list* '&key rest) args)
    ((list* '&rest rest) args)
    ((list* '&aux rest) args)
    ((list* thing rest)
     (list* thing (process-lambda-args rest)))))

(defun process-optional-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&key rest) args)
    ((list* '&rest rest) args)
    ((list* '&aux rest) args)
    ((list* (list name) rest)
     (list* (list name ''_) (process-optional-args rest)))
    ((list* (list name default) rest)
     (list* (list name default) (process-optional-args rest)))
    ((list* (list name default pred) rest)
     (list* (list name default pred) (process-optional-args rest)))
    ((list* name rest)
     (list* (list name ''_) (process-optional-args rest)))))
      
;;;; optimizer database
(lispn:define-namespace optimizer (function (list &key &allow-other-keys) list))
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

(defoptimizer :trivial (clauses &key &allow-other-keys)
  clauses)

;;;; primitive apis


(defun make-gensyms (list &optional (name "G"))
  (mapcar (lambda (x) (declare (ignore x)) (gensym name)) list))
(defun pad (max clause)
  (ematch0 clause
    ((list* patterns body)
     (let ((patterns (ensure-list patterns)))
       (list* (if (zerop max)
                  nil
                  (append patterns
                          (make-list (- max (length patterns))
                                     :initial-element '_)))
              body)))))

(defmacro match2 (what &body clauses)
  "In match2/match2*, the last clause is not enclosed in a block.
Therefore, using `next' in the last clause results in jumping to the next innermost matching construct,
or results in a compilation error when this is the outermost matching construct."
  `(match2+ ,what t ,@clauses))

(defmacro match2+ (what type &body clauses)
  "Variant of match2 : can specify the inferred type of the argument"
  `(match2*+ (,what) (,type)
     ,@(mapcar (lambda-ematch0
                 ((list* pattern body)
                  (list* (list pattern) body)))
               clauses)))


;;;; primitive multi-match

(defmacro match2* (whats &body clauses)
  "In match2/match2*, the last clause is not enclosed in a block.
Therefore, using `next' in the last clause results in jumping to the next innermost matching construct,
or results in a compilation error when this is the outermost matching construct."
  `(match2*+ ,whats
       ,(make-list (length whats) :initial-element t)
     ;; ,(mapcar #'form-type whats)
     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^ TODO: use Bike/compiler-macro
     ,@clauses))

(defmacro match2*+ ((&rest whats) (&rest types) &body clauses)
  "Variant of match2* : can specify the inferred types of each argument"
  ;; Actually, this is the main expander
  (let* ((args (make-gensyms whats "ARG"))
         (bindings (mapcar #'list args whats)) 
         (clauses (mapcar (curry #'pad (length whats)) clauses))
         (clauses (mapcar #'expand-clause clauses))
         (clauses* (if args
                       (funcall (symbol-optimizer *optimizer*)
                                clauses :types types)
                       ;; if the number of argument is zero, there is no use
                       clauses)))
    `(symbol-macrolet ,bindings
       (declare (ignorable ,@args))
       (declare ,@(remove nil
                          (mapcar (lambda (arg type)
                                    (unless (eq t type) `(type ,type ,arg)))
                                  args types)))
       ,(generate-multi-matcher args nil clauses*))))

(defun expand-clause (clause)
  (ematch0 clause
    ((list* patterns body)
     (list* (expand-multipatterns patterns)
            body))))
(defun expand-multipatterns (patterns)
  (ematch0 patterns
    ((list) nil)
    ((list* first rest)
     (let ((first* (correct-pattern (pattern-expand-all first))))
       (cons first*
             (let ((*lexvars* (variables first*)))
               (expand-multipatterns rest)))))))

(defun generate-multi-matcher (args *lexvars* clauses &optional in-clause-block)
  ;; take 3 : switch to the genuine BDD-based matcher
  ;;((lambda (x) (macroexpand x) x) ;; for error checking
  `(match1 ,(first args)
     ,@(mapcar1-with-next 
        (lambda (clause next-clause-exists)
          (ematch0 clause
            ((list* nil body)
             `(,(pattern-expand '_) ,@body))
            ((list* (list* pattern patterns) body)
             `(,pattern
               ,(generate-multi-matcher
                 (rest args)
                 (append *lexvars* (variables pattern)) ;; bind *lexvars*
                 `((,patterns ,@body))
                 (or in-clause-block next-clause-exists))))))
        clauses)
     ,@(when in-clause-block `((,(pattern-expand-all '_) (next))))))

(defun mapcar1-with-next (fn list)
  (ematch0 list
    ((cons it nil)
     (cons (funcall fn it nil) nil))
    ((cons it rest)
     (cons (funcall fn it t) (mapcar1-with-next fn rest)))))

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
   (values :initarg :values :reader match-error-values)))

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


(defmacro defun-match (name (arg) clauses)
  `(defun ,name (,arg)
     (match ,arg
       ,@clauses)))
(defmacro defun-ematch (name (arg) clauses)
  `(defun ,name (,arg)
     (ematch ,arg
       ,@clauses)))
(defmacro defun-cmatch (name (arg) clauses)
  `(defun ,name (,arg)
     (cmatch ,arg
       ,@clauses)))

(defmacro defun-match* (name args clauses)
  `(defun ,name ,args
     (match* ,args
       ,@clauses)))
(defmacro defun-ematch* (name args clauses)
  `(defun ,name ,args
     (ematch* ,args
       ,@clauses)))
(defmacro defun-cmatch* (name args clauses)
  `(defun ,name ,args
     (cmatch* ,args
       ,@clauses)))

