
(in-package :trivia.level2.impl)

;;;; derived pattern database

(lispn:define-namespace pattern function)
(lispn:define-namespace inline-pattern function)

(define-condition wildcard () ())

(defun wildcardp (pattern)
  (and (symbolp pattern)
       (or (string= "_" (symbol-name pattern))
           (string= "OTHERWISE" (symbol-name pattern)))))


(defun variablep (pattern)
  (and (symbolp pattern)
       (string/= "_" (symbol-name pattern))
       (string/= "OTHERWISE" (symbol-name pattern))))


(defun pattern-expand-1 (p)
  "expand the given pattern once, just like macroexpand-1.
 Returns (values expansion-form expanded-p)"
  (if (atom p)
      (values (cond
                ((constantp p) `(constant ,p)) ;; see derived
                ((wildcardp p)
                 (restart-case
                     ;; upper pattern-expand would handle this.
                     ;; below is a workaround for ECL's bug in with-condition-restart
                     #+ecl (progn (signal 'wildcard))
                     #-ecl (signal 'wildcard)
                   (continue ()))
                 (with-gensyms (it) `(guard1 ,it t)))
                ((variablep p)
                 `(guard1 ,p t))
                (t (error "what is this? ~a" p)))
              t)
      (ematch0 p
        ((list* 'guard1 _) p)
        ((list* 'or1 _)    p)
        ((list* name args)
         (values 
          (handler-case
              ;; If the derived pattern exists, then call the expander function
              (apply (symbol-pattern name) args)
            (unbound-pattern (c)
              ;; otherwise, unbound-pattern is signalled.
              ;; (see the macroexpansion of (lispn:define-namespace pattern function).)
              ;; If the pattern name matches some convention, like a class name,
              ;; then it is handled as an implicit pattern.
              ;; Otherwise, signal a compile error.
              (if (or (find-class name nil)
                      (fboundp (predicatep name))
                      (fboundp (predicate-p name)))
                  `(structure ,name ,@args)
                  (error c))))
          t)))))

(defun pattern-expand (p)
  "expand the given pattern downto level1 pattern (i.e. until no expansion is available),
just like macroexpand"
  (labels ((rec (p)
             (multiple-value-bind (new expanded1) (pattern-expand-1 p)
               (if expanded1
                   (rec new)
                   new))))
    (multiple-value-bind (new expanded1) (pattern-expand-1 p)
      (if expanded1
          (values (rec new) t)
          new))))

(defun pattern-expand-all (p)
  "expand the given pattern recursively"
  ;; should start by guard1 or or1
  (let ((p (inline-pattern-expand p)))
    (assert (= (length p) 1) nil "Toplevel inline pattern is invalid: ~a" p)
    (ematch0 (pattern-expand (first p))
      ((list* 'guard1 sym test more-patterns)
       (list* 'guard1 sym test
              (mappend
               (lambda (gen-pat)
                 (ematch0 gen-pat
                   ((cons generator subpattern)
                    (handler-case
                        (list generator (pattern-expand-all subpattern))
                      (wildcard () ;; remove unnecessary wildcard pattern
                        nil)))))
               (plist-alist more-patterns))))
      ((list* 'or1 subpatterns)
       (list* 'or1 (mapcar #'pattern-expand-all subpatterns))))))

(defmacro defpattern (name args &body body)
  "Adds a new derived pattern.
The default value of &optional arguments are '_, instead of nil."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-pattern ',name)
           #+sbcl
           (sb-int:named-lambda ',name ,(process-lambda-args args) ,@body)
           #-sbcl
           (lambda ,(process-lambda-args args) ,@body))
     ,@(when (stringp (first body))
         ;; lisp-namespace
         `((setf (documentation ',name 'pattern)
                 ,(let ((*print-pretty* t))
                    #-clisp
                    (format nil "~<Lambda-List: ~s~
                                 ~:@_~<  ~@;~a~:>~
                                 ~:@_Defined in ~a~
                               ~:>"
                            (list args (list (first body)) *compile-file-pathname*))
                    #+clisp
                    (format nil "Lambda-List: ~s~%~a"
                            args (first body))))))))


(defmacro defpattern-inline (name args &body body)
  "Adds a new inlined derived pattern. These patterns are evaluated from the innermost ones.
The default value of &optional arguments are '_, instead of nil."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-inline-pattern ',name)
           #+sbcl
           (sb-int:named-lambda ',name ,(process-lambda-args args) ,@body)
           #-sbcl
           (lambda ,(process-lambda-args args) ,@body))
     ,@(when (stringp (first body))
         ;; lisp-namespace
         `((setf (documentation ',name 'inline-pattern)
                 ,(let ((*print-pretty* t))
                    #-clisp
                    (format nil "~<Lambda-List: ~s~
                                 ~:@_~<  ~@;~a~:>~
                                 ~:@_Defined in ~a~
                               ~:>"
                            (list args (list (first body)) *compile-file-pathname*))
                    #+clisp
                    (format nil "Lambda-List: ~s~%~a"
                            args (first body))))))))


(defun inline-pattern-expand (p)
  "Given a pattern p, returns a list of patterns that should be inlined."
  (if (atom p)
      (list p)
      (ematch0 p
        ((list* head args)
         (cond
           ((not (and (symbolp head)
                      (or (pattern-boundp head)
                          (inline-pattern-boundp head))))
            ;; this is not a pattern, stop recursion
            (list p))
           ((and (atom args) (not (null args)))
            ;; p is a bare cons like (a . b)
            (warn "~a is not a proper list! Inline expansion fails" p)
            (list p))
           ((cdr (last args))
            (warn "~a is not a proper list! Inline expansion occurs only to the children and the last cdr" p)
            (let ((args (copy-tree args)))
              (setf (cdr (last args)) nil)
              (let ((pairs (mapcar (lambda (arg) (multiple-value-list (inline-pattern-expand arg))) args)))
                (values `((,head ,@(mappend #'car pairs) . ,(cdr (last args))))
                        (every #'cdr pairs)))))
           (t
            (let ((pairs (mapcar (lambda (arg) (multiple-value-list (inline-pattern-expand arg))) args)))
              (if (inline-pattern-boundp head)
                  (values (mappend #'inline-pattern-expand
                                   (apply (symbol-inline-pattern head) (mappend #'car pairs)))
                          t)
                  (values `((,head ,@(mappend #'car pairs)))
                          (every #'cdr pairs))))))))))

(defun process-lambda-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&optional rest)
     (list* '&optional (process-optional-args rest)))
    ((list* '&key rest) (list* '&key (process-keyword-args rest)))
    ((list* '&rest rest) (list* '&rest (process-rest-args rest)))
    ((list* '&aux rest) args)
    ((list* thing rest)
     (list* thing (process-lambda-args rest)))))

(defun process-optional-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&key rest) (list* '&key (process-keyword-args rest)))
    ((list* '&rest rest) (list* '&rest (process-rest-args rest)))
    ((list* '&aux rest) args)
    ((list* (list name) rest)
     (list* (list name ''_) (process-optional-args rest)))
    ((list* (list name default) rest)
     (list* (list name default) (process-optional-args rest)))
    ((list* (list name default pred) rest)
     (list* (list name default pred) (process-optional-args rest)))
    ((list* name rest)
     (list* (list name ''_) (process-optional-args rest)))))

(defun process-rest-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&key rest) (list* '&key (process-keyword-args rest)))
    ((list* '&aux rest) args)
    ((list* name rest)
     (list* name (process-rest-args rest)))))

(defun process-keyword-args (args)
  (ematch0 args
    (nil nil)
    ((list* '&aux rest) args)
    ((list* (list name) rest)
     (list* (list name ''_) (process-keyword-args rest)))
    ((list* (list name default) rest)
     (list* (list name default) (process-keyword-args rest)))
    ((list* (list name default pred) rest)
     (list* (list name default pred) (process-keyword-args rest)))
    ((list* name rest)
     (list* (list name ''_) (process-keyword-args rest)))))

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
           (lambda ,args ,@body))
     ,@(when (stringp (first body))
         ;; lisp-namespace
         `((setf (documentation ',name 'optimizer)
                 ,(let ((*print-pretty* t))
                    #-clisp
                    (format nil "~<Lambda-List: ~s~
                                 ~:@_~<  ~@;~a~:>~
                               ~:>"
                            (list args (list (first body))))
                    #+clisp
                    (format nil "Lambda-List: ~s~%~a"
                            args (first body))))))))

(defoptimizer :trivial (clauses &key &allow-other-keys)
  "Trivial pattern-match optimizer which does not do any optimization.
The number of checks increases linear to the number of clauses, and same checks could be run multiple times.
However, it is less likely to contain bugs."
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
         (clauses (mapcar (curry #'pad (length whats)) clauses)) ; adjust the length of the clauses
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
     (list* (postprocess-deferred (expand-multipatterns patterns))
            body))))
(defun expand-multipatterns (patterns)
  (ematch0 patterns
    ((list) nil)
    ((list* first rest)
     (let ((first* (correct-pattern (pattern-expand-all first))))
       (cons first*
             (let ((*lexvars* (variables first*)))
               (expand-multipatterns rest)))))))

(define-condition deferred ()
  ((test :initarg :test :accessor deferred-test)))

(defun postprocess-deferred (patterns)
  "equivalent to optima's lift 1/2:

   lift1:
       (list (guard x (consp x)) (guard y (eq y (car x))))
    => (guard (list x (guard y (eq y (car x)))) (consp x))
    => (guard (guard (list x y) (eq y (car x))) (consp x))
    => (guard (list x y) (and (consp x) (eq y (car x))))

   lift2:
       (list 3 (or 1 (guard x (evenp x))))
    => (or (list 3 1) (list 3 (guard x (evenp x))))
    => (or (list 3 1) (guard (list 3 x) (evenp x)))"
  ;; search for any deferred tests in patterns, then append them to the last pattern.
  (let (tests)
    (let ((patterns
           (handler-bind ((deferred (lambda (c)
                                      (push (deferred-test c) tests)
                                      (continue c))))
             (mapcar #'postprocess-deferred-1 patterns))))
      (if tests
          (with-gensyms (it)
            `(,@(butlast patterns) (guard1 ,it t
                                           ,it ,(lastcar patterns)
                                           ,@(mappend (lambda (test)
                                                        (with-gensyms (dummy)
                                                          `(t (guard1 ,dummy ,test))))
                                                      tests))))
          patterns))))

(defun postprocess-deferred-1 (pattern)
  (ematch0 pattern
    ((list* 'guard1 (list* sym fields) test more-patterns)
     (destructuring-bind (&key (deferred nil supplied-p) &allow-other-keys) fields
       (when supplied-p
         (restart-case
             ;; workaround for ECL bug;
             #-ecl (signal 'deferred :test deferred)
             #+ecl (signal (make-condition 'deferred :test deferred))
             (continue ()))))
     (list* 'guard1 (list* sym fields) test
            (alist-plist
             (mapcar (lambda-ematch0
                       ((cons gen pat)
                        (cons gen (postprocess-deferred-1 pat))))
                     (plist-alist more-patterns)))))
    ((list* 'or1 patterns)
     ;; deferred patterns do not go beyond or1 boundary.
     (list* 'or1 (mappend #'postprocess-deferred (mapcar #'list patterns))))))

;; (postprocess-deferred
;;  '((or1
;;     (guard1
;;      (a :type cons) (consp a)
;;      (car a) (guard1 (b :type t) t)
;;      (cdr a) (guard1 (c :type t) t))
;;     (guard1
;;      (a :type cons) (consp a)
;;      (car a) (guard1 (b :type t) t)
;;      (cdr a) (guard1 (c :type t :deferred (evenp c)) t)))))
;; 
;; (postprocess-deferred
;;  '((or1
;;     (guard1
;;      (a :type cons) (consp a)
;;      (car a) (guard1 (b :type t) t)
;;      (cdr a) (guard1 (c :type t :deferred (evenp c)) t))
;;     (guard1
;;      (a :type cons) (consp a)
;;      (car a) (guard1 (b :type t) t)
;;      (cdr a) (guard1 (c :type t) t)))))

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

