(in-package :trivia.level2.impl)

(defpattern and (&rest subpatterns)
  (expand-and subpatterns))
(defun expand-and (subpatterns)
  (ematch0 subpatterns
    ((list) '_)
    ((list sp) sp)
    ((list* subpatterns)
     ;; implementing and-pattern requires lifting or-patterns.
     ;; For example:
     ;; (and (or1 subpat1 subpat2)
     ;;      subpat3)
     ;; should be converted into
     ;; (or1 (and subpat1 subpat3)
     ;;      (and subpat2 subpat3))
     ;; optimising the slight inefficiency of having the duplicated subpat3
     ;; is a job of optimiser.
     (let* ((subpatterns (handler-bind ((wildcard (lambda (c) (continue c))))
                           (mapcar #'pattern-expand subpatterns)))
            (or1  (find 'or1 subpatterns :key #'car))
            (guard1-patterns (remove or1 subpatterns)))
       (if or1
           (ematch0 or1
             ((list* 'or1 or-subpatterns)
              (list* 'or1
                     (mapcar (lambda (or-sp)
                               `(and ,or-sp ,@guard1-patterns))
                             or-subpatterns))))
           ;; no or pattern; perform lifting
           (combine-guard1-subpatterns guard1-patterns))))))

(defun combine-guard1-subpatterns (guard1-patterns)
  (assert (every (lambda-match0 ((list* 'guard1 _) t)) guard1-patterns)
          nil "In EXPAND-AND: Found or1 pattern after or-lifting!")
  (with-gensyms (intersection)
    (labels ((wrap-test (syms tests more-patterns)
               (ematch0 tests
                 (nil more-patterns)
                 ((list* test t-rest)
                  `(,intersection (guard1 ,(first syms) ,test
                                          ,@(first more-patterns)
                                          ,@(wrap-test (rest syms) t-rest (rest more-patterns))))))))
      ;; now that all subpatterns are guard1, we can safely assume this;
      (let* ((symopts (mapcar #'second guard1-patterns))
             (tests  (mapcar #'third guard1-patterns))
             (more-patterns (mapcar #'cdddr guard1-patterns)))
        `(guard1 ,intersection t
                 ,@(wrap-test symopts tests more-patterns))))))

(defpattern guard (subpattern test-form &rest more-patterns)
  (with-gensyms (guard)
    `(and ,subpattern
          (guard1 (,guard :deferred ,test-form) t ,@more-patterns))))


(defun subst-notsym (pattern symopt?)
  "substitute a symbol with an anonymous symbol, in order to avoid capturing the variables inside NOT pattern.
For example,

  (let ((it 1))
    (match 2
      ((not (guard it (eql it 3))) it)
      (_ :fail)))

This should return 1, however without proper renaming of variable `it', `it' will be bound to NIL."
  (let ((sym (car (preprocess-symopts symopt? pattern))))
     (with-gensyms (notsym)
       (subst notsym sym pattern))))

(defun deferred-p (symopt?)
  "returns if the symopt has :deferred flag"
  (let ((sym (preprocess-symopts symopt? nil)))
    (destructuring-bind (oldsym &key (deferred nil supplied-p) &allow-other-keys) sym
      (declare (ignore oldsym deferred))
      supplied-p)))

(defun negate-deferred (symopt?)
  "make a new symopt whose :deferred test is negated if set"
  (let ((sym (copy-list (preprocess-symopts symopt? nil))))
    (with-gensyms (negsym)
      (destructuring-bind (oldsym &key (deferred nil supplied-p) &allow-other-keys) sym
        (when supplied-p
          (setf (getf (cdr sym) :deferred) `(not ,(subst negsym oldsym deferred)))))
      (setf (car sym) negsym))
    sym))

(defun make-negated-case (sym test)
  "Creates a negated pattern. If the pattern is deferred, the deferred test
should be negated, but the test itself should remain T
 (Otherwise the matching fails at this point because the test (not t) always fails.)"
  (let ((negsym (negate-deferred sym)))
    (if (deferred-p sym)
        `(guard1 ,negsym t)
        `(guard1 ,sym (not ,test)))))

(defpattern not (subpattern)
  (ematch0 (pattern-expand subpattern)
    ((list* 'guard1 sym test guard1-subpatterns)
         ;; no symbols are visible from the body
     (let ((negated-case (make-negated-case sym test)))
       (subst-notsym
        (if guard1-subpatterns
            `(or1 ,negated-case
                  (guard1 ,sym ,test
                          ,@(alist-plist
                             (mapcar
                              (lambda-ematch0
                                ((cons generator test-form)
                                 (cons generator `(not ,test-form)))) 
                              (plist-alist guard1-subpatterns)))))
            negated-case)
        sym)))
    ((list* 'or1 or-subpatterns)
     `(and ,@(mapcar (lambda (or-sp)
                       `(not ,or-sp))
                     or-subpatterns)))))

(defpattern or (&rest subpatterns)
  `(or1 ,@subpatterns))

(defpattern quote (x)
  `(constant ',x))

(defpattern cons (a b)
  (with-gensyms (it)
    `(guard1 (,it :type cons) (consp ,it) (car ,it) ,a (cdr ,it) ,b)))

(defpattern null ()
  (with-gensyms (it)
    `(guard1 (,it :type null) (null ,it))))

(defpattern list (&rest args)
  (if args
      `(cons ,(car args) (list ,@(cdr args)))
      `(null)))

(defpattern list* (&rest args)
  (if (cdr args)
      `(cons ,(car args) (list* ,@(cdr args)))
      (car args)))

(defun set-vector-matcher (name &optional (ref 'aref) need-type soft)
  (let* ((level2p (find-package :trivia.level2))
         (name* (intern (format nil "~a*" name) level2p)))
    (export name* level2p)
    (setf (symbol-pattern (if soft name* name))
          (lambda (&rest patterns)
            (with-gensyms (it)
              (let* ((len (length patterns))
                     (type `(,name
                             ,@(when need-type '(*))
                             ,(if soft '* len))))
                `(guard1 (,it :type ,type)
                         (typep ,it ',type)
                         ,@(mappend (lambda (pattern i)
                                      `(,(if soft
                                             `(when (< ,i (array-total-size ,it))
                                                (,ref ,it ,i))
                                             `(,ref ,it ,i)) ,pattern))
                                    patterns (iota len)))))))))

(dolist (s '(string bit-vector base-string))
  ;; strict vector matching
  (set-vector-matcher s)
  ;; soft vector matching where the insufficient elements are given NIL
  (set-vector-matcher s 'aref nil t))
(dolist (s '(simple-string simple-bit-vector simple-base-string))
  (set-vector-matcher s 'aref nil)
  (set-vector-matcher s 'aref nil t))

(set-vector-matcher 'vector 'aref t)
(set-vector-matcher 'vector 'aref t t)
(set-vector-matcher 'simple-vector 'svref nil)
(set-vector-matcher 'simple-vector 'svref nil t)

(defpattern sequence (&rest args)
  (with-gensyms (it)
    `(guard1 (,it :type sequence)
             (typep ,it 'sequence)
             ,@(mappend (lambda (arg i)
                          `((elt ,it ,i) ,arg))
                        args
                        (iota (length args))))))

(defpattern satisfies (predicate-name)
  (with-gensyms (it)
    `(guard1 ,it (,predicate-name ,it))))

;; here is a lot of possibility; e.g. strings can be compared in char-wise, etc.
(dolist (s '(equal equalp))
  (setf (symbol-pattern s)
        (lambda (arg)
          (with-gensyms (it)
            `(guard1 (,it :type ,(if (constantp arg)
                                     (type-of arg) t))
                     (,s ,it ,arg))))))
(dolist (s '(eq eql))
  (setf (symbol-pattern s)
        (lambda (arg)
          (with-gensyms (it)
            `(guard1 (,it :type (eql ,arg)) (,s ,it ,arg))))))


(defpattern type (type-specifier)
  (with-gensyms (it)
    `(guard1 (,it :type ,type-specifier)
             (typep ,it ',type-specifier))))

(defpattern access (accessor pattern)
  (let ((accessor (ematch0 accessor
                    ((list 'function name) name)
                    ((list 'quote name) name)
                    (_ (error "[access] 1st arg is not a function designator")))))
    (with-gensyms (it)
      `(guard1 ,it t (,accessor ,it) ,pattern))))

(defpattern assoc (item pattern &key key test)
  (with-gensyms (it)
    `(guard1 (,it :type list)
             (listp ,it)
             (cdr (assoc ,item ,it
                         ,@(when key `(:key ,key))
                         ,@(when test `(:test ,test)))) ,pattern)))

(defpattern property (key pattern &optional (default nil) foundp)
  (with-gensyms (it it2)
    `(guard1 (,it :type list)
             (listp ,it)
             (getf ,it ,key)
             (guard1 ,it2 t
                     (if ,it2 t nil) ,foundp
                     (or ,it2 ,default) ,pattern))))

(defpattern alist (&rest args)
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(assoc ,key ,pattern)))
                  args)))

(defpattern plist (&rest args)
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(property ,key ,pattern)))
                  (plist-alist args))))

;;;; class pattern etc.

(defpattern class (name &rest slots)
  ;; in v2, class pattern is just a sugar
  `(structure ,name ,@slots))

(defpattern structure (name &rest slots)
  (with-gensyms (it)
    `(guard1
      ,@(cond
          ((find-class name nil)
           `((,it :type ,name) (typep ,it ',name)))
          ((fboundp (predicatep name))
           (let ((fn-name (predicatep name)))
             `((,it :type ,(easy-infer-type fn-name)) (,fn-name ,it))))
          ((fboundp (predicate-p name))
           (let ((fn-name (predicate-p name)))
             `((,it :type ,(easy-infer-type fn-name)) (,fn-name ,it))))
          (t (simple-style-warning 
              "failed to infer the type-checking predicate of ~a in compile time, forced to use runtime check!"
              name)
             `(,it
               (or (typep ,it ',name)
                   (when (fboundp ',(predicatep name)) (funcall (symbol-function ',(predicatep name)) ,it))
                   (when (fboundp ',(predicate-p name)) (funcall (symbol-function ',(predicate-p name)) ,it))))))
      ,@(map-accessors (parse-slots slots)
                       it name))))


;;; checking the class type predicate

(defun easy-infer-type (fn-sym)
  "same thing as in type-i:unary-function. copied here in order to remove dependency"
  (let* ((name (symbol-name fn-sym))
         (l (length name)))
    (match0 (coerce (subseq name (- l 2) l) 'list)
      ((list #\- #\P)
       (when-let ((typesym (find-symbol (subseq name 0 (- l 2))
                                        (symbol-package fn-sym))))
         (if (subtypep typesym t) typesym t)))
      ((list _ #\P)
       (when-let ((typesym (find-symbol (subseq name 0 (- l 1))
                                        (symbol-package fn-sym))))
         (if (subtypep typesym t) typesym t))))))

(defun predicatep (type)
  (let* ((name (symbol-name type)))
    (find-symbol (format nil "~aP" name)
                 (symbol-package type))))

(defun predicate-p (type)
  (let* ((name (symbol-name type)))
    (find-symbol (format nil "~a-P" name)
                 (symbol-package type))))

;;; checking the slots
;; KLUDGE: both optima and trivia have a very naive specification of which
;; slot is actually used from the specified keyword, slot name and so on.
;; First, lets parse the slot specification patterns:

(defun parse-slots (slots)
  "canonicalize the slot into (symbol pattern) form"
  (ematch0 slots
    ((list) nil)
    ((list* (list name pattern) rest)
     (list* (list name pattern) (parse-slots rest)))
    ((list something)
     (etypecase something
       (keyword (error "the last slot is a keyword!"))
       (symbol
        (list (list something something)))))
    ((list something next)
     (etypecase something
       (keyword
        (list (list something next)))
       (symbol
        (list* (list something something)
               (parse-slots (list next))))))
    ((list* something next rest)
     (etypecase something
       (keyword
        (list* (list something next)
               (parse-slots rest)))
       (symbol
        (list* (list something something)
               (parse-slots (list* next rest))))))))

;; Next, lets review what is the most preferable method amoung several
;; means of accessing the slot value of an object. SBCL manual states that
;; (as of 2015/12/19):
;;
;; ----------quote starts here---------------
;; 6.1 Slot access
;; 6.1.1 Structure object slot access
;; 
;; Structure slot accessors are efficient only if the compiler
;; is able to open code them: compiling a call to a structure
;; slot accessor before the structure is defined, declaring one
;; notinline, or passing it as a functional argument to another
;; function causes severe performance degradation.
;; 
;; 6.1.2 Standard object slot access
;; 
;; The most efficient way to access a slot of a standard-object
;; is by using slot-value with a constant slot name argument
;; inside a defmethod body, where the variable holding the
;; instance is a specializer parameter of the method and is
;; never assigned to. The cost is roughly 1.6 times that of an
;; open coded structure slot accessor.
;; 
;; Second most efficient way is to use a CLOS slot accessor, or
;; slot-value with a constant slot name argument, but in
;; circumstances other than specified above. This may be up to 3
;; times as slow as the method described above.
;; 
;; ----------quote ends here---------------
;;
;; We consider the other implementation has the similar situation.

;; We anyway distinguish the cases where there are any class naming type...

(defun map-accessors (parsed it type)
  (if (find-class type nil)
      (mappend (curry #'accessor-form it type) parsed)
      (mappend (curry #'accessor-form-using-function it type) parsed)))

;; We then discuss the slot specification in the pattern. A slot
;; specification is a list (slot-name subpattern) where slot-name might be
;; a symbol (possibly keywords). A slot specification is an element of the
;; results returned from parse-slots.
;; 
;; Overall, in any case where it is not possible to reduce the
;; equally-preferable ways to access the slot to a single method, it should
;; signal an error. This principle also holds for the other cases.

;; In the following, we discuss the case when the slot specification is
;; applied to a class.  When the slot-name is a keyword, we should consider
;; the following cases:
;; 
;; 1. the keyword is one of the initargs of the slot. However there might
;; be multiple slots with the same initarg.  In this case, it signals an
;; error and suggest that the user should use another means of specifying
;; the slot.
;; 
;; 2. the keyword is not one of the initargs of the slot.  one of the the
;; slot name is string= to the keyword and the slot name is visible from
;; the current package. Since there is only one symbol with the same name
;; in the current package, this effectively limits the possible number of
;; candidates to one.
;; 
;; 3. the keyword is not one of the initargs of the class.  There are
;; several slots whose names are string= to the keyword but none of them
;; are visible from the current package. In this case, signal an error
;; suggesting that the slot name be imported.
;;
;; 4. the keyword is not one of the initargs of the class.  There are no
;; slots whose names are string= to the keyword either.  However there is a
;; reader whose names are string= to <class>-<slot> or <class><slot> or
;; <slot>.  similarly to the case 2. and case 3., they may or may not be
;; visible from the current package. Also multiple cases
;; (e.g. <class>-<slot> and <slot>) may match.


(defun accessor-form (it type parsed1)
  "used when the type is a class, structure etc."
  (let ((c (find-class type)))
    (ignore-errors
      (c2mop:finalize-inheritance c))
    (ematch0 parsed1
      ((list slot pattern)
       (or (if-let ((dslot (find-direct-slot slot c)))
             (if-let ((reader (first (c2mop:slot-definition-readers dslot))))
               `((,reader ,it) ,pattern)
               ;; structures
               (if-let ((reader (hyphened type slot)))
                 `((,reader ,it) ,pattern)
                 (progn
                   (simple-style-warning
                    "No reader for slot ~a in class ~a: Forced to use slot-value.
Maybe using conc-name for the structure-object?"
                    (c2mop:slot-definition-name dslot) type)
                   `((slot-value ,it ',(c2mop:slot-definition-name dslot))
                     ,pattern)))))
           ;; (simple-style-warning
           ;;  "Failed to find slot ~a in class ~a: Forced to infer function-based accessor"
           ;;  slot type)
           (map-accessors-function it type parsed1))))))

(defun find-direct-slot (slot/keyword c)
  (or (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name)
      (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name
            :test #'string=)
      (some (lambda (c)
              (find-direct-slot slot/keyword c))
            (c2mop:class-direct-superclasses c))))

;;; searching accessor functions

(defun accessor-form-using-function (it type parsed1)
  "Used when there are no such type. Certain naming conventions are
recognized. The predicate should exist in the current package or it will
not be recognized."
  (ematch0 parsed1
    ((list slot pattern)
     (if-let ((reader (or (hyphened type slot)
                          (concname type slot)
                          (nameonly type slot))))
       `((,reader ,it) ,pattern)
       (error "Failed to find the accessor of slot ~a of type ~a!" slot type)))))

(defun hyphened (type slot)
  (when-let ((sym (find-symbol
                   (concatenate 'string
                                (symbol-name type)
                                "-"
                                (symbol-name slot)))))
    (when (fboundp sym) sym)))

(defun concname (type slot)
  (when-let ((sym (find-symbol
                   (concatenate 'string
                                (symbol-name type)
                                (symbol-name slot)))))
    (when (fboundp sym) sym)))

(defun nameonly (type slot)
  (declare (ignorable type slot))
  (when-let ((sym (find-symbol (symbol-name slot)))) ; slot may be a keyword!
    (when (fboundp sym) sym)))


(defpattern constant (x)
  (typecase x
    (simple-base-string `(simple-base-string ,@(coerce x 'list)))
    (base-string `(base-string ,@(coerce x 'list)))
    (simple-string `(simple-string ,@(coerce x 'list)))
    (string `(string ,@(coerce x 'list)))
    (simple-bit-vector `(simple-bit-vector ,@(coerce x 'list)))
    (bit-vector `(bit-vector ,@(coerce x 'list)))
    (simple-vector `(simple-vector ,@(coerce x 'list)))
    (vector `(vector ,@(coerce x 'list)))
    ((or structure-object hash-table) `(equalp ,x))
    ((or array pathname) `(equal ,x))
    (cons `(equal ,x)) ;; (quote ...)
    ((or number character) `(eql ,x))
    (t `(eq ,x))))



(defpattern place (place &optional eager)
  ;; optional arguments in defpattern is defaulted to _, not nil
  `(guard1 (,place :place t) t ,place ,eager))
