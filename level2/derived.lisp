(in-package :trivia.level2.impl)

(defpattern and (&rest subpatterns)
  "Match when all subpatterns match."
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
  "If SUBPATTERN matches, TEST-FORM is evaluated under the lexical binding of variables in SUBPATTERN.
If TEST-FORM returns true, matching to MORE-PATTERNS are performed."
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
  "Matches when the SUBPATTERN does not match.
Variables in the subpattern are treated as dummy variables, and will not be visible from the clause body."
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
  "Match when some subpattern match."
  `(or1 ,@subpatterns))

(defpattern cons (car cdr)
  "Match against a cons cell."
  (with-gensyms (cons)
    `(guard1 (,cons :type cons) (consp ,cons) (car ,cons) ,car (cdr ,cons) ,cdr)))

(defpattern null ()
  "Match against a constant NIL."
  (with-gensyms (it)
    `(guard1 (,it :type null) (null ,it))))

(defpattern list (&rest args)
  "Match against a list with a specified length."
  (if args
      `(cons ,(car args) (list ,@(cdr args)))
      `(null)))

(defpattern list* (&rest args)
  "Match against a list with an unspecified length.
The last argument is matched against the rest of the list.
As a special case, if ARGS has a single element,
then it matches anything. This behavior is consistent with CL:LIST*
which returns itself if it takes a single argument."
  (if (cdr args)
      `(cons ,(car args) (list* ,@(cdr args)))
      (car args)))

(defpattern sequence (&rest args)
  "Match against any sequence."
  (with-gensyms (it)
    `(guard1 (,it :type sequence)
             (typep ,it 'sequence)
             ,@(mappend (lambda (arg i)
                          `((elt ,it ,i) ,arg))
                        args
                        (iota (length args))))))

(defpattern satisfies (predicate-name)
  "Match when (PREDICATE-NAME OBJ) returns true."
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
  "Match when (typep OBJ type-specifier) returns true."
  (with-gensyms (it)
    `(guard1 (,it :type ,type-specifier)
             (typep ,it ',type-specifier))))

(defpattern access (accessor pattern)
  "Matches PATTERN against the result of calling ACCESSOR."
  (let ((accessor (ematch0 accessor
                    ((list 'function name) name)
                    ((list 'quote name) name)
                    (_ (error "[access] 1st arg is not a function designator")))))
    (with-gensyms (it)
      `(guard1 ,it t (,accessor ,it) ,pattern))))

(defpattern assoc (item subpattern &key (key nil) (test nil))
  "It matches when the object X is a proper association list,
 and then further matches the contents
returned by (cdr (assoc item X...)) against SUBPATTERN.
If :KEY and :TEST are specified, they are passed to ASSOC.

The TYPE-ERROR signaled by ASSOC, which means improper association list,
is captured by the matcher and is not bubble up outside matcher.
However, when TYPE-ERROR is signalled by the :test or :key functions,
they are visible to the environment and the users are required to handle them.
"
  (with-gensyms (it flag x y blk)
    `(guard1 (,it :type list)
             (listp ,it)
             (let (,flag)
               (declare (special ,flag))
               (block ,blk
                 (handler-bind ((type-error
                                 (lambda (c)
                                   (unless ,flag
                                     ;; for those not familiar with condition system: when flag is set, this
                                     ;; is an error from :key or :test thus the handler should decline (==
                                     ;; should not cause control transfer e.g. return-from, go, throw)
                                     (return-from ,blk nil)))))
                   (assoc ,item ,it
                          ,@(when key
                              `(:key (lambda (,x)
                                       (handler-bind ((type-error (lambda (c)
                                                                    (let ((,flag t))
                                                                      (declare (special ,flag))
                                                                      (signal c)))))
                                         (funcall ,key ,x)))))
                          ,@(when test
                              `(:test (lambda (,x ,y)
                                        (handler-bind ((type-error (lambda (c)
                                                                     (let ((,flag t))
                                                                       (declare (special ,flag))
                                                                       (signal c)))))
                                          (funcall ,test ,x ,y)))))))))
             (cons _ ,subpattern))))

(defpattern property (key subpattern &optional (default nil) (foundp nil foundp-suppliedp))
  "It matches when the object X is a list, and then further matches the contents
returned by (getf KEY X DEFAULT) against SUBPATTERN.
FOUNDP is bound to T in order to indicate the reason that NIL is matched.
It is implementation-dependent whether it matches against a list of odd number of elements or it signals an error.
Also, the result may be affected by the safety setting of the optimization option.
"
  (with-gensyms (it it2 indicator)
    `(guard1 (,it :type list)
             (listp ,it)
             (getf ,it ,key ',indicator) ;; indicator is treated as a compile-time constant
             (guard1 ,it2 t
                     ,@(if foundp-suppliedp `((if (eql ,it2 ',indicator) nil t) ,foundp))
                     (if (eql ,it2 ',indicator) ,default ,it2) ,subpattern))))

(defpattern alist (&rest args)
  "alist and plist patterns expand into a collection of assoc and property patterns, respectively, connected
by an and pattern."
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(assoc ,key ,pattern)))
                  args)))

(defpattern plist (&rest args)
  "alist and plist patterns expand into a collection of assoc and property patterns, respectively, connected
by an and pattern."
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(property ,key ,pattern)))
                  (plist-alist args))))

;;; special patterns

;; 'a -> (quote a) -> (constant a) -> (eq 'a)
;; '(a) -> (quote (a)) -> (constant (a)) -> (list a)
;; #(a) -> (constant #(a)) -> (vector a)

(defpattern quote (x)
  "Synonym to the constant pattern."
  `(constant ,x))

(defpattern constant (x)
  "Constant-folds the argument.
   The argument should be a load/read-time constant such as 5, '(2), #(1 2 3), #S(foo :a 1).
   They are decomposed element-wise in the compile time, possibly merged by the optimizer in trivia."
  (typecase x
    (simple-base-string          `(simple-base-string ,@(coerce x 'list)))
    (base-string                 `(base-string ,@(coerce x 'list)))
    (simple-string               `(simple-string ,@(coerce x 'list)))
    (string                      `(string ,@(coerce x 'list)))
    (simple-bit-vector           `(simple-bit-vector ,@(coerce x 'list)))
    (bit-vector                  `(bit-vector ,@(coerce x 'list)))
    (simple-vector               `(simple-vector ,@(coerce x 'list)))
    (vector                      `(vector ,@(coerce x 'list)))
    (structure-object
     (let ((c (class-of x)))
       `(structure ,(class-name c)
                   ,@(mapcar (lambda (slotdef)
                               (let ((name
                                      (handler-case
                                          (c2mop:slot-definition-name slotdef)
                                        #+abcl
                                        (type-error ()
                                          ;; ABCL (as of 1.3.3) retuns a vector that looks like
                                          ;; #(SYSTEM::DEFSTRUCT-SLOT-DESCRIPTION X 0 POINT3-X COMMON-LISP:NIL COMMON-LISP:T COMMON-LISP:NIL)
                                          ;; This may be fixed when there is an update to ABCL or when
                                          ;; CLOSER-MOP provides a fix 
                                          (check-type slotdef vector)
                                          (aref slotdef 1)))))
                                 (list name (slot-value x name))))
                             (c2mop:class-slots c)))))
    (hash-table
     (warn "You seem to include a raw hash-table object in a pattern, perhaps~
            using a read-time evaluation ( #. ) form. This is not a wise act.
            The given hash table is compared against the input by equalp.")
     `(equalp ,x))
    ;; TODO
    (array                            `(equalp ,x))
    (pathname                         `(equal ,x))
    (symbol `(eq ',x))
    (cons `(list ,@x))
    ((or number character) `(eql ,x))
    (t `(eq ,x))))

(defpattern place (place &optional eager)
  "Declares the variable PLACE is setf-able.
   Since this is implemented as a symbol-macrolet, if the object is accessed by some accessor function each
   time, referencing PLACE would cause the invokation of the function each time. For this purpose, we also allow EAGER variable.
   The value of EAGER will be invalidated when PLACE is modified."
  ;; optional arguments in defpattern is defaulted to _, not nil
  `(guard1 (,place :place t) t ,place ,eager))

(defpattern cl:complex (r i)
  "Destructure a complex number."
  (with-gensyms (it)
    `(guard1 ,it (numberp ,it) (realpart ,it) ,r (imagpart ,it) ,i)))
