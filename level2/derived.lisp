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

(defpattern equal (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(if (constantp arg)
                             (type-of arg) t))
             (equal ,it ,arg))))
(defpattern equalp (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(if (constantp arg)
                             (type-of arg) t))
             (equalp ,it ,arg))))
(defpattern eq (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type (eql ,arg)) (eq ,it ,arg))))
(defpattern eql (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type (eql ,arg)) (eql ,it ,arg))))



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
                                   (declare (ignore c))
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
  "Synonym for the constant pattern."
  `(constant ,x))

(defpattern constant (x)
  "Constant-folds the argument.
The argument should be a compile-time constant such as 5, x, (2), #(1 2 3), #S(foo :a 1).
QUOTE pattern is a synonym for CONSTANT pattern. Thus (quote ...) is equivalent to (constant ...).

Elementary objects are compared by EQL. (characters, numbers, symbols)
Conses, structures, arrays are recursively decomposed element-wise in compile time.
Thus the comparison is different from either EQUAL or EQUALP:
EQUAL only descends into conses/strings/bitvec, and EQUALP uses case-insensitive comparison.

Pathnames are compared by EQUAL.
Hash-tables are compared by EQUALP, but this usage is not recommended and subject to change.
If it does not match any of the above types, it is compared by EQ.

When the given object is decomposed, each sub-object forms a pattern.
Thus, '(a b) matches '(1 0) where A and B is bound to 1 and 0.
If you want it to exactly match '(a b), use '('a 'b) or (list 'a 'b).

Similarly, #(a _ 0) matches #(2 1 0) where A is bound to 2.
#S(person :name (cons a b)) matches #S(person :name (\"Susan\" \"Calvin\")).
#S(person :name '(a b)) also matches #S(person :name (\"Susan\" \"Calvin\")).
#S(person :name (a b)) errors, because pattern (A B) is not defined.
This behavior is compatible to Optima.

Examples:

  (constant x) matches a symbol 'x, not a variable x under the scope.
  (constant x) does not match a symbol 'y.
  (constant #\x) matches a character #\x, and not #\X. (-> eql)
  (constant 1) matches 1.

Sequences (cons,bitvec,vector,string):
  (constant (a b)) matches '(1 0) where A and B is bound to 1 and 0.
  '(a b) == (quote (a b)) == (constant (a b)).
  (constant \"aaa\") matches \"aaa\".
  (constant #(0 1)) matches #(0 1).
  (constant #(_ _)) matches #(0 1).
Unlike EQUAL, fill-pointer is ignored (lengths should match).

Structures:
  (constant #(person :name \"Bob\")) matches #(person :name \"Bob\").
  (constant #(person :name \"Bob\")) does not match #(person :name \"bob\"). (-> case sensitive unlike EQUALP)
  (constant #(person :name a)) matches #(person :name \"Bob\"), with A bound to \"Bob\".

General Array:
  (constant #2A((0 1) (0 1))) matches #2A((0 1) (0 1)).    (-> similar to equalp)
  (constant #2A((#\a #\a))) does not match #2A((#\A #\A)). (-> case sensitive unlike EQUALP)
  (constant #2A((0 a) (b 1))) matches #2A((0 1) (0 1)) where A and B bound to 1 and 0.

"
  (typecase x
    (simple-base-string          `(simple-base-string ,@(coerce x 'list)))
    (base-string                 `(base-string ,@(coerce x 'list)))
    (simple-string               `(simple-string ,@(coerce x 'list)))
    (string                      `(string ,@(coerce x 'list)))
    (simple-bit-vector           `(simple-bit-vector ,@(coerce x 'list)))
    (bit-vector                  `(bit-vector ,@(coerce x 'list)))
    (simple-vector               `(simple-vector ,@(coerce x 'list)))
    (vector                      `(vector ,@(coerce x 'list)))
    (hash-table
     (warn "You seem to include a raw hash-table object in a pattern, perhaps~
            using a read-time evaluation ( #. ) form. This is not a wise act.
            The given hash table is compared against the input by equalp.")
     `(equalp ,x))
    ;; TODO
    (array
     `(array :dimensions ',(array-dimensions x)
             :rank       ,(array-rank x)
             ;; element-type is not here since it is not visible from the literal
             :contents   ,(decompose-array-contents x)))
    (pathname                    `(equal ,x))
    (symbol                      `(eq ',x))
    (cons                        `(cons ,(car x) (constant ,(cdr x))))
    ((or number character) `(eql ,x))
    (structure-object
     ;; this should be in the last of TYPECASE because some implementations
     ;; implement PATHNAMEs as structures
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
    (t `(eq ,x))))

(defun decompose-array-contents (x &optional (dim 0) (offset 0))
  (if (< dim (array-rank x))
      (mapcar (lambda (i)
                (decompose-array-contents
                 x (1+ dim)
                 (+ offset (* i (reduce #'* (subseq (array-dimensions x) (1+ dim)))))))
              (iota (array-dimension x dim)))
      (row-major-aref x offset)))


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

(defpattern dynamic (variable)
  "Takes a single variable.
Declares that the value bound to the variable has a dynamic-extent."
  (check-type variable symbol)
  `(guard1 (,variable :DYNAMIC-EXTENT t) t))
