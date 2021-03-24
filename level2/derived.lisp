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

(defpattern guard (subpattern test-form)
  "If SUBPATTERN matches, TEST-FORM is evaluated under the lexical binding of variables in SUBPATTERN.
If TEST-FORM returns true, the entire match succeeds.

TEST-FORM in guard pattern is able to see the variables in the surrounding patterns. That is,
the following pattern is valid and should compile successfully:

```
 (list (guard x (eql x y)) y)
```

The mechanics behind this is that the guard patterns are always `lifted`, i.e.,
transformed into a form equivalent to

```
 (guard (list x y) (eql x y))
```

which is further equivalent to

```
 (and (list x y)
      (guard1 _ (eql x y))).      (*)
```

When GUARD pattern is used under NOT pattern, the following form

```
 (not (guard <SUBPATTERN> <test-form>)),
```

 is equivalent to:

```
 (or (not <SUBPATTERN>)
     (guard <SUBPATTERN> (not <test-form>)))
```

 then

```
 (or (not <SUBPATTERN>)
     (and <SUBPATTERN>
          (guard1 _ (not <test-form>)))).
```

That is,

1. The object is first matched against SUBPATTERN.
2. When SUBPATTERN fails to match, then the match against the guard pattern should be successful.
3. When SUBPATTERN matches, then TEST-FORM is evaluated.
4. If TEST-FORM evaluates to true, then the match should fail; If false, then the match should be successful.

Notice that this compilation is equivalent to the negation of the form (*).

```
 (not (and (list x y)
           (guard1 _ (eql x y))))
 ==
 (or (not (list x y))
     (and (list x y)
          (guard1 _ (not (eql x y)))))
```

Also note that the variables bound in the NOT pattern are renamed and are made not
accessible in the clause. --- see NOT pattern documentation.
"
  (restart-case
      (progn (signal 'guard-pattern
                     :subpattern subpattern
                     :test test-form)
             (with-gensyms (guard-dummy)
               `(and ,subpattern
                     (guard1 ,guard-dummy ,test-form))))
    (use-value (v)
      v)))

(defun anonymize-pattern (pattern sym)
  (let ((sym (car (preprocess-symopts sym pattern))))
    (with-gensyms (anon)
      (subst anon sym pattern))))

(defun anonymize-level1-pattern (pattern)
  "Recursively anonymize the given pattern. Only accepts guard1 and or1 patterns"
  (let (syms)
    ;; analysis phase
    (labels ((analyze-more-patterns (more-patterns)
               (ematch0 more-patterns
                 ((list _ pattern)
                  (analyze-pattern pattern))
                 ((list* _ pattern more-patterns)
                  (analyze-pattern pattern)
                  (analyze-more-patterns more-patterns))))
             (analyze-pattern (pattern)
               (ematch0 pattern
                 ((list 'guard1 sym test)
                  (pushnew (first (ensure-list sym)) syms))
                 ((list* 'guard1 sym test more-patterns)
                  (pushnew (first (ensure-list sym)) syms)
                  (analyze-more-patterns more-patterns))
                 ((list* 'or1 or-subpatterns)
                  (map nil #'analyze-pattern or-subpatterns)))))
      (analyze-pattern pattern))

    (reduce #'anonymize-pattern syms :initial-value pattern)))

(defun negate-level1-pattern (pattern)
  "Recursively anonymize the given pattern. Only accepts guard1 and or1 patterns"
  (ematch0 pattern
    ((list* 'guard1 sym test guard1-subpatterns)
     (if guard1-subpatterns
         `(or1 (guard1 ,sym (not ,test))
               (guard1 ,sym ,test
                       ,@(alist-plist
                          (mapcar
                           (lambda-ematch0
                             ((cons generator subpattern)
                              ;; this not pattern is expanded further
                              (cons generator `(not ,subpattern))))
                           (plist-alist guard1-subpatterns)))))
         `(guard1 ,sym (not ,test))))
    ((list* 'or1 or-subpatterns)
     `(and ,@(mapcar (lambda (or-sp)
                       `(not ,or-sp))
                     or-subpatterns)))))

(defpattern not (subpattern)
  "Matches when the SUBPATTERN does not match.
Variables in the subpattern are treated as dummy variables, and will not be visible from the clause body."
  (multiple-value-bind (result guard-tests) (pattern-expand-all/lift0 subpattern)
    (anonymize-level1-pattern
     (pattern-expand-all
      (let ((negated (negate-level1-pattern result)))
        (if guard-tests
            (with-gensyms (lift-dummy)
              ;; (not RESULT) will be reexpanded, but it no longer contains the guarded tests
              `(or1 ,negated
                    (and ,result
                         (guard1 ,lift-dummy
                                 (not (and ,@(nreverse guard-tests)))))))
            negated))))))

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

(defun type-of-quoted-form (form)
  (match0 form
    ((cons a b)
     `(cons ,(type-of-quoted-form a)
            ,(type-of-quoted-form b)))
    (_
     (typecase form
       ((or symbol number character)
        `(eql ,form))
       (t
        (type-of form))))))

(defun type-of-form (form &optional weak)
  "Returns a type of FORM.
When WEAK is non-nil, it returns a decomposed type of the quoted cons cells instead of eql type for the cons object itself.
Note that this is equivalent to the full type-inference mechanism, so we are reinventing a wheel here."
  ;;
  ;; See carefully below:
  ;; file:///usr/share/doc/hyperspec/Body/f_eql.htm
  ;; file:///usr/share/doc/hyperspec/Body/t_eql.htm
  ;; file:///usr/share/doc/hyperspec/Body/f_consta.htm
  (match0 form
    ((list 'quote thing)
     (typecase thing
       ((or symbol number character)    ;e.g. '42, '#\c, 'a --- if they are equal/equalp, it implies eql
        `(eql ,thing))
       (t
        ;; '"string", '#S(point :x 5 :y 5), arrays, conses, standard-objects etc.
        (if weak
            ;; if the comparison is done in equal / equalp, then the object
            ;; might not be (most likely not) eql to the object used in the macro expansion.
            (type-of-quoted-form thing)
            ;; if the comparison is done in eq / eql, then the matched object
            ;; should be eql to the object that appeared in the form, thus it is
            ;; safe to declare the type with eql. BTW, the only case that this
            ;; happens is when THING is cached somewhere and is eq to the object
            ;; in the form.
            `(eql ,thing)))))
    (_
     ;; not quoted: evaluated.
     ;; at least try to macroexpand it
     (multiple-value-bind (form expanded) (macroexpand form)
       (cond
         (expanded
          ;; retry
          (type-of-form form))
         
         ((constantp form)
          ;; (constantp form) returns true when FORM is either:
          ;; * self-evaluating atoms (e.g. numbers, chars, arrays, structs),
          ;; * constant vars (keywords, nil, t, pi, etc.),
          ;; * quoted forms (which is already handled in the first clause),
          ;; * some impl-dependent form that is reasoned to be a constant (e.g. (* 2 pi))
          (handler-case
              (type-of-form `(quote ,(eval form)) weak)
            (unbound-variable ()
              ;; handle potential unbound-variable.
              ;; CLHS constantp:
              ;; > An implementation may choose to evaluate the value-form at compile time, load time, or both
              ;; the value may not be bound in the compile time.
              t)))

         (t t))))))

(defpattern equal (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(type-of-form arg t))
             (equal ,it ,arg))))

(defpattern equalp (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(type-of-form arg t))
             (equalp ,it ,arg))))

(defpattern eq (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(type-of-form arg)) (eq ,it ,arg))))

(defpattern eql (arg)
  "Compare the matching value against ARG (evaluated)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(type-of-form arg)) (eql ,it ,arg))))

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
is captured by the matcher and treated as a match failure (proceeding to the next clause),
rather than making it visible to the outer environment.
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

By default, PROPERTY pattern matches even if the given KEY is missing in the property list,
since the value of a missing key is NIL, due to closed-world assumption --- a bit of classical AI context.
If you want to make it fail when KEY is missing, i.e. to treat a property list like a structure,

+ Use FOUNDP optional argument: (property :key subpattern nil t) does not match when :key is missing in the list,
  since the FOUNDP value is NIL and it does not match T.
+ We also provide a syntax sugar, PROPERTY!, i.e. a 'stricter' PROPERTY pattern." 
  (with-gensyms (it it2 indicator)
    `(guard1 (,it :type list)
             (listp ,it)
             (getf ,it ,key ',indicator) ;; indicator is treated as a compile-time constant
             (guard1 ,it2 t
                     ,@(if foundp-suppliedp `((if (eql ,it2 ',indicator) nil t) ,foundp))
                     (if (eql ,it2 ',indicator) ,default ,it2) ,subpattern))))

(defpattern property! (key subpattern &optional (default nil))
  "This is a simple syntax sugar on top of PROPERTY pattern, using (PROPERTY KEY SUBPATTERN DEFAULT T).

It matches when the object X is a list, and then further matches the contents
returned by (getf KEY X DEFAULT) against SUBPATTERN.
Here, unlike PROPERTY pattern, an additional constraint is applied:
the KEY element should exist in the property list.
When KEY is missing in the property list, it fails and proceeds to the next pattern.

Note that it matches when the property list contains KEY and its value is NIL, e.g. :B in (:A 2 :B NIL)."
  `(property ,key ,subpattern ,default t))

(defpattern alist (&rest args)
  "alist and plist patterns expand into a collection of assoc and property patterns, respectively, connected
by an and pattern.  Example: (alist (:key1 . _) (:key2 . value))"
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(assoc ,key ,pattern)))
                  args)))

(defpattern plist (&rest args)
  "alist and plist patterns expand into a collection of assoc and property patterns, respectively, connected
by an and pattern. Example: (plist :key1 _ :key2 value)"
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(property ,key ,pattern)))
                  (plist-alist args))))

(defpattern hash-table-entry (key pattern &optional (default nil) (foundp nil foundp-suppliedp))
  "Matches HASH-TABLE value at KEY against PATTERN.

Calls GETHASH with KEY on current matched hash table. If KEY is not present, then DEFAULT is used as value. If FOUNDP is supplied, it is match against second value of GETHASH result."
  (with-gensyms (it value missing-value-indicator)
    `(guard1 (,it :type hash-table)
             (hash-table-p ,it)
             (gethash ,key ,it ',missing-value-indicator)
             (guard1 ,value t
                     ,@(when foundp-suppliedp
                         `((not (eq ,value ',missing-value-indicator)) ,foundp))
                     ,@(if (eq t foundp)
                           `(,value ,pattern)
                           `((if (eq ,value ',missing-value-indicator) ,default ,value) ,pattern))))))

(defpattern hash-table-entry! (key pattern)
  "Same as HASH-TABLE-ENTRY but requires KEY to be present in hash table."
  `(hash-table-entry ,key ,pattern nil t))

(define-condition hash-table-odd-number-of-entries-warning (simple-style-warning)
  ())

(defmacro %define-hash-table-entries-pattern (name entry-pattern)
  `(defpattern ,name (key pattern &rest keys-and-patterns)
    ,(format nil "Matches hash table which has KEY set to value matching PATTERN.~
                  Multiple KEY PATTERN pairs can be provided, e.g. `(~A :key1 _ :key2 value :key3 1)`.~
                  Expands into list of ~A patterns combined with AND."
             name entry-pattern)
    (when (oddp (length keys-and-patterns))
      (warn 'hash-table-odd-number-of-entries-warning
            :format-control ,(format nil "Odd number of arguments given to ~A pattern. NIL will be used as ~~S pattern." name)
            :format-arguments (last keys-and-patterns)))
    `(and ,(list ',entry-pattern key pattern)
          ,@(loop :for (key pattern) :on keys-and-patterns :by #'cddr
                  :collect (list ',entry-pattern key pattern)))))

(%define-hash-table-entries-pattern hash-table-entries hash-table-entry)
(%define-hash-table-entries-pattern hash-table-entries! hash-table-entry!)

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
 (constant #(a _ 0)) matches #(2 1 0) where A is bound to 2.
 (constant #S(person :name (cons a b))) matches #S(person :name (\"Susan\" \"Calvin\")).
 (constant #S(person :name (a b))) errors, because pattern expander A is not defined.
This behavior is compatible to Optima.

However, this rule does not apply to conses, as the feature is instead supported by quasiquotes (see issue #86).
i.e., (constant (a b)) DOES NOT match '(1 0) and matches '(a b).


Examples:

  (constant x) matches a symbol 'x, not a variable x under the scope.
  (constant x) does not match a symbol 'y.
  (constant #\x) matches a character #\x, and not #\X. (-> eql)
  (constant 1) matches 1.

Sequences (cons,bitvec,vector,string):
  (constant (a b)) matches '(A B) and not '(1 0).
  (constant \"aaa\") matches \"aaa\".
  (constant #(0 1)) matches #(0 1).
  (constant #(_ _)) matches #(0 1).
  (constant #(A B)) matches #(0 1) with A bound to 1, B bound to 0.
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
    ;; WITH variables-in-quoted-literal extension (issue #86). Removed, since
    ;; for conses we have quasiquotes. Arrays/structs does not have unquote, so
    ;; the extensions are allowed.
    #+(or)
    (cons                        `(cons ,(car x) (constant ,(cdr x))))
    ;; WITHOUT variables-in-quoted-literal extension
    (cons                        `(cons (constant ,(car x)) (constant ,(cdr x))))
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
  `(guard1 (,place :binder symbol-macrolet) t ,place ,eager))


(defpattern progv (name)
  "Binds the current matching value to a dynamic variable named by NAME.
NAME is evaluated and should evaluate to a symbol.
In another word, the variable to bind the value can be altered in runtime."
  `(guard1 (,name :binder progv) t))


(defpattern cl:complex (r i)
  "Destructure a complex number."
  (with-gensyms (it)
    `(guard1 ,it (numberp ,it) (realpart ,it) ,r (imagpart ,it) ,i)))

(defpattern dynamic (variable)
  "Takes a single variable.
Declares that the value bound to the variable has a dynamic-extent."
  (check-type variable symbol)
  `(guard1 (,variable :DYNAMIC-EXTENT t) t))
