(in-package :trivia.level2.impl)

(defpattern and (&rest subpatterns)
  (expand-and subpatterns))
(defun expand-and (subpatterns)
  (ematch0 subpatterns
    ((list) '_)
    ((list sp) sp)
    ((list* subpatterns)
     (let* ((subpatterns (handler-bind ((wildcard (lambda (c) (continue c))))
                           (mapcar #'pattern-expand subpatterns)))
            (or1  (find 'or1 subpatterns :key #'car))
            (rest (remove or1 subpatterns)))
       (if or1
           (ematch0 or1
             ((list* 'or1 or-subpatterns)
              (list* 'or1
                     (mapcar (lambda (or-sp)
                               `(and ,or-sp ,@rest))
                             or-subpatterns))))
           ;; no or pattern; perform lifting
           (with-gensyms (intersection)
             (labels ((wrap-test (syms tests body)
                        (ematch0 tests
                          ((list test)
                           `(guard1 ,(first syms) ,test ,@body))
                          ((list* test t-rest)
                           `(guard1 ,(first syms) ,test
                                    ,intersection ,(wrap-test (rest syms) t-rest body))))))
               ;; now that all subpatterns are guard1, we can safely assume this;
               (let* ((symopts (mapcar #'second rest))
                      (tests  (mapcar #'third rest)))
                 `(guard1 ,intersection t
                          ,intersection
                          ,(wrap-test symopts tests (mappend #'cdddr rest)))))))))))

(defpattern guard (subpattern test-form &rest more-patterns)
  (with-gensyms (guard)
    `(and ,subpattern
          (guard1 ,guard ,test-form ,@more-patterns))))

(defpattern not (subpattern)
  (ematch0 (pattern-expand subpattern)
    ((list* 'guard1 sym test guard1-subpatterns)
     (with-gensyms (notsym)
       (let ((sym (car (preprocess-symopts sym subpattern))))
         ;; no symbols are visible from the body
         (subst notsym sym
                (if guard1-subpatterns
                    `(or1 (guard1 ,sym (not ,test))
                          (guard1 ,sym ,test
                                  ,@(alist-plist
                                     (mapcar
                                      (lambda-ematch0
                                        ((cons generator test-form)
                                         (cons generator `(not ,test-form)))) 
                                      (plist-alist guard1-subpatterns)))))
                    `(guard1 ,sym (not ,test)))))))
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

;; should be rewritten with c2mop:class-slots and
;; slot-definition-readers 

(defun parse-slots (slots)
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

(defun map-accessors (parsed it type)
  (let ((package (symbol-package type)))
    (if (find-class type nil)
        (mappend (curry #'map-accessors-class it package type) parsed)
        (mappend (curry #'map-accessors-function it package type) parsed))))

(defun map-accessors-class (it package type parsed1)
  (let ((c (find-class type)))
    (ignore-errors
      (c2mop:finalize-inheritance c))
    (ematch0 parsed1
      ((list slot pattern)
       (or (if-let ((dslot (find-direct-slot slot c)))
             (if-let ((reader (first (c2mop:slot-definition-readers dslot))))
               `((,reader ,it) ,pattern)
               ;; structures
               (if-let ((reader (hyphened type slot package)))
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
           (map-accessors-function it package type parsed1))))))

(defun map-accessors-function (it package type parsed1)
  (ematch0 parsed1
    ((list slot pattern)
     (let* ((h (hyphened type slot package))
            (c (concname type slot package))
            (n (nameonly type slot package))
            (reader (cond ((fboundp h) h)
                          ((fboundp c) c)
                          ((fboundp n) n))))
       (if reader
           `((,reader ,it) ,pattern)
           (progn
             (simple-style-warning
              "failed to find the accessor for slot ~a! Using ~a"
              slot h)
             `((,h ,it) ,pattern)))))))

(defun find-direct-slot (slot/keyword c)
  (or (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name)
      (find slot/keyword (c2mop:class-direct-slots c)
            :key #'c2mop:slot-definition-name
            :test #'string=)
      (some (lambda (c)
              (find-direct-slot slot/keyword c))
            (c2mop:class-direct-superclasses c))))

(defun hyphened (type slot package)
  (when-let ((sym (find-symbol
                   (concatenate 'string
                                (symbol-name type)
                                "-"
                                (symbol-name slot))
                   package)))
    sym))

(defun concname (type slot package)
  (when-let ((sym (find-symbol
                   (concatenate 'string
                                (symbol-name type)
                                (symbol-name slot))
                   package)))
    sym))

(defun nameonly (type slot package)
  (declare (ignorable type slot))
  (when-let ((sym (find-symbol (symbol-name slot) package))) ; slot may be a keyword!
    sym))


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
