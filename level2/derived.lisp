(in-package :optima.level2.impl)

(defpattern and (&rest subpatterns)
  (ematch0 subpatterns
    ((list) '_)
    ((list sp) sp)
    ((list* subpatterns)
     (let* ((subpatterns (mapcar #'pattern-expand subpatterns))
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
           (labels ((wrap-bind (syms body)
                      (ematch0 syms
                        ((list) body)
                        ((list* sym rest)
                         `(guard1 ,sym t ,sym ,(wrap-bind rest body)))))
                    (wrap-test (tests body)
                      (ematch0 tests
                        ((list) body)
                        ((list* test rest)
                         (with-gensyms (it)
                           `(guard1 ,it ,test ,it ,(wrap-test rest body)))))))
             ;; now that all subpatterns are guard1, we can safely assume this;
             (wrap-bind (mapcar #'second rest)
                        (wrap-test (mapcar #'third rest)
                                   (with-gensyms (it)
                                     `(guard1 ,it t ,@(mappend #'cdddr rest)))))))))))

(defpattern guard (subpattern test-form &rest more-patterns)
  (with-gensyms (it)
    `(and ,subpattern
          (guard1 ,it ,test-form ,more-patterns))))

(defpattern not (subpattern)
  (ematch0 (pattern-expand subpattern)
    ((list* 'guard1 sym test guard1-subpatterns)
     (with-gensyms (dummy)
       ;; no symbols are visible from the body
       (subst dummy sym
              (if guard1-subpatterns
                  `(or1 (guard1 ,sym (not ,test))
                        (guard1 ,sym ,test
                                ,@(alist-plist
                                   (mapcar
                                    (lambda-ematch0
                                      ((cons generator test-form)
                                       (cons generator `(not ,test-form)))) 
                                    (plist-alist guard1-subpatterns)))))
                  `(guard1 ,sym (not ,test))))))
    ((list* 'or1 or-subpatterns)
     `(and ,@(mapcar (lambda (or-sp)
                       `(not ,or-sp))
                     or-subpatterns)))))

(defpattern or (&rest subpatterns)
  `(or1 ,@subpatterns))

(defpattern quote (x)
  `(eql ',x))

(defpattern cons (a b)
  (with-gensyms (it)
    `(guard1 ,it (consp ,it) (car ,it) ,a (cdr ,it) ,b)))

(defpattern null ()
  (with-gensyms (it)
    `(guard1 ,it (null ,it))))

(defpattern list (&rest args)
  (if args
      `(cons ,(car args) (list ,@(cdr args)))
      `(null)))

(defpattern list* (&rest args)
  (if (cdr args)
      `(cons ,(car args) (list* ,@(cdr args)))
      (car args)))

(dolist (s '(string
             simple-string
             simple-vector
             vector
             array
             simple-array
             bit-vector
             simple-bit-vector
             sequence))
  (setf (symbol-pattern s)
        (lambda (&rest args)
          (with-gensyms (it)
            `(guard1 ,it (typep ,it ',s)
                     ,@(mappend (lambda (arg i)
                                  `((elt ,it ,i) ,arg))
                                args
                                (iota (length args))))))))

(defpattern satisfies (predicate-name)
  (with-gensyms (it)
    `(guard1 ,it (,predicate-name ,it))))

(dolist (s '(eq eql equal equalp))
  (setf (symbol-pattern s)
        (lambda (arg)
          (with-gensyms (it)
            `(guard1 ,it (,s ,it ,arg))))))

(defpattern type (type-specifier)
  (with-gensyms (it)
    `(guard1 ,it (typep ,it ',type-specifier))))

(defpattern access (accessor pattern)
  (let ((accessor (ematch0 accessor
                    ((list 'function name) name)
                    ((list 'quote name) name)
                    (_ (if (symbolp accessor)
                           accessor
                           (error "[access] 1st arg is not a function designator"))))))
    (with-gensyms (it)
      `(guard1 ,it (,accessor ,it)
               ,it ,pattern))))

(defpattern assoc (item pattern &key key test)
  (with-gensyms (it)
    `(guard1 ,it (listp ,it)
             (cdr (assoc ,item ,it
                         ,@(when key `(:key ,key))
                         ,@(when test `(:test ,test)))) ,pattern)))

(defpattern property (key pattern)
  (with-gensyms (it)
    `(guard1 ,it (listp ,it)
             (getf ,it ,key) ,pattern)))

(defpattern alist (&rest args &key &allow-other-keys)
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(assoc ,key ,pattern)))
                  args)))

(defpattern plist (&rest args &key &allow-other-keys)
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
    `(guard1 ,it ,(cond
                    ((find-class name nil) `(typep ,it ',name))
                    ((fboundp (predicatep name)) `(,(predicatep name) ,it))
                    ((fboundp (predicate-p name)) `(,(predicate-p name) ,it))
                    (t (simple-style-warning 
                        "failed to infer the type-checking predicate of ~a in compile time, forced to use runtime check!"
                        name)
                       `(or (typep ,it ',name)
                            (when (fboundp ',(predicatep name)) (funcall (symbol-function ',(predicatep name)) ,it))
                            (when (fboundp ',(predicate-p name)) (funcall (symbol-function ',(predicate-p name)) ,it)))))
             ,@(map-accessors (parse-slots slots)
                              it name))))

;; should be rewrote with c2mop:class-slots and
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
    (if (and (find-class type nil)
             ;; (subtypep type 'standard-object)
             )
        (let ((c (find-class type)))
          (c2mop:finalize-inheritance c)
          (mappend (lambda-ematch0
                     ((list slot pattern)
                      (if-let ((dslot (find-direct-slot slot c)))
                        (if-let ((reader (first (c2mop:slot-definition-readers dslot))))
                          `((,reader ,it) ,pattern)
                          (progn
                            (simple-style-warning
                             "No reader for slot ~a in class ~a: Forced to use slot-value"
                             (c2mop:slot-definition-name dslot) type)
                            `((slot-value ,it ',(c2mop:slot-definition-name dslot))
                              ,pattern)))
                        (progn
                          (simple-style-warning
                           "Failed to find slot ~a in class ~a: Forced to use slot-value"
                           slot type)
                          `((slot-value ,it ',slot) ,pattern)))))
                   parsed))
        (mappend (lambda-ematch0
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
                            `((,h ,it) ,pattern))))))
                 parsed))))

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


