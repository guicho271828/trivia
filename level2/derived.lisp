(in-package :optima.level2.impl)

(defpattern and (&rest subpatterns)
  (ematch0 subpatterns
    ((list) '_)
    ((list sp) sp)
    ((list* sp and-subpatterns)
     (ematch0 (pattern-expand sp) ;; level1 patterns
       ((list* 'guard1 sym test guard1-subpatterns)
        `(guard1 ,sym ,test ,@guard1-subpatterns
                 ,sym (and ,@and-subpatterns)))
       ((list* 'or1 or-subpatterns)
        (list* 'or1
               (mapcar (lambda (or-sp)
                         `(and ,or-sp ,@and-subpatterns))
                       or-subpatterns)))))))

(defpattern guard (subpattern test-form)
  (with-gensyms (it)
    `(and ,subpattern
          (guard1 ,it ,test-form))))

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

;; no good idea right now
;; low priority
;; (defpattern not (pattern)
;;   (with-gensyms (it)

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
            `(guard1 ,it (typep ,it ,s)
                     ,@(mappend (lambda (arg i)
                                  `((elt ,it ,i) ,arg))
                                args))))))

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
             (assoc ,item ,it
                    ,@(when key `((:key ,key)))
                    ,@(when test `((:test ,test)))) ,pattern)))

(defpattern property (key pattern)
  (with-gensyms (it)
    `(guard1 ,it (listp ,it)
             (getf ,it ,key) ,pattern)))

