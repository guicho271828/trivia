(in-package :trivia.level2.impl)

;;;; class pattern etc.

(defpattern class (name &rest slots)
  "Synonym to STRUCTURE pattern."
  ;; in v2, class pattern is just a sugar
  `(structure ,name ,@slots))

(defpattern structure (name &rest slots)
  "Complex pattern. Matches against a structure or a class named NAME.
Match the slots against the subpatterns described in SLOTS.
In order to maintain compatibility to OPTIMA, these slot specifications are very complex.

Long description:

NAME

A symbol which satisfies one of the following:
    (find-class '<NAME>) returns a class.
    Symbol <NAME>-p in the same package is fbound. It should be a unary function, or the consequence is undefined.
    Symbol <NAME>p in the same package is fbound. It should be a unary function, or the consequence is undefined.

    Note that since the existence of the actual class <NAME> is not required, it is permissible to assume a
    normal function like STRINGP or PATHNAMEP (including user-defined function like MYPRED-P) in the second
    and the third case.

SLOTS

one of the following:

make-instance style plist:  :keyword subpattern
    Example:  (structure myclass :slot-a (list a b))
with-slots style:           (slot-name subpattern)
    Example:  (structure myclass (slot-a (list a b)))
with-accessors style: (accessor-name subpattern)

    accessor-name is a symbol, where the corresponding accessor function should be fbound to one of the
    following symbols:

        Symbol accessor-name itself. (common style for generic functions)
        Symbol <class-name>-<accessor-name> in the same package. (common style for structure accessor functions)
        Symbol <class-name><accessor-name> in the same package. (compatibility to Optima)

        Example: (structure mystructure (a (list a b))) tries to find an accessor named either A ,
        MYSTRUCTURE-A or MYSTRUCTUREA .

slot-name :

     The value returned by (slot-value <obj> '<slot-name>) is bound to the variable of the same name. Consider
     it as an abbreviation of (slot-name slot-name).

     Example : (structure myclass slot-a)

accessor-name :

     The value returned by the accessor is bound to the variable of the same name.  Consider it as an
     abbreviation of (accessor-name accessor-name).

     Example : (structure mystructure a)
"
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

;; We then discuss the slot specification in the pattern. A slot
;; specification is a list (slot-name subpattern) where slot-name might be
;; a symbol (possibly keywords). A slot specification is an element of the
;; results returned by PARSE-SLOTS.
;; 
;; Overall, in any case where it is not possible to reduce the
;; equally-preferable ways to access the slot to a single method, it should
;; signal an error AMBIGUOUS-SLOT-ERROR.

(define-condition ambiguous-slot-error (simple-error) ())
(defun ambiguous-slot-error (format-control &rest args)
  (error 'ambiguous-slot-error
         :format-control format-control
         :format-arguments args))

;; In the following, we discuss how we find the slot from the slot
;; specification and a class or a structure. This is done by the function
;; FIND-EFFECTIVE-SLOT. We should consider the following cases:
;;
;; 1. SYMBOL is EQ to one of the initargs of the slot. However there might
;; be multiple slots with the same initarg.
;;
;; 2. SYMBOL is none of the initargs of the slots. Several slot names are
;; string= to SYMBOL.
;;
;; If SYMBOL is a keyword, we try to find a slot whose name is STRING= to
;; the keyword and visible from the current package. Since there is only
;; one symbol with the same name in the current package, this effectively
;; limits the possible number of candidates to one.
;;
;; If SYMBOL is not a keyword and is EQ to some of the slot names
;; (i.e. when SYMBOL is referred by package prefix), then use it.
;;
;; note: accessing the slots not visible from the current package may
;; invalidate capsulation, so we avoid it.
;;
;; 3. Otherwise, try to find the reader methods visible from the
;; current package.

(defun find-effective-slot (symbol type)
  (block nil
    (let ((slots
           ;; this is the effective slots of the class, so there is no
           ;; ambiguity wrto slot names
           (c2mop:compute-slots (find-class type))))
      ;; check the initargs
      (match0 (remove-if-not (curry #'member symbol) slots
                             :key #'c2mop:slot-definition-initargs)
        ((list slot) (return slot))
        ((list) nil) ;; skip
        (slots
         (ambiguous-slot-error
          "Found multiple slots with :initarg ~s: ~{~s~^, ~}"
          symbol (mapcar #'c2mop:slot-definition-name slots))))
      ;; check the slot names
      (let ((slots (remove-if-not (curry #'string= symbol) slots ; set up the candidates
                                  :key #'c2mop:slot-definition-name)))
        (if (keywordp symbol)
            (ematch0 (remove-if-not #'find-symbol ; visible in the current package
                                    slots
                                    :key (compose #'symbol-name
                                                  #'c2mop:slot-definition-name))
              ((list)
               (when slots
                 (simple-style-warning
                  "Found slots ~{~s~^, ~} but they are not accessible in the current package."
                  (mapcar #'c2mop:slot-definition-name slots))))
              ((list slot) (return slot)))
            (ematch0 (remove-if-not (curry #'eq symbol) slots :key #'c2mop:slot-definition-name)
              ((list)
               (when slots
                 (simple-style-warning "Found slots ~{~s~^, ~} but they are not EQ to ~s."
                                       (mapcar #'c2mop:slot-definition-name slots) symbol)))
              ((list slot) (return slot))))))))

;; Next, lets review what is the most preferable method amoung several
;; means of accessing the slot value of an object. SBCL manual states that
;; (as of 2015/12/19):

(defun map-accessors (parsed it type)
  (if (find-class type nil)
      (mappend (curry #'accessor-form it type) parsed)
      (mappend (curry #'accessor-form-using-function it type) parsed)))

(defun accessor-form (it type parsed1)
  "used when the type is a class, structure etc."
  (let ((c (find-class type)))
    (c2mop:ensure-finalized c)
    (typecase c
      (structure-class
       (accessor-form-on-structure it type parsed1))
      (built-in-class
       (accessor-form-on-built-in-class it type parsed1))
      (t
       ;; other metaclasses including standard-class
       (accessor-form-on-class it type parsed1)))))

;; 6.1.1 Structure object slot access
;; 
;; Structure slot accessors are efficient only if the compiler
;; is able to open code them: compiling a call to a structure
;; slot accessor before the structure is defined, declaring one
;; notinline, or passing it as a functional argument to another
;; function causes severe performance degradation.  *end quote*
;;
;; We consider the other implementation has the similar situation.

;; Note: In case of structure-objects, things seem a little bit
;; complicated. Some metaobject does not hold the same amount of
;; information as in standard-object.

(defun accessor-form-on-structure (it type parsed1)
  (ematch0 parsed1
    ((list slot pattern)
     (let (effslot reader)
       (cond
         ((setf reader (find-reader slot type))
          `((,reader ,it) ,pattern))
         ((setf effslot (find-effective-slot slot type))
          `((slot-value ,it ',(c2mop:slot-definition-name effslot)) ,pattern))
         (t
          (error "Failed to deduce the way access the slot ~s" slot)))))))

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
;; times as slow as the method described above. *end quote*
;; 
;; We consider the other implementation has the similar situation.

(defun accessor-form-on-class (it type parsed1)
  (ematch0 parsed1
    ((list slot pattern)
     (let (effslot reader)
       (cond
         ((setf effslot (find-effective-slot slot type))
          `((slot-value ,it ',(c2mop:slot-definition-name effslot)) ,pattern))
         ((setf reader (find-reader slot type))
          `((,reader ,it) ,pattern))
         (t
          (error "Failed to deduce the way to access the slot ~s" slot)))))))

;; finally, built-in-class is problematic

(defun accessor-form-on-built-in-class (it type parsed1)
  (ematch0 parsed1
    ((list slot pattern)
     (let (effslot reader)
       (cond
         ((setf reader (find-reader slot type))
          `((,reader ,it) ,pattern))
         #+sbcl
         ((setf effslot (find-effective-slot slot type))
          `((slot-value ,it ',(c2mop:slot-definition-name effslot)) ,pattern))
         (t
          (error "Failed to deduce the way access the slot ~s" slot)))))))

;;; searching accessor functions
;; finally, handle the case any reader function should be found.

(defvar *arity-check-by-test-call* t
  "If enabled (non-nil), UNARY-FUNCTION-P tests the arity of the candidate accessor function
 by FUNCALLing it with *TEST-CALL-ARGUMENT* (see the docstring of *TEST-CALL-ARGUMENT*).

PROGRAM-ERROR is treated as a reason of rejection; A function of arity != 1.
Other errors, as well as completion of the call without errors, are treated as a success.")
(defvar *arity-check-by-test-call-warning-shown* nil
  "A flag which controls the style-warning produced by using *arity-check-by-test-call*.")
(defvar *test-call-argument* 42
  "An argument used to call the candidate function in UNARY-FUNCTION-P.
See *ARITY-CHECK-BY-TEST-CALL* for details.")

(defun lambda-list-unary-p (lambda-list)
  (or (and (= 1 (length lambda-list))
           ;; to reject cases like (&optional)
           (not (member (first lambda-list) lambda-list-keywords)))
      (and (< 1 (length lambda-list))
           ;; to reject cases like (&optional &key)
           (not (member (first lambda-list) lambda-list-keywords))
           ;; accept cases like (arg &optional something)
           ;; reject cases like (arg1 arg2)
           (member (second lambda-list) lambda-list-keywords))))

(defun unary-function-p (fn)
  "test if a function is unary."
  (etypecase fn
    (generic-function
     (= 1 (length (c2mop:generic-function-lambda-list fn))))
    (function
     #+ccl
     (when (= 1 (ccl:function-args fn))
       ;; "Returns 9 values, as follows: req = number of required arguments ..."
       (return-from unary-function-p t))
     #+lispworks
     (when (lambda-list-unary-p (lw:function-lambda-list fn))
       (return-from unary-function-p t))
     (when *arity-check-by-test-call*
       (handler-case (funcall fn *test-call-argument*)
         (program-error () (return-from unary-function-p nil))
         (error (c)
           (unless *arity-check-by-test-call-warning-shown*
             (simple-style-warning
              "Calling ~a failed, but not by program-error (~a).
Trivia probed candidate function ~a by calling it with 
a single dummy argument ~a. The call may fail due to various reasons,
but program-error is a strong indication of not being unary.
 In order to disable this probing, run ~s .

Note: This style warning is shown only once."
              fn (type-of c) fn *test-call-argument*
              `(setf *arity-check-by-test-call* nil))
             (setf *arity-check-by-test-call-warning-shown* t)))))
     (match (function-lambda-expression fn)
       ;; When there is no information, trusts that the function binding is correct
       (nil t)
       ((list* _ lambda-list _)
        (lambda-list-unary-p lambda-list))))))

(defun find-reader (slot type)
  (flet ((finder (&rest args)
           (let ((sym (find-symbol
                       (apply #'concatenate 'string args))))
             (when (and (fboundp sym)
                        ;; CLHS symbol-function --- If the symbol is globally
                        ;; defined as a macro or a special operator, an object
                        ;; of implementation-dependent nature and identity is
                        ;; returned. http://clhs.lisp.se/Body/f_symb_1.htm Thus
                        ;; these cases should be explicitly removed.
                        (not (macro-function sym))
                        (not (special-operator-p sym))
                        (unary-function-p (symbol-function sym)))
               sym))))
    (flet ((hyphened () (finder (symbol-name type) "-" (symbol-name slot)))
           (concname () (finder (symbol-name type) (symbol-name slot)))
           (nameonly ()
             (let ((sym (finder (symbol-name slot))))
               ;; KLUDGE: nameonly is problematic when common name such as
               ;; COUNT is mistreated as CL:COUNT function.
               (unless (eq (find-package :cl) (symbol-package sym))
                 sym))))
      (match0 (remove nil (list (hyphened)
                                (concname)
                                (nameonly)))
        ((list sym) sym)
        ((list) nil)
        (readers
         (ambiguous-slot-error "Multiple candidates of the accessor function: ~{~a~^, ~}" readers))))))

(defun accessor-form-using-function (it type parsed1)
  (ematch0 parsed1
    ((list slot pattern)
     `((,(find-reader slot type) ,it) ,pattern))))

