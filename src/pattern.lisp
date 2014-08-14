(in-package :optima)

;;; Data Destructor

(defstruct destructor
  bindings
  predicate-form
  accessor-forms)

;;; Pattern Data Structure

(defstruct pattern)

(defstruct (variable-pattern (:include pattern)
                             (:constructor %make-variable-pattern (name)))
  name)

(defun make-variable-pattern (&optional name)
  (when (or (eq name 'otherwise)
            (string= name "_"))
    (setq name nil))
  (%make-variable-pattern name))

(defstruct (place-pattern (:include pattern)
                          (:constructor make-place-pattern (name)))
  name)

(defstruct (constant-pattern (:include pattern)
                             (:constructor make-constant-pattern (value)))
  value)

(defstruct (complex-pattern (:include pattern))
  subpatterns)

(defstruct (guard-pattern (:include complex-pattern)
                          (:constructor make-guard-pattern (subpattern test-form
                                                            &aux (subpatterns (list subpattern)))))
  test-form)

(defun guard-pattern-subpattern (pattern)
  (first (complex-pattern-subpatterns pattern)))

(defstruct (not-pattern (:include complex-pattern)
                        (:constructor make-not-pattern (subpattern
                                                        &aux (subpatterns (list subpattern))))))

(defun not-pattern-subpattern (pattern)
  (first (complex-pattern-subpatterns pattern)))

(defstruct (or-pattern (:include complex-pattern)
                       (:constructor make-or-pattern (&rest subpatterns))))

(defstruct (and-pattern (:include complex-pattern)
                        (:constructor make-and-pattern (&rest subpatterns))))

(defstruct (constructor-pattern (:include complex-pattern)))

(defun constructor-pattern-arity (pattern)
  (length (constructor-pattern-subpatterns pattern)))

(defgeneric constructor-pattern-destructor-sharable-p (x y))
(defgeneric constructor-pattern-make-destructor (pattern var))

(defstruct (cons-pattern (:include constructor-pattern)
                         (:constructor make-cons-pattern (car-pattern cdr-pattern
                                                          &aux (subpatterns (list car-pattern
                                                                                  cdr-pattern))))))

(defun cons-pattern-car-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defun cons-pattern-cdr-pattern (pattern)
  (second (constructor-pattern-subpatterns pattern)))

(defmethod constructor-pattern-destructor-sharable-p ((x cons-pattern) (y cons-pattern))
  t)

(defmethod constructor-pattern-make-destructor ((pattern cons-pattern) var)
  (make-destructor :predicate-form `(consp ,var)
                   :accessor-forms (list `(car ,var) `(cdr ,var))))

(defstruct (assoc-pattern (:include constructor-pattern)
                          (:constructor make-assoc-pattern (item value-pattern
                                                            &key key test
                                                            &aux (subpatterns (list value-pattern)))))
  item key test)

(defun assoc-pattern-value-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defmethod constructor-pattern-destructor-sharable-p ((x assoc-pattern) (y assoc-pattern))
  (and (eq (assoc-pattern-key x)
           (assoc-pattern-key y))
       (eq (assoc-pattern-test x)
           (assoc-pattern-test y))
       ;; FIXME: Don't use EQL
       (eql (assoc-pattern-item x)
            (assoc-pattern-item y))))

(defmethod constructor-pattern-make-destructor ((pattern assoc-pattern) var)
  (with-slots (item key test) pattern
    (with-unique-names (it)
      (make-destructor :bindings `((,it (%assoc ',item ,var
                                                ,@(when key `(:key #',key))
                                                ,@(when test `(:test #',test)))))
                       :predicate-form it
                       :accessor-forms (list `(cdr ,it))))))

(defstruct (property-pattern (:include constructor-pattern)
                             (:constructor make-property-pattern (item value-pattern
                                                                  &aux (subpatterns (list value-pattern)))))
  item)

(defun property-pattern-value-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defmethod constructor-pattern-destructor-sharable-p ((x property-pattern) (y property-pattern))
  (eq (property-pattern-item x) (property-pattern-item y)))

(defmethod constructor-pattern-make-destructor ((pattern property-pattern) var)
  (with-slots (item) pattern
    (with-unique-names (it)
      (make-destructor :bindings `((,it (%get-property ',item ,var)))
                       :predicate-form it
                       :accessor-forms (list `(car ,it))))))

(defstruct (vector-pattern (:include constructor-pattern)
                           (:constructor make-vector-pattern (&rest subpatterns))))

(defmethod constructor-pattern-destructor-sharable-p ((x vector-pattern) (y vector-pattern))
  (= (constructor-pattern-arity x)
     (constructor-pattern-arity y)))

(defmethod constructor-pattern-make-destructor ((pattern vector-pattern) var)
  (make-destructor :predicate-form `(typep ,var '(vector * ,(constructor-pattern-arity pattern)))
                   :accessor-forms (loop for i from 0 below (constructor-pattern-arity pattern)
                                         collect `(aref ,var ,i))))

(defstruct (simple-vector-pattern (:include constructor-pattern)
                                  (:constructor make-simple-vector-pattern (&rest subpatterns))))

(defmethod constructor-pattern-destructor-sharable-p ((x simple-vector-pattern) (y simple-vector-pattern))
  (= (constructor-pattern-arity x)
     (constructor-pattern-arity y)))

(defmethod constructor-pattern-make-destructor ((pattern simple-vector-pattern) var)
  (make-destructor :predicate-form `(typep ,var '(simple-vector ,(constructor-pattern-arity pattern)))
                   :accessor-forms (loop for i from 0 below (constructor-pattern-arity pattern)
                                         collect `(svref ,var ,i))))

(defstruct (class-pattern (:include constructor-pattern)
                          (:constructor %make-class-pattern))
  class-name slot-names)

(defun make-class-pattern (class-name &rest slot-specs)
  (%make-class-pattern :class-name class-name
                       :slot-names (mapcar #'first slot-specs)
                       :subpatterns (mapcar #'second slot-specs)))

(defmethod constructor-pattern-destructor-sharable-p ((x class-pattern) (y class-pattern))
  (and (eq (class-pattern-class-name x)
           (class-pattern-class-name y))
       (equal (class-pattern-slot-names x)
              (class-pattern-slot-names y))))

(defmethod constructor-pattern-make-destructor ((pattern class-pattern) var)
  (make-destructor :predicate-form `(typep ,var ',(class-pattern-class-name pattern))
                   :accessor-forms (loop for slot-name in (class-pattern-slot-names pattern)
                                         collect `(slot-value ,var ',slot-name))))

(defstruct (structure-pattern (:include constructor-pattern)
                              (:constructor %make-structure-pattern))
  conc-name slot-names)

(defun make-structure-pattern (conc-name &rest slot-specs)
  (%make-structure-pattern :conc-name conc-name
                           :slot-names (mapcar #'first slot-specs)
                           :subpatterns (mapcar #'second slot-specs)))

(defmethod constructor-pattern-destructor-sharable-p ((x structure-pattern) (y structure-pattern))
  (and (string= (structure-pattern-conc-name x)
                (structure-pattern-conc-name y))
       (equal (structure-pattern-slot-names x)
              (structure-pattern-slot-names y))))

(defmethod constructor-pattern-make-destructor ((pattern structure-pattern) var)
  (make-destructor :predicate-form `(,(symbolicate (structure-pattern-conc-name pattern) :p) ,var)
                   :accessor-forms (loop with conc-name = (structure-pattern-conc-name pattern)
                                         for slot-name in (structure-pattern-slot-names pattern)
                                         collect `(,(symbolicate conc-name slot-name) ,var))))

;;; Pattern Utilities

(defun pattern-variables (pattern)
  "Returns the set of variables in PATTERN. If PATTERN is not linear,
an error will be raised."
  (flet ((check (vars)
           (loop for var in vars
                 if (find var seen)
                   do (error "Non-linear pattern: ~S"
                             (unparse-pattern pattern))
                 collect var into seen
                 finally (return vars))))
    (typecase pattern
      (variable-pattern
       (if-let ((name (variable-pattern-name pattern)))
         (list name)))
      (or-pattern
       (let ((vars (mappend #'pattern-variables (or-pattern-subpatterns pattern))))
         (check (remove-duplicates vars))))
      (complex-pattern
       (check (mappend #'pattern-variables (complex-pattern-subpatterns pattern)))))))

(defun place-pattern-included-p (pattern)
  (typecase pattern
    (place-pattern t)
    (complex-pattern
     (some #'place-pattern-included-p
           (complex-pattern-subpatterns pattern)))))

(defun check-patterns (patterns)
  "Check if PATTERNS are valid. Otherwise, an error will be raised."
  (loop for var in (mappend #'pattern-variables patterns)
        if (find var seen)
          do (error "Non-linear patterns: ~S"
                    (mapcar #'unparse-pattern patterns))
        collect var into seen
        finally (return t)))

(defun lift-guard-patterns-1 (pattern)
  "Lifts GUARD patterns in PATTERN so that the guards can see
any variables in PATTERN. The transform looks like:

       (list x (guard y (equal x y)))
    => (guard (list x y) (equal x y))

Multiple guards will be connected with conjunction in order of
occurence like:

       (list (guard x (consp x)) (guard y (eq y (car x))))
    => (guard (list x (guard y (eq y (car x)))) (consp x))
    => (guard (guard (list x y) (eq y (car x))) (consp x))
    => (guard (list x y) (and (consp x) (eq y (car x))))"
  (cond
    ((guard-pattern-p pattern)
     (let ((subpattern (lift-guard-patterns (guard-pattern-subpattern pattern)))
           (test-form (guard-pattern-test-form pattern)))
       (if (guard-pattern-p subpattern)
           ;; Connect with conjunction like:
           ;; (guard (guard p g2) g1) => (guard p (and g1 g2))
           (let ((test-form `(and ,test-form ,(guard-pattern-test-form subpattern)))
                 (subpattern (guard-pattern-subpattern subpattern)))
             (make-guard-pattern subpattern test-form))
           pattern)))
    ((not-pattern-p pattern)
     ;; Stop lifting on not pattern.
     (let* ((subpattern (not-pattern-subpattern pattern))
            (lifted-subpattern (lift-guard-patterns subpattern)))
       (if (and (not (guard-pattern-p subpattern))
                (guard-pattern-p lifted-subpattern))
           (make-not-pattern lifted-subpattern)
           pattern)))
    ((or-pattern-p pattern)
     ;; OR local lift.
     (let* ((subpatterns (or-pattern-subpatterns pattern))
            (lifted-subpatterns (mapcar #'lift-guard-patterns subpatterns)))
       (if (every #'eq subpatterns lifted-subpatterns)
           pattern
           (apply #'make-or-pattern lifted-subpatterns))))
    ((complex-pattern-p pattern)
     (let ((subpatterns (mapcar #'lift-guard-patterns (complex-pattern-subpatterns pattern))))
       (if (some #'guard-pattern-p subpatterns)
           ;; Lift guard patterns like:
           ;; (c ... (guard p g) ...) => (guard (c ... p ...) g)
           (loop for subpattern in subpatterns
                 if (guard-pattern-p subpattern)
                   collect (guard-pattern-test-form subpattern) into test-forms
                   and do (setq subpattern (guard-pattern-subpattern subpattern))
                 collect subpattern into new-subpatterns
                 finally
                    (let ((pattern (copy-structure pattern)))
                      (setf (complex-pattern-subpatterns pattern) new-subpatterns)
                      (return (make-guard-pattern pattern `(and ,.test-forms)))))
           ;; Otherwise, just return the original pattern
           pattern)))
    (t pattern)))

(defun lift-guard-patterns-2 (pattern)
  "Lifts OR patterns that include guards like:

       (list 3 (or 1 (guard x (evenp x))))
    => (or (list 3 1) (list 3 (guard x (evenp x))))
    => (or (list 3 1) (guard (list 3 x) (evenp x)))"
  (flet ((guards-or-pattern-p (p)
           (and (or-pattern-p p)
                (some #'guard-pattern-p (or-pattern-subpatterns p)))))
    (cond
      ((or-pattern-p pattern)
       (let ((subpatterns (mapcar #'lift-guard-patterns (or-pattern-subpatterns pattern))))
         (if (some #'or-pattern-p subpatterns)
             ;; Expand nested OR patterns
             (loop for subpattern in subpatterns
                   if (or-pattern-p subpattern)
                     append (or-pattern-subpatterns subpattern) into new-subpatterns
                   else
                     collect subpattern into new-subpatterns
                   finally (return (lift-guard-patterns (apply #'make-or-pattern new-subpatterns))))
             pattern)))
      ((complex-pattern-p pattern)
       (let ((subpatterns (mapcar #'lift-guard-patterns (complex-pattern-subpatterns pattern))))
         (if (some #'guards-or-pattern-p subpatterns)
             ;; Lift fitst OR pattern that include GUARD patterns like:
             ;; (c ... (or ... (guard p g) ...) ...) => (or ... (c ... (guard p g) ...) ...)
             (loop for i from 0
                   for subpattern in subpatterns
                   if (guards-or-pattern-p subpattern)
                     return (loop for subpat in (or-pattern-subpatterns subpattern)
                                  for pat = (copy-structure pattern)
                                  for pat-subpats = (copy-list (complex-pattern-subpatterns pat))
                                  do (setf (nth i pat-subpats) subpat
                                           (complex-pattern-subpatterns pat) pat-subpats)
                                  collect pat into new-pats
                                  finally (return (lift-guard-patterns (apply #'make-or-pattern new-pats)))))
             ;; Otherwise, just return the original pattern
             pattern)))
      (t pattern))))

(defun lift-guard-patterns (pattern)
  (let ((new-pattern (lift-guard-patterns-1 (lift-guard-patterns-2 (lift-guard-patterns-1 pattern)))))
    (if (eq pattern new-pattern)
        new-pattern
        (lift-guard-patterns new-pattern))))

;;; Pattern Specifier

(defun pattern-expand-function (name)
  (get name 'pattern-expand-function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun (setf pattern-expand-function) (function name)
    (setf (get name 'pattern-expand-function) function)))

(defun pattern-expand-1 (pattern)
  (if-let (it (and (consp pattern)
                   (symbolp (car pattern))
                   (pattern-expand-function (car pattern))))
    (apply it (cdr pattern))
    pattern))

(defun pattern-expand (pattern)
  (let ((expansion (pattern-expand-1 pattern)))
    (if (eq pattern expansion)
        pattern
        (pattern-expand expansion))))

(defun pattern-expand-all (pattern)
  (setq pattern (pattern-expand pattern))
  (if (consp pattern)
      (cons (car pattern)
            (mapcar #'pattern-expand-all (cdr pattern)))
      pattern))

(defmacro defpattern (name lambda-list &body body)
  "Defines a derived pattern specifier named NAME. This is analogous
to DEFTYPE.

Examples:

    ;; Defines a LIST pattern.
    (defpattern list (&rest args)
      (when args
        `(cons ,(car args) (list ,@(cdr args)))))"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (pattern-expand-function ',name) (lambda ,lambda-list ,@body))))

(defpattern list (&rest args)
  (when args
    `(cons ,(car args) (list ,@(cdr args)))))

(defpattern list* (arg &rest args)
  (if (null args)
      `(and ,arg (type list))
      `(cons ,arg (list* ,@args))))

(defpattern satisfies (predicate-name)
  (with-unique-names (it)
    `(guard ,it (,predicate-name ,it))))

(defpattern eq (arg)
  (with-unique-names (it)
    `(guard ,it (eq ,it ,arg))))

(defpattern eql (arg)
  (with-unique-names (it)
    `(guard ,it (eql ,it ,arg))))

(defpattern equal (arg)
  (with-unique-names (it)
    `(guard ,it (equal ,it ,arg))))

(defpattern equalp (arg)
  (with-unique-names (it)
    `(guard ,it (equalp ,it ,arg))))

(defpattern type (type-specifier)
  (with-unique-names (it)
    `(guard ,it (typep ,it ',type-specifier))))

;;; Pattern Specifier Parser

(defun parse-pattern (pattern)
  (when (pattern-p pattern)
    (return-from parse-pattern pattern))
  (setq pattern (pattern-expand pattern))
  (typecase pattern
    ((or (eql t) null keyword)
     (make-constant-pattern pattern))
    (symbol
     (make-variable-pattern pattern))
    (cons
     (destructuring-case pattern
       ((variable name)
        (make-variable-pattern name))
       ((place name)
        (make-place-pattern name))
       ((quote value)
        (make-constant-pattern value))
       ((guard subpattern test-form)
        (make-guard-pattern (parse-pattern subpattern) test-form))
       ((not subpattern)
        (make-not-pattern (parse-pattern subpattern)))
       ((or &rest subpatterns)
        (if (= (length subpatterns) 1)
            (parse-pattern (first subpatterns))
            (let ((subpatterns (mapcar #'parse-pattern subpatterns)))
              (when (some #'place-pattern-included-p subpatterns)
                (error "Or-pattern can't include place-patterns."))
              (apply #'make-or-pattern subpatterns))))
       ((and &rest subpatterns)
        (if (= (length subpatterns) 1)
            (parse-pattern (first subpatterns))
            (apply #'make-and-pattern (mapcar #'parse-pattern subpatterns))))
       ((otherwise &rest args)
        (apply #'parse-constructor-pattern (car pattern) args))))
    (otherwise
     (make-constant-pattern pattern))))

(defgeneric parse-constructor-pattern (name &rest args))

(defmethod parse-constructor-pattern ((name (eql 'cons)) &rest args)
  (unless (= (length args) 2)
    (error "Malformed pattern: ~S" (list* 'cons args)))
  (apply #'make-cons-pattern (mapcar #'parse-pattern args)))

(defmethod parse-constructor-pattern ((name (eql 'assoc)) &rest args)
  (destructuring-bind (item pattern &key key test) args
    (make-assoc-pattern item (parse-pattern pattern) :key key :test test)))

(defmethod parse-constructor-pattern ((name (eql 'property)) &rest args)
  (destructuring-bind (item pattern) args
    (make-property-pattern item (parse-pattern pattern))))

(defmethod parse-constructor-pattern ((name (eql 'vector)) &rest args)
  (apply #'make-vector-pattern (mapcar #'parse-pattern args)))

(defmethod parse-constructor-pattern ((name (eql 'simple-vector)) &rest args)
  (apply #'make-simple-vector-pattern (mapcar #'parse-pattern args)))

(defun parse-class-pattern (class-name &rest slot-specs)
  ;; Transform MAKE-INSTANCE style syntax.  During the transformation,
  ;; we also resolve the slot names via MOP.  If no slot found or too
  ;; many slots found, we will raise an error.
  (when (keywordp (first slot-specs))
    (let ((class (find-class class-name nil)))
      (unless (closer-mop:class-finalized-p class)
        (closer-mop:finalize-inheritance class))
      (setq slot-specs
            (loop with all-slot-names = (mapcar #'closer-mop:slot-definition-name
                                                (closer-mop:class-slots class))
                  for (slot-name . pattern) in (plist-alist slot-specs)
                  for slot-names = (remove-if (lambda (name) (string/= slot-name name))
                                           all-slot-names)
                  collect (case (length slot-names)
                            (0 (error "Slot ~S not found" slot-name))
                            (1 `(,(first slot-names) ,pattern))
                            (t (error "Ambiguous slot name ~S" slot-name)))))))
  (apply #'make-class-pattern class-name
         (loop for slot-spec in slot-specs
               do (setq slot-spec (ensure-list slot-spec))
               collect (let ((slot-name (first slot-spec)))
                         (list slot-name
                               (if (rest slot-spec)
                                   (parse-pattern `(and ,@(rest slot-spec)))
                                   (make-variable-pattern slot-name)))))))

(defun parse-structure-pattern (conc-name &rest slot-specs)
  ;; Transform MAKE-INSTANCE style syntax.
  (when (keywordp (first slot-specs))
    (setq slot-specs (mapcar (lambda (assoc) (list (car assoc) (cdr assoc)))
                             (plist-alist slot-specs))))
  (apply #'make-structure-pattern conc-name
         (loop for slot-spec in slot-specs
               do (setq slot-spec (ensure-list slot-spec))
               collect (let ((slot-name (first slot-spec)))
                         (list slot-name
                               (if (rest slot-spec)
                                   (parse-pattern `(and ,@(rest slot-spec)))
                                   (make-variable-pattern slot-name)))))))

(defmethod parse-constructor-pattern ((name (eql 'class)) &rest args)
  (apply #'parse-class-pattern args))

(defmethod parse-constructor-pattern ((name (eql 'structure)) &rest args)
  (apply #'parse-structure-pattern args))

(defmethod parse-constructor-pattern (name &rest slot-specs)
  (if (find-class name nil)
      (apply #'parse-class-pattern name slot-specs)
      (apply #'parse-structure-pattern name slot-specs)))

;;; Pattern Specifier Parser

(defgeneric unparse-pattern (pattern))

(defmethod unparse-pattern ((pattern variable-pattern))
  (or (variable-pattern-name pattern) '_))

(defmethod unparse-pattern ((pattern place-pattern))
  `(place ,(place-pattern-name pattern)))

(defmethod unparse-pattern ((pattern constant-pattern))
  (with-slots (value) pattern
    (cond
      ((typep value '(and symbol (not keyword) (not (member nil t))))
       `(quote ,value))
      ((atom value)
       value)
      (t
       `(quote ,value)))))

(defmethod unparse-pattern ((pattern guard-pattern))
  `(guard ,(unparse-pattern (guard-pattern-subpattern pattern))
          ,(guard-pattern-test-form pattern)))

(defmethod unparse-pattern ((pattern not-pattern))
  `(not ,(unparse-pattern (not-pattern-subpattern pattern))))

(defmethod unparse-pattern ((pattern or-pattern))
  `(or ,@(mapcar #'unparse-pattern (or-pattern-subpatterns pattern))))

(defmethod unparse-pattern ((pattern and-pattern))
  `(and ,@(mapcar #'unparse-pattern (and-pattern-subpatterns pattern))))

(defmethod unparse-pattern ((pattern cons-pattern))
  `(cons ,(unparse-pattern (cons-pattern-car-pattern pattern))
         ,(unparse-pattern (cons-pattern-cdr-pattern pattern))))

(defmethod unparse-pattern ((pattern assoc-pattern))
  (with-slots (item key test) pattern
    `(assoc ,item
            ,(unparse-pattern (assoc-pattern-value-pattern pattern))
            ,@(when key (list :key key))
            ,@(when test (list :test test)))))

(defmethod unparse-pattern ((pattern property-pattern))
  `(property ,(property-pattern-item pattern)
             ,(unparse-pattern (property-pattern-value-pattern pattern))))

(defmethod unparse-pattern ((pattern vector-pattern))
  `(vector ,@(mapcar #'unparse-pattern (vector-pattern-subpatterns pattern))))

(defmethod unparse-pattern ((pattern simple-vector-pattern))
  `(simple-vector ,@(mapcar #'unparse-pattern (simple-vector-pattern-subpatterns pattern))))

(defmethod unparse-pattern ((pattern class-pattern))
  `(class ,(class-pattern-class-name pattern)
          ,@(loop for slot-name in (class-pattern-slot-names pattern)
                  for subpattern in (class-pattern-subpatterns pattern)
                  collect (list slot-name (unparse-pattern subpattern)))))

(defmethod unparse-pattern ((pattern structure-pattern))
  `(structure ,(structure-pattern-conc-name pattern)
              ,@(loop for slot-name in (structure-pattern-slot-names pattern)
                      for subpattern in (structure-pattern-subpatterns pattern)
                      collect (list slot-name (unparse-pattern subpattern)))))

(defmethod print-object ((pattern pattern) stream)
  ;; NOTE: printing the pattern specifier might not be valid but this
  ;; is useful for debugging the process of pattern matching compiler.
  (format stream "~S" (unparse-pattern pattern)))
