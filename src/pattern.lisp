(in-package :optima)

;;; Data Destructor

(defgeneric destructor-equal (dtor1 dtor2))
(defgeneric destructor-predicate-form (dtor var))
(defgeneric destructor-forms (dtor var))

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
                       (:constructor %make-or-pattern))
  ;; A symbol that describes which branch is matched. The value is an
  ;; integer which starts from zero. This is used by compiler
  ;; internally.
  tag)

(defun make-or-pattern (&rest subpatterns)
  (%make-or-pattern :subpatterns subpatterns))

(defun make-or*-pattern (tag &rest subpatterns)
  (%make-or-pattern :tag tag :subpatterns subpatterns))

(defstruct (and-pattern (:include complex-pattern)
                        (:constructor make-and-pattern (&rest subpatterns))))

(defstruct (constructor-pattern (:include complex-pattern)))

(defun constructor-pattern-arity (pattern)
  (length (constructor-pattern-subpatterns pattern)))

(defstruct (cons-pattern (:include constructor-pattern)
                         (:constructor make-cons-pattern (car-pattern cdr-pattern
                                                          &aux (subpatterns (list car-pattern
                                                                                  cdr-pattern))))))

(defun cons-pattern-car-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defun cons-pattern-cdr-pattern (pattern)
  (second (constructor-pattern-subpatterns pattern)))

(defmethod destructor-equal ((x cons-pattern) (y cons-pattern))
  t)

(defmethod destructor-predicate-form ((pattern cons-pattern) var)
  `(consp ,var))

(defmethod destructor-forms ((pattern cons-pattern) var)
  (list `(car ,var) `(cdr ,var)))

(defstruct (assoc-pattern (:include constructor-pattern)
                          (:constructor make-assoc-pattern (item value-pattern
                                                            &key key test
                                                            &aux (subpatterns (list value-pattern)))))
  item key test)

(defun assoc-pattern-value-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defmethod destructor-equal ((x assoc-pattern) (y assoc-pattern))
  (and (eq (assoc-pattern-key x)
           (assoc-pattern-key y))
       (eq (assoc-pattern-test x)
           (assoc-pattern-test y))
       ;; FIXME: Don't use EQL
       (eql (assoc-pattern-item x)
            (assoc-pattern-item y))))

(defmethod destructor-predicate-form ((pattern assoc-pattern) var)
  (with-slots (item key test) pattern
    (values `(%assoc ,item ,var
                     ,@(when key `(:key #',key))
                     ,@(when test `(:test #',test)))
            t)))

(defmethod destructor-forms ((pattern assoc-pattern) var)
  (list `(cdr ,var)))

(defstruct (passoc-pattern (:include constructor-pattern)
                           (:constructor make-passoc-pattern (item value-pattern
                                                              &aux (subpatterns (list value-pattern)))))
  item)

(defun passoc-pattern-value-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defmethod destructor-equal ((x passoc-pattern) (y passoc-pattern))
  (eq (passoc-pattern-item x) (passoc-pattern-item y)))

(defmethod destructor-predicate-form ((pattern passoc-pattern) var)
  (with-slots (item) pattern
    (values `(%passoc ,item ,var) t)))

(defmethod destructor-forms ((pattern passoc-pattern) var)
  (list `(cadr ,var)))

(defstruct (vector-pattern (:include constructor-pattern)
                           (:constructor make-vector-pattern (&rest subpatterns))))

(defmethod destructor-equal ((x vector-pattern) (y vector-pattern))
  (= (constructor-pattern-arity x)
     (constructor-pattern-arity y)))

(defmethod destructor-predicate-form ((pattern vector-pattern) var)
  `(typep ,var '(vector * ,(constructor-pattern-arity pattern))))

(defmethod destructor-forms ((pattern vector-pattern) var)
  (loop for i from 0 below (constructor-pattern-arity pattern)
        collect `(aref ,var ,i)))

(defstruct (simple-vector-pattern (:include constructor-pattern)
                                  (:constructor make-simple-vector-pattern (&rest subpatterns))))

(defmethod destructor-equal ((x simple-vector-pattern) (y simple-vector-pattern))
  (= (constructor-pattern-arity x)
     (constructor-pattern-arity y)))

(defmethod destructor-predicate-form ((pattern simple-vector-pattern) var)
  `(typep ,var '(simple-vector ,(constructor-pattern-arity pattern))))

(defmethod destructor-forms ((pattern simple-vector-pattern) var)
  (loop for i from 0 below (constructor-pattern-arity pattern)
        collect `(svref ,var ,i)))

(defstruct (class-pattern (:include constructor-pattern)
                          (:constructor %make-class-pattern))
  class-name slot-names)

(defun make-class-pattern (class-name &rest slot-specs)
  (%make-class-pattern :class-name class-name
                       :slot-names (mapcar #'first slot-specs)
                       :subpatterns (mapcar #'second slot-specs)))

(defmethod destructor-equal ((x class-pattern) (y class-pattern))
  (and (eq (class-pattern-class-name x)
           (class-pattern-class-name y))
       (equal (class-pattern-slot-names x)
              (class-pattern-slot-names y))))

(defmethod destructor-predicate-form ((pattern class-pattern) var)
  `(typep ,var ',(class-pattern-class-name pattern)))

(defmethod destructor-forms ((pattern class-pattern) var)
  (loop for slot-name in (class-pattern-slot-names pattern)
        collect `(slot-value ,var ',slot-name)))

(defstruct (structure-pattern (:include constructor-pattern)
                              (:constructor %make-structure-pattern))
  conc-name slot-names)

(defun make-structure-pattern (conc-name &rest slot-specs)
  (%make-structure-pattern :conc-name conc-name
                           :slot-names (mapcar #'first slot-specs)
                           :subpatterns (mapcar #'second slot-specs)))

(defmethod destructor-equal ((x structure-pattern) (y structure-pattern))
  (and (string= (structure-pattern-conc-name x)
                (structure-pattern-conc-name y))
       (equal (structure-pattern-slot-names x)
              (structure-pattern-slot-names y))))

(defmethod destructor-predicate-form ((pattern structure-pattern) var)
  `(,(symbolicate (structure-pattern-conc-name pattern) :p) ,var))

(defmethod destructor-forms ((pattern structure-pattern) var)
  (loop with conc-name = (structure-pattern-conc-name pattern)
        for slot-name in (structure-pattern-slot-names pattern)
        collect `(,(symbolicate conc-name slot-name) ,var)))

;;; Pattern Utilities

(defun gentempvar ()
  "Generate a temporary pattern variable which is able to be accessed
through matching test."
  (let ((var (gensym)))
    (setf (get var 'temporary) t)
    var))

(defun temporary-variable-p (var)
  (eq (get var 'temporary) t))

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
       (let ((name (variable-pattern-name pattern)))
         (when (and name (not (temporary-variable-p name)))
           (list name))))
      (or-pattern
       (let ((vars-list (mappend #'pattern-variables (or-pattern-subpatterns pattern))))
         (check (remove-duplicates vars-list))))
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

(defun lift-guard-patterns (pattern)
  "Lifts GUARD patterns in PATTERN so that the guards can see any
variables in PATTERN. The transform looks like:

       (list x (guard y (equal x y)))
    => (guard (list x y) (equal x y))

Multiple guards will be connected with conjunction in order of
occurence like:

       (list (guard x (consp x)) (guard y (eq y (car x))))
    => (guard (list x (guard y (eq y (car x)))) (consp x))
    => (guard (guard (list x y) (eq y (car x))) (consp x))
    => (guard (list x y) (and (consp x) (eq y (car x))))

OR patterns that include guards are handled specially using OR*
pattern like:

       (or 1 (guard x (evenp x)))
    => (guard (or* tag 1 x) (case tag (1 (evenp x)) (t t)))"
  (cond
    ((guard-pattern-p pattern)
     (let ((subpattern (guard-pattern-subpattern pattern))
           (test-form (guard-pattern-test-form pattern)))
       (setq subpattern (lift-guard-patterns subpattern))
       (when (typep subpattern 'guard-pattern)
         ;; Connect with conjunction like:
         ;; (guard (guard p g2) g1) => (guard p (and g1 g2))
         (setq test-form `(and ,test-form ,(guard-pattern-test-form subpattern))
               subpattern (guard-pattern-subpattern subpattern)))
       (make-guard-pattern subpattern test-form)))
    ((or-pattern-p pattern)
     ;; We assume that if the OR pattern has non-nil tag, then the
     ;; pattern has been already lifted.
     (when (or-pattern-tag pattern)
       (return-from lift-guard-patterns pattern))
     ;; Otherwise, try to lift the pattern.
     (let ((subpatterns (mapcar #'lift-guard-patterns (or-pattern-subpatterns pattern))))
       (if (some #'guard-pattern-p subpatterns)
           ;; Lift guard patterns like:
           ;; (or ... (guard p g) ...) => (guard (or* tag ... p ...) (case tag (n g) (t t))
           ;; Here, a tag is a symbol which describes which branch is matched.
           (loop with tag = (gensym "TAG")
                 for i from 0
                 for subpattern in subpatterns
                 if (guard-pattern-p subpattern)
                   collect (list i (guard-pattern-test-form subpattern)) into case-clauses
                   and do (setq subpattern (guard-pattern-subpattern subpattern))
                 collect subpattern into new-subpatterns
                 finally
                    (return
                      (make-guard-pattern (apply #'make-or*-pattern tag new-subpatterns)
                                          `(case ,tag ,@case-clauses (t t)))))
           ;; Otherwise, just return the original pattern.
           pattern)))
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
           ;; Otherwise, just return the original pattern.
           pattern)))
    (t pattern)))

;;; Pattern Specifier

(defun pattern-expand-function (name)
  (get name 'pattern-expand-function))

(defun (setf pattern-expand-function) (function name)
  (setf (get name 'pattern-expand-function) function))

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
  `(setf (pattern-expand-function ',name) (lambda ,lambda-list ,@body)))

(defpattern list (&rest args)
  (when args
    `(cons ,(car args) (list ,@(cdr args)))))

(defpattern list* (arg &rest args)
  `(cons ,arg
         ,(cond ((null args))
                ((= (length args) 1)
                 (car args))
                (t
                 `(list* ,(car args) ,@(cdr args))))))

(defpattern when (test)
  (let* ((var (gentempvar))
         (test `(let ((* ,var))
                  (declare (ignorable *))
                  ,test)))
    `(guard ,var ,test)))

(defpattern unless (test)
  (let* ((var (gentempvar))
         (test `(let ((* ,var))
                  (declare (ignorable *))
                  (not ,test))))
    `(guard ,var ,test)))

(defpattern satisfies (predicate-name)
  `(when (,predicate-name *)))

(defpattern eq (arg)
  `(when (eq * ,arg)))

(defpattern eql (arg)
  `(when (eql * ,arg)))

(defpattern equal (arg)
  `(when (equal * ,arg)))

(defpattern equalp (arg)
  `(when (equalp * ,arg)))

(defpattern typep (type-specifier)
  `(when (typep * ',type-specifier)))

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
       ((or* tag &rest subpatterns)
        (apply #'make-or*-pattern tag (mapcar #'parse-pattern subpatterns)))
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

(defmethod parse-constructor-pattern ((name (eql 'passoc)) &rest args)
  (destructuring-bind (item pattern) args
    (make-passoc-pattern item (parse-pattern pattern))))

(defmethod parse-constructor-pattern ((name (eql 'vector)) &rest args)
  (apply #'make-vector-pattern (mapcar #'parse-pattern args)))

(defmethod parse-constructor-pattern ((name (eql 'simple-vector)) &rest args)
  (apply #'make-simple-vector-pattern (mapcar #'parse-pattern args)))

(defun parse-class-pattern (class-name &rest slot-specs)
  (apply #'make-class-pattern class-name
         (loop for slot-spec in slot-specs
               do (setq slot-spec (ensure-list slot-spec))
               collect (let ((slot-name (first slot-spec)))
                         (list slot-name
                               (if (rest slot-spec)
                                   (parse-pattern `(and ,@(rest slot-spec)))
                                   (make-variable-pattern slot-name)))))))

(defun parse-structure-pattern (conc-name &rest slot-specs)
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
    (if (atom value)
        value
        `(quote ,value))))

(defmethod unparse-pattern ((pattern guard-pattern))
  `(guard ,(unparse-pattern (guard-pattern-subpattern pattern))
          ,(guard-pattern-test-form pattern)))

(defmethod unparse-pattern ((pattern not-pattern))
  `(not ,(unparse-pattern (not-pattern-subpattern pattern))))

(defmethod unparse-pattern ((pattern or-pattern))
  (let ((subpatterns (mapcar #'unparse-pattern (or-pattern-subpatterns pattern))))
    (with-slots (tag) pattern
      (if tag
          `(or* ,tag ,.subpatterns)
          `(or ,.subpatterns)))))

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

(defmethod unparse-pattern ((pattern passoc-pattern))
  `(passoc ,(passoc-pattern-item pattern)
           ,(unparse-pattern (passoc-pattern-value-pattern pattern))))

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
