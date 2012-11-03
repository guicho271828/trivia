(in-package :optima)

;;; Pattern Data Structure

(defstruct pattern
  ;; The original pattern specifier
  (specifier (required-argument)))

(defmethod print-object ((pattern pattern) stream)
  ;; Note: printing the pattern specifier might not be valid but this
  ;; is useful for debugging the process of pattern matching compiler.
  (format stream "~S" (pattern-specifier pattern)))

(defstruct (variable-pattern (:include pattern))
  name)

(defstruct (place-pattern (:include pattern))
  name)

(defstruct (constant-pattern (:include pattern))
  value)

(defstruct (constructor-pattern (:include pattern))
  "A constructor-pattern matches not a value itself but a structure of
the value
  SIGNATURE - a mostly-unique data structure
  ARGUMENTS - List of subpatterns
  PREDICATE - lambda-predicate returning T if the currently matched data (its argument)
              can be matched by this constructor pattern without regard for subpatterns
  ACCESSOR - lambda of two arguments: the data and the currently matched subpattern
             index. Should return the corresponding piece of data structure to be matched
             by this subpattern"
  signature
  arguments
  predicate
  accessor)

(defstruct (guard-pattern (:include pattern))
  sub-pattern test-form)

(defstruct (not-pattern (:include pattern))
  sub-pattern)

(defstruct (or-pattern (:include pattern))
  sub-patterns)

(defstruct (and-pattern (:include pattern))
  sub-patterns)

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
  (typecase pattern
    (variable-pattern
     (let ((name (variable-pattern-name pattern)))
       (when (and name (not (temporary-variable-p name)))
         (list name))))
    (constructor-pattern
     (mappend #'pattern-variables (constructor-pattern-arguments pattern)))
    ((or guard-pattern not-pattern)
     (pattern-variables (slot-value pattern 'sub-pattern)))
    ((or or-pattern and-pattern)
     (mappend #'pattern-variables (slot-value pattern 'sub-patterns)))))

(defun place-pattern-included-p (pattern)
  (typecase pattern
    (place-pattern t)
    (constructor-pattern
     (some #'place-pattern-included-p
           (constructor-pattern-arguments pattern)))
    ((or guard-pattern not-pattern)
     (place-pattern-included-p (slot-value pattern 'sub-pattern)))
    ((or or-pattern and-pattern)
     (some #'place-pattern-included-p
           (slot-value pattern 'sub-patterns)))))

(defun linear-pattern-p (pattern)
  "Returns true if PATTERN is a linear pattern.
If not, the cause variable will be returned as a second value."
  (flet ((check (vars)
           (loop for var in vars
                 if (find var seen)
                   return (values nil var)
                 collect var into seen
                 finally (return t))))
    (typecase pattern
      (constructor-pattern
       (check (mappend #'pattern-variables (constructor-pattern-arguments pattern))))
      ((or guard-pattern not-pattern)
       (linear-pattern-p (slot-value pattern 'sub-pattern)))
      (or-pattern
       (every #'linear-pattern-p (or-pattern-sub-patterns pattern)))
      (and-pattern
       (check (mappend #'pattern-variables (and-pattern-sub-patterns pattern))))
      (t t))))

(defun check-pattern (pattern)
  "Check if PATTERN is valid. Otherwise, an error will be raised."
  (cond ((not (linear-pattern-p pattern))
         (error "Non-linear pattern: ~S"
                (pattern-specifier pattern)))))

;;; Pattern Specifier

(defun pattern-expand-function (name)
  (get name 'pattern-expand-function))

(defun (setf pattern-expand-function) (function name)
  (setf (get name 'pattern-expand-function) function))

(defun pattern-expand-1 (pattern)
  (aif (and (consp pattern)
            (symbolp (car pattern))
            (pattern-expand-function (car pattern)))
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

(defun make-bind-pattern (name)
  (flet ((var-name (name)
           (unless (or (eq name 'otherwise)
                       (string= name "_"))
             name)))
    (make-variable-pattern :specifier name
                           :name (var-name name))))

(defun parse-pattern (pattern)
  (when (pattern-p pattern)
    (return-from parse-pattern pattern))
  (setq pattern (pattern-expand pattern))
  (typecase pattern
    ((or (eql t) null keyword)
     (make-constant-pattern :specifier pattern
                            :value pattern))
    (symbol
     (make-bind-pattern pattern))
    (cons
     (destructuring-case pattern
       ((variable name)
        (make-bind-pattern name))
       ((place name)
        (make-place-pattern :specifier pattern
                            :name name))
       ((symbol-macrolet name)
        (make-place-pattern :specifier pattern
                            :name name))
       ((quote value)
        (make-constant-pattern :specifier `(quote ,value)
                               :value value))
       ((guard sub-pattern test-form)
        (make-guard-pattern :specifier pattern
                            :sub-pattern (parse-pattern sub-pattern)
                            :test-form test-form))
       ((not sub-pattern)
        (make-not-pattern :specifier pattern
                          :sub-pattern (parse-pattern sub-pattern)))
       ((or &rest sub-patterns)
        (if (= (length sub-patterns) 1)
            (parse-pattern (first sub-patterns))
            (let ((sub-patterns (mapcar #'parse-pattern sub-patterns)))
              (when (some #'place-pattern-included-p sub-patterns)
                (error "Or-pattern can't include place-patterns."))
              (make-or-pattern :specifier pattern
                               :sub-patterns sub-patterns))))
       ((and &rest sub-patterns)
        (if (= (length sub-patterns) 1)
            (parse-pattern (first sub-patterns))
            (make-and-pattern :specifier pattern
                              :sub-patterns (mapcar #'parse-pattern sub-patterns))))
       ((otherwise &rest args)
        (apply #'parse-constructor-pattern (car pattern) args))))
    (otherwise
     (make-constant-pattern :specifier pattern
                            :value pattern))))

(defgeneric parse-constructor-pattern (name &rest args))

(defmethod parse-constructor-pattern ((name (eql 'cons)) &rest args)
  (unless (= (length args) 2)
    (error "Invalid number of arguments: ~D" (length args)))
  (destructuring-bind (car-pattern cdr-pattern)
      (mapcar #'parse-pattern args)
    (make-constructor-pattern
     :specifier `(cons ,@args)
     :signature '(cons car cdr)
     :arguments (list car-pattern cdr-pattern)
     :predicate (lambda (var) `(consp ,var))
     :accessor (lambda (var i) `(,(ecase i (0 'car) (1 'cdr)) ,var)))))

(defmethod parse-constructor-pattern ((name (eql 'vector)) &rest args)
  (let* ((args (mapcar #'parse-pattern args))
         (arity (length args)))
    (make-constructor-pattern
     :specifier `(vector ,@args)
     :signature `(vector ,arity)
     :arguments args
     :predicate (lambda (var) `(typep ,var '(vector * ,arity)))
     :accessor (lambda (var i) `(aref ,var ,i)))))

(defmethod parse-constructor-pattern ((name (eql 'simple-vector)) &rest args)
  (let* ((args (mapcar #'parse-pattern args))
         (arity (length args)))
    (make-constructor-pattern
     :specifier `(simple-vector ,@args)
     :signature `(simple-vector ,arity)
     :arguments args
     :predicate (lambda (var) `(typep ,var '(simple-vector ,arity)))
     :accessor (lambda (var i) `(svref ,var ,i)))))

(defun parse-class-constructor-pattern (class-name &rest slot-patterns)
  (setq slot-patterns (mapcar #'ensure-list slot-patterns))
  (let* ((class (find-class class-name))
         (slot-defs (class-slots class))
         (slot-names (mapcar #'slot-definition-name slot-defs)))
    (awhen (first (set-difference (mapcar #'car slot-patterns) slot-names))
      (error "Unknown slot name ~A for ~A" it class-name))
    (let ((signature
            `(,class-name ,@(mapcar #'car slot-patterns)))
          (arguments
            (loop for (slot-name . slot-sub-pats) in slot-patterns
                  if slot-sub-pats
                    collect (parse-pattern `(and ,@slot-sub-pats))
                  else
                    collect (make-bind-pattern slot-name)))
          (predicate
            (lambda (var)
              `(typep ,var ',class-name)))
          (accessor
            (lambda (var i)
              `(slot-value ,var ',(car (nth i slot-patterns))))))
      (make-constructor-pattern :specifier `(class ,class-name ,@slot-patterns)
                                :signature signature
                                :arguments arguments
                                :predicate predicate
                                :accessor accessor))))

(defun parse-struct-constructor-pattern (conc-name &rest slot-patterns)
  (setq slot-patterns (mapcar #'ensure-list slot-patterns))
  (let* ((slot-names (mapcar #'car slot-patterns))
         (arguments
           (loop for slot-pattern in slot-patterns
                 collect
                 (if (cdr slot-pattern)
                     (parse-pattern `(and ,@(cdr slot-pattern)))
                     (make-bind-pattern (car slot-pattern)))))
         (predicate (lambda (var) `(,(symbolicate conc-name :p) ,var)))
         (accessor (lambda (var i) `(,(symbolicate conc-name (nth i slot-names)) ,var))))
    (make-constructor-pattern :specifier `(,conc-name ,@slot-patterns)
                              :signature `(,conc-name ,@slot-names)
                              :arguments arguments
                              :predicate predicate
                              :accessor accessor)))

(defmethod parse-constructor-pattern ((name (eql 'class)) &rest args)
  (apply #'parse-class-constructor-pattern args))

(defmethod parse-constructor-pattern ((name (eql 'structure)) &rest args)
  (apply #'parse-struct-constructor-pattern args))

(defmethod parse-constructor-pattern (name &rest slot-patterns)
  (if (find-class name nil)
      (apply #'parse-class-constructor-pattern name slot-patterns)
      (apply #'parse-struct-constructor-pattern name slot-patterns)))
