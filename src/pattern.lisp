(in-package :optima)

;;;
;;; Pattern Data Structure
;;;

(defstruct pattern)

(defstruct (variable-pattern (:include pattern))
  name)

(defstruct (constant-pattern (:include pattern))
  value)

(defstruct (constructor-pattern (:include pattern))
  name
  arity
  type
  arguments
  predicate
  accessor)

(defstruct (as-pattern (:include pattern))
  sub-pattern name)

(defstruct (guard-pattern (:include pattern))
  sub-pattern
  test-form)

(defstruct (not-pattern (:include pattern))
  sub-pattern)

(defstruct (or-pattern (:include pattern))
  sub-patterns)

;;;
;;; Pattern Utilities
;;;

(defun pattern-guarded-p (pattern)
  (typecase pattern
    (constructor-pattern
     (some #'pattern-guarded-p (constructor-pattern-arguments pattern)))
    (as-pattern
     (pattern-guarded-p (as-pattern-sub-pattern pattern)))
    (guard-pattern t)
    (not-pattern
     (pattern-guarded-p (not-pattern-sub-pattern pattern)))
    (or-pattern
     (some #'pattern-guarded-p (or-pattern-sub-patterns pattern)))))

(defun pattern-variables (pattern)
  ;; TODO check for linear pattern
  (typecase pattern
    (variable-pattern
     (awhen (variable-pattern-name pattern)
       (list it)))
    (constructor-pattern
     (mappend #'pattern-variables (constructor-pattern-arguments pattern)))
    (as-pattern
     (pattern-variables (as-pattern-sub-pattern pattern)))
    (guard-pattern
     (pattern-variables (guard-pattern-sub-pattern pattern)))
    (not-pattern
     (pattern-variables (not-pattern-sub-pattern pattern)))
    (or-pattern
     (mappend #'pattern-variables (or-pattern-sub-patterns pattern)))))

(defun pattern-type (pattern)
  (typecase pattern
    (variable-pattern t)
    (constant-pattern
     `(eql ,(constant-pattern-value pattern)))
    (constructor-pattern
     (constructor-pattern-type pattern))
    (as-pattern
     (pattern-type (as-pattern-sub-pattern pattern)))
    (guard-pattern
     (pattern-type (guard-pattern-sub-pattern pattern)))
    (not-pattern
     `(not ,(pattern-type (not-pattern-sub-pattern pattern))))
    (or-pattern
     `(or ,@(mapcar #'pattern-type (or-pattern-sub-patterns pattern))))))

;;;
;;; Pattern Specifier
;;;

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

;;;
;;; Pattern Specifier Parser
;;;

(defun parse-pattern (pattern)
  (when (pattern-p pattern)
    (return-from parse-pattern pattern))
  (setq pattern (pattern-expand pattern))
  (typecase pattern
    ((or (eql t) null keyword)
     (make-constant-pattern :value pattern))
    (symbol
     (let ((name (unless (or (eq pattern 'otherwise)
                             (string= pattern "_"))
                   pattern)))
       (make-variable-pattern :name name)))
    (cons
     (destructuring-case pattern
       ((quote value)
        (make-constant-pattern :value value))
       ((as sub-pattern name)
        (make-as-pattern :sub-pattern (parse-pattern sub-pattern)
                         :name name))
       ((guard sub-pattern test-form)
        (make-guard-pattern :sub-pattern (parse-pattern sub-pattern)
                            :test-form test-form))
       ((not sub-pattern)
        (make-not-pattern :sub-pattern (parse-pattern sub-pattern)))
       ((or sub-patterns)
        (make-or-pattern :sub-patterns (mapcar #'parse-pattern sub-patterns)))
       ((otherwise &rest args)
        (apply #'parse-constructor-pattern (car pattern) args))))
    (otherwise
     (make-constant-pattern :value pattern))))

(defgeneric parse-constructor-pattern (name &rest args))

(defmethod parse-constructor-pattern ((name (eql 'cons)) &rest args)
  (unless (= (length args) 2)
    (error "Invalid number of arguments: ~D" (length args)))
  (destructuring-bind (car-pattern cdr-pattern)
      (mapcar #'parse-pattern args)
    (make-constructor-pattern
     :name 'cons
     :arity 2
     :type `(cons ,(pattern-type car-pattern) ,(pattern-type cdr-pattern))
     :arguments (list car-pattern cdr-pattern)
     :predicate (lambda (var) `(consp ,var))
     :accessor (lambda (var i) `(,(ecase i (0 'car) (1 'cdr)) ,var)))))

(defmethod parse-constructor-pattern ((name (eql 'vector)) &rest args)
  (let* ((args (mapcar #'parse-pattern args))
         (element-type `(or ,@(mapcar #'pattern-type args)))
         (arity (length args)))
    (make-constructor-pattern
     :name 'vector
     :arity arity
     :type `(vector ,element-type ,arity)
     :arguments args
     :predicate (lambda (var) `(typep ,var '(vector * ,arity)))
     :accessor (lambda (var i) `(aref ,var ,i)))))

(defmethod parse-constructor-pattern ((name (eql 'simple-vector)) &rest args)
  (let* ((args (mapcar #'parse-pattern args))
         (arity (length args)))
    (make-constructor-pattern
     :name 'simple-vector
     :arity arity
     :type `(simple-vector ,arity)
     :arguments args
     :predicate (lambda (var) `(typep ,var '(simple-vector ,arity)))
     :accessor (lambda (var i) `(svref ,var ,i)))))

(defmethod parse-constructor-pattern (class-name &rest slot-patterns)
  (setq slot-patterns (mapcar #'ensure-list slot-patterns))
  (let* ((class (find-class class-name))
         (slot-defs (class-slots class))
         (slot-names (mapcar #'slot-definition-name slot-defs)))
    (awhen (first (set-difference (mapcar #'car slot-patterns) slot-names))
      (error "Unknown slot name ~A for ~A" it class-name))
    (let ((arguments
            (loop for slot-name in slot-names
                  for slot-pattern = (assoc slot-name slot-patterns)
                  collect
                  (if slot-pattern
                      (if (cdr slot-pattern)
                          (parse-pattern (second slot-pattern))
                          (make-variable-pattern :name (car slot-pattern)))
                      (make-variable-pattern))))
          (predicate (lambda (var) `(typep ,var ',class-name)))
          (accessor (lambda (var i) `(slot-value ,var ',(nth i slot-names)))))
      (make-constructor-pattern :name class-name
                                :arity (length arguments)
                                :type class-name
                                :arguments arguments
                                :predicate predicate
                                :accessor accessor))))
