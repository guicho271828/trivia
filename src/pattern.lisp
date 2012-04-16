(in-package :fivepm)

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
  arguments
  predicate
  accessor)

(defstruct (guard-pattern (:include pattern))
  pattern
  test-form)

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
     (make-variable-pattern :name (unless (string= pattern "_") pattern)))
    (cons
     (case (first pattern)
       (quote
        (make-constant-pattern :value (second pattern)))
       (guard
        (make-guard-pattern :pattern (parse-pattern (second pattern))
                            :test-form (third pattern)))
       (otherwise
        (apply #'parse-constructor-pattern (car pattern) (cdr pattern)))))
    (otherwise
     (make-constant-pattern :value pattern))))

(defgeneric parse-constructor-pattern (name &rest args))

(defmethod parse-constructor-pattern ((name (eql 'cons)) &rest args)
  (unless (= (length args) 2)
    (error "invalid number of arguments: ~D" (length args)))
  (make-constructor-pattern
   :name 'cons
   :arity 2
   :arguments (mapcar #'parse-pattern args)
   :predicate (lambda (var) `(consp ,var))
   :accessor (lambda (var i) `(,(ecase i (0 'car) (1 'cdr)) ,var))))

(macrolet ((define-sequece-constructor-pattern (type elt)
             `(defmethod parse-constructor-pattern ((name (eql ',type)) &rest args)
                (make-constructor-pattern
                 :name ',type
                 :arity (length args)
                 :arguments (mapcar #'parse-pattern args)
                 :predicate (lambda (var) `(typep ,var ',',type))
                 :accessor (lambda (var i) `(,',elt ,var ,i))))))
  (define-sequece-constructor-pattern sequence elt)
  (define-sequece-constructor-pattern array aref)
  (define-sequece-constructor-pattern vector aref)
  (define-sequece-constructor-pattern simple-vector svref))

(defmethod parse-constructor-pattern (class-name &rest slot-patterns)
  (setq slot-patterns (mapcar #'ensure-list slot-patterns))
  (let* ((class (find-class class-name))
         (slot-defs (class-slots class))
         (slot-names (mapcar #'slot-definition-name slot-defs)))
    (awhen (first (set-difference (mapcar #'car slot-patterns) slot-names))
      (error "unknown slot name ~A for ~A" it class-name))
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
                                :arguments arguments
                                :predicate predicate
                                :accessor accessor))))
