
;;;    Type Relationships
(in-package :optima.level2.impl)
;; CLHS glossary
;; 
;; exhaustive partition n. (of a type) a set of pairwise disjoint types
;; that form an exhaustive union.
;; 
;; exhaustive union n. (of a type) a set of subtypes of the type, whose
;; union contains all elements of that type.

(defun disjointp (t1 t2)
  (not (or (subtypep t1 t2)
           (subtypep t2 t1)
           (and (subtypep t1 'standard-object)
                (subtypep t2 'standard-object)
                (intersection
                 (all-subclasses (find-class t1))
                 (all-subclasses (find-class t2)))))))

(defun all-subclasses (type/class)
  "for a given type or class, return the list of all subclasses"
  (do ((class (find-class type/class) (car open))
       (open   nil  (cdr open))
       (closed nil  (cons class closed)))
      ((null class) closed)
    (let ((scs (c2mop:class-direct-subclasses class)))
      ;; (format t "~&~a ~a" class scs)
      (appendf open (set-difference scs closed)))))

;; hash table seems an overkill currently
#+nil 
(defun all-subclasses (class)
  (do ((class class (car open))
       (open   nil  (cdr open))
       (closed (make-hash-table)
               (progn (setf (gethash class closed) t) closed)))
      ((null class) (hash-table-keys closed))
    (let ((scs (c2mop:class-direct-subclasses class)))
      ;; (format t "~&~a ~a" class scs)
      (appendf open (set-difference scs closed)))))

(defun exhaustive-partitions (type/class)
  (mapcar #'class-name
          (closer-mop:class-direct-subclasses
           (find-class type/class))))

(defun exhaustive-union (types)
  ;; is there a set of types whose instances are exactly the same set as that of given types,
  ;; but the number of types are minimized?
  (let* ((classes (mapcar #'find-class (remove-redundunt-types types)))
         (best classes)
         (bound (length classes)))
    (do ((open nil (cdr open))
         (now classes (car open)))
        ((null now) (mapcar #'class-name best))
      ;;(print now)
      (dolist (which now)
        (let* ((supers (c2mop:class-direct-superclasses which))
               (covered (remove-duplicates
                         (mappend #'c2mop:class-direct-subclasses supers))))
          (when (and covered (every (lambda (c) (member c now)) covered)) 
            (let* ((new-union (append supers (set-difference now covered)))
                   (len (length new-union)))
              (cond
                ((= bound len)
                 (push new-union open))
                ((> bound len)
                 (setf bound len
                       best new-union
                       open (list new-union)))))))))))

(defun remove-redundunt-types (types)
  (ematch0 types
    ((list) nil)
    ((list* type rest)
     (if (some (lambda (c) (subtypep type c)) rest)
         (remove-redundunt-types rest)
         (cons type
              (remove-redundunt-types
               (remove-if (lambda (c) (subtypep c type)) rest)))))))


;;; common lisp types that may not corresponds to a class
;;;; predefined classes
(defvar *predefined-classes*
    `(arithmetic-error generic-function simple-error array hash-table
      simple-type-error bit-vector integer simple-warning broadcast-stream
      list standard-class built-in-class logical-pathname
      standard-generic-function cell-error method standard-method character
      method-combination standard-object class null storage-condition
      complex number stream concatenated-stream package stream-error
      condition package-error string cons parse-error string-stream
      control-error pathname structure-class division-by-zero
      print-not-readable structure-object echo-stream program-error
      style-warning end-of-file random-state symbol error ratio
      synonym-stream file-error rational t file-stream reader-error
      two-way-stream float readtable type-error floating-point-inexact real
      unbound-slot floating-point-invalid-operation restart
      unbound-variable floating-point-overflow sequence undefined-function
      floating-point-underflow serious-condition vector function
      simple-condition warning))

;;;; predefined atomic type specifiers
(defvar *predefined-atomic-types*
    `(arithmetic-error function simple-condition array generic-function
      simple-error atom hash-table simple-string base-char integer
      simple-type-error base-string keyword simple-vector bignum list
      simple-warning bit logical-pathname single-float bit-vector
      long-float standard-char broadcast-stream method standard-class
      built-in-class method-combination standard-generic-function
      cell-error nil standard-method character null standard-object class
      number storage-condition compiled-function package stream complex
      package-error stream-error concatenated-stream parse-error string
      condition pathname string-stream cons print-not-readable
      structure-class control-error program-error structure-object
      division-by-zero random-state style-warning double-float ratio symbol
      echo-stream rational synonym-stream end-of-file reader-error t error
      readtable two-way-stream extended-char real type-error file-error
      restart unbound-slot file-stream sequence unbound-variable fixnum
      serious-condition undefined-function float short-float unsigned-byte
      floating-point-inexact signed-byte vector
      floating-point-invalid-operation simple-array warning
      floating-point-overflow simple-base-string floating-point-underflow
      simple-bit-vector))


;;;; their differences

(defvar *predefined-atomic-types-no-classes*
    (set-difference *predefined-atomic-types*
                    *predefined-classes*))

;; (simple-bit-vector simple-base-string simple-array signed-byte unsigned-byte short-float fixnum extended-char
;;  double-float compiled-function nil standard-char long-float single-float bit bignum simple-vector keyword
;;  base-string base-char simple-string atom)




