
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


