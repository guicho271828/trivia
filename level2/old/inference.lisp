
(error "this file is no longer used")

;;;    Type and Test Inference
;;;
;;; this file provides utility to infer about primitive types and user-defined types
;;; actual definitions of primitive tests vs type are in tests.lisp
;;; 
(in-package :optima.level2.impl)


;;;    Type vs Test Forms
;; 
;; one-to-one mapping
;; (test-type '(null ?)) -> 'null
;; (test-type '(typep ? 'null)) -> 'null
;; 
;;  one-to-many mapping
;; (type-tests 'null) -> '(null ?) '(typep ? 'null)

(defvar *type-tests* (make-hash-table :test #'equal))

(defvar *test-type* (make-hash-table :test #'equal))

(defun add-type-test (type test)
  "function API"
  (setf (gethash test *test-type*) type)
  (pushnew test (gethash type *type-tests*) :test #'equal))

(defmacro define-type-tests (type args &body tests)
  "macro API"
  (declare (ignorable args))
  (setf tests (union (mappend #'gen-test-permutation tests)
                     (gen-default-type-tests type)
                     :test #'equal))
  `(dolist (test ',tests)
     (add-type-test ',type test)))

(defun gen-test-permutation (test)
  (case (car test)
    (and (let (results)
           (alexandria:map-permutations
            (lambda (perm) (push perm results))
            (mapcar #'cdr test))
           results))
    (or (list (mapcar #'gen-test-permutation (cdr test))))
    (otherwise (list test))))

(defun type-tests (type)
  (or (gethash type *type-tests*)
      (setf (gethash type *type-tests*)
            (gen-default-type-tests type))))

(defun gen-default-type-tests (type)
  (let (result)
    (push `(typep ? ',type) result)
    (when (subtypep type 'structure-object)
      (cond
        ((fboundp (symbolicate type '-p))
         (push `(,(symbolicate type '-p) ?) result))
        ((fboundp (symbolicate type 'p))
         (push `(,(symbolicate type 'p) ?) result))
        (t (simple-style-warning "failed to infer a structure type predicate"))))
    result))

(defun test-type (test) (gethash test *test-type*))

(defun test-compatible-p (t1 t2)
  (member t2 (type-tests (test-type t1)) :key #'equal))



