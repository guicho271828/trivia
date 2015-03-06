;;;    Type and Test Inference
;;;
;;; these works as the hints for the compiler.
;;; include only those which is not trivially defined.
;;; By default, type-tests finds appropriate functions automatically
;;; e.g. for type X, predicates like X-p or Xp, and also (typep ? X).
;;; 
;;; CL allows abbreviated compound specifier, like (array type
;;; dimensions), (array type), (array), array. The purpose of
;;; define-compound-type-tests is to ease adding these types of variations.

;;; 
(in-package :optima.level2.impl)

(lispn:define-namespace inference-rules function)

(defmacro define-inference-rule (name args &body body)
  (assert (= (length args) 1))
  `(setf (symbol-inference-rules ',name)
         #+sbcl
         (sb-int:named-lambda ',name ,args ,@body)
         #-sbcl
         (lambda ,args ,@body)))

(defun test-type (test)
  "infer the type which the given test form is trying to test against."
  ;; if the speed matters, it is possible to memoize the result.
  (let (closed)
    (do ((open nil (cdr open))
         (now test (car open)))
        ((null now)
         (warn "failed to infer the type from test ~a !" test)
         nil)
      (push now closed)
      (maphash (lambda (key fn)
                 (when-let ((successors (funcall fn now)))
                   (format t "~& ~<expanded ~a with rule ~a -> ~_~{~a~^, ~:_~}~:>" (list now key successors))
                   (dolist (s successors)
                     (match0 s
                       ((list 'typep '? type)
                        (when (symbolp type)
                          (return-from test-type type)))))
                   (unionf open (set-difference successors closed :test #'equal)
                           :test #'equal)))
               *INFERENCE-RULES-TABLE*))))

(defun type-tests (type)
  (let (closed)
    (do ((open nil (cdr open))
         (now `(typep ? ',type) (car open)))
        ((null now) closed)
      (push now closed)
      (maphash (lambda (key fn)
                 (when-let ((successors (funcall fn now)))
                   (format t "~& ~<expanded ~a with rule ~a -> ~_~{~a~^, ~:_~}~:>" (list now key successors))
                   (unionf open (set-difference successors closed :test #'equal)
                           :test #'equal)))
               *INFERENCE-RULES-TABLE*))))


;;; typep

(define-inference-rule typep (test)
  (match0 test
    ((list 'typep '? (list 'quote type))
     (append (mapcar (lambda (type) `(typep ? ',type))
                     (all-compound-types type))
             (if (atom type)
                 (append (predicatep-function type)
                         (predicate-p-function type))
                 (when (every (curry #'eq '*) (cdr type))
                   (append (predicatep-function (car type))
                           (predicate-p-function (car type)))))))))


(defun predicatep (type)
  (let* ((name (symbol-name type)))
    (find-symbol (format nil "~aP" name)
                 (symbol-package type))))

(defun predicate-p (type)
  (let* ((name (symbol-name type)))
    (find-symbol (format nil "~a-P" name)
                 (symbol-package type))))

(defun predicatep-function (type)
  (let ((fnsym (predicatep type)))
    (when (fboundp fnsym)
      `((,fnsym ?)))))

(defun predicate-p-function (type)
  (let ((fnsym (predicate-p type)))
    (when (fboundp fnsym)
      `((,fnsym ?)))))

(defvar *max-compound-type-arguments* 10)
(defvar *compound-infer-level*)
(defun all-compound-types (compound &optional (*compound-infer-level* 0))
  ;; in ANSI, functions like find-type is not implemented.  Also, the
  ;; consequences are undefined if the type-specifier is not a type
  ;; specifier.  however, in most implementations the default behavior is
  ;; to signal an error, verified in CCL and SBCL.
  ;; however, we safeguard with *max-level*. for most types it would be enough :)
  ;; Also, if the type is not valid, it returns nil.
  (unless (< *max-compound-type-arguments* *compound-infer-level*)
    (handler-case
        (progn (typep nil compound)
               (cons compound
                     (all-compound-types
                      (if (consp compound) 
                          `(,@compound *)
                          `(,compound))
                      *compound-infer-level*)))
      (error (c)
        (declare (ignorable c))
        nil))))
;; (all-compound-types '(array))
;; ((ARRAY) (ARRAY *) (ARRAY * *))

;;; unary function

(define-inference-rule unary-function (test)
  (match0 test
    ((list function '?)
     (let ((name (symbol-name function)))
       (match0 (coerce (subseq name (- (length name) 2) (length name)) 'list)
         ((list #\- #\P)
          (when-let ((typesym (find-symbol (subseq name 0 (- (length name) 2))
                                           (symbol-package function))))
            (typep-form typesym)))
         ((list _ #\P)
          (when-let ((typesym (find-symbol (subseq name 0 (- (length name) 1))
                                           (symbol-package function))))
            (typep-form typesym))))))))

(defun typep-form (typesym)
  (when-let ((compounds (all-compound-types typesym)))
    ;; array, (array), (array *), ...
    (mapcar (lambda (x) `(typep ? ,x)) compounds)))



;;; null

(defvar +null-tests+
    `((null ?)
      (typep ? null)
      (eql ? nil)
      (eql nil ?)
      (eq ? nil)
      (eq nil ?)
      (equal ? nil)
      (equal nil ?)
      (equalp ? nil)
      (equalp nil ?)))

(define-inference-rule null-tests (test)
  (when (member test +null-tests+ :test #'equal)
    +null-tests+))

;;; user-defined types with deftype
;; requires typexpand

(define-inference-rule derived-type (test)
  (match0 test
    ((list 'typep '? (list 'quote type))
     (list (list 'typep '? (list 'quote (typexpand-1 type))))))) ;; ensure all intermediate types are added



