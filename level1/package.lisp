(defpackage :optima.level1
  (:export :match1* :match1 :or1 :guard1 :variables :next :or1-pattern-inconsistency :vars1 :vars2 :?))

(defpackage :optima.level1.impl
  (:use :cl
        :alexandria
        :optima.level0
        :optima.level1))

(in-package :optima.level1.impl)

;;; match1 specification

;; NOTE: There are several restrictions in the input of level-1
;; pattern match.

;; First of all, level-1 `match' accepts or1/guard1 patterns only.
;; syntax:
;;  (or1 subpattens*)
;;  (guard1 symbol test-form {generator-form subpattern}*)

;; Level-1 guard1 patterns do not allow subpatterns in `symbol'.  1 guard1
;; pattern corresponds to exactly 1 type checking.  (note: the task of the
;; optimizer is to minimize the number of checking).

;; Level-1 patterns should be canonical. That is, there are no
;; forward/backward-referenced symbols, and all subpatterns of or1-pattern
;; share the same set of variables.

;; Thus, compilation of level-1 `match' is equivalent to just building a
;; form consisting of `if' and `let' binding. level-1 `match' assumes the
;; tree is already valid and optimized.

;;; API

(defmacro match1* (whats &body clauses)
  (assert (listp whats))
  (let* ((args (mapcar (gensym* "ARG") whats))
         (bindings (mapcar #'list args whats))
         (clauses
          (mapcar (lambda (clause)
                    (with-gensyms (it) ;; peudo arg
                      (match0 clause
                        ((list* patterns body)
                         (list* `(guard1 ,it t ,@(mappend #'list args patterns))
                                body)))))
                  clauses)))
    `(let ,bindings
       (match1 t ,@clauses))))

(defmacro match1 (what &body clauses)
  (once-only (what)
    (%match what clauses)))

;;; implementation

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun %match (arg clauses)
  `(block nil
     ,@(match-clauses arg clauses)))

(defun match-clauses (arg clauses)
  (mapcar
   (lambda-match0
     ((list* pattern body)
      (match-clause arg pattern `(return (progn ,@body)))))
   clauses))

(defun match-clause (arg pattern body)
  (match0 pattern
    ((list* 'guard1 symbol test-form more-patterns)
     (assert (symbolp symbol) nil "guard1 pattern accepts symbol only !
    --> (guard1 symbol test-form {generator subpattern}*)
    symbol: ~a" symbol)
     `(let ((,symbol ,arg))
        (when ,test-form
          ,(destructure-guard1-subpatterns more-patterns body))))
    ((list* 'or1 subpatterns)
     (let ((fn (gensym "FN"))
           (vars (variables pattern)))
       `(flet ((,fn ,vars
                 (declare (ignorable ,@vars))
                 ,body))
          (declare (dynamic-extent (function ,fn)))
          ,@(mapcar (lambda (subpattern)
                      (match-clause arg subpattern `(,fn ,@vars)))
                    subpatterns))))
    (_ (error "[~a] huh? : ~a" 'match-pattern-against pattern))))

(defun destructure-guard1-subpatterns (more-patterns body)
  (match0 more-patterns
    (nil body)
    ((list* generator subpattern more-patterns)
     (with-gensyms (field)
       `(let ((,field ,generator))
          ,(match-clause field
                         subpattern
                         (destructure-guard1-subpatterns more-patterns body)))))
    (_ (error "huh? ~a" more-patterns))))

;;; utility: variable-list

(defun set-equal-or-nil (seq1 seq2)
  (when (set-equal seq1 seq2)
    seq1))

(define-condition or1-pattern-inconsistency (error)
  ((vars1 :initarg :vars1 :reader vars1)
   (vars2 :initarg :vars2 :reader vars2)))

(define-condition guard1-pattern-nonlinear (error)
  ((vars :initarg :vars :reader vars)
   (pattern :initarg :pattern :reader pattern)))

(defun variables (pattern)
  "given a pattern, traverse the matching tree and returns a list of variables bounded by guard1 pattern.
gensym'd anonymous symbols are not accounted i.e. when symbol-package is non-nil.
When or1 subpatterns have inconsistency, it signals a continuable error, with use-value restarts."
  (match0 pattern
    ((list* 'guard1 symbol _ more-patterns)
     (assert (symbolp symbol) nil
             "guard1 pattern accepts symbol only ! ~_--> (guard1 symbol test-form {generator subpattern}*) symbol: ~a" symbol)
     (if (symbol-package symbol)
         ;; consider the explicitly named symbols only
         (let ((more-vars (variables-more-patterns more-patterns)))
           (assert (not (member symbol more-patterns))
                   () 'guard1-pattern-nonlinear
                   :vars more-vars
                   :pattern pattern)
           (cons symbol more-vars))
         (variables-more-patterns more-patterns)))
    ((list* 'or1 subpatterns)
     (reduce (lambda (vars next)
               (restart-case
                   (when (or vars next)
                     ;; handles the cases where both vars and next are nil
                     (assert (set-equal-or-nil vars next)
                             (vars)
                             'or1-pattern-inconsistency
                             :vars1 vars
                             :vars2 next)
                     vars)
                 (use-value (value)
                   value)))
             (mapcar #'variables subpatterns)))
    (_ (error "[variables] huh? : ~a" pattern))))

(defun variables-more-patterns (more-patterns)
  (match0 more-patterns
    (nil nil)
    ((list* _ subpattern more-patterns)
     (union (variables subpattern)
            (variables-more-patterns more-patterns)))
    (_ (error "[variables-more-patterns] huh? ~a" more-patterns))))

;; (variables `(guard1 x t (car x) (guard1 y t)))

