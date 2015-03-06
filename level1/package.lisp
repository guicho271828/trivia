;;; level1 implementation

(defpackage :optima.level1
  (:export :match1* :match1 :or1 :guard1 :variables :next
           :*or-pattern-allow-unshared-variables*
           :or1-pattern-inconsistency
           :guard1-pattern-nonlinear
           :conflicts :pattern :repair-pattern :?))

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
  "based on match1"
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

;;; syntax error

(define-condition or1-pattern-inconsistency (error)
  ((pattern :initarg :pattern :accessor pattern)
   (conflicts :initarg :conflicts :accessor conflicts))
  (:report (lambda (c s)
             (format s "~< subpatterns of ~:_ ~a ~:_ binds different set of variables: ~:_ ~a, ~:_ ~a ~:>"
                     (list (pattern c)
                           (first (conflicts c))
                           (second (conflicts c)))))))

(define-condition guard1-pattern-nonlinear (error)
  ((pattern :initarg :pattern :accessor pattern)
   (conflicts :initarg :conflicts :accessor conflicts))
  (:report (lambda (c s)
             (format s "~< guard1 pattern ~:_ ~a ~:_ rebinds a variable ~a. current context: ~:_ ~a ~:>"
                     (list (pattern c)
                           (apply #'intersection (conflicts c))
                           (first (conflicts c)))))))

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
      (match-clause arg
                    (correct-pattern pattern)
                    `(return (progn ,@body)))))
   clauses))

;;; pattern syntax validation

(defvar *lexvars* nil)  ;; list of symbols bound in the context

(defun %correct-more-patterns (more)
  (ematch0 more
    (nil nil)
    ((list* gen sub more)
     (let ((newsub (correct-pattern sub)))
       (list* gen newsub
              (let ((*lexvars* (append (variables newsub) *lexvars*)))
                (%correct-more-patterns more)))))))

(defvar *or-pattern-allow-unshared-variables* t)

(defun union* (&optional x y) (union x y))

(defun correct-pattern (pattern)
  (match0 pattern
    ((list* 'guard1 symbol _)
     (restart-case
         (progn
           (check-guard1 symbol pattern)
           (let ((*lexvars* (cons symbol *lexvars*)))
             (list* 'guard1 symbol test
                    (%correct-more-patterns more-patterns))))
       (repair-pattern (pattern)
         (correct-pattern pattern))))
    ((list* 'or1 subpatterns)
     (let ((subpatterns (mapcar #'correct-pattern subpatterns)))
       (let ((all-vars (reduce #'union* subpatterns :key #'variables)))
         `(or1 ,@(mapcar
                  (lambda (sp)
                    (let* ((vars (variables sp))
                           (missing (set-difference all-vars vars)))
                      (if *or-pattern-allow-unshared-variables*
                          (bind-missing-vars-with-nil sp missing)
                          (restart-case
                              (progn (assert (null missing)
                                             nil
                                             'or1-pattern-inconsistency
                                             :pattern pattern
                                             :conflicts (list all-vars vars))
                                     sp)
                            (repair-pattern (sp) sp)))))
                  subpatterns)))))))

(defun bind-missing-vars-with-nil (pattern missing)
  (ematch0 missing
    ((list) pattern)
    ((list* var rest)
     (bind-missing-vars-with-nil
      (with-gensyms (it)
        `(guard1 ,it t
                 nil (guard1 ,var t)
                 ,it ,pattern))
      rest))))


(defun check-guard1 (sym pattern)
  (assert (symbolp sym)
          nil
          "guard1 pattern accepts symbol only !
    --> (guard1 symbol test-form {generator subpattern}*)
    symbol: ~a" sym)
  (assert (not (member sym *lexvars*))
          nil
          'guard1-pattern-nonlinear
          :pattern pattern
          :conflicts `((,sym) ,*lexvars*)))

;;; matching form generation

(defun match-clause (arg pattern body)
  (match0 pattern
    ((list* 'guard1 symbol test-form more-patterns)
     (let ((*lexvars* (cons symbol *lexvars*)))
       `(let ((,symbol ,arg))
          (when ,test-form
            ,(destructure-guard1-subpatterns more-patterns body)))))
    ((list* 'or1 subpatterns)
     (let* ((vars (variables pattern)))
       (with-gensyms (fn)
         `(flet ((,fn ,vars
                   (declare (ignorable ,@vars))
                   ;; ,@(when vars `((declare (ignorable ,@vars))))
                   ,body))
            (declare (dynamic-extent (function ,fn)))
            ;; we do not want to mess up the looking of expansion
            ;; with symbol-macrolet
            ,@(mapcar (lambda (pattern)
                        (match-clause arg pattern `(,fn ,@vars)))
                      subpatterns)))))))

(defun destructure-guard1-subpatterns (more-patterns body)
  (match0 more-patterns
    (nil body)
    ((list* generator subpattern more-patterns)
     (with-gensyms (field)
       `(let ((,field ,generator))
          ,(match-clause field
                         subpattern
                         (let ((*lexvars* (append (variables subpattern)
                                                  *lexvars*)))
                           (destructure-guard1-subpatterns more-patterns body))))))))

;;; utility: variable-list

(defun set-equal-or-error (&optional seq1 seq2)
  (if (set-equal seq1 seq2)
      seq1
      (error "~a and ~a differs! this should have been fixed by correct-pattern, why!!??"
             seq1 seq2)))

(defun variables (pattern &optional *lexvars*)
  (%variables (correct-pattern pattern)))

(defun %variables (pattern)
  "given a pattern, traverse the matching tree and returns a list of variables bounded by guard1 pattern.
gensym'd anonymous symbols are not accounted i.e. when symbol-package is non-nil.
When or1 subpatterns have inconsistency, it signals a continuable error, with use-value restarts."
  (match0 pattern
    ((list* 'guard1 symbol _ more-patterns)
     (if (symbol-package symbol)
         ;; consider the explicitly named symbols only
         (cons symbol (%variables-more-patterns more-patterns))
         (%variables-more-patterns more-patterns)))
    ((list* 'or1 subpatterns)
     (reduce #'set-equal-or-error
             (mapcar #'%variables subpatterns)))))

(defun %variables-more-patterns (more-patterns)
  (ematch0 more-patterns
    (nil nil)
    ((list* _ subpattern more-patterns)
     (append (%variables subpattern)
             (%variables-more-patterns more-patterns)))))

;; (variables `(guard1 x t (car x) (guard1 y t)))

