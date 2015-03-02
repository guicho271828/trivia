(defpackage :optima.level1
  (:export :match* :match :guard :variables :next :or-pattern-inconsistency))

(defpackage :optima.level1.impl
  (:use :cl
        :alexandria
        :optima.level0
        :optima.level1)
  (:shadowing-import-from
   :optima.level0
   :match))

(in-package :optima.level1.impl)

(defmacro optima.level1:match* (whats &body clauses)
  ;; multi-in multi-match by default
  (assert (listp whats))
  (%match whats
          (mapcar (lambda-match0
                    ((list* pattern body)
                     (list* (list pattern) body)))
                  clauses)))

(defmacro optima.level1:match (what &body clauses)
  `(match* (,what) ,@clauses))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defvar *args*)
(setf (documentation '*args* 'variable)
      "lists of gensym symbols. matching clause tests against these
variables. the body is wrapped with `let' bounding these variables.")

(defun %match (args clauses)
  (let ((*args* (mapcar (gensym* "ARG") args)))
    `(let ,(mapcar #'list *args* args)
       (block nil
         ,@(match-clauses clauses)
         #+nil
         (tagbody
           ,@(match-clauses clauses))))))

(defun match-clauses (clauses)
  (mapcar
   (lambda-match0
     ((list* patterns body)
      (match-clause patterns `(return (progn ,@body)))))
   clauses)
  #+nil
  (alexandria:mappend
   (lambda-match
     ((list* patterns body)
      (let ((tag (tag))
            (form (match-clause patterns `(return (progn ,@body)))))
        `((macrolet ((next () (go ,tag)))
            ,form)
          ,tag))))
   clauses))

;; Level-1 `match' accepts or/guard patterns only.
;; syntax:
;;  (or subpattens*)
;;  (guard symbol test-form {generator-form subpattern}*)

;; NOTE: There are several restrictions in the input of level-1 pattern
;; match. Level-1 patterns are canonical. That is, there are no
;; forward/backward-referenced symbols, and all subpatterns of or-pattern share
;; the same set of variables.

;; Also, level-1 guard patterns do not allow subpatterns in `symbol'.
;; 1 guard pattern corresponds to exactly 1 type checking.
;; (note: the task of the optimizer is to minimize the number of checking).

;; thus, compilation of level-1 `match' is equivalent to just building a
;; form consisting of `if' and `let' binding. level-1 `match' assumes the tree is
;; already valid and optimized.


(defvar *patterns*)
(setf (documentation '*patterns* 'variable) "")
(defvar *body*)
(setf (documentation '*body* 'variable) "")

(defun match-clause (*patterns* *body* &optional (*args* *args*))
  (assert (= (length *args*) (length *patterns*)))
  (match-remaining-patterns))

(defun match-remaining-patterns ()
  (match0 *patterns*
    (nil *body*)
    ((list* pattern *patterns*)
     (match0 *args*
       ((list* arg *args*)
        (match-pattern-against pattern arg))))
    (_ (error "[~a] huh?" 'match-remaining-patterns))))

(defun match-pattern-against (p arg)
  ;; returns a form that check if p matches arg, and if so, bind some variables.
  (match0 p
    ((list* 'guard symbol test-form more-patterns)
     `(let ((,symbol ,arg))
        (when ,test-form
          ,(destructure-more-patterns more-patterns))))
    ((list* 'or subpatterns)
     (let ((fn (gensym "FN"))
           (vars (variables p)))
       `(flet ((,fn ,vars
                 (declare (ignorable ,@vars))
                 ,(match-remaining-patterns)))
          (declare (dynamic-extent (function ,fn)))
          ,@(mapcar (lambda (subpattern)
                      (match-clause (list subpattern)
                                    `(,fn ,@vars)
                                    (list arg)))
                    subpatterns))))
    (_ (error "[~a] huh?" 'match-pattern-against))))

(defun destructure-more-patterns (more-patterns)
  (match0 more-patterns
    (nil (match-remaining-patterns)) ;; next pattern
    ((list* generator subpattern more-patterns)
     (let* ((further-expansion (destructure-more-patterns more-patterns))
            (arg (gensym "DESTRCT")))
       `(let ((,arg ,generator)) ;; no need to add to *lexvars*
          ,(match-clause `(,subpattern) further-expansion (list arg)))))
    (_ (error "huh? ~a" more-patterns))))

;;; utility: variable-list

(defun set-equal-or-nil (seq1 seq2)
  (when (set-equal seq1 seq2)
    seq1))

(define-condition or-pattern-inconsistency (error)
  ((subpatterns :initarg :subpatterns :reader subpatterns)))

(defun variables (pattern)
  (match0 pattern
    ((list* 'guard symbol _ more-patterns)
     (let ((morevar (variables-more-patterns more-patterns)))
       (assert (not (member symbol morevar)))
       (cons symbol morevar)))
    ((list* 'or subpatterns)
     (let ((variables-set (mapcar #'variables subpatterns)))
       (assert (reduce #'set-equal-or-nil variables-set)
               nil
               'or-pattern-inconsistency
               :subpatterns subpatterns)
       (first variables-set)))
    (_ (error "[variables] huh? : ~a" pattern))))

(defun variables-more-patterns (more-patterns)
  (match0 more-patterns
    (nil nil)
    ((list* _ subpattern more-patterns)
     (union (variables subpattern)
            (variables-more-patterns more-patterns)))
    (_ (error "[variables-more-patterns] huh? ~a" more-patterns))))

;; (variables `(guard x t (car x) (guard y t)))


