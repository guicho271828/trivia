(in-package :trivia.level1.impl)

;;; API

(defmacro match1 (what &body clauses)
  (let ((whatvar (gensym "WHAT1"))
        (toplevel-place-pattern nil))
    (let ((bodyform 
           (handler-bind ((place-pattern
                           (lambda (c)
                             (declare (ignore c))
                             (setf toplevel-place-pattern t))))
             (%match whatvar clauses))))
      `(,(if toplevel-place-pattern 'symbol-macrolet 'let) ((,whatvar ,what))
         (declare (ignorable ,whatvar))
         ,bodyform))))


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

;;; outer construct

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))


(defun %match (arg clauses)
  `(block whole ;; << return from the whole match
     ,@(match-clauses arg clauses)))

;;; `next' implementation
;; we use block-based one, due to the simplicity of the implementation.
;; this, however, does not allow the use of (next) in the last matching
;; clause. This should be handled by the level2 layer, which appends (_
;; nil) clause (in case of normal match) and (_ (error ...)) (in ematch
;; family).
;;;; block-based `next' implementation

(defun match-clauses (arg clauses)
  (mapcar1-with-last
   (lambda (clause last?)
     (ematch0 clause
       ((list* pattern body)
        ((lambda (x) (if last? x `(block clause ,x)))
         ;; << (return-from clause) = go to next clause
         ;; the last pattern does not have this block, allowing a direct jump to the upper block
         (match-clause arg
                       (correct-pattern pattern)
                       ;; << return from the whole match
                       `(return-from whole (locally ,@body)))))))
   clauses))

(defun mapcar1-with-last (fn list)
  (ematch0 list
    ((cons it nil)
     (cons (funcall fn it t) nil))
    ((cons it rest)
     (cons (funcall fn it nil) (mapcar1-with-last fn rest)))))

(defmacro next () `(return-from clause nil))
(defmacro fail () `(next))
(defmacro skip () `(next))

;;;; catch-based `next' implementation
;; using catch is not appropriate for our purpose, according to CLHS,
;; "catch and throw are normally used when the exit point must have dynamic
;; scope (e.g., the throw is not lexically enclosed by the catch), while
;; block and return are used when lexical scope is sufficient. "


;; (defun match-clauses (arg clauses)
;;   (mapcar1-with-last
;;    (lambda (clause last?)
;;      (ematch0 clause
;;        ((list* pattern body)
;;         ((lambda (x) (if last? x `(catch 'next ,x)))
;;          (match-clause arg
;;                        (correct-pattern pattern)
;;                        `(return (locally ,@body)))))))
;;    clauses))
;; 
;; (defun mapcar1-with-last (fn list)
;;   (ematch0 list
;;     ((cons it nil)
;;      (cons (funcall fn it t) nil))
;;     ((cons it rest)
;;      (cons (funcall fn it nil) (mapcar1-with-last fn rest)))))
;; 
;; (declaim (inline next fail))
;; (defun next () (throw 'next nil))
;; (defun fail () (next))

;;;; tagbody-based `next' implementation
;; This implementation results in a bloated expansion which contains lots
;; of macrolets.

;; (defun match-clauses (arg clauses)
;;   `((tagbody
;;       ,@(mappend
;;          (lambda (clause)
;;            (ematch0 clause
;;              ((list* pattern body)
;;               ((lambda (x)
;;                  (with-gensyms (tag)
;;                    `((with-next (,tag) ,x) ,tag)))
;;                (match-clause arg
;;                              (correct-pattern pattern)
;;                              `(return (locally ,@body)))))))
;;          clauses))))
;; 
;; (defun mapcar1-with-last (fn list)
;;   (ematch0 list
;;     ((cons it nil)
;;      (cons (funcall fn it t) nil))
;;     ((cons it rest)
;;      (cons (funcall fn it nil) (mapcar1-with-last fn rest)))))
;; 
;; (defmacro with-next ((tag) &body body)
;;   `(macrolet ((next () `(go ,',tag))) ,@body))
;; 
;; (defmacro next () `(error "NEXT called outside matching clauses"))
;; (defmacro fail () `(next))

;;; pattern syntax validation

(defvar *lexvars* nil "List of symbol-and-options in the current parsing context.")

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

(defparameter *correct-pattern-cache* nil)

(defun correct-pattern (pattern)
  (let ((cache *correct-pattern-cache*))
    (if cache
        (let ((key (cons *lexvars* pattern)))
          (multiple-value-bind (val present?)
              (gethash key cache)
            (if present?
                val
                (let ((new-val (correct-pattern* pattern)))
                  (setf (gethash key cache) new-val)))))
        (let ((*correct-pattern-cache* (make-hash-table :test #'equal)))
          (correct-pattern pattern)))))

(defun correct-pattern* (pattern)
  "Recursively check and try to correct the mismatch in the set of variables in or1 patterns"
  (ematch0 pattern
    ((list* 'guard1 symbol? test more-patterns)
     (restart-case
         (let ((symopts (preprocess-symopts symbol? pattern)))
           (let ((*lexvars* (cons symopts *lexvars*)))
             (list* 'guard1 symopts test
                    (%correct-more-patterns more-patterns))))
       (repair-pattern (pattern)
         (correct-pattern pattern))))
    ((list* 'or1 subpatterns)
     (let ((subpatterns (mapcar #'correct-pattern subpatterns)))
       (let ((all-vars (reduce #'union* subpatterns :key #'variables)))
         `(or1 ,@(mapcar
                  (lambda (sp)
                    (let* ((vars (variables sp))
                           (missing (set-difference all-vars vars :key #'car)))
                      (if *or-pattern-allow-unshared-variables*
                          (correct-pattern
                           (bind-missing-vars-with-nil sp missing))
                          (restart-case
                              (progn (assert (null missing)
                                             nil
                                             'or1-pattern-inconsistency
                                             :pattern pattern
                                             :conflicts (list all-vars vars))
                                     sp)
                            (repair-pattern (sp) sp)))))
                  subpatterns)))))))

(defmacro ensure-getf (place key &optional default)
  (once-only (key default)
    `(setf (getf ,place ,key) (getf ,place ,key ,default))))

(defun preprocess-symopts (symopt? pattern)
  "Ensure the symopts being a list, plus sets some default values."
  (match0 (ensure-list symopt?)
    ((list* sym options)
     ;; error check
     ;; symbol
     (assert (symbolp sym)
             nil
             "guard1 pattern accepts symbol only !
    --> (guard1 symbol test-form {generator subpattern}*)
    symbol: ~a" sym)
     (assert (not (member sym *lexvars* :key #'car))
             nil
             'guard1-pattern-nonlinear
             :pattern pattern
             :conflicts `((,sym) ,(mapcar #'car *lexvars*)))
     (destructuring-bind (&key
                          (type t)
                          (binder 'let)
                          (ignorable (if (symbol-package sym) nil t))
                          dynamic-extent
                          special
                          &allow-other-keys)
         options
       ;; the implementer of level2 patterns can append arbitrary
       ;; meta-infomation as a keyword.
       (ensure-getf options :type type)
       (ensure-getf options :binder binder)
       (ensure-getf options :ignorable ignorable)
       (ensure-getf options :dynamic-extent dynamic-extent)
       (ensure-getf options :special special)
       (list* sym options)))))

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

;;; matching form generation

(define-condition place-pattern () ())

(defvar *trace-dispatching* nil
  "Used only for debugging. When non-nil, test-form for pattern matching is printed on the stream.")

(defmacro trace-when (condition &rest body)
  "Evaluate and print the result of the form when *trace-dispatching* is non-nil."
  (with-gensyms (result trace-block)
    (if *trace-dispatching*
        `(block ,trace-block
           (let ((,result ,condition))
             (pprint-logical-block (*trace-output* nil :per-line-prefix "|")
               ,(if (consp condition)
                    `(format *trace-output* "  ~s~:_~1,16@T= ~s~:_~1,16@T= ~s~:@_"
                             ',condition (list ',(car condition) ,@(cdr condition)) ,result)
                    `(format *trace-output* "  ~s~:_~1,16@T= ~s~:@_"
                             ',condition ,result))
               (return-from ,trace-block
                 (when ,result ,@body)))))
        `(when ,condition ,@body))))

(defun match-clause (arg pattern body)
  ;; All patterns are corrected by correct-pattern. The first argument of
  ;; guard1 patterns are converted into a list (symbol &key
  ;; &allow-other-keys)
  (ematch0 pattern
    ((list* 'guard1 symopts test-form more-patterns)
     (let ((*lexvars* (cons symopts *lexvars*)))
       (ematch0 symopts
         ((list* symbol options)
          `(,@(ecase (getf options :binder)
                (symbol-macrolet
                  (signal 'place-pattern)
                  `(symbol-macrolet ((,symbol ,arg))))
                (let
                  `(let ((,symbol ,arg))))
                (progv
                  `(progv (,symbol) (,arg))))
             ,@(when (getf options :ignorable)
                 `((declare (ignorable ,symbol))))
             ,@(when (getf options :dynamic-extent)
                 `((declare (dynamic-extent ,symbol))))
             ,@(when (getf options :special)
                 `((declare (special ,symbol))))
             ,((lambda (x)
                 (if (eq t test-form)
                     ;; remove redundunt IF. x contains a return to NIL==toplevel.
                     x
                     `(trace-when ,test-form ,x)))
               (let ((type (getf options :type)))
                 ((lambda (x)
                    (if (eq t type) x ;; remove redundunt DECLARE
                        `(locally (declare (type ,type ,symbol)) ,x)))
                  (destructure-guard1-subpatterns more-patterns body)))))))))
    ((list* 'or1 subpatterns)
     (let* ((vars (variables pattern)))
       (with-gensyms (fn)
         `(flet ((,fn ,(mapcar #'first vars)
                   (declare
                    ,@(mapcar (lambda-ematch0
                                ((list* var options)
                                 `(type ,(getf options :type) ,var)))
                              vars))
                   ,body))
            (declare (dynamic-extent (function ,fn)))
            ;; we do not want to mess up the looking of expansion
            ;; with symbol-macrolet
            ,@(mapcar (lambda (pattern)
                        (match-clause arg pattern `(,fn ,@(mapcar #'first vars))))
                      subpatterns)))))))

(defun destructure-guard1-subpatterns (more-patterns body)
  (ematch0 more-patterns
    (nil body)
    ((list generator)
     (error "Odd number of elements in {generator subpattern}*: subpattern for ~a missing!"
            generator))
    ((list* generator subpattern more-patterns)
     (match-clause generator
                   subpattern
                   (let ((*lexvars* (append (variables subpattern)
                                            *lexvars*)))
                     (destructure-guard1-subpatterns more-patterns body))))))

;;; utility: variable-list

(defun set-equal-or-error (&optional seq1 seq2)
  (if (set-equal seq1 seq2 :key #'car)
      ;; FIXME: this does not seem correct, considering there are several options now.
      ;; type: type information of or-pattern is...?
      ;; place: T supersedes NIL
      ;; ignorable: never appears here since ignorable variables are excluded
      (merge-variables seq1 seq2)
      (error "~<~s and ~s differs! this should have been fixed by correct-pattern, why!!??~:@>"
             (list seq1 seq2))))

(defun merge-variables (seq1 seq2)
  (match0 seq1
    ((list* (list* v1 o1) rest)
     (match0 (find v1 seq2 :key #'car)
       ((list* _ o2)
        (cons (list v1
                    :type `(or ,(getf o1 :type) ,(getf o2 :type))
                    :place (or (getf o1 :place) (getf o2 :place))
                    :ignorable (or (getf o1 :ignorable) (getf o2 :ignorable)))
              (merge-variables rest seq2)))))))

(defun variables (pattern &optional *lexvars*)
  (%variables (correct-pattern pattern)))

(defun %variables (pattern)
  "given a pattern, traverse the matching tree and returns a list of variables bounded by guard1 pattern.
Temporary symbols are not accounted."
  (ematch0 pattern
    ((list* 'guard1 symopts _ more-patterns)
     (if (getf (cdr symopts) :ignorable)
         ;; consider the explicitly named symbols only
         (%variables-more-patterns more-patterns)
         (cons symopts (%variables-more-patterns more-patterns))))
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

