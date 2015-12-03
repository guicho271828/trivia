;;;; trivia level2
(defpackage :trivia.level2
  (:use :trivia.level1)
  ;; level1
  #.(let (acc)
      (do-external-symbols (s :trivia.level1 `(:export ,@acc))
        (push s acc)))
  (:export :match2 :match2* ; primitives
           :match2+ :match2*+
           :generate-multi-matcher
           ;; external
           :match :match*
           :ematch :ematch*
           :cmatch :cmatch*
           :match-error
           :match-error-pattern
           :match-error-values
           :multiple-value-cmatch
           :multiple-value-ematch
           :multiple-value-match
           :lambda-match
           :lambda-ematch
           :lambda-cmatch
           :lambda-match*
           :lambda-ematch*
           :lambda-cmatch*
           :defun-match
           :defun-ematch
           :defun-cmatch
           :defun-match*
           :defun-ematch*
           :defun-cmatch*
           ;; 
           :guard
           :alist
           :plist
           :property
           :access
           :$guard1
           :$or1
           :<>
           :@
           :@@
           :defpattern-inline
           :inline-pattern-expand
           :place
	   :lambda-list
	   :λlist
           ;; 
           :defpattern
           :pattern-expand
           :pattern-expand-1
           :pattern-expand-all
           :defoptimizer
           ;; optimizer
           :optimizer
           :*optimizer*
           :in-optimizer
           :defoptimizer
           :symbol-optimizer)
  (:nicknames :trivia))

(defpackage :trivia.level2.impl
  (:use :cl :alexandria
        :trivia.next
        :trivia.level0
        :trivia.level1
        :trivia.level2)
  (:export :predicatep :predicate-p))
