;;;; trivia level2
(defpackage :trivia.level2
  (:use :trivia.level1)
  ;; level1
  (:export :*or-pattern-allow-unshared-variables*
           :match1 :guard1 :or1)
  (:export :match2 :match2* ; primitives
           :match2+ :match2*+
           :generate-multi-matcher
           ;; external
           :match :match*
           :ematch :ematch*
           :cmatch :cmatch*
           :match-error
           :match-error-pattern
           :match-error-patterns        ; optima compatibility
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
           :lambda-match1
           :lambda-ematch1
           :lambda-cmatch1
           :defun-match
           :defun-ematch
           :defun-cmatch
           :defun-match*
           :defun-ematch*
           :defun-cmatch*
           :if-match
           :when-match
           :unless-match
           :let-match
           :let-match*
           :let-match1
           ;; 
           :guard
           :alist
           :plist
           :property
           :property!
           :hash-table-entry
           :hash-table-entry!
           :hash-table-entries
           :hash-table-entries!
           :access
           :$guard1
           :$or1
           :<>
           :row-major-array
           :row-major-array*
           :@
           :@@
           :defpattern-inline
           :inline-pattern-expand
           :place
           :dynamic
	   :lambda-list :lambda-list-nc
	   :λlist :λlist-nc
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
           :symbol-optimizer
           ;; for accessor arity check
           :*test-call-argument*
           :*arity-check-by-test-call*

           :bit-vector*
           :simple-string*
           :base-string*
           :simple-bit-vector*
           :vector*
           :simple-vector*
           :string*
           :simple-base-string*
           )
  (:nicknames :trivia))

(defpackage :trivia.level2.impl
  (:use :cl :alexandria
        :trivia.next
        :trivia.level0
        :trivia.level1
        :trivia.level2)
  (:export :predicatep :predicate-p))
