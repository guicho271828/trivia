(defpackage :optima
  (:use :cl)
  (:import-from :alexandria
                #:ensure-car
                #:ensure-list
                #:mappend
                #:symbolicate
                #:make-keyword
                #:make-gensym-list
                #:required-argument
                #:with-unique-names
                #:once-only
                #:if-let
                #:when-let
                #:destructuring-case
                #:plist-alist)
  (:import-from :closer-mop
                #:slot-definition-name
                #:class-slots)
  (:export #:match
           #:multiple-value-match
           #:ematch
           #:multiple-value-ematch
           #:cmatch
           #:multiple-value-cmatch

           #:fail
           #:match-error
           #:match-error-values
           #:match-error-patterns

           #:place
           #:guard
           #:defpattern

           #:if-match
           #:when-match
           #:unless-match
           #:with-match
           #:lambda-match
           #:lambda-ematch
           #:lambda-cmatch
           #:lambda-match1
           #:lambda-ematch1
           #:lambda-cmatch1
           #:defun-match
           #:defun-ematch
           #:defun-cmatch
           #:defun-match1
           #:defun-ematch1
           #:defun-cmatch1))
