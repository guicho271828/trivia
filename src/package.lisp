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
           #:property
           #:defpattern))
