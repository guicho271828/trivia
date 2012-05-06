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
                #:with-gensyms
                #:once-only
                #:when-let
                #:destructuring-case)
  (:import-from :anaphora
                #:aif
                #:awhen
                #:it)
  (:import-from :closer-mop
                #:slot-definition-name
                #:class-slots)
  (:export #:match
           #:multiple-value-match
           #:smatch
           #:multiple-value-smatch
           #:ematch
           #:multiple-value-ematch
           #:esmatch
           #:multiple-value-esmatch
           #:cmatch
           #:multiple-value-cmatch
           #:csmatch
           #:multiple-value-csmatch
           #:xmatch
           #:defpattern
           #:guard))
