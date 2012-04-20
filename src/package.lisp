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
                #:when-let)
  (:import-from :anaphora
                #:aif
                #:awhen
                #:it)
  (:import-from :closer-mop
                #:slot-definition-name
                #:class-slots)
  (:export #:defpattern
           #:match
           #:multiple-value-match
           #:ematch
           #:multiple-value-ematch
           #:cmatch
           #:multiple-value-cmatch
           #:xmatch
           #:guard))
