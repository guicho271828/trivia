
(defpackage :optima
  (:use :trivia :trivia.fail)
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

(defpackage :optima.extra
  (:use :trivia)
  (:export #:alist
           #:plist

           #:if-match
           #:when-match
           #:unless-match
           #:let-match
           #:let-match*
           #:let-match1

           #:lambda-match
           #:lambda-ematch
           #:lambda-cmatch
           #:lambda-match1
           #:lambda-ematch1
           #:lambda-cmatch1))


