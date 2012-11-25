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

(defpackage :optima.extra
  (:use :cl :optima)
  (:import-from :alexandria
                #:plist-alist)
  (:export #:alist
           #:plist

           #:if-match
           #:when-match
           #:unless-match
           #:let-match
           #:let-match*
           #:let-match1)
  (:documentation "
### [Pattern] alist

Syntax:

    (alist (KEY . PATTERN)*)

Expansion:

    (alist (k . p)*) => (and (assoc k p)*)

Examples:

    (match '((1 . :one) (2 . :two) (3 . :three))
      ((alist (1 . x) (3 . y)) (list x y)))
    => (:ONE :THREE)

### [Pattern] plist

Syntax:

    (plist {KEY PATTERN}*)

Expansion:

    (plist {k p}*) => (and (passoc k p)*)

Examples:

    (match '(:name \"John\" :age 23)
      ((plist :name \"John\" :age age) age))
    => 23"))
