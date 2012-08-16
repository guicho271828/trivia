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
                #:when-let
                #:destructuring-case)
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

           #:if-match
           #:if-smatch
           #:when-match
           #:when-smatch
           #:unless-match
           #:with-match
           #:lambda-match
           #:lambda-ematch
           #:lambda-cmatch
           #:defun-match
           #:defun-ematch
           #:defun-cmatch))

(defpackage :optima.extra
  (:use :cl :optima)
  (:export #:plist
           #:alist)
  (:documentation
   "This package contains derived and constructor patterns with
designators not from COMMON-LISP package.

#### PLIST

Syntax:

    plist-constructor-pattern ::= (plist (key PATTERN)*)

Examples:

    (match '(:one 1 :two 2 :three 3)
      ((plist :one 1 :two x) x))
    => 2

#### ALIST

Syntax:

    alist-constructor-pattern ::= (alist (key . PATTERN)*)

Examples:

    (match '((:one . 1) (:two . 2) (:three . 3))
      ((alist (:one . 1) (:two . x)) x))
    => 2"))
