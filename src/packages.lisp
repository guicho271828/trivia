(defpackage :optima.core
  (:use :cl)
  (:export ;; Runtime
           %equal
           %equals
           %svref
           %assoc
           %get-property

           ;; Data Destructor
           #:destructor
           #:destrucotr-bindings
           #:destructor-form
           #:destructor-accessor-forms
           #:make-destructor

           ;; Patterns
           #:variable-pattern
           #:variable-pattern-name
           #:make-variable-pattern

           #:place-pattern
           #:place-pattern-name
           #:make-place-pattern

           #:constant-pattern
           #:constant-pattern-value
           #:make-constant-pattern

           #:complex-pattern
           #:subpatterns
           #:complex-pattern-subpatterns

           #:guard-pattern
           #:guard-pattern-test-form
           #:guard-pattern-subpattern
           #:make-guard-pattern

           #:not-pattern
           #:not-pattern-subpattern
           #:make-not-pattern

           #:or-pattern
           #:or-pattern-subpatterns
           #:make-or-pattern

           #:and-pattern
           #:and-pattern-subpatterns
           #:make-and-pattern

           #:constructor-pattern
           #:constructor-pattern-subpatterns
           #:constructor-pattern-arity
           #:constructor-pattern-destructor-sharable-p
           #:constructor-pattern-make-destructor

           #:cons-pattern
           #:cons-pattern-car-pattern
           #:cons-pattern-cdr-pattern
           #:make-cons-pattern

           #:assoc-pattern
           #:assoc-pattern-item
           #:assoc-pattern-key
           #:assoc-pattern-test
           #:assoc-pattern-value-pattern
           #:make-assoc-pattern

           #:property-pattern
           #:property-pattern-item
           #:property-pattern-value-pattern
           #:make-property-pattern

           #:vector-pattern
           #:vector-pattern-subpatterns
           #:make-vector-pattern

           #:simple-vector-pattern
           #:simple-vector-pattern-subpatterns
           #:make-simple-vector-pattern

           #:class-pattern
           #:class-pattern-subpatterns
           #:class-pattern-class-name
           #:class-pattern-slot-names
           #:make-class-pattern

           #:structure-pattern
           #:structure-pattern-subpatterns
           #:structure-pattern-conc-name
           #:structure-pattern-slot-names
           #:make-structure-pattern

           ;; Pattern Utilities
           #:pattern-variables
           #:place-pattern-included-p
           #:check-patterns
           #:lift-guard-patterns

           ;; Pattern Specifier
           #:pattern-expand-function
           #:pattern-expand-1
           #:pattern-expand
           #:pattern-expand-all

           ;; Pattern Specifier Parser
           #:parse-pattern
           #:parse-constructor-pattern
           #:unparse-pattern))

(defpackage :optima
  (:use :cl :optima.core)
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
                #:plist-alist
                #:with-unique-names)
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
           #:lambda-cmatch1)
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

    (plist {k p}*) => (and (property k p)*)

Examples:

    (match '(:name \"John\" :age 23)
      ((plist :name \"John\" :age age) age))
    => 23"))
