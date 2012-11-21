optima - Optimized Pattern Matching Library
===========================================

optima is a very fast pattern matching library
which uses optimizing techniques widely used in a functional
programming world. See the following references for more detail:

* [Optimizing Pattern Matching](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.6.5507) by Fabrice Le Fessant, Luc Maranget
* [The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/) by Simon Peyton Jones

Pattern Language
----------------

A pattern specifier, or a pattern for short unless ambiguous, is an
expression that describes how a value matches the pattern. Pattern
specifiers are defined as follows:

    pattern-specifier ::= constant-pattern
                        | variable-pattern
                        | place-pattern
                        | guard-pattern
                        | not-pattern
                        | or-pattern
                        | and-pattern
                        | constructor-pattern
                        | derived-pattern
    
    constant-pattern ::= t | nil
                       | atom-except-symbol
                       | (quote VALUE)
    
    variable-pattern ::= SYMBOL | (variable SYMBOL)
    
    place-pattern ::= (place SYMBOL)
    
    guard-pattern ::= (guard PATTERN TEST-FORM)
    
    not-pattern ::= (not PATTERN)
    
    or-pattern ::= (or PATTERN*)
    
    and-pattern ::= (and PATTERN*)
    
    constructor-pattern ::= (NAME ARG*)
    
    derived-pattern ::= (NAME PATTERN*)

### Constant-Pattern

A constant-pattern matches the constant itself.

Examples:

    (match 1 (1 2)) => 2
    (match "foo" ("foo" "bar")) => "bar"
    (match '(1) ('(1) 2)) => 2

### Variable-Pattern

A variable-pattern matches any value and bind the value to the
variable. _ and otherwise is a special variable-pattern (a.k.a
wildcard-pattern) which matches any value but doesn't bind.

Examples:

    (match 1 (x x)) => 1
    (match 1 (_ 2)) => 2
    (match 1
      (2 2)
      (otherwise 'otherwise))
    => OTHERWISE

### Place-Pattern

A place-pattern matches any value like variable-patterns but bind the
value with SYMBOL-MACROLET.

Examples:

    (defvar c (cons 1 2))
    (match c ((cons (place x) y) (incf x) (incf y)))
    c
     => (2 . 2)

### Guard-Pattern

A guard-pattern is a special pattern that also tests whether TEST-FORM
satisfies in the current matching context.

Examples:

    (match 1 ((guard x (eql x 2)) t))
    => NIL
    (match 1 ((guard x (eql x 1)) t))
    => T

### Not-Pattern

A not-pattern matches a value that is not matched with sub-PATTERN.

Examples:

    (match 1 ((not 2) 3)) => 3
    (match 1 ((not (not 1)) 1)) => 1

### Or-Pattern

An or-pattern matches a value that is matched with one of
sub-PATTERNs. There is a restriction that every pattern of
sub-PATTERNs must have same set of variables.

Examples:

    (match '(2 . 1) ((or (cons 1 x) (cons 2 x)) x))
    => 1

### And-Pattern

An and-pattern matches a value that is matched with all of
sub-PATTERNs. The most common use case is to match a value and bind
the value to a variable.

Examples:

    (match 1 ((and 1 x) x))
    => 1

### Constructor-Pattern

A constructor-pattern matches not a value itself but a structure of
the value. The following constructors are available:

#### CONS

Syntax:

    cons-constructor-pattern ::= (cons CAR-PATTERN CDR-PATTERN)

Examples:

    (match '(1 . 2)
      ((cons a b) (+ a b)))
     => 3

#### ASSOC

Syntax:

    assoc-constructor-pattern ::= (assoc ITEM PATTERN &key key test)

Examples:

    (match '((1 . :one))
      ((assoc 1 x) x))
    => :ONE
    (match '((1 . :one) (2 . :two))
      ((assoc 2 x) x))
    => :TWO
    (match '(1 (2 . 3))
      ((assoc 2 x) x))
    => 3
    (match '(("a" . 123))
      ((assoc "A" 123 :test #'string-equal) t))
    => T

#### PROPERTY

Syntax:

    property-constructor-pattern ::= (property KEY PATTERN)

Examples:

    (match '(:a 1)
      ((property :a x) x))
    => 1
    (match '(:a 1 :b 2)
      ((property :b x) x))
    => 2
    (match '(:a 1 2)
      ((property :a x) x))
    => 1

#### VECTOR

Syntax:

    vector-constructor-pattern ::= (vector PATTERN*)

Examples:

    (match #(1 2)
      ((vector a b) (+ a b)))
    => 3

#### SIMPLE-VECTOR

Syntax:

    simple-vector-constructor-pattern ::= (simple-vector PATTERN*)

Examples:

    (match #(1 2)
      ((simple-vector a b) (+ a b)))
    => 3

#### CLASS

Matches an instance of a given subclass of standard-class, as well as
the instance's slots.

Syntax:

    class-constructor-pattern ::= (class NAME slot*)
                                | (NAME slot*)
    
    slot ::= SLOT-NAME
           | (SLOT-NAME PATTERN*)

CLASS can be omitted. If slot is a symbol, then it will be regarded
as (slot slot). If more than one PATTERN are given, then they will be
wrapped by and-pattern like (and PATTERN*).

Examples:

    (defclass point ()
      ((x :initarg :x)
       (y :initarg :y)))
    (defvar p (make-instance 'point :x 1 :y 2))
    (match p
      ((point x y) (list x y)))
    => (1 2)
    (match p
      ((point (x 1 x)) x))
    => 1
    (defstruct person (name age))
    (defvar foo (make-person :name "foo" :age 30))
    (match foo
      ((person name age) (list name age)))
    => ("foo" 30)

#### STRUCTURE

Matches any structure value, and its slot values.

Syntax:

    structure-constructor-pattern ::= (structure CONC-NAME slot*)
                                    | (CONC-NAME slot*)
    
    slot ::= SLOT-NAME
           | (SLOT-NAME PATTERN*)

As well as CLASS constructor-pattern, STRUCTURE can be
omitted. CONC-NAME is a prefix string of a predicate (CONC-NAME +
"p") and accessors (CONC-NAME + SLOT-NAME). For example, if we have
the following defstruct,

    (defstruct person name age)

the structure constructor-pattern (person- name age) is valid because
PERSON-P, PERSON-NAME and PERSON-AGE are available here. Technically,
we don't need a structure defined. If we have the following code, for
instance,

    (defun point-p (p) (consp p))
    (defun point-x (p) (car p))
    (defun point-y (p) (cdr p))

the pattern matching below is valid.

    (match (cons 1 2)
      ((point- x y) (list x y)))
    => (1 2)

Examples:

    (defstruct (person (:conc-name :p-)
                       (:predicate p-p))
      name age)
    (match (make-person :name "foo" :age 30)
      ((p- name age) (list name age)))
    => ("foo" 30)

### Derived-Pattern

A derived-pattern is a pattern that is defined with DEFPATTERN. There
are some builtin dervied patterns as below:

#### LIST

Expansion of LIST derived patterns=

    (list a b c) => (cons a (cons b (cons c nil)))

#### LIST*

Expansion of LIST* derived patterns:

    (list a b c) => (cons a (cons b c))

#### SATISFIES

Expansion of SATISFIES derived patterns:

    (satisfies f) => (guard it (f it))

#### EQ, EQL, EQUAL, EQUALP

Expansion of EQ, EQL, EQUAL, EQUALP derived patterns:

    (eq 'foo) => (guard it (eq it 'foo))
    (eql 123) => (guard it (eql it 123))
    (equal '(1 2)) => (guard it (equal it '(1 2)))
    (equalp "foo") => (guard it (equalp it "foo"))

#### TYPEP

Expansion of TYPEP derived patterns:

    (TYPEP type) => (when (typep * 'type))

[Package] optima
----------------

## [Macro] match

    match arg &body clauses

Matches ARG with CLAUSES. CLAUSES is a list of the form of (PATTERN
. BODY) where PATTERN is a pattern specifier and BODY is an implicit
progn. If ARG is matched with some PATTERN, then evaluates
corresponding BODY and returns the evaluated value. Otherwise, returns
NIL.

Evaluating a form (FAIL) in the clause body causes the latest pattern
matching be failed. For example,

    (match 1
      (x (if (eql x 1)
             (fail)
             x))
      (_ 'ok))

returns OK, because the form (FAIL) in the first clause is
evaluated.

If BODY starts with the symbols WHEN or UNLESS, then the next form
will be used to introduce (FAIL). That is,

    (match list ((list x) when (oddp x) x))
    (match list ((list x) unless (evenp x) x))

will be translated to

    (match list ((list x) (if (oddp x) x (fail))))
    (match list ((list x) (if (evenp x) (fail) x)))

Examples:

    (match 1 (1 1))
    => 1
    (match 1 (2 2))
    => 2
    (match 1 (x x))
    => 1
    (match (list 1 2 3)
      (list x y z) (+ x y z))
    => 6

## [Macro] multiple-value-match

    multiple-value-match values-form &body clauses

Matches the multiple values of VALUES-FORM with CLAUSES. Unlike
MATCH, CLAUSES have to have the form of (PATTERNS . BODY), where
PATTERNS is a list of patterns. The number of values that will be used
to match is determined by the maximum arity of PATTERNS among CLAUSES.

Examples:

    (multiple-value-match (values 1 2)
     ((2) 1)
     ((1 y) y))
    => 2

## [Macro] ematch

    ematch arg &body clauses

Same as MATCH, except MATCH-ERROR will be raised if not matched.

## [Macro] multiple-value-ematch

    multiple-value-ematch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except MATCH-ERROR will be raised if
not matched.

## [Macro] cmatch

    cmatch arg &body clauses

Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched.

## [Macro] multiple-value-cmatch

    multiple-value-cmatch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except continuable MATCH-ERROR will
be raised if not matched.

## [Macro] fail

    fail

Causes the latest pattern matching be failed and continue to do the
rest of pattern matching.

## [Class] match-error

## [Type] match-error

    match-error

## [Function] match-error-values

    match-error-values condition

## [Function] match-error-patterns

    match-error-patterns condition

## [Macro] defpattern

    defpattern name lambda-list &body body

Defines a derived pattern specifier named NAME. This is analogous
to DEFTYPE.

Examples:

    ;; Defines a LIST pattern.
    (defpattern list (&rest args)
      (when args
        `(cons ,(car args) (list ,@(cdr args)))))

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL

optima.contrib - Contribution library for optima
================================================

Contribution library for optima.

Available Patterns
------------------

### ALIST

Syntax:

    (alist (KEY . PATTERN)*)

Expansion:

    (alist (k . p)*) => (and (assoc k p)*)

Examples:

    (match '((1 . :one) (2 . :two) (3 . :three))
      ((alist (1 . x) (3 . y)) (list x y)))
    => (:ONE :THREE)

### PLIST

Syntax:

    (plist {KEY PATTERN}*)

Expansion:

    (plist {k p}*) => (and (passoc k p)*)

Examples:

    (match '(:name "John" :age 23)
      ((plist :name "John" :age age) age))
    => 23

### PPCRE

Syntax:

    (ppcre REGEXP PATTERN*)

Matches REGEXP against the target string. Sub-PATTERNs will be used to
match the matched groups, if REGEXP matched.

Examples:

    (match "2012-11-04"
      ((ppcre "^\\d{4}-\\d{2}-\\d{2}$" year month day)
       (list year month day)))
    => ("2012" "11" "04")

[Package] optima.contrib
------------------------

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL
