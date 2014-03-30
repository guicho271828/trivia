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
                       | keyword
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
sub-PATTERNs.

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

You can also use MAKE-INSTANCE style pattern syntax like:

    (match foo
      ((person :name name :age age) (list name age)))
    => ("foo" 30)

This is equal to the example above except this implicitly resolves the
slot names using Meta Object Protocol. In this case, you have to make
sure the slot names can be determined uniquely during the
compilation. Otherwise, you will get a compilation error.

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

Same as class constructor-pattern, you can also use MAKE-INSTANCE
style pattern syntax like:

    (match (cons 1 2)
      ((point- :x x :y y) (list x y)))
    => (1 2)

### Derived-Pattern

A derived-pattern is a pattern that is defined with DEFPATTERN. There
are some builtin dervied patterns as below:

#### LIST

Expansion of LIST derived patterns:

    (list a b c) => (cons a (cons b (cons c nil)))

#### LIST*

Expansion of LIST* derived patterns:

    (list* a b c) => (cons a (cons b c))

#### SATISFIES

Expansion of SATISFIES derived patterns:

    (satisfies f) => (guard it (f it))

#### EQ, EQL, EQUAL, EQUALP

Expansion of EQ, EQL, EQUAL, EQUALP derived patterns:

    (eq 'foo) => (guard it (eq it 'foo))
    (eql 123) => (guard it (eql it 123))
    (equal '(1 2)) => (guard it (equal it '(1 2)))
    (equalp "foo") => (guard it (equalp it "foo"))

#### TYPE

Expansion of TYPE derived patterns:

    (TYPE type) => (guard it (typep it 'type))

Quasiquotation
--------------

You may want to use a quasiquote in a pattern specifier like:

    (match '(1 2 3 4)
      (`(1 ,x ,@y) (list x y)))

To do so, you need to use a specific quasiquote reader, for example
[fare-quasiquote](http://cliki.net/fare-quasiquote) with loading
fare-quasiquote-optima system, because an expanded form of a
quasiquote reader is not standardized.

Define Constructor Patterns
---------------------------

You can define your own constructor patterns by using `OPTIMA.CORE`
package.  Firstly, define a data structore for the constructor
pattern.

    (defstruct (my-cons-pattern (:include constructor-pattern)
                                (:constructor make-cons-pattern (car-pattern cdr-pattern
                                                                 &aux (subpatterns (list car-pattern
                                                                                         cdr-pattern))))))

Note that you must keep `SUBPATTERNS` of the constructor pattern in
sync so that optima can take care of them.  Secondly, specify a
condition when destructor of the constructor patterns can be shared.
Sharing destructors removes redundant data checks, that is,
pattern-matching can get more faster.


    (defmethod constructor-pattern-destructor-sharable-p ((x my-cons-pattern) (y my-cons-pattern))
      t)

Thirdly, define a destructor generator for the constructor pattern,
whichs generate a destructor that specifies how to check the the
data (`PREDICATE-FORM`) and how to access the data (`ACCESSOR-FORMS`).

    (defmethod constructor-pattern-make-destructor ((pattern my-cons-pattern) var)
      (make-destructor :predicate-form `(consp ,var)
                       :accessor-forms (list `(car ,var) `(cdr ,var))))

Finally, define a parser and an unparser for the constructor pattern.

    (defmethod parse-constructor-pattern ((name (eql 'my-cons)) &rest args)
      (apply #'make-my-cons-pattern (mapcar #'parse-pattern args)))
    
    (defmethod unparse-pattern ((pattern my-cons-pattern))
      `(cons ,(unparse-pattern (my-cons-pattern-car-pattern pattern))
             ,(unparse-pattern (my-cons-pattern-cdr-pattern pattern))))

See the source code for more detail.

[Package] optima.core
---------------------

### [Function] %equal

    %equal a b

Equality function for comparing patten constants.

### [Macro] %equals

    %equals var value

Equality macro for comparing pattern constants. This specializes
the comparison form to some specific form as follows:

    (equals x nil)    => (null x)
    (equals x 'foo)   => (eq x 'foo)
    (equals x 123)    => (eql x 123)
    (equals x '(a b)) => (%equals x '(a b))

### [Function] %svref

    %svref simple-vector index

Safe SVREF.

### [Function] %assoc

    %assoc item alist &key (test #'eql)

Safe ASSOC.

### [Function] %get-property

    %get-property item plist

Safe GETF.

### [Class] destructor

### [Type] destructor

    destructor

### [Function] destructor-accessor-forms

    destructor-accessor-forms instance

Return whether debug-block represents elsewhere code.

### [Function] make-destructor

    make-destructor &key ((bindings dum0) nil) ((predicate-form dum1) nil) ((accessor-forms
                                                                             dum2)
                                                                            nil)

### [Class] variable-pattern

### [Type] variable-pattern

    variable-pattern

### [Function] variable-pattern-name

    variable-pattern-name instance

Return whether debug-block represents elsewhere code.

### [Function] make-variable-pattern

    make-variable-pattern &optional name

### [Class] place-pattern

### [Type] place-pattern

    place-pattern

### [Function] place-pattern-name

    place-pattern-name instance

Return whether debug-block represents elsewhere code.

### [Function] make-place-pattern

    make-place-pattern name

### [Class] constant-pattern

### [Type] constant-pattern

    constant-pattern

### [Function] constant-pattern-value

    constant-pattern-value instance

Return whether debug-block represents elsewhere code.

### [Function] make-constant-pattern

    make-constant-pattern value

### [Class] complex-pattern

### [Type] complex-pattern

    complex-pattern

### [Function] complex-pattern-subpatterns

    complex-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Class] guard-pattern

### [Type] guard-pattern

    guard-pattern

### [Function] guard-pattern-test-form

    guard-pattern-test-form instance

Return whether debug-block represents elsewhere code.

### [Function] guard-pattern-subpattern

    guard-pattern-subpattern pattern

### [Function] make-guard-pattern

    make-guard-pattern subpattern test-form &aux (subpatterns (list subpattern))

### [Class] not-pattern

### [Type] not-pattern

    not-pattern

### [Function] not-pattern-subpattern

    not-pattern-subpattern pattern

### [Function] make-not-pattern

    make-not-pattern subpattern &aux (subpatterns (list subpattern))

### [Class] or-pattern

### [Type] or-pattern

    or-pattern

### [Function] or-pattern-subpatterns

    or-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] make-or-pattern

    make-or-pattern &rest subpatterns

### [Class] and-pattern

### [Type] and-pattern

    and-pattern

### [Function] and-pattern-subpatterns

    and-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] make-and-pattern

    make-and-pattern &rest subpatterns

### [Class] constructor-pattern

### [Type] constructor-pattern

    constructor-pattern

### [Function] constructor-pattern-subpatterns

    constructor-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] constructor-pattern-arity

    constructor-pattern-arity pattern

### [Function] constructor-pattern-destructor-sharable-p

    constructor-pattern-destructor-sharable-p x y

### [Function] constructor-pattern-make-destructor

    constructor-pattern-make-destructor pattern var

### [Class] cons-pattern

### [Type] cons-pattern

    cons-pattern

### [Function] cons-pattern-car-pattern

    cons-pattern-car-pattern pattern

### [Function] cons-pattern-cdr-pattern

    cons-pattern-cdr-pattern pattern

### [Function] make-cons-pattern

    make-cons-pattern car-pattern cdr-pattern &aux (subpatterns
                                                    (list car-pattern cdr-pattern))

### [Class] assoc-pattern

### [Type] assoc-pattern

    assoc-pattern

### [Function] assoc-pattern-item

    assoc-pattern-item instance

Return whether debug-block represents elsewhere code.

### [Function] assoc-pattern-key

    assoc-pattern-key instance

Return whether debug-block represents elsewhere code.

### [Function] assoc-pattern-test

    assoc-pattern-test instance

Return whether debug-block represents elsewhere code.

### [Function] assoc-pattern-value-pattern

    assoc-pattern-value-pattern pattern

### [Function] make-assoc-pattern

    make-assoc-pattern item value-pattern &key (key nil) (test nil) &aux (subpatterns
                                                                          (list
                                                                           value-pattern))

### [Class] property-pattern

### [Type] property-pattern

    property-pattern

### [Function] property-pattern-item

    property-pattern-item instance

Return whether debug-block represents elsewhere code.

### [Function] property-pattern-value-pattern

    property-pattern-value-pattern pattern

### [Function] make-property-pattern

    make-property-pattern item value-pattern &aux (subpatterns (list value-pattern))

### [Class] vector-pattern

### [Type] vector-pattern

    vector-pattern

### [Function] vector-pattern-subpatterns

    vector-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] make-vector-pattern

    make-vector-pattern &rest subpatterns

### [Class] simple-vector-pattern

### [Type] simple-vector-pattern

    simple-vector-pattern

### [Function] simple-vector-pattern-subpatterns

    simple-vector-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] make-simple-vector-pattern

    make-simple-vector-pattern &rest subpatterns

### [Class] class-pattern

### [Type] class-pattern

    class-pattern

### [Function] class-pattern-subpatterns

    class-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] class-pattern-class-name

    class-pattern-class-name instance

Return whether debug-block represents elsewhere code.

### [Function] class-pattern-slot-names

    class-pattern-slot-names instance

Return whether debug-block represents elsewhere code.

### [Function] make-class-pattern

    make-class-pattern class-name &rest slot-specs

### [Class] structure-pattern

### [Type] structure-pattern

    structure-pattern

### [Function] structure-pattern-subpatterns

    structure-pattern-subpatterns instance

Return whether debug-block represents elsewhere code.

### [Function] structure-pattern-conc-name

    structure-pattern-conc-name instance

Return whether debug-block represents elsewhere code.

### [Function] structure-pattern-slot-names

    structure-pattern-slot-names instance

Return whether debug-block represents elsewhere code.

### [Function] make-structure-pattern

    make-structure-pattern conc-name &rest slot-specs

### [Function] pattern-variables

    pattern-variables pattern

Returns the set of variables in PATTERN. If PATTERN is not linear,
an error will be raised.

### [Function] place-pattern-included-p

    place-pattern-included-p pattern

### [Function] check-patterns

    check-patterns patterns

Check if PATTERNS are valid. Otherwise, an error will be raised.

### [Function] lift-guard-patterns

    lift-guard-patterns pattern

### [Function] pattern-expand-function

    pattern-expand-function name

### [Function] pattern-expand-1

    pattern-expand-1 pattern

### [Function] pattern-expand

    pattern-expand pattern

### [Function] pattern-expand-all

    pattern-expand-all pattern

### [Function] parse-pattern

    parse-pattern pattern

### [Function] parse-constructor-pattern

    parse-constructor-pattern name &rest args

### [Function] unparse-pattern

    unparse-pattern pattern

[Package] optima
----------------

### [Macro] match

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

### [Macro] multiple-value-match

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

### [Macro] ematch

    ematch arg &body clauses

Same as MATCH, except MATCH-ERROR will be raised if not matched.

### [Macro] multiple-value-ematch

    multiple-value-ematch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except MATCH-ERROR will be raised if
not matched.

### [Macro] cmatch

    cmatch arg &body clauses

Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched.

### [Macro] multiple-value-cmatch

    multiple-value-cmatch values-form &body clauses

Same as MULTIPLE-VALUE-MATCH, except continuable MATCH-ERROR will
be raised if not matched.

### [Macro] fail

    fail

Causes the latest pattern matching be failed and continue to do the
rest of pattern matching.

### [Class] match-error

### [Type] match-error

    match-error

### [Function] match-error-values

    match-error-values condition

### [Function] match-error-patterns

    match-error-patterns condition

### [Macro] defpattern

    defpattern name lambda-list &body body

Defines a derived pattern specifier named NAME. This is analogous
to DEFTYPE.

Examples:

    ;; Defines a LIST pattern.
    (defpattern list (&rest args)
      (when args
        `(cons ,(car args) (list ,@(cdr args)))))

[Package] optima.extra
----------------------


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

    (match '(:name "John" :age 23)
      ((plist :name "John" :age age) age))
    => 23

### [Macro] if-match

    if-match pattern arg &body (then &optional else)

Equivalent to (match ARG (PATTERN THEN) (otherwise ELSE)).

### [Macro] when-match

    when-match pattern arg &body body

Equivalent to (match ARG (PATTERN BODY...)).

### [Macro] unless-match

    unless-match pattern arg &body body

Equivalent to (match ARG (PATTERN) (otherwise BODY...)).

### [Macro] let-match

    let-match bindings &body body

Similar to LET, except not only a variable but also a pattern can
be used in BINDINGS.

### [Macro] let-match*

    let-match* bindings &body body

Similar to LET-MATCH but matches sequentially.

### [Macro] let-match1

    let-match1 pattern arg &body body

Equivalent to (let-match ((PATTERN ARG)) BODY...).

### [Macro] lambda-match

    lambda-match &body clauses

Equivalent to (lambda (arg) (match arg CLAUSES...)).

### [Macro] lambda-ematch

    lambda-ematch &body clauses

Equivalent to (lambda (arg) (ematch arg CLAUSES...)).

### [Macro] lambda-cmatch

    lambda-cmatch &body clauses

Equivalent to (lambda (arg) (cmatch arg CLAUSES...)).

### [Macro] lambda-match1

    lambda-match1 pattern &body body

Equivalent to (lambda-match (PATTERN BODY...)).

### [Macro] lambda-ematch1

    lambda-ematch1 pattern &body body

Equivalent to (lambda-ematch (PATTERN BODY...)).

### [Macro] lambda-cmatch1

    lambda-cmatch1 pattern &body body

Equivalent to (lambda-cmatch (PATTERN BODY...)).

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL

optima.ppcre - CL-PPCRE support for optima
==========================================

[Package] optima.ppcre
----------------------


### [Pattern] ppcre

Syntax:

    (ppcre REGEXP PATTERN*)

Matches REGEXP against the target string. Sub-PATTERNs will be used to
match the matched groups, if REGEXP matched.

Examples:

    (match "2012-11-04"
      ((ppcre "^(\\d{4})-(\\d{2})-(\\d{2})$" year month day)
       (list year month day)))
    => ("2012" "11" "04")

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL
