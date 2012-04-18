fivepm - Very Fast Pattern Matching Library
===========================================

fivepm is a very fast pattern matching library
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
                        | constructor-pattern
                        | guard-pattern
    
    constant-pattern ::= t | nil
                       | atom-except-symbol
                       | (quote VALUE)
    
    variable-pattern ::= SYMBOL
    
    constructor-pattern ::= (NAME PATTERN*)
    
    guard-pattern ::= (guard PATTERN TEST-FORM)

### Constant Pattern

A constant pattern matches the constant itself.

Examples:

    (match 1 (1 2)) => 2
    (match "foo" ("foo" "bar")) => "bar"
    (match '(1) ('(1) 2)) => 2

### Variable Pattern

A variable pattern matches any value and bind the value to the
variable. _ is a special variable pattern which matches any value but
doesn't bind.

Examples:

    (match 1 (x x)) => 1
    (match 1 (_ 2)) => 2

### Constructor Pattern

A constructor pattern matches not a value itself but a structure of
the value. The following constructors are available:

* (cons car cdr)
* (vector &rest elements)
* (simple-vector &rest elements)

Examples:

    (match '(1 . 2) ((cons a b) (+ a b))) => 3
    (match #(1 2) ((simple-vector a b) (+ a b))) => 3

LIST is not a constructor pattern but a dervied pattern. If we see the
following pattern specifier

    (list a b c)

then we expand it into

    (cons a (cons b (cons c nil)))

See DEFPATTERN for more detail.

In addition to constructor patterns above, there is one special
constructor pattern which matches any value of type of STANDARD-CLASS.
The form of the pattern looks like

    (class-name &rest slots)

where CLASS-NAME is a class name of the value, and SLOTS is a list of
the form of (SLOT-NAME PATTERN). You can also specify the element like
SLOT-NAME, which is a shorthand for (SLOT-NAME SLOT-NAME).

Examples:

    (defstruct person name age)
    (defvar foo (make-person :name "foo" :age 30))
    (match foo
      ((person name age) (list name age)))
    => ("foo" 30)
    (match foo
      ((person (name "bar")) 'matched)
      (_ 'not-matched))
    => NOT-MATCHED

### Guard Pattern

A guard pattern restricts a matching of PATTERN with a post condition
TEST-FORM. Note that a symbol GUARD is not (yet) exported. See also
MATCH documentation.

Examples:

    (match 1 ((fivepm::guard x (evenp x)) 'even))
    => NIL

[Package] fivepm
----------------

## [Macro] defpattern

    defpattern name lambda-list &body body

Defines a derived pattern specifier named NAME. This is analogous
to DEFTYPE.

Examples:

    ;; Defines a LIST pattern.
    (defpattern list (&rest args)
      (when args
        `(cons ,(car args) (list ,@(cdr args)))))

## [Macro] match

    match arg &body clauses

Matches ARG with CLAUSES. CLAUSES is a list of the form of (PATTERN
. BODY) where PATTERN is a pattern specifier and BODY is an implicit
progn. If ARG is matched with some PATTERN, then evaluates
corresponding BODY and returns the evaluated value. Otherwise, returns
NIL.

If BODY starts with a symbol WHEN, then the next form will be used to
introduce a guard for PATTERN. That is,

    (match list ((list x) when (oddp x) x))

will be translated to

    (match list ((guard (list x) (oddp x)) x))

## [Macro] ematch

    ematch arg &body clauses

Same as MATCH, except MATCH-ERROR will be raised if not matched.

## [Macro] cmatch

    cmatch arg &body clauses

Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched.

## [Macro] xmatch

    xmatch arg &body clauses

Same as MATCH, except XMATCH does exhaustiveness analysis over
CLAUSES with a type of ARG. If the type is not covered by CLAUSES, in
other words, if the type is not a subtype of an union type of CLAUSES,
then a compile-time error will be raised.

You need to specify the type of ARG with THE special operator like:

    (xmatch (the type arg) ...)

Authors
-------

* Tomohiro Matsuyama

License
-------

LLGPL
