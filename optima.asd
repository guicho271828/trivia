(asdf:defsystem :optima
  :description "Optimized Pattern Matching Library"
  :long-description "optima is a very fast pattern matching library
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
                        | or-pattern
    
    constant-pattern ::= t | nil
                       | atom-except-symbol
                       | (quote VALUE)
    
    variable-pattern ::= SYMBOL
    
    constructor-pattern ::= (NAME PATTERN*)
    
    guard-pattern ::= (guard PATTERN TEST-FORM)
    
    or-pattern ::= (or PATTERN*)

### Constant Pattern

A constant pattern matches the constant itself.

Examples:

    (match 1 (1 2)) => 2
    (match \"foo\" (\"foo\" \"bar\")) => \"bar\"
    (match '(1) ('(1) 2)) => 2

### Variable Pattern

A variable pattern matches any value and bind the value to the
variable. _ is a special variable pattern (a.k.a wildcard pattern)
which matches any value but doesn't bind.

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
    (defvar foo (make-person :name \"foo\" :age 30))
    (match foo
      ((person name age) (list name age)))
    => (\"foo\" 30)
    (match foo
      ((person (name \"bar\")) 'matched)
      (_ 'not-matched))
    => NOT-MATCHED

### Guard Pattern

A guard pattern restricts a matching of PATTERN with a post condition
TEST-FORM. See also MATCH documentation.

Examples:

    (match 1 ((guard x (evenp x)) 'even))
    => NIL

### Or Pattern

An or pattern matches a value that is matched with one of
PATTERNs. There is a restriction that every pattern of PATTERNs must
have same set of variables.

Examples:

    (match '(2 . 1) ((or (cons 1 x) (cons 2 x)) x))
    => 1"
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria
               :anaphora
               :closer-mop)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "equal")
                             (:file "pattern")
                             (:file "compiler")
                             (:file "match")
                             (:file "xmatch")))))
