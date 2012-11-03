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
                        | symbol-pattern
                        | constructor-pattern
                        | derived-pattern
                        | guard-pattern
                        | not-pattern
                        | or-pattern
                        | and-pattern
    
    constant-pattern ::= t | nil
                       | atom-except-symbol
                       | (quote VALUE)
    
    variable-pattern ::= SYMBOL | (variable SYMBOL)
    
    symbol-pattern ::= (symbol SYMBOL) | (symbol-macrolet SYMBOL)
    
    constructor-pattern ::= (NAME ARG*)
    
    derived-pattern ::= (NAME PATTERN*)
    
    guard-pattern ::= (when TEST-FORM) | (unless TEST-FORM)
    
    not-pattern ::= (not PATTERN)
    
    or-pattern ::= (or PATTERN*)
    
    and-pattern ::= (and PATTERN*)

### Constant-Pattern

A constant-pattern matches the constant itself.

Examples:

    (match 1 (1 2)) => 2
    (match \"foo\" (\"foo\" \"bar\")) => \"bar\"
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

### Symbol-Pattern

A symbol-pattern matches any value as variable-patterns but bind the
value with SYMBOL-MACROLET.

Examples:

    (defvar c (cons 1 2))
    (match c ((cons (symbol x) y) (incf x) (incf y)))
    c
     => (2 . 2)

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
    (defvar foo (make-person :name \"foo\" :age 30))
    (match foo
      ((person name age) (list name age)))
    => (\"foo\" 30)

#### STRUCTURE

Matches any structure value, and its slot values.

Syntax:

    structure-constructor-pattern ::= (structure CONC-NAME slot*)
                                    | (CONC-NAME slot*)

    slot ::= SLOT-NAME
           | (SLOT-NAME PATTERN*)

As well as CLASS constructor-pattern, STRUCTURE can be
omitted. CONC-NAME is a prefix string of a predicate (CONC-NAME +
\"p\") and accessors (CONC-NAME + SLOT-NAME). For example, if we have
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
    (match (make-person :name \"foo\" :age 30)
      ((p- name age) (list name age)))
    => (\"foo\" 30)

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

    (satisfies evenp) => (when (evenp *))

#### EQ, EQL, EQUAL, EQUALP

Expansion of EQ, EQL, EQUAL, EQUALP derived patterns:

    (eq 'foo) => (satisfies eq 'foo)
    (eql 123) => (satisfies eql 123)
    (equal '(1 2)) => (satisfies equal '(1 2))
    (equalp \"foo\") => (satisfies equalp \"foo\")

#### TYPEP

Expansion of TYPEP derived patterns:

    (TYPEP string) => (satisfies typep 'string)

### Guard-Pattern

A guard-pattern is a special pattern that tests TEST-FORM satisfies in
the current matching context. A special symbol * in the predicate form
refers to the value being matched. See the examples below.

Examples:

    (match 1 ((when (evenp *)) 'even))
    => NIL
    (match 1 ((unless (evenp *)) 'even))
    => EVEN

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
                             (:file "extra")
                             (:file "fail")
                             (:file "compiler")
                             (:file "match")
                             (:file "macros")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :optima))))
  (asdf:load-system :optima-test)
  (eval (read-from-string "(eos:run! 'optima-test::optima-test)"))
  t)

(asdf:defsystem :optima-test
  :depends-on (:optima :eos)
  :components ((:file "test/suite")))
