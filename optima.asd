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
    (match '((\"a\" . 123))
      ((assoc \"A\" 123 :test #'string-equal) t))
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
    (defvar foo (make-person :name \"foo\" :age 30))
    (match foo
      ((person name age) (list name age)))
    => (\"foo\" 30)

You can also use MAKE-INSTANCE style pattern syntax like:

    (match foo
      ((person :name name :age age) (list name age)))
    => (\"foo\" 30)

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
    (equalp \"foo\") => (guard it (equalp it \"foo\"))

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

See the source code for more detail."
  :version "1.0"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria
               :closer-mop)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "util")
                             (:file "runtime")
                             (:file "pattern")
                             (:file "fail")
                             (:file "compiler")
                             (:file "match")
                             (:file "extra")))))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (asdf:find-system :optima))))
  (asdf:load-system :optima.test)
  (eval (read-from-string "(eos:run! 'optima.test::optima-test)"))
  t)
