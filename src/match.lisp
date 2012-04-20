(in-package :fivepm)

(defmacro match (arg &body clauses)
  "Matches ARG with CLAUSES. CLAUSES is a list of the form of (PATTERN
. BODY) where PATTERN is a pattern specifier and BODY is an implicit
progn. If ARG is matched with some PATTERN, then evaluates
corresponding BODY and returns the evaluated value. Otherwise, returns
NIL.

If BODY starts with a symbol WHEN, then the next form will be used to
introduce a guard for PATTERN. That is,

    (match list ((list x) when (oddp x) x))

will be translated to

    (match list ((guard (list x) (oddp x)) x))"
  (compile-match-1 arg clauses nil))

(defmacro match-values (values-form &body clauses)
  "Matches the multiple values of VALUES-FORM with CLAUSES. Unlike
MATCH, CLAUSES have to have the form of (PATTERNS . BODY), where
PATTERNS is a list of patterns. The number of values that will be used
to match is determined by the maximum arity of PATTERNS among CLAUSES.

Examples:

    (match-values (values 1 2)
     ((2) 1)
     ((1 y) y))
    => 2"
  (compile-match-values values-form clauses nil))

(defmacro ematch (arg &body clauses)
  "Same as MATCH, except MATCH-ERROR will be raised if not matched."
  (compile-ematch-1 arg clauses))

(defmacro ematch-values (values-form &body clauses)
  "Same as MATCH-VALUES, except MATCH-ERROR will be raised if not
matched."
  (compile-ematch-values values-form clauses))

(defmacro cmatch (arg &body clauses)
  "Same as MATCH, except continuable MATCH-ERROR will be raised if not
matched."
  (compile-cmatch-1 arg clauses))

(defmacro cmatch-values (values-form &body clauses)
  "Same as MATCH-VALUES, except continuable MATCH-ERROR will be raised
if not matched."
  (compile-cmatch-values values-form clauses))
