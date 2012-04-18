(in-package :fivepm)

(defmacro xmatch ((the type arg) &body clauses)
  "Same as MATCH, except XMATCH does exhaustiveness analysis over
CLAUSES with a type of ARG. If the type is not covered by CLAUSES, in
other words, if the type is not a subtype of an union type of CLAUSES,
then a compile-time error will be raised.

You need to specify the type of ARG with THE special operator like:

    (xmatch (the type arg) ...)

Examples:

    (xmatch (the (member :a :b) :b) (:a 1) (:b 2) (:c 3))
    => 2
    (xmatch (the (member :a :b) :b) (:a 1))
    => COPMILE-TIME-ERROR"
  (unless (eq the 'the) (error "~S must be ~S" the 'the))
  (let ((union-type
          `(or ,@(loop for clause in clauses
                       for pattern = (parse-pattern (car clause))
                       for guarded = (or (eq (second clause) 'when)
                                         (pattern-guarded-p pattern))
                       unless guarded
                         collect (type-of-pattern (parse-pattern pattern))))))
    (if (subtypep type union-type)
        (once-only (arg)
          `(match ,arg . ,clauses))
        (error "Non exhaustive pattern matching for ~S.~@
                   Subtype relation ~S <: ~S is not satisfied."
               arg type union-type))))
