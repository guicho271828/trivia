(in-package :optima.extra)

;;; Extra patterns

(defpattern alist (&rest args)
  `(and ,@(loop for (key . value) in args
                collect `(assoc ,key ,value))))

(defpattern plist (&rest args)
  `(and ,@(loop for (key . value) in (plist-alist args)
                collect `(property ,key ,value))))

;;; Extra macros

(defmacro if-match (pattern arg &body (then &optional else))
  "Equivalent to (match ARG (PATTERN THEN) (otherwise ELSE))."
  `(match ,arg
     (,pattern ,then)
     (otherwise ,else)))

(defmacro when-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN BODY...))."
  `(match ,arg (,pattern ,.body)))

(defmacro unless-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN) (otherwise BODY...))."
  `(match ,arg
     (,pattern)
     (otherwise ,.body)))

(defmacro let-match (bindings &body body)
  "Similar to LET, except not only a variable but also a pattern can
be used in BINDINGS."
  `(optima::ematch* ,(mapcar #'cadr bindings)
     (,(mapcar #'car bindings) ,.body)))

(defmacro let-match* (bindings &body body)
  "Similar to LET-MATCH but matches sequentially."
  (reduce (lambda (binding form) `(let-match (,binding) ,form))
          bindings
          :from-end t
          :initial-value `(locally ,.body)))

(defmacro let-match1 (pattern arg &body body)
  "Equivalent to (let-match ((PATTERN ARG)) BODY...)."
  `(let-match ((,pattern ,arg)) ,.body))

(defmacro lambda-match (&body clauses)
  "Equivalent to (lambda (arg) (match arg CLAUSES...))."
  (with-unique-names (arg)
    `(lambda (,arg) (match ,arg ,@clauses))))

(defmacro lambda-ematch (&body clauses)
  "Equivalent to (lambda (arg) (ematch arg CLAUSES...))."
  (with-unique-names (arg)
    `(lambda (,arg) (ematch ,arg ,@clauses))))

(defmacro lambda-cmatch (&body clauses)
  "Equivalent to (lambda (arg) (cmatch arg CLAUSES...))."
  (with-unique-names (arg)
    `(lambda (,arg) (cmatch ,arg ,@clauses))))

(defmacro lambda-match1 (pattern &body body)
  "Equivalent to (lambda-match (PATTERN BODY...))."
  `(lambda-match (,pattern ,@body)))

(defmacro lambda-ematch1 (pattern &body body)
  "Equivalent to (lambda-ematch (PATTERN BODY...))."
  `(lambda-ematch (,pattern ,@body)))

(defmacro lambda-cmatch1 (pattern &body body)
  "Equivalent to (lambda-cmatch (PATTERN BODY...))."
  `(lambda-cmatch (,pattern ,@body)))
