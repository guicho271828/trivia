(in-package :optima)

(defmacro if-match (pattern arg &body (then &optional else))
  "Equivalent to (match ARG (PATTERN THEN) (otherwise ELSE))."
  `(match ,arg
     (,pattern ,then)
     (otherwise ,else)))

(defmacro when-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN BODY...))."
  `(match ,arg (,pattern ,@body)))

(defmacro unless-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN) (otherwise BODY...))."
  `(match ,arg
     (,pattern)
     (otherwise ,@body)))

(defmacro with-match (pattern arg &body body)
  "Equivalent to (ematch ARG (PATTERN BODY...))."
  `(ematch ,arg (,pattern ,@body)))

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

(defmacro defun-match (name &body clauses)
  "Equivalent to (defun NAME (arg) (match arg CLAUSES...))."
  (with-unique-names (arg)
    `(defun ,name (,arg)
       (match ,arg ,@clauses))))

(defmacro defun-ematch (name &body clauses)
  "Equivalent to (defun NAME (arg) (ematch arg CLAUSES...))."
  (with-unique-names (arg)
    `(defun ,name (,arg)
       (ematch ,arg ,@clauses))))

(defmacro defun-cmatch (name &body clauses)
  "Equivalent to (defun NAME (arg) (cmatch arg CLAUSES...))."
  (with-unique-names (arg)
    `(defun ,name (,arg)
       (cmatch ,arg ,@clauses))))

(defmacro defun-match1 (name pattern &body body)
  "Equivalent to `(defun-match name (PATTERN BODY...))."
  `(defun-match ,name (,pattern ,@body)))

(defmacro defun-ematch1 (name pattern &body body)
  "Equivalent to `(defun-ematch name (PATTERN BODY...))."
  `(defun-ematch ,name (,pattern ,@body)))

(defmacro defun-cmatch1 (name pattern &body body)
  "Equivalent to `(defun-cmatch name (PATTERN BODY...))."
  `(defun-cmatch ,name (,pattern ,@body)))
