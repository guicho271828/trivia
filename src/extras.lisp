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
