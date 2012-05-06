(in-package :optima)

(defmacro if-match (pattern arg then &optional else)
  "Equivalent to (match ARG (PATTERN THEN) (otherwise ELSE))."
  `(match ,arg
     (,pattern ,then)
     (otherwise ,else)))

(defmacro if-smatch (pattern arg then &optional else)
  "Equivalent to (smatch ARG (PATTERN THEN) (otherwise ELSE))."
  `(smatch ,arg
     (,pattern ,then)
     (otherwise ,else)))

(defmacro when-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN BODY...))."
  `(match ,arg (,pattern ,@body)))

(defmacro when-smatch (pattern arg &body body)
  "Equivalent to (smatch ARG (PATTERN BODY...))."
  `(smatch ,arg (,pattern ,@body)))

(defmacro unless-match (pattern arg &body body)
  "Equivalent to (match ARG (PATTERN) (otherwise BODY...))."
  `(match ,arg
     (,pattern)
     (otherwise ,@body)))

(defmacro with-match (pattern arg &body body)
  "Equivalent to (ematch ARG (PATTERN BODY...))."
  `(ematch ,arg (,pattern ,@body)))
