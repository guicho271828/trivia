(in-package :optima)

(defun %equal (a b)
  "Equality function for comparing patten constants."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (or (equal a b)
      (cond ((and (stringp a) (stringp b))
             (string= a b))
            ((and (consp a) (consp b))
             (and (%equal (car a) (car b))
                  (%equal (cdr a) (cdr b)))))))

(defmacro equals (var value)
  "Equality macro for comparing pattern constants. This specializes
the comparison form to some specific form as follows:

    (equals x nil)    => (null x)
    (equals x 'foo)   => (eq x 'foo)
    (equals x 123)    => (eql x 123)
    (equals x '(a b)) => (%equals x '(a b))"
  (cond ((null value) `(null ,var))
        ((symbolp value) `(eq ,var ',value))
        ((literalp value) `(eql ,var ,value))
        ((consp value) `(%equal ,var ',value))
        (t `(%equal ,var ,value))))
