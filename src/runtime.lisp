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
  (typecase value
    (null                  `(null ,var))
    (symbol                `(eq ,var ',value))
    ((or number character) `(eql ,var ,value))
    (cons                  `(%equal ,var ',value))
    (t                     `(%equal ,var ,value))))

(defun %assoc (item alist &key (key #'identity) (test #'eql))
  "Safe ASSOC."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (loop
    (unless (consp alist) (return))
    (let ((cons (car alist)))
      (when (and (consp cons)
                 (funcall test item (funcall key (car cons))))
        (return cons)))
    (setq alist (cdr alist))))

(defun %passoc (item plist)
  "Safe plist assoc."
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (loop
    (unless (consp plist) (return))
    (let ((cons (cdr plist)))
      (unless (consp cons) (return))
      (when (eql item (car plist))
        (return plist))
      (setq plist (cdr cons)))))
