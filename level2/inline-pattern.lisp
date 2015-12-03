(in-package :trivia.level2.impl)

;; see impl.lisp:

(defpattern-inline @ (&rest patterns)
    ;; identity function
    patterns)

;; (define-condition inline-pattern (simple-error)
    ;; identity function
;;   ((patterns :initarg :patterns :accessor patterns))
;;   (:report (lambda (c s)
;;              (format s "Toplevel inline pattern is invalid: ~a" (patterns c)))))

;; (cerror 'inline-pattern :patterns patterns)


(defpattern-inline @@ (n &rest patterns)
    "inline N successive identical patterns"
  (assert (integerp n))
  (mappend (lambda (i)
             (declare (ignore i))
             (copy-list patterns))
           (iota n)))

