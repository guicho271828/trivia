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
