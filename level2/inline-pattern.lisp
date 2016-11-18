(in-package :trivia.level2.impl)

;; see impl.lisp:

(defpattern-inline @ (&rest patterns)
    "Expands into itself."
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


;; (match* (array LU)
;;   (((array (element   k     k     ckk-a)  (displaced k     (k N) ck*-a)
;;            (displaced (k N) k     c*k-a)  (displaced (k N) (k N) c**-a))
;;     (array (element   k     k     ckk-lu) (displaced k     (k N) ck*-lu)
;;            (displaced (k N) k     c*k-lu) (displaced (k N) (k N) c**-lu)))
;;    (array-setf c*k-lu (map 'vector (lambda (cjk) (/ cjk ckk)) c*k-a))
;;    (array-setf c**-lu (matrix-sub c**-a (elementwise-prod ck*-a c*k-lu)))))

