(in-package :optima)

(defmacro %or (&rest forms)
  "Causes the latest pattern matching to fail.  After this failure, matching
continues at the next pattern."
  (setq forms (remove '(fail) forms :test #'equal))
  (cond ((null forms)
         '(fail))
        ((null (rest forms))
         (first forms))
        ((self-evaluating-object-p (first forms))
         (first forms))
        (t
         (let ((block (gensym "BLOCK")))
           `(block ,block
              (tagbody
                 ,@(loop for form in (butlast forms)
                         for tag = (gensym "FAIL")
                         ;; FIXME: ITER doesn't support MACROLET in
                         ;; iteration body.  So we use SYMBOL-MACROLET
                         ;; instead.
                         ;collect `(return-from ,block
                         ;           (macrolet ((fail () `(go ,',tag)))
                         ;             ,form))
                         collect `(return-from ,block
                                    (symbol-macrolet ((%fail (go ,tag)))
                                      ,form))
                         collect tag)
                 (return-from ,block ,(car (last forms)))))))))

(defmacro %if (test then else)
  "Similar to IF except %IF also allows to call (FAIL) in THEN branch
to jump to ELSE branch."
  `(%or (if ,test ,then (fail)) ,else))

(defmacro fail ()
  "Causes the latest pattern matching be failed and continue to do the
rest of pattern matching."
  ;; FIXME: Don't raise an error but expand to %FAIL for the hack
  ;; above.
  ;(error "Not pattern matching.")
  '%fail)

(define-symbol-macro %fail (error "Not pattern matching."))
