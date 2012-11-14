(in-package :optima)

(defmacro %or (&rest forms)
  "Similar to OR except %OR also allows to call (FAIL) in each branch
to jump to its next branch."
  (setq forms (remove '(fail) forms :test #'equal))
  (cond ((null forms) '(fail))
        ((null (rest forms)) (first forms))
        (t
         (let* ((block (gensym "BLOCK"))
                (else (car (last forms)))
                (forms (butlast forms))
                (tags (make-gensym-list (length forms) "TAG"))
                (else-tag (car (last tags))))
           `(block ,block
              (tagbody
                 ,@(loop for form in forms
                         for tag in tags
                         collect
                         `(return-from ,block
                            (macrolet ((fail (&optional all)
                                         (if all
                                             `(go ,',else-tag)
                                             `(go ,',tag))))
                              ,form))
                         collect tag)
                 (return-from ,block ,else)))))))

(defmacro %if (test then else)
  "Similar to IF except %IF also allows to call (FAIL) in THEN branch
to jump to ELSE branch."
  `(%or (if ,test ,then (fail)) ,else))

(defmacro fail (&optional all)
  "If ALL is true, this causes the all pattern matching be failed. If
ALL is false, this causes the latest pattern matching be failed and
continue to do the rest of pattern matching."
  (declare (ignore all))
  (error "Not pattern matching."))
