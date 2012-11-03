(in-package :optima)

(defmacro try (form else)
  "Tries to evaluate FORM. If a form (FAIL) found in FORM, then go to
ELSE branch for evaluation."
  (let ((block (gensym "MATCH-TRY"))
        (tag (gensym "MATCH-FAIL")))
    `(block ,block
       (tagbody
          (return-from ,block
            (macrolet ((fail () `(go ,',tag)))
              ,form))
          ,tag
          (return-from ,block ,else)))))

(defmacro iff (test then else)
  "Similar to IF except IFF also allows to go to ELSE branch from THEN
branch with (FAIL) form."
  `(try (if ,test ,then (fail)) ,else))

(defmacro fail ()
  "Causes the latest pattern matching be failed and continue to do the
rest of pattern matching."
  (error "Not pattern matching."))
