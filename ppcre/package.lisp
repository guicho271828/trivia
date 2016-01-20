(defpackage :trivia.ppcre
  (:export :ppcre))

(defpackage :trivia.ppcre.impl
  (:use :cl :ppcre :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.level2
        :trivia.ppcre))

(in-package :trivia.ppcre.impl)

;; we may even be able to implement ppcre-specific optimizer, do we?

(defpattern ppcre (regexp-and-options &rest subpatterns)
  (destructuring-bind (regexp &key start end sharedp) (ensure-list regexp-and-options)
    (when (stringp regexp)
      (let ((register-num (count :register (flatten (parse-string regexp)))))
        (unless (= (length subpatterns) register-num)
          (simple-style-warning
           "The number of registers ~a in ~a is different than the # of subpatterns ~a in ~a!"
           register-num regexp (length subpatterns) subpatterns))))
    (with-gensyms (it)
      `(guard1 ,it (stringp ,it)
               (nth-value 1 (scan-to-strings
                             ,regexp ,it
                             ,@(when start `(:start ,start))
                             ,@(when end `(:end ,end))
                             ,@(when sharedp `(:sharedp ,sharedp))))
               ;; soft vector length check
               (simple-vector* ,@subpatterns)))))

(defmacro ppcre ((regexp &key start end sharedp) &rest subpatterns)
  "Accepts both a short form and a long form.
Long  form: ppcre (regexp &key start end sharedp) &rest subpatterns
Short form: ppcre regexp &rest subpatterns
"
  (declare (ignorable regexp start end sharedp subpatterns))
  (warn "stub!"))


