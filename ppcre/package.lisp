(defpackage :trivia.ppcre
  (:import-from :cl-ppcre :split)
  (:export :ppcre
           :split
           :split*))

(defpackage :trivia.ppcre.impl
  (:use :cl :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.level2
        :trivia.ppcre))

(in-package :trivia.ppcre.impl)

;; we may even be able to implement ppcre-specific optimizer, do we?

(defpattern ppcre (regexp-and-options &rest subpatterns)
  "Accepts both a short form and a long form.
Long  form: ppcre (regexp &key start end sharedp) &rest subpatterns
Short form: ppcre regexp &rest subpatterns
"
  (destructuring-bind (regexp &key start end sharedp) (ensure-list regexp-and-options)
    (typecase regexp
      (string
       (let ((register-num (count :register (flatten (ppcre:parse-string regexp)))))
         (unless (= (length subpatterns) register-num)
           (simple-style-warning
            "The number of registers ~a in ~a is different than the # of subpatterns ~a in ~a!"
            register-num regexp (length subpatterns) subpatterns))))
      (t
       (simple-style-warning
        "The given regexp is not a literal, and it prevents compile-time optimization")))
    (with-gensyms (it)
      `(guard1 (,it :type string) (stringp ,it)
               (nth-value 1 (ppcre:scan-to-strings
                             ,regexp ,it
                             ,@(when start `(:start ,start))
                             ,@(when end `(:end ,end))
                             ,@(when sharedp `(:sharedp ,sharedp))))
               ;; soft vector length check
               (simple-vector* ,@subpatterns)))))

(defmacro ppcre ((regexp &key start end sharedp) &rest subpatterns)
  "A dummy macro for PPCRE pattern to be recognized in eldoc"
  (declare (ignorable regexp start end sharedp subpatterns))
  (warn "stub!"))

(defpattern split (regex &rest subpatterns)
  "Treat the string as a list of strings, split by the strings matching to REGEX.
The list which is a result of applying PPCRE:SPLIT to the string is matched against list pattern."
  (with-gensyms (it)
    `(guard1 (,it :type string) (stringp ,it)
             (ppcre:split ,regex ,it)
             (list ,@subpatterns))))

(defpattern split* (regex &rest subpatterns)
  "Soft-match variants of SPLIT: the number of elements do not have to match the number of subpatterns.
The list which is a result of applying PPCRE:SPLIT to the string is matched against list* pattern."
  (with-gensyms (it)
    `(guard1 (,it :type string) (stringp ,it)
             (ppcre:split ,regex ,it)
             (list* ,@subpatterns))))
