(defpackage :optima.ppcre
  (:export :ppcre))

(defpackage :optima.ppcre.impl
  (:use :cl :ppcre :alexandria
        :optima.level0
        :optima.level1
        :optima.level2
        :optima.ppcre))

(in-package :optima.ppcre.impl)

;; we may even be able to implement ppcre-specific optimizer, do we?

(defpattern ppcre (regexp-and-options &rest subpatterns)
  (destructuring-bind (regexp &key start end sharedp)
      (ensure-list regexp-and-options)
    (let ((register-num (count :register (flatten (parse-string regexp)))))
      (if (= (length subpatterns) register-num)
          (with-gensyms (it)
            `(guard1 ,it (stringp ,it)
                     (nth-value 1 (scan-to-strings
                                   ,regexp ,it
                                   ,@(when start `(:start ,start))
                                   ,@(when end `(:end ,end))
                                   ,@(when sharedp `(:sharedp ,sharedp))))
                     (simple-vector ,@subpatterns)))
          (with-gensyms (it)
            (simple-style-warning
             "The number of registers ~a in ~a is different than the # of subpatterns ~a in ~a!"
             register-num regexp (length subpatterns) subpatterns)
            `(guard1 ,it (stringp ,it)
                     (nth-value 1 (scan-to-strings
                                   ,regexp ,it
                                   ,@(when start `(:start ,start))
                                   ,@(when end `(:end ,end))
                                   ,@(when sharedp `(:sharedp ,sharedp))))
                     ;; soft vector length check
                     (simple-vector* ,@subpatterns)))))))
