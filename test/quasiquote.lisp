(defpackage :trivia.quasiquote.test
  (:use :cl :trivia :named-readtables :fiveam))

(in-package :trivia.quasiquote.test)

(in-readtable :fare-quasiquote)

(in-suite :trivia.quasiquote)

(test (quasiquote :compile-at :run-time)
  (finishes
   (ematch (list 1 2)
     (`(,b ,c)
       (is (= 1 b))
       (is (= 2 c)))))
  (finishes
   (ematch (list 1 2 3)
     (`(,b ,@c)
       (is (= 1 b))
       (is (equal c (list 2 3)))))))

