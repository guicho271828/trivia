(defpackage :trivia.quasiquote.test
  (:use :cl :trivia :named-readtables :fiveam))

(in-package :trivia.quasiquote.test)

(in-readtable :fare-quasiquote)

(def-suite :trivia.quasiquote)
(in-suite :trivia.quasiquote)

(test quasiquote
  (finishes
   (match (list 1 2)
     (`(,b ,c)
       (is (= 1 b))
       (is (= 2 c)))))
  (finishes
   (match (list 1 2 3)
     (`(,b ,@c)
       (is (= 1 b))
       (is (equal c (list 2 3)))))))

