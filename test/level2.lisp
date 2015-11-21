
(defpackage :trivia.level2.test
  (:use :cl :fiveam :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.level2))
(in-package :trivia.level2.test)

(def-suite :trivia.level2)
(in-suite :trivia.level2)


(test defpattern
  (finishes (print (pattern-expand '(cons a b)))))

(test pad
  (is (= 1 (match* nil (() 1) (() 1)))))

(test destructuring-key
  (is (and
       (equal '(a b c) (match '(a b :c c) ((λlist a b &key (c :cd)) (list a b c))))
       (equal '(a b :cd) (match '(a b) ((λlist a b &key (c :cd)) (list a b c))))
       (equal nil (match '(a b :c) ((λlist a b &key (c :cd)) (list a b c)))))))

(test destructuring-opt-key
  (is (and (equal
	    '(a b c (:c c :d d) d)
	    (match '(a b c :c c :d d)
	      ((λlist a b &optional c &rest rr &key (d -1) &allow-other-keys)
	       (list a b c rr d))))
	   (equal
	    nil
	    (match '(a b :c c :d d)
	      ((λlist a b &optional c &rest rr &key (d -1) &allow-other-keys)
	       (list a b c rr d)))))))

(eval-when (:load-toplevel :execute)
  (run! :trivia.level2))

