
(in-package :trivia.test)
(def-suite :trivia.level2 :in :trivia)
(in-suite :trivia.level2)

(test defpattern
  (finishes (print (pattern-expand '(cons a b)))))

(test pad
  (is (= 1 (match* nil (() 1) (() 1)))))

(defun testmatcher (list)
  (match list ((λlist a b &key (c -1)) (list a b c))))

(test destructuring-key
  (is (equal '(1 2 3)   (testmatcher '(1 2 :c 3))))
  (is (equal '(1 2 -1)  (testmatcher '(1 2))))
  (is (equal nil        (testmatcher '(1 2 :c)))))

(defun testmatcher2 (list)
  (match list
    ((λlist a b &optional c &rest rr &key (d -1) &allow-other-keys)
     (list a b c rr d))))

(test destructuring-opt-key
  (is (equal '(1 2 3 (:c 4 :d 5) 5)
             (testmatcher2 '(1 2 3 :c 4 :d 5))))
  (is (equal nil
             ;; because &optional c consumes one value, it becomes an invalid plist
             (testmatcher2 '(1 2 :c 3 :d 4)))))


