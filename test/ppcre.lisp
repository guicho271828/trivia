
(defpackage :optima.ppcre.test
  (:use :cl :fiveam :alexandria
        :optima.level2
        :optima.ppcre))
(in-package :optima.ppcre.test)

(def-suite :optima.ppcre)
(in-suite :optima.ppcre)

(defmacro is-match (arg pattern)
  `(is-true (match ,arg (,pattern t))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

;;; Contrib tests

(test ppcre
  (is-match "a" (ppcre "^a$"))
  (is-not-match "a" (ppcre "^b$"))
  (is-not-match 1 (ppcre "a"))
  (is-not-match :A (ppcre "A"))
  (is-true (match "a"
             ((ppcre "^(.)$")
              t)))
  (is (equal '("a" nil)
             (match "a"
               ((ppcre "(a)" x y)
                (list x y)))))
  (is (equal '("2012" "11" "04")
             (match "2012-11-04"
               ((ppcre "^(\\d+)-(\\d+)-(\\d+)$" year month day)
                (list year month day))))))

(eval-when (:load-toplevel :execute)
  (run! :optima.ppcre))

