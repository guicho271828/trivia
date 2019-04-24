
(defpackage :trivia.ppcre.test
  (:use :cl :fiveam :alexandria
        :trivia.level2
        :trivia.ppcre))
(in-package :trivia.ppcre.test)

(in-suite :trivia.ppcre)

(defmacro is-match (arg pattern)
  `(is-true (match ,arg (,pattern t))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

;;; Contrib tests

(test (ppcre :compile-at :run-time)
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
                (list year month day)))))
  (is (equal "1"
             (let ((url-regexp "^/user/(\\w+)/$"))
               (match "/user/1/"
                 ((ppcre url-regexp user-id) user-id))))))


