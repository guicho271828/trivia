
(defpackage :trivia.cffi.test
  (:use :cl :fiveam :alexandria
        :trivia.level2
        :trivia.cffi))
(in-package :trivia.cffi.test)

(def-suite :trivia.cffi)
(in-suite :trivia.cffi)

(defmacro is-match (arg pattern)
  `(is-true (match ,arg (,pattern t))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

;;; Contrib tests

(test cffi
  )


