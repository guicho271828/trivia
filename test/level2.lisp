
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

(eval-when (:load-toplevel :execute)
  (run! :trivia.level2))

