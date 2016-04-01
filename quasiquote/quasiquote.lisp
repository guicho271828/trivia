(defpackage :trivia.quasiquote.impl
  (:use :cl :fare-quasiquote :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.level2)
  (:shadowing-import-from :fare-quasiquote :list :list* :cons :quote))

(in-package :trivia.quasiquote.impl)


(defpattern list  (&rest r) `(cl:list  ,@r))
(defpattern list* (&rest r) `(cl:list* ,@r))
(defpattern cons  (&rest r) `(cl:cons  ,@r))
(defpattern quote (&rest r) `(cl:quote ,@r))

(defpattern quasiquote (x)
  (quasiquote-expand x))

