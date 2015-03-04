(defpackage :optima.ppcre
  (:export :ppcre))

(defpackage :optima.ppcre.impl
  (:use :cl :ppcre :alexandria
        :optima.level0
        :optima.level1
        :optima.level2))

(in-package :optima.ppcre.impl)

;; we may even be able to implement ppcre-specific optimizer, do we?

(defpattern ppcre (regexp &rest args)
  )

