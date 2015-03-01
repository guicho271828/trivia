(defpackage :optima.level2
  (:export :match))

(defpackage :optima.level2.impl
  (:use :cl
        :alexandria
        :optima.level0
        :optima.level1)
  (:shadowing-import-from
   :optima.level0
   :match))

(in-package :optima.level2.impl)

