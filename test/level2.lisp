
(defpackage :optima.level2.test
  (:use :cl :fiveam :optima.level2))
(in-package :optima.level2.test)

(def-suite :optima.level2)
(in-suite :optima.level2)



(eval-when (:load-toplevel :execute)
  (run! :optima.level2))

