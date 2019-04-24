
(defpackage :trivia.cffi.test
  (:use :cl :fiveam :alexandria
        :trivia.level2
        :trivia.cffi))
(in-package :trivia.cffi.test)

(in-suite :trivia.cffi)

(defmacro is-match (arg pattern)
  `(is-true (match ,arg (,pattern t))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

;;; Contrib tests

(test (cffi :compile-at :run-time)
  (finishes
    (pattern-expand
     `(-> (:struct dd-node)
        type                             ; foreign-slot-value access (leaf pattern)
        (& type)                         ; explicit foreign-slot-pointer access
        (type type)                      ; implicit foreign-slot-pointer access (nested pattern)
        (type                            ; implicit foreign-slot-pointer access (nested pattern)
         (-> (:union dd-node/type)
           value))))))


;; well, below does not actually compile because it rebinds multiple patterns to a same variable
;; (match (cudd-regular node) ; returns a pointer
;;   ((-> (:struct dd-node)
;;      type                             ; foreign-slot-value access (leaf pattern)
;;      (& type)                         ; explicit foreign-slot-pointer access
;;      (type type)                      ; implicit foreign-slot-pointer access (nested pattern)
;;      (type                            ; implicit foreign-slot-pointer access (nested pattern)
;;       (-> (:union dd-node/type)
;;         value)))                      ; leaf node: value access
;;    value))
