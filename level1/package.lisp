;;; level1 implementation

(defpackage :trivia.level1
  (:export :match1 :or1 :guard1 :variables
           :*or-pattern-allow-unshared-variables*
           :*lexvars*
           :or1-pattern-inconsistency
           :guard1-pattern-nonlinear
           :conflicts :pattern :repair-pattern
           :correct-pattern
           :preprocess-symopts
           :*trace-dispatching*
           :trace-when))

(defpackage :trivia.fail (:export :fail))
(defpackage :trivia.skip (:export :skip))
(defpackage :trivia.next (:export :next))

(defpackage :trivia.level1.impl
  (:use :cl
        :alexandria
        :trivia.level0
        :trivia.level1
        :trivia.fail
        :trivia.skip
        :trivia.next))

