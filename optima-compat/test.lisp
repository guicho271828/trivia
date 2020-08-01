
(defpackage trivia.optima-compat.test
  (:use :cl))

(in-package :trivia.optima-compat.test)

(defvar *who-depends-on*
  '("check-it"
    "cl-environments"
    "cl-htmlprag"
    "cl-iterative"
    "cl-mock"
    "cl-quil"
    "cl-vhdl-tests"
    "clj"
    "datafly"
    
    "erlang-term-optima"
    "fact-base"
    "fare-quasiquote-optima"
    "hash-set"
    "house"
    "hu.dwim.def.test"
    "inferior-shell"
    
    "minilem"
    "mito-core"
    "quux-hunchentoot"
    "spatial-trees.nns"
    
    "spatial-trees.nns.test"
    "sxql"
    "taglib"
    "transparent-wrap"
    "varjo.import"
    "weblocks"
    
    "weblocks-util"
    "zenekindarl"
    ))

(defparameter *errors* nil)

(progn
  (setf *errors* nil)
  (dolist (system *who-depends-on*)
    (handler-case
        (ql:quickload system)
      (error (c)
        (let ((pair (cons system c)))
          (print pair)
          (push pair *errors*)))))
  (print *errors*))


