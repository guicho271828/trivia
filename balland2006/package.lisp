#|
  This file is a part of trivia.balland2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivia.balland2006
  (:use :cl :trivia :trivia.level1 :alexandria :type-i :iterate :trivia.skip)
  (:export
   #:apply-fusion
   #:fusiblep
   #:fuse
   #:apply-interleaving
   #:interleave
   #:apply-swapping
   #:swappable
   #:pattern-dependent
   #:pattern-dependencies
   #:*trace-optimization*
   #:*optimization-timeout*))
(in-package :trivia.balland2006)

;; blah blah blah.
