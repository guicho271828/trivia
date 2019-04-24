#|
  This file is a part of trivia.balland2006 project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(defsystem trivia.balland2006.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system for trivia.balland2006.enabled"
  :license "LLGPL"
  :depends-on (:trivia.balland2006 :trivia.level2.test)
  :in-order-to ((test-op (test-op "trivia.level2.test"))))
