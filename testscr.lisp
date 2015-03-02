
(in-package :cl-user)

(defun test (sys)
  (handler-case
      (progn
        (ql:quickload sys)
        (asdf:test-system sys)
        (fiveam:run sys))
    (serious-condition (c)
      (describe c)
      (uiop:quit 2))))

(uiop:quit (if (every #'fiveam::TEST-PASSED-P
                       (append (test :optima.level0)
                               (test :optima.level1)
                               (test :optima.level2)))
               0 1))
