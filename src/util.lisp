(in-package :fivepm)

(defun span (list &key (test #'eql) (key #'identity))
  (loop with item = (funcall key (first list))
        for (x . rest) on list
        if (funcall test item (funcall key x))
          collect x into span
        else
          return (values span (cons x rest))
        finally (return (values span nil))))

(defun group (list &key (test #'eql) (key #'identity))
  (loop with span
        while list
        do (multiple-value-setq (span list) (span list :test test :key key))
        collect span))
