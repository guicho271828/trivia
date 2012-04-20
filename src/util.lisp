(in-package :optima)

(defun literalp (value)
  (typep value '(or symbol number character)))

(defun count-occurrences (tree subtree &key (test #'eql))
  (let ((count (if (funcall test tree subtree) 1 0)))
    (if (consp tree)
        (+ count
           (count-occurrences (car tree) subtree :test test)
           (count-occurrences (cdr tree) subtree :test test))
        count)))

(defun set-equal (set1 set2)
  (and (null (set-difference set1 set2))
       (null (set-difference set2 set1))))

(defun span (list &key (test #'eql) (key #'identity))
  (loop with item = (funcall key (first list))
        for (x . rest) on list
        if (funcall test item (funcall key x))
          collect x into span
        else
          return (if span
                     (values span (cons x rest))
                     (values (list x) rest))
        finally (return (values span nil))))

(defun group (list &key (test #'eql) (key #'identity))
  (loop with span
        while list
        do (multiple-value-setq (span list) (span list :test test :key key))
        collect span))
