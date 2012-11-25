(in-package :optima)

(defun self-evaluating-object-p (object)
  (typep object '(and (not cons) (or (not symbol) (member t nil)))))

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

(defmacro once-only* (symbol &body body)
  (with-unique-names (form var vars bindings)
    `(let (,vars ,bindings)
       (dolist (,form ,symbol)
         (if (symbolp ,form)
             (push ,form ,vars)
             (let ((,var (gensym "VAR")))
               (push ,var ,vars)
               (push (list ,var ,form) ,bindings))))
       (setq ,vars (nreverse ,vars)
             ,bindings (nreverse ,bindings))
       (if ,bindings
           `(let ,,bindings
              ;(declare (ignorable ,@(mapcar #'car ,bindings)))
              ,(let ((,symbol ,vars))
                 ,@body))
           (progn ,@body)))))
