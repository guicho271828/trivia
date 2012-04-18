(in-package :fivepm)

(defun compile-clause-body (body)
  (cond ((null body)
         nil)
        ((and (consp (first body))
              (eq (car (first body)) 'declare))
         `(locally . ,body))
        ((= (length body) 1)
         (first body))
        (t
         `(progn . ,body))))

(defun compile-match-variable-group (vars clauses else)
  (compile-match
   (cdr vars)
   (loop for ((pattern . rest) . then) in clauses
         for name = (variable-pattern-name pattern)
         collect
         (if name
             `(,rest (let ((,name ,(car vars))) . ,then))
             `(,rest . ,then)))
   else))

(defun compile-match-constant-group (vars clauses else)
  `(if ,(with-slots (value) (caaar clauses)
          `(equals ,(car vars) ,value))
       ,(compile-match
         (cdr vars)
         (loop for ((nil . rest) . then) in clauses
               collect `(,rest . ,then))
         else)
       ,else))

(defun compile-match-constructor-group (vars clauses else)
  (with-slots (arity arguments predicate accessor) (caaar clauses)
    (let* ((var (car vars))
           (test-form (funcall predicate var))
           (new-vars (make-gensym-list arity)))
      `(if ,test-form
           (let ,(loop for i from 0
                       for new-var in new-vars
                       for access = (funcall accessor var i)
                       collect `(,new-var ,access))
             (declare (ignorable ,@new-vars))
             ,(compile-match
               (append new-vars (cdr vars))
               (loop for ((pattern . rest) . then) in clauses
                     for args = (constructor-pattern-arguments pattern)
                     collect `((,@args . ,rest) . ,then))
               else))
           ,else))))

(defun compile-match-guard-group (vars clauses else)
  (assert (= (length clauses) 1))
  (let ((clause (first clauses)))
    (destructuring-bind ((pattern . rest) . then) clause
      (with-slots (pattern test-form) pattern
        (compile-match
         (list (car vars))
         `(((,pattern)
            (if ,test-form
                ,(compile-match
                  (cdr vars)
                  `((,rest . ,then))
                  else)
                ,else)))
         else)))))

(defun compile-match-empty-group (clauses else)
  (loop for (pattern . then) in clauses
        if (null pattern)
          do (return (compile-clause-body then))
        finally (return else)))

(defun compile-match-group-1 (vars group else)
  (if vars
      (etypecase (caaar group)
        (variable-pattern
         (compile-match-variable-group vars group else))
        (constant-pattern
         (compile-match-constant-group vars group else))
        (constructor-pattern
         (compile-match-constructor-group vars group else))
        (guard-pattern
         (compile-match-guard-group vars group else)))
      (compile-match-empty-group group else)))

(defun compile-match-group (vars group else)
  (let* ((fail '(match-fail))
         (compiled (compile-match-group-1 vars group fail))
         (fail-count (count-occurrences compiled fail :test #'equal)))
    (cond
      ((or (literalp else)
           (= fail-count 1))
       (subst else fail compiled :test #'equal))
      ((or (equal else fail)
           (zerop fail-count))
       compiled)
      (t
       (let ((block (gensym "MATCH"))
             (tag (gensym "MATCH-FAIL")))
         `(block ,block
            (tagbody
               (return-from ,block
                 ,(subst `(go ,tag) fail compiled :test #'equal))
               ,tag
               (return-from ,block ,else))))))))

(defun compile-match-groups (vars groups else)
  (reduce (lambda (group else) (compile-match-group vars group else))
          groups
          :initial-value else
          :from-end t))

(defun group-match-clauses (clauses)
  (flet ((same-group-p (x y)
           (and (eq (type-of x) (type-of y))
                (cond ((constant-pattern-p x)
                       (%equal (constant-pattern-value x)
                               (constant-pattern-value y)))
                      ((constructor-pattern-p x)
                       (and (eq (constructor-pattern-name x)
                                (constructor-pattern-name y))
                            (= (constructor-pattern-arity x)
                               (constructor-pattern-arity y))))
                      ((guard-pattern-p x)
                       ;; Never group guard patterns.
                       nil)
                      (t t)))))
    (group clauses :test #'same-group-p :key #'caar)))

(defun desugar-match-clause (clause)
  (if (car clause)
      (destructuring-bind ((pattern . rest) . then) clause
        (if (and (>= (length then) 2)
                 (eq (first then) 'when))
            (let* ((test (second then))
                   (then (cddr then)))
              `(((guard ,pattern ,test) . ,rest) . ,then))
            `((,pattern . ,rest) . ,then)))
      clause))

(defun parse-match-clause (clause)
  (if (car clause)
      (destructuring-bind ((pattern . rest) . then)
          (desugar-match-clause clause)
        (let ((pattern (parse-pattern pattern)))
          `((,pattern . ,rest) . ,then)))
      clause))

(defun compile-match (vars clauses else)
  (let* ((clauses (mapcar #'parse-match-clause clauses))
         (groups (group-match-clauses clauses)))
    (compile-match-groups vars groups else)))

(defun compile-match-1 (var clauses else)
  (compile-match
   (list var)
   (mapcar (lambda (clause) (cons (list (car clause)) (cdr clause))) clauses)
   else))
