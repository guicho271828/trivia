(in-package :fivepm)

(defmacro with-match-fail (form else)
  (flet ((literalp (value)
           (typep value '(or symbol number character string))))
    (cond
      ((literalp else)
       `(macrolet ((match-fail () ',else))
          ,form))
      ((equal else '(match-fail))
       form)
      (t
       (let ((block (gensym "MATCH"))
             (tag (gensym "MATCH-FAIL")))
         `(block ,block
            (tagbody
               (return-from ,block
                 (macrolet ((match-fail () '(go ,tag)))
                   ,form))
               ,tag
               (return-from ,block ,else))))))))

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
  `(%match ,(cdr vars)
           ,(loop for ((pattern . rest) . then) in clauses
                  for name = (variable-pattern-name pattern)
                  collect
                  (if name
                      `(,rest (let ((,name ,(car vars))) . ,then))
                      `(,rest . ,then)))
           ,else))

(defun compile-match-constant-group (vars clauses else)
  `(with-match-fail
       (if ,(with-slots (value) (caaar clauses)
              `(equals ,(car vars) ,value))
           (%match ,(cdr vars)
                   ,(loop for ((nil . rest) . then) in clauses
                          collect `(,rest . ,then))
                   (match-fail))
           (match-fail))
     ,else))

(defun compile-match-constructor-group (vars clauses else)
  (with-slots (arity arguments predicate accessor) (caaar clauses)
    (let* ((var (car vars))
           (test-form (funcall predicate var))
           (new-vars (make-gensym-list arity)))
      `(with-match-fail
           (if ,test-form
               (let ,(loop for i from 0
                           for new-var in new-vars
                           for access = (funcall accessor var i)
                           collect `(,new-var ,access))
                 (declare (ignorable ,@new-vars))
                 (%match (,@new-vars . ,(cdr vars))
                         ,(loop for ((pattern . rest) . then) in clauses
                                for args = (constructor-pattern-arguments pattern)
                                collect `((,@args . ,rest) . ,then))
                         (match-fail)))
               (match-fail))
         ,else))))

(defun compile-match-guard (vars clause else)
  (destructuring-bind ((pattern . rest) . then) clause
    (with-slots (pattern test-form) pattern
      `(with-match-fail
           (%match (,(car vars))
                   (((,pattern)
                     (if ,test-form
                         (%match ,(cdr vars)
                                 ((,rest . ,then))
                                 (match-fail))
                         (match-fail))))
                   (match-fail))
         ,else))))

(defun compile-match-guard-group (vars clauses else)
  (reduce (lambda (clause else) (compile-match-guard vars clause else))
          clauses
          :initial-value else
          :from-end t))

(defun compile-match-empty-group (clauses else)
  (loop for (pattern . then) in clauses
        if (null pattern)
          do (return (compile-clause-body then))
        finally (return else)))

(defun compile-match-group (vars group else)
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

(defun compile-match-groups (vars groups else)
  (reduce (lambda (group else) (compile-match-group vars group else))
          groups
          :initial-value else
          :from-end t))

(defgeneric same-group-p (pattern1 pattern2)
  (:method (pattern1 pattern2) t))

(defmethod same-group-p ((pattern1 constant-pattern) (pattern2 constant-pattern))
  (%equal (constant-pattern-value pattern1)
          (constant-pattern-value pattern2)))

(defmethod same-group-p ((pattern1 constructor-pattern) (pattern2 constructor-pattern))
  (and (eq (constructor-pattern-name pattern1)
           (constructor-pattern-name pattern2))
       (= (constructor-pattern-arity pattern1)
          (constructor-pattern-arity pattern2))))

(defun group-match-clauses (clauses)
  (group clauses :test #'same-group-p :key #'caar))

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

(defmacro %match (vars clauses else)
  (let* ((clauses (mapcar #'parse-match-clause clauses))
         (groups (group-match-clauses clauses)))
    (compile-match-groups vars groups else)))

(defmacro %match-1 (var clauses else)
  `(%match (,var)
           ,(loop for (pattern . then) in clauses
                  collect `((,pattern) . ,then))
           ,else))
