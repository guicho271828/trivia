(in-package :optima)

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

(defun compile-match-fail (fail form else)
  (let ((fail-count (count-occurrences form fail)))
    (cond
      ((or (eq else fail)
           (zerop fail-count))
       form)
      ((or (literalp else)
           (= fail-count 1))
       (subst else fail form :test #'equal))
      (t
       (let ((block (gensym "MATCH"))
             (tag (gensym "MATCH-FAIL")))
         `(block ,block
            (tagbody
               (return-from ,block
                 ,(subst `(go ,tag) fail form))
               ,tag
               (return-from ,block ,else))))))))

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

(defun compile-match-symbol-macro-group (vars clauses else)
  (compile-match
   (cdr vars)
   (loop for ((pattern . rest) . then) in clauses
         for name = (symbol-macro-pattern-name pattern)
         collect
         (if name
             `(,rest (symbol-macrolet ((,name ,(car vars))) . ,then))
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
           (new-vars (make-gensym-list arity))
           (then (compile-match
                  (append new-vars (cdr vars))
                  (loop for ((pattern . rest) . then) in clauses
                        for args = (constructor-pattern-arguments pattern)
                        collect `((,@args . ,rest) . ,then))
                  else)))
      (loop for i from 0 below arity
            for new-var in new-vars
            for access = (funcall accessor var i)
            for binding = `(,new-var ,access)
            if (loop for ((pattern . rest) . then) in clauses
                     for arg = (nth i (constructor-pattern-arguments pattern))
                     never (pattern-symbol-macro-included-p arg))
              collect binding into let-bindings
            else
              collect binding into symbol-macro-bindings
            finally
               (when symbol-macro-bindings
                 (setq then `(symbol-macrolet ,symbol-macro-bindings
                               (declare (ignorable ,@(mapcar #'car symbol-macro-bindings)))
                               ,then)))
               (when let-bindings
                 (setq then `(let ,let-bindings
                               (declare (ignorable ,@(mapcar #'car let-bindings)))
                               ,then)))
               (return
                 `(if ,test-form
                      ,then
                      ,else))))))

(defun compile-match-guard-group (vars clauses else)
  (assert (= (length clauses) 1))
  (destructuring-bind ((pattern . rest) . then)
      (first clauses)
    (let ((then `(if ,(guard-pattern-test-form pattern)
                     ,(compile-clause-body then)
                     ,else)))
      (compile-match vars `((,rest ,then)) else))))

(defun compile-match-or-group (vars clauses else)
  (assert (= (length clauses) 1))
  (destructuring-bind ((pattern . rest) . then)
      (first clauses)
    (let ((patterns (or-pattern-sub-patterns pattern)))
      (unless patterns
        (return-from compile-match-or-group else))
      (let ((new-vars (pattern-variables (car patterns))))
        (unless (loop for pattern in (cdr patterns)
                      for vars = (pattern-variables pattern)
                      always (set-equal new-vars vars))
          (error "Or-pattern must share same set of variables."))
        (let* ((block (gensym "MATCH"))
               (tag (gensym "MATCH-FAIL"))
               (fail `(go ,tag)))
          `(block ,block
             (tagbody 
                (return-from ,block
                  (multiple-value-bind ,new-vars
                      ,(compile-match-1
                        (first vars)
                        (loop for pattern in patterns
                              collect `(,pattern (values ,@new-vars)))
                        fail)
                    ,(compile-match
                      (cdr vars)
                      `((,rest . ,then))
                      fail)))
                ,tag
                (return-from ,block ,else))))))))

(defun compile-match-and-group (vars clauses else)
  (assert (= (length clauses) 1))
  (destructuring-bind ((pattern . rest) . then)
      (first clauses)
    (let ((patterns (and-pattern-sub-patterns pattern)))
      (unless patterns
        (return-from compile-match-and-group else))
      (compile-match
       (append (make-list (length patterns) :initial-element (first vars))
               (cdr vars))
       ;; Reverse patterns here so that the pattern matching is
       ;; executed in order of the patterns. This is important
       ;; especially for guard patterns.
       `(((,@(reverse patterns) . ,rest) . ,then))
       else))))

(defun compile-match-not-group (vars clauses else)
  (assert (= (length clauses) 1))
  (destructuring-bind ((pattern . rest) . then)
      (first clauses)
    (let ((pattern (not-pattern-sub-pattern pattern)))
      (compile-match-1
       (first vars)
       `((,pattern ,else))
       (compile-match
        (cdr vars)
        `((,rest . ,then))
        else)))))

(defun compile-match-empty-group (clauses else)
  (loop for (pattern . then) in clauses
        if (null pattern)
          do (return (compile-clause-body then))
        finally (return else)))

(defun compile-match-group (vars group else)
  (let ((fail (gensym "FAIL")))
    (compile-match-fail
     fail
     (aif (and vars (caaar group))
          (etypecase it
            (variable-pattern
             (compile-match-variable-group vars group fail))
            (symbol-macro-pattern
             (compile-match-symbol-macro-group vars group fail))
            (constant-pattern
             (compile-match-constant-group vars group fail))
            (constructor-pattern
             (compile-match-constructor-group vars group fail))
            (guard-pattern
             (compile-match-guard-group vars group fail))
            (not-pattern
             (compile-match-not-group vars group fail))
            (or-pattern
             (compile-match-or-group vars group fail))
            (and-pattern
             (compile-match-and-group vars group fail)))
          (compile-match-empty-group group fail))
     else)))

(defun compile-match-groups (vars groups else)
  (reduce (lambda (group else) (compile-match-group vars group else))
          groups
          :initial-value else
          :from-end t))

(defun group-match-clauses (clauses)
  (flet ((same-group-p (x y)
           (and (eq (type-of x) (type-of y))
                (typecase x
                  (constant-pattern
                   (%equal (constant-pattern-value x)
                           (constant-pattern-value y)))
                  (constructor-pattern
                   (and (eq (constructor-pattern-name x)
                            (constructor-pattern-name y))
                        (= (constructor-pattern-arity x)
                           (constructor-pattern-arity y))))
                  ((or guard-pattern not-pattern or-pattern and-pattern)
                   nil)
                  (otherwise t)))))
    (group clauses :test #'same-group-p :key #'caar)))

(defun compile-match (vars clauses else)
  (let* ((clauses
           (mapcar (lambda (clause)
                     (when (car clause)
                       (destructuring-bind ((pattern . rest) . then) clause
                         ;; Desugar WHEN.
                         (when (and (>= (length then) 2)
                                    (eq (first then) 'when))
                           (setq pattern `(and ,pattern (when ,(second then)))
                                 then (cddr then)))
                         (setq pattern (parse-pattern pattern))
                         (setq clause `((,pattern . ,rest) . ,then))))
                     clause)
                   clauses))
         (groups (group-match-clauses clauses)))
    (compile-match-groups vars groups else)))

(defun compile-match-1 (form clauses else)
  (let ((clauses (mapcar (lambda (c) (cons (list (car c)) (cdr c))) clauses)))
    (if (symbolp form)
        (compile-match (list form) clauses else)
        (once-only (form)
          (compile-match (list form) clauses else)))))

(defun compile-multiple-value-match (values-form clauses else)
  (let* ((arity (loop for (patterns . nil) in clauses
                      maximize (length patterns)))
         (vars (make-gensym-list arity "VAR")))
    `(multiple-value-bind ,vars ,values-form
       ,(compile-match vars clauses else))))
