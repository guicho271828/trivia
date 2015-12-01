(in-package :trivia.level2.impl)

;; FIXME --- this is hard to understand! Why not using trivia itself to implement this?

(deftype variable-symbol () `(and symbol (not (member ,@lambda-list-keywords)) (not keyword) (not boolean)))
;;Borrowed from Matlisp

(declaim (inline getf!))
(defun getf! (place indicator &optional checkp)
  (declare (type keyword indicator)
	   (type list place))
  (let ((lst+ (cons :head place)))
    (do ((x (cdr lst+) (cddr x))
	 (x- lst+ (cdr x)))
	((or (not (and x (consp (cdr x)) (keywordp (first x))))
	     (and (not checkp) (eql (first x) indicator)))
	 (cond
	   ((not x) (values t (cdr lst+)))
	   ((not (and (consp (cdr x)) (keywordp (first x)))) (values nil (cdr lst+)))
	   (t (setf (cdr x-) (cddr x)
		    (cddr x) (cdr lst+)
		    (cdr lst+) x)
	      (values t (cdr lst+))))))))



(defun take-while (list predicate)
  "
Checks if predicate returns true, for each element of a list.
Collect the elements until it returns false.
Returns the collected elements and the remaining elements.
If the list exhausted by cdr of the list being an atom (not necessarily nil),
then similarly returns the collected elements and the atom, which might be nil in case of regular list,
or otherwise it can be anything (e.g. (take-while '(a . b) (constantly t)) returns (values '(a) 'b)).
"
  (let (acc)
    (loop for sub on list
          do (unless (funcall predicate (car sub))
               (return (values (nreverse acc) sub)))
             (push (car sub) acc)
          finally (return (values (nreverse acc) sub)))))

;; (take-while '(1 3 5 7 8 3) #'evenp)
;; NIL
;; (1 3 5 7 8 3)
;; (take-while '(1 3 5 7 8 3) #'oddp)
;; (1 3 5 7)
;; (8 3)
;; (take-while '(1 3 5 7 3) #'oddp)
;; (1 3 5 7 3)
;; NIL
;; (take-while '(1 3 5 7 . 3) #'oddp)
;; (1 3 5 7)
;; 3

(defun parse-lambda-list (argv)
  (let (results)
    (labels ((lambda-list-keyword-p (thing) (not (member thing lambda-list-keywords)))
             (parse-whole (argv)
               (match argv
                 ((list* '&whole var rest)
                  (push (list :whole var) results)
                  (parse-required rest))
                 (_
                  (parse-required argv))))
             (parse-required (argv)
               (multiple-value-bind (argv rest) (take-while argv #'lambda-list-keyword-p)
                 (when argv (push `(:atom ,@argv) results))
                 (ematch rest
                   (nil)                ;do nothing
                   ((type atom) (push (list :rest rest) results))
                   ((type cons) (parse-optional rest)))))
             (parse-optional (argv)
               (match argv
                 ((list* '&optional argv)
                  (multiple-value-bind (argv rest) (take-while argv #'lambda-list-keyword-p)
                    (when argv (push `(:optional ,@(mapcar #'ensure-list argv)) results))
                    (ematch rest
                      (nil)                ;do nothing
                      ((type atom) (push (list :rest rest) results))
                      ((type cons) (parse-rest rest)))))
                 (_
                  (parse-rest argv))))
             (parse-rest (argv)
               (match argv
                 ((list* (or '&rest '&body) var rest)
                  (push `(:rest ,var) results)
                  (parse-key rest))
                 (_
                  (parse-key argv))))
             (parse-key (argv)
               (match argv
                 ((list* '&key argv)
                  (multiple-value-bind (argv rest) (take-while argv #'lambda-list-keyword-p)
                    (when argv
                      (match rest
                        ((list* '&allow-other-keys rest2)
                         (push `(:keyword ,@(mapcar #'ensure-list argv) :more-keywords) results)
                         (setf rest rest2))
                        (_
                         (push `(:keyword ,@(mapcar #'ensure-list argv)) results))))
                    (ematch rest
                      (nil)                ;do nothing
                      ((type atom) (push (list :rest rest) results))
                      ((type cons) (parse-aux rest)))))
                 (_
                  (parse-aux argv))))
             (parse-aux (argv)
               (match argv
                 ((list* '&aux argv)
                  (multiple-value-bind (argv rest) (take-while argv #'lambda-list-keyword-p)
                    (when argv (push `(:aux ,@(mapcar #'ensure-list argv)) results))
                    (ematch rest
                      (nil)                ; do nothing
                      ((type atom) (push (list :rest rest) results))
                      ((type cons)))))  ; do nothing
                 (_))))                   ; do nothing
      (parse-whole argv)
      (nreverse results))))

;; (parse-lambda-list '(a . b))         ((:ATOM A) (:REST B))
;; (parse-lambda-list '(a &optional b)) ((:ATOM A) (:OPTIONAL (B)))
;; (parse-lambda-list '(a &optional b x))  ((:ATOM A) (:OPTIONAL (B) (X)))
;; (parse-lambda-list '(a &optional (b 1) x)) ((:ATOM A) (:OPTIONAL (B 1) (X)))
;; (parse-lambda-list '(a &optional (b 1 supplied) x)) ((:ATOM A) (:OPTIONAL (B 1 SUPPLIED) (X)))
;; (parse-lambda-list '(&whole whole a &optional (b 1 supplied) x)) ((:WHOLE WHOLE) (:ATOM A) (:OPTIONAL (B 1 SUPPLIED) (X)))

(defun compile-destructuring-pattern (ops &optional (default '(type null)))
  (if (not ops) default
      (let ((head (first ops)))
	(ecase (first head)
	  (:whole
	   (let ((var (second head)))
	     (assert (typep var 'variable-symbol) nil "invalid lambda list")
	     `(trivia:<> ,(compile-destructuring-pattern (cdr ops)) ,var ,var)))
	  (:atom `(list* ,@(cdr head) ,(compile-destructuring-pattern (cdr ops))))
	  (:optional
	   (if-let ((tail (cdr head)))
	     (let ((guard nil) (optpat (car tail)))
	       (when (eql (car optpat) 'guard)
		 (destructuring-bind (guard-sym pattern predicate &rest more-patterns) optpat
		   (declare (ignorable guard-sym))
		   (setf guard `(t (guard1 ,(gensym) ,predicate ,@more-patterns))
			 optpat (ensure-list pattern))))
	       (destructuring-bind (var &optional default (key nil keyp) &aux (lst (gensym))) optpat
		 (assert (and (typep var 'variable-symbol) (or (not keyp) (typep key 'variable-symbol))) nil "invalid lambda list")
		 `(guard1 (,lst :type list) (listp ,lst) (if ,lst (car ,lst) ,default) ,var ,@(if keyp `((if ,lst t nil) ,key))
			  ,@guard
			  (cdr ,lst) ,(compile-destructuring-pattern (list* (list* :optional (cdr tail)) (cdr ops))))))
	     (compile-destructuring-pattern (cdr ops))))
	  (:rest
	   (let ((var (second head)))
	     (assert (typep var 'variable-symbol) nil "invalid lambda list")
	     `(trivia:<> ,(compile-destructuring-pattern (cdr ops) '_) ,var ,var)))
	  (:keyword
	   (with-gensyms (lst)
	     `(guard1 ,lst (listp ,lst) (copy-list ,lst) ,(compile-destructuring-pattern (list* (list* :keyword-processing (cdr head)) (cdr ops))))))
	  (:keyword-processing
	   (if-let ((tail (cdr head)))
	     (if (eql (car tail) :more-keywords)
		 (with-gensyms (lst)
		   `(guard1 (,lst :type list) (getf! ,lst :more-keywords t) nil ,(compile-destructuring-pattern (cdr ops) '_)))
		 (let ((guard nil) (keypat (car tail)))
		   (when (eql (car keypat) 'guard)
		     (destructuring-bind (guard-sym pattern predicate &rest more-patterns) keypat
		       (declare (ignorable guard-sym))
		       (setf guard `(t (guard1 ,(gensym) ,predicate ,@more-patterns))
			     keypat (ensure-list pattern))))
		   (destructuring-bind (var &optional default (key nil keyp) &aux (varkey (intern (symbol-name var) :keyword))) keypat
		     (assert (and (typep var 'variable-symbol) (or (not keyp) (typep key 'variable-symbol))) nil "invalid lambda list")
		     (with-gensyms (lst plistp newlst found-key?)
		       `(guard1 (,lst :type list) (multiple-value-bind (,plistp ,newlst) (getf! ,lst ',varkey) (when ,plistp (setf ,lst ,newlst) t))
				(eql (first ,lst) ',varkey) ,found-key?
				(if ,found-key? (second ,lst) ,default) ,var ,@(if keyp `((if ,found-key? t nil) ,key))
				,@guard
				(if ,found-key? (cddr ,lst) ,lst) ,(compile-destructuring-pattern (list* (list* :keyword-processing (cdr tail)) (cdr ops))))))))
	     (compile-destructuring-pattern (cdr ops))))
	  (:aux
	   `(and ,default (guard1 ,(gensym) t ,@(mapcan #'(lambda (x)
							    (destructuring-bind (var &optional expr) (ensure-list x)
							      (assert (typep var 'variable-symbol) nil "invalid lambda list")
							      `(,expr ,var)))
							(cdr head)))))))))

;(compile-destructuring-pattern (parse-lambda-list '(a . b)))

(defpattern lambda-list (&rest pattern)
  (compile-destructuring-pattern (or (parse-lambda-list pattern) (error "invalid lambda list"))))

(defpattern λlist (&rest pattern)
  (compile-destructuring-pattern (or (parse-lambda-list pattern) (error "invalid lambda list"))))

#+nil
(trivia:match '(1 (2 3) -1)
  ((λlist a (λlist b c) &optional (x 2)) (list a b c x)))

#+nil
(pattern-expand-1 '(lambda-list a &key c))
#+nil
(trivia:match '(1 :c 2)
  ((lambda-list a &key (c -1) &aux (xx (+ a c))) xx))

#+nil
(defpattern <> (pattern value &optional (var (gensym "BIND")))
  "The current matching value is bound to `var'.
The result of evaluating `value' using `var' is then matched against `pattern'.
`var' can be omitted."
  (assert (symbolp var))
  `(guard1 ,var t ,value ,pattern))
