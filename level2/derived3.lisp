(in-package :trivia.level2.impl)

(deftype variable-symbol () `(and symbol (not (member ,@lambda-list-keywords)) (not keyword) (not boolean)))
;;Borrowed from Matlisp
(defmacro recurse-maadi (x match &rest dispatchers)
  ;;recurse-ಮಾಡಿ ಸಕ್ಕತ್ತಾಗಿ!
  (assert (eql (first match) :match) nil "invalid dispatch name")
  (let ((macros (mapcar #'(lambda (x) (list* (the (and keyword (not (member :and :or :* :not :.))) (car x))
					     (gensym "dispatch") (cdr x))) (list* match dispatchers))))
    (labels ((recurse (p)
	       (cond
		 ((and (listp p) (member (car p) (list* :and :or :* :not :. (mapcar #'car (cdr macros)))))
		  (case (car p)
		    (:and `(and ,@(mapcar #'recurse (cdr p))))
		    (:or `(or ,@(mapcar #'recurse (cdr p))))
		    ((:* :not) (destructuring-bind (term clause) p
				 `(not ,(if (eql term :*)
					    `(do () ((not ,(recurse clause))))
					    (recurse clause)))))
		    (:. `(locally ,@(cdr p)))
		    (t `(,(second (assoc (car p) macros)) ,@(cdr p)))))
		 (t `(,(second (assoc :match macros)) ,p)))))
      `(macrolet (,@(mapcar #'cdr macros)) ,(recurse x)))))

(defun getf! (place indicator &optional checkp indicatorp)
  (declare (type keyword indicator)
	   (type list place))
  (let ((lst+ (cons :head place)))
    (do ((x (cdr lst+) (cddr x))
	 (x- lst+ (cdr x)))
	((or (not x)
	     (and (not checkp)		  
		  (progn
		    (assert (and (consp (cdr x)) (keywordp (first x))) nil "invalid property list.")
		    (eql (first x) indicator))))
	 (when x ;;move matching to the head
	   (setf (cdr x-) (cddr x)
		 (cddr x) (cdr lst+)
		 (cdr lst+) x)
	   (if (not indicatorp) x
	       (list* t x)))))))

(defun parse-lambda-list (pattern &aux (ptn pattern) compiler accum)
  (if (recurse-maadi
       (:and
	;;&whole
	(:or (:and '&whole (:. (push `(:whole ,(second ptn)) compiler)) (:pop 2))
	     (:and))
	;;args
	(:. (setf accum nil) t)
	(:* (:and (:. (consp ptn)) (:not (:λkey)) (:. (push (first ptn) accum)) (:pop)))
	(:. (push `(:atom ,@(reverse accum)) compiler) t)
	;;&optional       
	(:or (:not (:. (listp ptn)))
	     (:and '&optional (:pop)
		   (:. (setf accum nil) t)
		   (:* (:and (:. (consp ptn)) (:not (:λkey))
			     (:. (push (ensure-list (first ptn)) accum)) (:pop)))
		   (:. (push `(:optional ,@(reverse accum)) compiler) t))
	     (:and))
	;;&rest
	(:or (:and (:. (not (listp ptn))) (:. (push `(:rest ,ptn) compiler) (setq ptn nil) t))
	     (:and (:or '&rest '&body) (:. (push `(:rest ,(second ptn)) compiler)) (:pop 2))
	     (:and))
	;;&key
	(:or (:and '&key (:pop)
		   (:. (setf accum nil) t)
		   (:* (:and (:. (consp ptn)) (:not (:λkey))
			     (:. (push (ensure-list (first ptn)) accum)) (:pop)))
		   ;;&allow-other-keys
		   (:or (:and '&allow-other-keys (:pop) (:. (push :more-keywords accum)))
			(:and))
		   (:. (push `(:keyword ,@(reverse accum)) compiler) t))
	     (:and))
	;;&aux
	(:or (:and '&aux (:pop)
		   (:. (setf accum nil) t)
		   (:* (:and (:. (consp ptn)) (:. (push (first ptn) accum)) (:pop)))
		   (:. (push `(:aux ,@(reverse accum)) compiler)))
	     (:and))
	(:. (null ptn)))
       ;;
       (:match (x) `(eql (car ptn) ,x))
       (:λkey () `(member (car ptn) cl:lambda-list-keywords))
       (:pop (&optional (n 1)) `(progn ,@(loop :repeat n :collect `(pop ptn)))))
      (reverse compiler)))

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
	     (destructuring-bind (var &optional default (key nil keyp) &aux (lst (gensym))) (car tail)
	       (assert (and (typep var 'variable-symbol) (or (not keyp) (typep key 'variable-symbol))) nil "invalid lambda list")
	       `(trivia:<> (list* ,@(if keyp `(,key)) ,var ,(compile-destructuring-pattern (list* (list* :optional (cdr tail)) (cdr ops))))
			   (or (and ,lst ,@(if keyp `((cons t ,lst)))) (list ,@(if keyp `(nil)) ,default)) ,lst))
	     (compile-destructuring-pattern (cdr ops))))
	  (:rest
	   (let ((var (second head)))
	     (assert (typep var 'variable-symbol) nil "invalid lambda list")
	     `(trivia:<> ,(compile-destructuring-pattern (cdr ops) '_) ,var ,var)))
	  (:keyword
	   (with-gensyms (lst)
	     `(and (type list) (trivia:<> ,(compile-destructuring-pattern (list* (list* :keyword-processing (cdr head)) (cdr ops))) (copy-list ,lst) ,lst))))
	  (:keyword-processing
	   (if-let ((tail (cdr head)))
	     (if (eql (car tail) :more-keywords)
		 (with-gensyms (lst)
		   `(and (guard ,lst (getf! ,lst nil t nil)) ,(compile-destructuring-pattern (cdr ops) '_)))
		 (destructuring-bind (var &optional default (key nil keyp) &aux (lst (gensym))) (car tail)
		   (assert (and (typep var 'variable-symbol) (or (not keyp) (typep key 'variable-symbol))) nil "invalid lambda list")
		   (let ((varkey (intern (symbol-name var) :keyword)))
		     `(trivia:<> (list* ,@(if keyp `(,key)) ,varkey ,var ,(compile-destructuring-pattern (list* (list* :keyword-processing (cdr tail)) (cdr ops))))
				 (or (getf! ,lst ,varkey nil ,(if keyp t nil)) (list* ,@(if keyp `(nil)) ,varkey ,default ,lst)) ,lst))))
	     (compile-destructuring-pattern (cdr ops))))
	  (:aux
	   `(and ,default ,@(mapcar #'(lambda (x)
					(destructuring-bind (var &optional expr) (ensure-list x)
					  (assert (typep var 'variable-symbol) nil "invalid lambda list")
					  `(trivia:<> ,var ,expr)))
				    (cdr head))))))))

;(compile-destructuring-pattern (parse-lambda-list '(a . b)))

(defpattern lambda-list (&rest pattern)
  (compile-destructuring-pattern (parse-lambda-list pattern)))

(defpattern λlist (&rest pattern)
  (compile-destructuring-pattern (parse-lambda-list pattern)))

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
