(in-package :trivia.level2.impl)

(defpattern λlist-o (&rest patterns)
  (compile-destructuring-pattern-o (parse-lambda-list patterns t)))

#+nil
(let ((lst '(1 2 :x 1 :b 2))
      (big-lst (list* 1 2 :x 1 :b 2 (loop :repeat 100 :collect (intern (symbol-name (gensym)) "KEYWORD"))))
      (tmp 0))
  (time
   (dotimes (i 1000)
     ;;(tmp 1 2 :x 1 :b 2)
     #+nil
     (destructuring-bind (a0 a1 &key x b) lst
       ;;(incf tmp a1)
       t)
     
     (match big-lst
	    ((λlist a0 a1 &key x b) (incf tmp)))
     #+nil     
     (match big-lst
       ((λlist-o a0 a1 &key x b &allow-other-keys) (incf tmp)))))
  tmp)

(defun compile-destructuring-pattern-o (ops &optional default)
  (match ops
    (nil default)
    ((list* (list :whole subpattern) rest)
     `(and ,subpattern ,(compile-destructuring-pattern-o rest)))
    ((list* (list* :atom subpatterns) rest)
     `(list* ,@subpatterns ,(compile-destructuring-pattern-o rest)))
    ((list* (list :optional) rest)
     (compile-destructuring-pattern-o rest))
    ((list* (list* :optional subpattern more-subpatterns) rest)
     (with-gensyms (lst supplied-p-default-sym)
       (destructuring-bind (subpattern &optional default (supplied-p-pattern supplied-p-default-sym supplied-p-pattern-supplied)) subpattern
         `(guard1 (,lst :type list) (listp ,lst)
                  (if ,lst (car ,lst) ,default) ,subpattern
                  ,@(when supplied-p-pattern-supplied
                      `((if ,lst t nil) ,supplied-p-pattern))
                  (cdr ,lst) ,(compile-destructuring-pattern-o `((:optional ,@more-subpatterns) ,@rest))))))
    ((list* (list :rest pattern) rest)
     `(and ,pattern ,(compile-destructuring-pattern-o rest '_)))
    ((list* (list* (and mode (or :keyword (list :keyword-allow-other-keys rem))) subpatterns) rest)
     ;; case 1,2 of the &key forms are already compiled into the 3rd form ; see parse-lambda-list
     `(and (type list)
           ;; sequentially accumulate keys
	   ,(optimized-key-access (if (eq mode :keyword) nil (or rem '_)) subpatterns)
           ;; compile the rest
           ,(compile-destructuring-pattern-o rest '_)))
    ((list (list* :aux subpatterns))
     `(guard1 ,(gensym) t ,@(mapcan #'(lambda (x)
                                        (destructuring-bind (var &optional expr) (ensure-list x)
                                          (assert (typep var 'variable-symbol) nil "invalid lambda list")
                                          `(,expr ,var)))
                                    subpatterns)))))

;;(defun tmp (a0 a1 &key x b) t)
(defun optimized-key-access (remainder-pattern subpatterns)
  ;; NOTE: uses a binary heap (instead) to achieve O(n lg n) speed using a single pass
  (let* ((props (compile-keyword-patterns subpatterns))
	 (skeys (sort (mapcar #'second props) #'string<)))
    (with-gensyms (lst kargs indicator)
      `(guard1 ,lst t
	       (let ((,kargs (make-array ,(length skeys))))
		 (declare (type (simple-array t (*)) ,kargs))
		 ,@(loop :for ii :below (length skeys) :collect `(setf (aref ,kargs ,ii) ',indicator))
		 ,kargs) ,kargs
		 (kargs-parse ',indicator ,lst ,(coerce skeys '(simple-array keyword (*))) ,kargs ,(if (eql remainder-pattern '_) nil t)) (list ,remainder-pattern nil)
		,@(mapcan #'(lambda (x)
			     (destructuring-bind (key subpattern default &optional (supplied-p-pattern nil supplied-p-pattern-suppliedp)) (cdr x)
			       (let ((pos (position (symbol-name key) skeys :test #'string=)))
				 `((if (eql (aref ,kargs ,pos) ',indicator) ,default (aref ,kargs ,pos)) ,subpattern
				   ,@(if supplied-p-pattern-suppliedp
					 `((if (eql (aref ,kargs ,pos) ',indicator) nil t) ,supplied-p-pattern))))))
			  props)))))

(declaim (inline kargs-parse))
(defun kargs-parse (indicator lst heap kargs &optional collect &aux rest)
  ;;keyword
  (declare (type (simple-array keyword (*)) heap)
	   (type (simple-array t (*)) kargs)
	   (type symbol indicator)
	   (optimize (speed 3) (safety 0)))
  (list
   (loop :for (k v . r) :on lst :by #'cddr
      :while (keywordp k)
      :for pos := #+nil(linear-search k 0 (length heap) heap) (binary-search k 0 (length heap) heap)
      :if pos :do (if (eql (aref kargs pos) indicator) (setf (aref kargs pos) v))
      :else :if collect :collect k :and :collect v
      :do (setf rest r))
   rest))

(declaim (inline linear-search))
(defun linear-search (val lb ub vec)
  (declare (type fixnum lb ub)
	   (type keyword val) ;;keyword
	   (type (simple-array keyword (*)) vec)) ;;keyword
  (loop :for jj :of-type fixnum :from lb :below ub
     :if (eq (aref vec jj) val) :do (return jj)))

(declaim (inline binary-search))
(defun binary-search (val lb ub vec)
  (declare (type fixnum lb ub)
	   (type keyword val) ;;keyword
	   (type (simple-array keyword (*)) vec)) ;;keyword
  (cond
    ((or (= lb ub) (string< val (aref vec lb))) nil)
    ((string< (aref vec (1- ub)) val) nil)
    (t (loop :for j :of-type fixnum := (floor (+ lb ub) 2)
	  :repeat #.(ceiling (log array-dimension-limit 2))
	  :do (cond ((eq (aref vec j) val) (return j))
		    ((>= lb (1- ub)) (return nil))
		    (t (if (string< val (aref vec j))
			   (setf ub j)
			   (setf lb (1+ j)))))))))

;;
;; (defun optimized-key-access (remainder-pattern subpatterns)
;;   ;; NOTE: uses a binary heap (instead) to achieve O(n lg n) speed using a single pass
;;   (let* ((props (compile-keyword-patterns subpatterns))
;; 	 (skeys (sort (mapcar #'(lambda (x) (second x)) props) #'string<)))
;;     (with-gensyms (lst kargs indicator)
;;       `(guard1 ,lst t
;; 	       (let ((,kargs (make-array ,(length skeys) :element-type 'keyword)))
;; 		 (declare (type (simple-array t (*)) ,kargs))
;; 		 ,@(loop :for ii :below (length skeys) :collect `(setf (aref ,kargs ,ii) ',indicator))
;; 		 ,kargs) ,kargs
;; 		 (kargs-parse nil #+nil',indicator ,lst ,(let ((ret (make-hash-table :test 'eq)))
;; 							   (map nil #'(lambda (x) (setf (gethash x ret) (hash-table-count ret))) skeys)
;; 							   ret) ,kargs ,(if (eql remainder-pattern '_) nil t)) (list ,remainder-pattern nil)
;; 		,@(mapcan #'(lambda (x)
;; 			     (destructuring-bind (key subpattern default &optional (supplied-p-pattern nil supplied-p-pattern-suppliedp)) (cdr x)
;; 			       (let ((pos (position key skeys)))
;; 				 `((if (eql (aref ,kargs ,pos) ',indicator) ,default (aref ,kargs ,pos)) ,subpattern
;; 				   ,@(if supplied-p-pattern-suppliedp
;; 					 `((if (eql (aref ,kargs ,pos) ',indicator) nil t) ,supplied-p-pattern))))))
;; 			  props)))))
;; ;;(eq :x :x)

;; (defun kargs-parse (indicator lst heap kargs &optional collect &aux rest)
;;   ;;keyword
;;   (declare (type hash-table heap)
;; 	   (type (simple-array t (*)) kargs)
;; 	   (type symbol indicator)
;; 	   (optimize (speed 3) (safety 0)))
;;   (list
;;    (loop :for (k v . r) :on lst :by #'cddr
;;       :while (keywordp k)
;;       :for pos := (gethash k heap) ;;(binary-search (symbol-name k) 0 (length heap) heap)
;;       :if pos :do (if (eql (aref kargs pos) indicator) (setf (aref kargs pos) v))
;;       :else :if collect :collect k :and :collect v
;;       :do (setf rest r))
;;    rest))
