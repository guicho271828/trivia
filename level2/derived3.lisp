(in-package :trivia.level2.impl)

;; FIXME --- this is hard to understand! Why not using trivia itself to implement this?

(deftype variable-symbol () `(and symbol (not (member ,@lambda-list-keywords)) (not keyword) (not boolean)))

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
    (loop :for sub :on list
          :do (unless (funcall predicate (car sub))
               (return (values (nreverse acc) sub)))
             (push (car sub) acc)
          :finally (return (values (nreverse acc) sub)))))

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

(defun parse-lambda-list (argv &optional (canonicalp t))
  (let (results)
    (labels ((!lambda-list-keyword-p (thing) (not (member thing lambda-list-keywords)))
             (parse-whole (argv)
               (ematch argv
                 ((list* '&whole var rest)
                  (push (list :whole var) results)
                  (parse-required rest))
                 (_
                  (parse-required argv))))
             (parse-required (argv)
               (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
                 (when argv (push `(:atom ,@argv) results))
                 (ematch rest
                   (nil)                ;do nothing
                   ((type atom) (push (list :rest rest) results))
                   ((type cons) (parse-optional rest)))))
             (parse-optional (argv)
               (ematch argv
                 ((list* '&optional argv)
                  (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
                    (when argv (push `(:optional ,@(mapcar #'ensure-list argv)) results))
                    (ematch rest
                      (nil)                ;do nothing
                      ((type atom) (push (list :rest rest) results))
                      ((type cons) (parse-rest rest)))))
                 (_
                  (parse-rest argv))))
             (parse-rest (argv)
               (ematch argv
                 ((list* (or '&rest '&body) var rest)
                  (push `(:rest ,var) results)
                  (parse-key rest))
                 (_
                  (parse-key argv))))
             (compile-keyword-pattern (keypat)
               ;; destructuring-lambda-list-keyword     := var | ({var | (keyword-name var)}        [init-form [supplied-p-parameter]])
               ;; destructuring-pattern-keyword-pattern := var | ({var | (keyword-name subpattern)} [init-form [supplied-p-parameter]])
               ;; (&key (a 1) b ((c subpattern) 1 supplied-p)) -> (((a a) 1) ((b b)) ((c subpattern) 1 supplied-p))
               (destructuring-bind (var-or-var-subpattern &rest rest) (ensure-list keypat)
                 (ematch var-or-var-subpattern
                   ((or (and (type variable-symbol) var subpattern) ; var == subpattern, both are the same symbol
                        (list (and var (type variable-symbol)) subpattern)) ; ((var subpattern) nil supplied-p)
                    `((,var ,subpattern) ,@rest)))))
	     (parse-key (argv)
	       (ematch argv
		 ((list* '&key argv)
		  (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
		    (match rest
		      ((or (list* '&allow-other-keys (and rest2 (or (list* '&aux _) nil)))
			   (guard (list* '&allow-other-keys var (and rest2 (or (list* '&aux _) nil))) (not canonicalp)))
		       (push `((:keyword-allow-other-keys ,var) ,@(mapcar #'compile-keyword-pattern argv)) results)
		       (setf rest rest2))
		      (_ (when argv (push `(:keyword ,@(mapcar #'compile-keyword-pattern argv)) results))))
		    (ematch rest
		      (nil)                ;do nothing
		      ((type cons) (parse-aux rest)))))
                 (_
                  (parse-aux argv))))
             (parse-aux (argv)
               (ematch argv
                 ((list* '&aux argv)
                  (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
                    (when argv (push `(:aux ,@(mapcar #'ensure-list argv)) results))
                    (ematch rest (nil))))
                 (nil))))
      (parse-whole argv)
      (nreverse results))))

;; (parse-lambda-list '(a . b))         ((:ATOM A) (:REST B))
;; (parse-lambda-list '(a &optional b)) ((:ATOM A) (:OPTIONAL (B)))
;; (parse-lambda-list '(a &optional b x))  ((:ATOM A) (:OPTIONAL (B) (X)))
;; (parse-lambda-list '(a &optional (b 1) x)) ((:ATOM A) (:OPTIONAL (B 1) (X)))
;; (parse-lambda-list '(a &optional (b 1 supplied) x)) ((:ATOM A) (:OPTIONAL (B 1 SUPPLIED) (X)))
;; (parse-lambda-list '(&whole whole a &optional (b 1 supplied) x)) ((:WHOLE WHOLE) (:ATOM A) (:OPTIONAL (B 1 SUPPLIED) (X)))

;; non-canonical syntax
;; (parse-lambda-list '(&key x &allow-other-keys rem) nil) (((:KEYWORD-ALLOW-OTHER-KEYS REM) ((X X))))

(defun compile-destructuring-pattern (ops &optional default)
  (match ops
    (nil default)
    ((list* (list :whole subpattern) rest)
     `(and ,subpattern ,(compile-destructuring-pattern rest)))
    ((list* (list* :atom subpatterns) rest)
     `(list* ,@subpatterns ,(compile-destructuring-pattern rest)))
    ((list* (list :optional) rest)
     (compile-destructuring-pattern rest))
    ((list* (list* :optional subpattern more-subpatterns) rest)
     (with-gensyms (lst supplied-p-default-sym)
       (destructuring-bind (subpattern &optional default (supplied-p-pattern supplied-p-default-sym supplied-p-pattern-supplied)) subpattern
         `(guard1 (,lst :type list) (listp ,lst)
                  (if ,lst (car ,lst) ,default) ,subpattern
                  ,@(when supplied-p-pattern-supplied
                      `((if ,lst t nil) ,supplied-p-pattern))
                  (cdr ,lst) ,(compile-destructuring-pattern `((:optional ,@more-subpatterns) ,@rest))))))
    ((list* (list :rest pattern) rest)
     `(and ,pattern ,(compile-destructuring-pattern rest '_)))
    ((list* (list* (and mode (or :keyword (list :keyword-allow-other-keys rem))) subpatterns) rest)
     ;; case 1,2 of the &key forms are already compiled into the 3rd form ; see parse-lambda-list
     `(and (type list)
           ;; proper plist
           ,(with-gensyms (it)
              `(guard1 ,it (evenp (length ,it))))
           ,@(when (or (eq mode :keyword) rem)
               (let ((valid-keywords (mapcar (compose #'make-keyword #'caar) subpatterns)))
		 (with-gensyms (lst key val)
		   (if (eq mode :keyword)
		       ;; match only when there are no invalid keywords.
		       `((guard1 ,lst (loop :for ,key :in ,lst :by #'cddr :always (member ,key ',valid-keywords))))
		       ;; In contrast, :keyword-allow-other-keys does not check the invalid keywords
		       ;; In used in the non-canonical mode, sets rem to the additional keys given
		       `((guard1 ,lst t (loop :for (,key ,val) :on ,lst :by #'cddr
					   :unless (member ,key ',valid-keywords)
					   :collect ,key :and :collect ,val)
				 ,rem))))))
           ;; match the keywords
	   ,@(compile-keyword-patterns subpatterns)
           ;; compile the rest
           ,(compile-destructuring-pattern rest '_)))
    ((list (list* :aux subpatterns))
     `(guard1 ,(gensym) t ,@(mapcan #'(lambda (x)
                                        (destructuring-bind (var &optional expr) (ensure-list x)
                                          (assert (typep var 'variable-symbol) nil "invalid lambda list")
                                          `(,expr ,var)))
                                    subpatterns)))))

;;
(defun compile-keyword-patterns (subpatterns)
  ;; FIXME: possible optimization --- copy-list the input and modify the
  ;; list removing the element, in order to make the worst-case complexity
  ;; from O(n^2) to O(n)
  (mapcar (lambda (keypat)
            (destructuring-bind ((var subpattern)
				 &optional default
				 (supplied-p-pattern nil supplied-p-pattern-suppliedp)) keypat
	      `(property ,(make-keyword var)
			 ,subpattern ,default ,@(if supplied-p-pattern-suppliedp `(,supplied-p-pattern)))))
          subpatterns))

;(compile-destructuring-pattern (parse-lambda-list '(a . b)))
(defpattern lambda-list (&rest patterns)
  "Matches to a list conforming to the lambda-list specified by PATTERN. In other words, it supports the same
   arguments as DESTRUCTURING-BIND. For example,

  (1 2 3)   matches against        (lambda-list a b &optional c)
  (1 2 3 4) does not match against (lambda-list a b &optional c)
  (1 2 3 4 5) matches against      (lambda-list a b &rest rest), where rest is bound to '(3 4 5) "
  (compile-destructuring-pattern (parse-lambda-list patterns)))

(defpattern λlist (&rest patterns)
  "Alias to lambda-list"
  (compile-destructuring-pattern (parse-lambda-list patterns)))

(defpattern lambda-list-nc (&rest patterns)
  "lambda-list but with additional non-canonical syntax,
   1. (lambda-list &key a &allow-other-keys rem)
      REM collects every key that wasn't declared"
  (compile-destructuring-pattern (parse-lambda-list patterns nil)))

(defpattern λlist-nc (&rest patterns)
  "Alias to lambda-list-nc"
  (compile-destructuring-pattern (parse-lambda-list patterns nil)))
