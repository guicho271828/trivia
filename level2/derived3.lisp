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
    (labels ((!lambda-list-keyword-p (thing) (not (member thing lambda-list-keywords)))
             (parse-whole (argv)
               (match argv
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
               (match argv
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
               (match argv
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
               (match argv
                 ((list* '&key argv)
                  (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
                    (when argv
                      (match rest
                        ((list* '&allow-other-keys rest2)
                         (push `(:keyword-allow-other-keys
                                 ,@(mapcar #'compile-keyword-pattern argv)) results)
                         (setf rest rest2))
                        (_
                         (push `(:keyword ,@(mapcar #'compile-keyword-pattern argv)) results))))
                    (ematch rest
                      (nil)                ;do nothing
                      ((type atom) (push (list :rest rest) results))
                      ((type cons) (parse-aux rest)))))
                 (_
                  (parse-aux argv))))
             (parse-aux (argv)
               (match argv
                 ((list* '&aux argv)
                  (multiple-value-bind (argv rest) (take-while argv #'!lambda-list-keyword-p)
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

(defun compile-destructuring-pattern (ops &optional (default '_))
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
     `(and ,pattern ,(compile-destructuring-pattern rest)))
    ((list* (list* (and mode (or :keyword :keyword-allow-other-keys)) subpatterns) rest)
     ;; case 1,2 are already compiled into the 3rd pattern ; see parse-lambda-list
     ((lambda (property-patterns)               ; lambda form (see CLHS lambda-form)
        (with-gensyms (it)
          `(and (type list)
                ;; proper plist
                (guard1 ,it (evenp (length ,it)))
                ,@(when (eq mode :keyword)
                    ;; match only when there are no invalid keywords.
                    ;; In contrast, :keyword-allow-other-keys does not check the invalid keywords
                    (let ((valid-keywords (mapcar (compose #'make-keyword #'caar) subpatterns)))
                      (with-gensyms (lst key)
                        `((guard1 ,lst (loop for ,key in ,lst by #'cddr always (member ,key ',valid-keywords)))))))
                ;; match the keywords
                ,@property-patterns
                ;; compile the rest
                ,(compile-destructuring-pattern rest))))
      (mapcar (lambda (keypat)
                (with-gensyms (supplied-p-default-sym)
                  (destructuring-bind ((var subpattern)
                                       &optional default
                                       (supplied-p-pattern supplied-p-default-sym)) keypat
                    `(property ,(make-keyword var)
                               ,subpattern ,default ,supplied-p-pattern))))
              subpatterns)))
    ((list (list* :aux subpatterns))
     `(guard1 ,(gensym) t ,@(mapcan #'(lambda (x)
                                        (destructuring-bind (var &optional expr) (ensure-list x)
                                          (assert (typep var 'variable-symbol) nil "invalid lambda list")
                                          `(,expr ,var)))
                                    subpatterns)))))

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
