(defpackage :trivia.fset
  (:use :cl))

(defpackage :trivia.fset.impl
  (:use :cl :trivia.level0 :trivia.level1 :trivia.level2
        :alexandria
        :trivia.fset))

(in-package :trivia.fset.impl)

(defpattern fset:equal? (arg)
  "Matches for equality of fset collections. See `fset:equal?`."
  (with-gensyms (it)
    `(guard1 (,it :type ,(type-of arg))
             (fset:equal? ,it ,arg))))

(defpattern fset:map (&rest args)
  "Matches on existance of one or multiple keys in the fset:map collection. Capturing also works."
  (print args)
  `(and ,@(mapcar (lambda-match0
                    ((cons key pattern)
                     `(fset-map-key ,key ,pattern)))
                  (mapcar (lambda (l) `(,(car l) . ,(cadr l))) args))))

(defpattern fset-map-key (key subpattern &optional (default nil) (foundp nil foundp-suppliedp))
  (with-gensyms (it value indicator)
    `(guard1 (,it :type fset:map)
             (fset:map? ,it)
             (fset-lookup ,it ,key ',indicator)
             (guard1 ,value t
                     ,@(if foundp-suppliedp `((if (eql ,value ',indicator) nil t) ,foundp))
                     (if (eql ,value ',indicator) ,default ,value) ,subpattern))))

(defun fset-lookup (collection key default)
  (or (fset:lookup collection key) default))

(defpattern fset:set (&rest args)
  "Matches on existance of one or multiple elements in the given set. fset:set is unordered."
  `(and ,@(mapcar (lambda-match0
                    (element
                     `(fset-set-member ,element)))
                  args)))

(defpattern fset-set-member (element)
  (with-gensyms (it)
    `(guard1 (,it :type fset:set)
             (fset:contains? ,it ,element))))

(defpattern fset:seq (&rest args)
  "Matches against fset:seq. The sizes of patterns and fset:seq size does not need to match.
The last subpattern is destructured against the tail (or a subseq) of fset:seq."
  (if (cdr args)
      `(fset-cons ,(car args) (fset:seq ,@(cdr args)))
      (car args)))

;; (defpattern fset:seq (&rest args)
;;   "Matches against the same size of elements in fset:seq."
;;   (if args
;;       `(fset-cons ,(car args) (fset:seq ,@(cdr args)))
;;       `(fset-null)))

(defpattern fset-cons (head tail)
  (with-gensyms (fset-cons)
    `(guard1 (,fset-cons :type fset:seq)
             (fset:seq? ,fset-cons)
             (fset:first ,fset-cons) ,head
             (fset:subseq ,fset-cons 1) ,tail)))

(defpattern fset-null ()
  (with-gensyms (it)
    `(guard1 (,it :type fset:seq) (fset:empty? ,it))))
