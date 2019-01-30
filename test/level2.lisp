
(in-package :trivia.test)
(def-suite :trivia.level2 :in :trivia)
(in-suite :trivia.level2)

(test defpattern
  (finishes (print (pattern-expand '(cons a b)))))

(test pad
  (is (= 1 (match* nil (() 1) (() 1)))))

(defmacro testmatcher (list)
  `(match ,list ((λlist a b &key (c -1)) (list a b c))))

(test destructuring-key
  (is (equal '(1 2 3)   (testmatcher '(1 2 :c 3))))
  (is (equal '(1 2 -1)  (testmatcher '(1 2))))
  (is (equal nil        (testmatcher '(1 2 :c)))))

(defmacro testmatcher2 (list)
  `(match ,list
     ((λlist a b &optional c &rest rr &key (d -1) &allow-other-keys)
      (list a b c rr d))))

(test destructuring-opt-key
  (is (equal '(1 2 3 (:c 4 :d 5) 5)
             (testmatcher2 '(1 2 3 :c 4 :d 5))))
  (is (equal nil
             ;; because &optional c consumes one value, it becomes an invalid plist
             (testmatcher2 '(1 2 :c 3 :d 4)))))

;;complex
(test complex
  (is (= 0 (match #c(0 1) ((cl:complex r 1) r)))))

;; inline patterns

(test vector-inline-patterns
  (is (equal '((vector 1 _ _ _ _ _ _ _ _ _ _ 5))
             (inline-pattern-expand '(vector 1 (@@ 10 _) 5))))
  (is-match (vector 1 2 3 4 5 6 7 8 9 10)
            (vector 1 (@@ 8 _) 10)))

(test issue-21
  ;; inline-pattern-expand is confused when the pattern contains non-pattern forms
  (finishes
    (inline-pattern-expand '(guard x (let ((y 1)) (= x y))))))

(test issue-32
  (match (list 1 2 3)
    ((lambda-list 1 2)
     (fail "should not match"))
    (_
     (pass)))
  (signals error
    (pattern-expand-1 `(lambda-list a &rest b &optional c)))
  (signals error
    (pattern-expand-1 `(lambda-list a &aux (c 2) &rest d))))

(test issue-41
  (match (list :x 1 :y 1)
    ((lambda-list &key &allow-other-keys)
     (pass))
    (_
     (fail "should not match")))
  (signals error
    (pattern-expand-1 `(lambda-list &allow-other-keys))))

(test lambda-list-nc
  (match (list :x 1 :y 1)
    ((lambda-list-nc &key x &allow-other-keys (list :y 1))
     (pass))
    (_
     (fail "should not match")))
  (signals error
    (pattern-expand-1 `(lambda-list &key x &allow-other-keys (list :y 1)))))

(test array
  (match #2A((0 1) (2 3))
    ((array :adjustable nil
            :has-fill-pointer nil
            :displaced-to nil
            :displaced-index-offset 0
            :dimensions '(2 2)
            :rank 2
            :total-size 4
            :contents ((a b) (c d)))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against array")))
  (match #2A((0 1) (2 3))
    ((simple-array :rank 2 :contents ((a b) (c d)))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against simple-array")))
  (signals error
    ;; rank is not determined
    (eval
     '(match #2A((0 1) (2 3))
       ((simple-array :contents ((a b) (c d)))))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '(2 2) :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions 2 :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '2 :contents ((a b) (c d))))
  ;; quoted conses should not expand to patterns (issue #86)
  #+(or)
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions '(_ _) :contents ((a b) (c d))))
  (is-match #2A((0 1) (2 3))
    (simple-array :dimensions (list _ _) :contents ((a b) (c d))))
  (match #2A((0 1) (2 3))
    ((row-major-array :contents (a b c d))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2))
     (is (= d 3)))
    (_
     (fail "failed to match against row-major-array")))
  (match #2A((0 1) (2 3))
    ((row-major-array* :contents (a b c))
     (is (= a 0))
     (is (= b 1))
     (is (= c 2)))
    (_
     (fail "failed to match against row-major-array*")))

  (let* ((a (make-array 10))
         (d (make-array 8 :displaced-to a :displaced-index-offset 2)))
    (is-match d (array))))

(test last
      (is-match (alexandria:iota 5)
                (last (list 3 4) 2))
      (is-not-match (alexandria:iota 5)
                    (last (list 4) 2))
      (is-not-match 5 (last (list 3 4) 2))
      (signals error
               (macroexpand
                '(match x
                  ((last _ -1)
                   t)))))

(test issue-81
  (is-match 'x 'x))

(test extensive-eq-test
  ;; note: whether eq returns t for read-time literal is implementation-defined
  ;; except symbols
  
  (is-match 'x 'x)
  (is-match 'x (eq 'x))
  (is-match 'x (eql 'x))
  (is-match 'x (equal 'x))
  (is-match 'x (equalp 'x))
  (is-not-match 'y 'x)
  
  (is-match 1 1)

  ;; http://clhs.lisp.se/Body/f_eq.htm , see Notes:
  ;; 
  ;; An implementation is permitted to make ``copies'' of characters and numbers
  ;; at any time. The effect is that Common Lisp makes no guarantee that eq is
  ;; true even when both its arguments are ``the same thing'' if that thing is a
  ;; character or number.
  ;; 
  #+undefined (is-match 1 (eq 1))
  
  (is-match 1 (eql 1))
  (is-match 1 (equal 1))
  (is-match 1 (equalp 1))
  
  (is-match #\a #\a)
  #+undefined (is-match #\a (eq #\a))
  (is-match #\a (eql #\a))
  (is-match #\a (equal #\a))
  (is-match #\a (equalp #\a))
 
  (is-not-match #\A #\a)
  (is-not-match #\A (eq #\a))
  (is-not-match #\A (eql #\a))
  (is-not-match #\A (equal #\a))
  (is-match #\A (equalp #\a))
 
  (is-match #c(0 1) #c(0 1))
  #+undefined (is-match #c(0 1) (eq #c(0 1)))
  (is-match #c(0 1) (eql #c(0 1)))
  (is-match #c(0 1) (equal #c(0 1)))
  (is-match #c(0 1) (equalp #c(0 1)))
  
  (is-match '(0 1) '(0 1))
  #+undefined (is-match '(0 1) (eq '(0 1)))
  #+undefined (is-match '(0 1) (eql '(0 1)))
  (is-match '(0 1) (equal '(0 1)))
  (is-match '(0 1) (equalp '(0 1)))

  (is-match '(0 . 1) '(0 . 1))
  #+undefined (is-match '(0 . 1) (eq '(0 . 1)))
  #+undefined (is-match '(0 . 1) (eql '(0 . 1)))
  (is-match '(0 . 1) (equal '(0 . 1)))
  (is-match '(0 . 1) (equalp '(0 . 1)))
  
  (is-match '(a b) '(a b))
  #+undefined (is-match '(a b) (eq '(a b)))
  #+undefined (is-match '(a b) (eql '(a b)))
  (is-match '(a b) (equal '(a b)))
  (is-match '(a b) (equalp '(a b)))

  ;; quoted conses should not expand to patterns (issue #86)
  #+(or)
  (is-match '(0 1) '(a b))
  (is-not-match '(0 1) '(a b))
  (is-not-match '(0 1) (eq '(a b)))
  (is-not-match '(0 1) (eql '(a b)))
  (is-not-match '(0 1) (equal '(a b)))
  (is-not-match '(0 1) (equalp '(a b)))
  
  (is-match #(0 1) #(0 1))
  #+undefined (is-match #(0 1) (eq #(0 1)))
  #+undefined (is-match #(0 1) (eql #(0 1)))
  #+undefined (is-match #(0 1) (equal #(0 1)))
  (is-match #(0 1) (equalp #(0 1)))
  (is-match #(0 1) #(_ _))
  
  (is-match "aaa" "aaa")
  #+undefined (is-match "aaa" (eq "aaa"))
  #+undefined (is-match "aaa" (eql "aaa"))
  (is-match "aaa" (equal "aaa"))
  (is-match "aaa" (equalp "aaa"))
  
  (is-match #*0010 #*0010)
  #+undefined (is-match #*0010 (eq #*0010))
  #+undefined (is-match #*0010 (eql #*0010))
  (is-match #*0010 (equal #*0010))
  (is-match #*0010 (equalp #*0010))
  
  (is-match #2A((0 1) (1 0)) #2A((0 1) (1 0)))
  #+undefined (is-match #2A((0 1) (1 0)) (eq #2A((0 1) (1 0))))
  #+undefined (is-match #2A((0 1) (1 0)) (eql #2A((0 1) (1 0))))
  #+undefined (is-match #2A((0 1) (1 0)) (equal #2A((0 1) (1 0))))
  (is-match #2A((0 1) (1 0)) (equalp #2A((0 1) (1 0))))

  (is-match #2A((#\a #\a) (#\a #\a)) #2A((#\a #\a) (#\a #\a)))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (eq #2A((#\a #\a) (#\a #\a))))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (eql #2A((#\a #\a) (#\a #\a))))
  #+undefined (is-match #2A((#\a #\a) (#\a #\a)) (equal #2A((#\a #\a) (#\a #\a))))
  (is-match #2A((#\a #\a) (#\a #\a)) (equalp #2A((#\a #\a) (#\a #\a))))
  
  (is-not-match #2A((#\a #\a) (#\a #\a)) #2A((#\A #\A) (#\A #\A)))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (eq #2A((#\A #\A) (#\A #\A))))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (eql #2A((#\A #\A) (#\A #\A))))
  (is-not-match #2A((#\a #\a) (#\a #\a)) (equal #2A((#\A #\A) (#\A #\A))))
  (is-match #2A((#\a #\a) (#\a #\a)) (equalp #2A((#\A #\A) (#\A #\A))))
  
  (is-match #p"/usr/src/" #p"/usr/src/")
  #+undefined (is-match #p"/usr/src/" (eq #p"/usr/src/"))
  #+undefined (is-match #p"/usr/src/" (eql #p"/usr/src/"))
  (is-match #p"/usr/src/" (equal #p"/usr/src/"))
  (is-match #p"/usr/src/" (equalp #p"/usr/src/"))
  #+implementation-defined (is-match #p"/usr/src/" (equal #p"/USR/SRC/"))
  #+implementation-defined (is-match #p"/usr/src/" (equalp #p"/USR/SRC/"))
  
  (is-match #S(POINT :x "A" :y "B") #S(POINT :x "A" :y "B"))
  (is-not-match #S(POINT :x "A" :y "B") (eq #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") (eql #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") (equal #S(POINT :x "A" :y "B")))
  (is-match #S(POINT :x "A" :y "B") (equalp #S(POINT :x "A" :y "B")))
  (is-not-match #S(POINT :x "A" :y "B") #S(POINT :x "a" :y "b")))

(test issue-86
  (let ((a 0))
    (match `(1)
      ('(a)
        (fail "should not match")))))
  
(test issue-89-property!
  (is (eq nil
          (match '(:y 88)
            ((property :x x) x)
            ((property :y y) y))))
  (is (eq 88
          (match '(:y 88)
            ((property :x (and x (not nil))) x)
            ((property :y y) y))))

  (is (eq nil
          (match '(:y 88 :x nil)
            ((property :x x nil t) x)  ; constant t matches the constant t.
            ((property :y y) y))))

  (is (eq 88
          (match '(:y 88)
            ((property :x x nil t) x)  ; t does not match nil, thus fail
            ((property :y y) y))))

  (is (eq nil
          (match '(:y 88 :x nil)
            ((property! :x x) x)
            ((property! :y y) y))))
  (is (eq 88
          (match '(:y 88)
            ((property! :x x) x)
            ((property! :y y) y)))))

(test issue-93-eq-type-inference

  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form 42 t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form '42 t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form `(quote 42) t)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form 42 nil)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form '42 nil)))
  (is (equal '(eql 42)
             (trivia.level2.impl::type-of-form `(quote 42) nil)))

  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form #\c t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form '#\c t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form `(quote #\c) t)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form #\c nil)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form '#\c nil)))
  (is (equal '(eql #\c)
             (trivia.level2.impl::type-of-form `(quote #\c) nil)))

  (is (equal t
             (trivia.level2.impl::type-of-form 'a t)))
  (is (equal '(eql a)
             (trivia.level2.impl::type-of-form `(quote a) t)))
  (is (equal t
             (trivia.level2.impl::type-of-form 'a nil)))
  (is (equal '(eql a)
             (trivia.level2.impl::type-of-form `(quote a) nil)))

  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form :keyword t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form ':keyword t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form `(quote :keyword) t)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form :keyword nil)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form ':keyword nil)))
  (is (equal '(eql :keyword)
             (trivia.level2.impl::type-of-form `(quote :keyword) nil)))

  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form "str" t)))
  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form '"str" t)))
  (is (equal (type-of "str")
             (trivia.level2.impl::type-of-form `(quote "str") t)))
  (let ((s "str"))
    (is (equal `(eql ,s)
               (trivia.level2.impl::type-of-form s nil)))
    (is (equal `(eql ,s)
               (trivia.level2.impl::type-of-form `(quote ,s) nil))))
  
  (is (equal 'person
             (trivia.level2.impl::type-of-form (make-instance 'person) t)))
  (is (equal 'person
             (trivia.level2.impl::type-of-form `(quote ,(make-instance 'person)) t)))
  (let ((p (make-instance 'person)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form p nil)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form `(quote ,p) nil))))

  (is (equal 'point
             (trivia.level2.impl::type-of-form (make-point) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form (quote #.(make-point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form `(quote ,(make-point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form #S(point) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form (quote #S(point)) t)))
  (is (equal 'point
             (trivia.level2.impl::type-of-form `(quote #S(point)) t)))
  
  (let ((p #S(point)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form p nil)))
    (is (equal `(eql ,p)
               (trivia.level2.impl::type-of-form `(quote ,p) nil))))

  ;; structural (conses)
  (is (equal t
             (trivia.level2.impl::type-of-form `(unknownfunctionform a b) t)))
  (is (equal t
             (trivia.level2.impl::type-of-form `(unknownfunctionform a b) nil)))
  (defconstant +answer-to-everything+ 42)
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `+answer-to-everything+ t)))
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `+answer-to-everything+ nil)))
  (define-symbol-macro %%answer-to-everything%% +answer-to-everything+)
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `%%answer-to-everything%% t)))
  (is (equal `(eql 42)
             (trivia.level2.impl::type-of-form `%%answer-to-everything%% nil)))

  (is (equal '(cons point point)
             (trivia.level2.impl::type-of-form `(quote (,(make-point) . ,(make-point))) t)))
  
  (let* ((p #S(point))
         (c `(,p . ,p)))
    (is (equal '(cons point point)
               (trivia.level2.impl::type-of-form `(quote ,c) t)))
    (is (equal `(eql ,c)
               (trivia.level2.impl::type-of-form `(quote ,c) nil))))

  (let ((c '(x y z)))
    (is (equal '(cons (eql x) (cons (eql y) (cons (eql z) (eql nil))))
               (trivia.level2.impl::type-of-form `(quote ,c) t)))
    (is (equal `(eql ,c)
               (trivia.level2.impl::type-of-form `(quote ,c) nil)))))
