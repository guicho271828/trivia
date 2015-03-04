
(error "this file is no longer used")

;;;     Tests Variations
;;; 
;;; these works as the hints for the compiler.
;;; include only those which is not trivially defined.
;;; By default, type-tests finds corresponding function automatically
;;; e.g. for type X, predicates like X-p or Xp, and also (typep ? X).
;;; also, since CL allows abbreviating compound specifier, like
;;; (array type dimensions), (array type), (array), array, they also detects these things.

;; `and' treated specially; all permutations are considered
;; `or' is also treated specially. -- how?

(define-type-tests null ()
  (null ?)
  (eq ? nil))

;; and           long-float    simple-base-string  
;; array         member        simple-bit-vector   
;; base-string   mod           simple-string       
;; bit-vector    not           simple-vector       
;; complex       or            single-float        
;; cons          rational      string              
;; double-float  real          unsigned-byte       
;; eql           satisfies     values              
;; float         short-float   vector              
;; function      signed-byte                       
;; integer       simple-array                      
;; 
;; Figure 4-3. Standardized Compound Type Specifier Names

(defmacro define-compound-type-tests (name &rest stars)
  (assert (every (lambda (x) (eq '* x)) stars))
  (labels ((rec (stars)
             (cons `(typep ? '(array ,@stars))
                       (when stars
                         (rec (cdr stars))))))
    `(define-type-tests ,name ()
       ,@(rec stars))))

(define-compound-type-tests array * *)
(define-compound-type-tests base-string * *)
(define-compound-type-tests bit-vector * *)
(define-compound-type-tests complex * *)
(define-compound-type-tests cons * *)
(define-compound-type-tests double-float * *)
(define-compound-type-tests float * *)
(define-compound-type-tests function * *)
(define-compound-type-tests integer * *)
(define-compound-type-tests long-float * *)
(define-compound-type-tests rational * *)
(define-compound-type-tests real * *)
(define-compound-type-tests short-float * *)
(define-compound-type-tests signed-byte * *)
(define-compound-type-tests simple-array * *)
(define-compound-type-tests simple-base-string *)
(define-compound-type-tests simple-bit-vector *)
(define-compound-type-tests simple-string *)
(define-compound-type-tests simple-vector *)
(define-compound-type-tests single-float * *)
(define-compound-type-tests string *)
(define-compound-type-tests unsigned-byte *)
(define-compound-type-tests vector *)

;;; subtypes are automatically recognized, so no need to add these statements
#+nil
(define-type-tests list ()
  (listp ?)
  (or (consp ?) (null ?)))

;;; no arguments as of now
#+nil
(define-type-tests array (type)
  (typep ? `(array ,type *))
  (typep ? `(array ,type))
  (and (arrayp ?)
       (subtypep (array-element-type ?) ,type)))
#+nil
(define-type-tests array (type rank)
  (typep ? `(array ,type ,rank))
  (and (arrayp ?)
       (subtypep (array-element-type ?) ,type)
       (= (array-rank ?) ,rank)))
#+nil
(define-type-tests array (type (&rest dimensions))
  (typep ? `(array ,type ,dimensions))
  (and (arrayp ?)
       (subtypep (array-element-type ?) ,type)
       (every (lambda (decl actual)
                (or (eq decl '*)
                    (= decl actual)))
              ',dimensions
              (array-dimensions ?))))

