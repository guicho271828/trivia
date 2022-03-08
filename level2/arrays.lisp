(in-package :trivia.level2.impl)

(defun-match* array-type-spec (adjustable has-fill-pointer displaced-to)
  ((nil nil nil) 'simple-array)
  (_ 'array))

(defun-match element-type-spec (element-type)
  (t t)
  ((list 'quote typespec) typespec)
  (_ '*))

(defun common-specs (dimensions rank total-size &optional rank-error)
  "Deduce the rank from array DIMENSIONS, then normalize and check the consistency of the specification.
For example, DIMENSION = '(1 2 3) and RANK = 2 is inconsistent, the RANK should be 3."
  (let* ((rank2
          (match dimensions
            ;; Parses the form in DIMENSIONS.
            ;; Supports only a limited kind of forms
            ((or (list 'quote (and (type list) dimensions)) (list* 'list dimensions))
             (when (integerp rank) (assert (= (length dimensions) rank)))
             (length dimensions))
            ((or (and (type integer) n) (list 'quote (and (type integer) n)))
             (when (integerp rank) (assert (= n rank)))
             n)
            (_
             (if (integerp rank)
                 rank
                 (if rank-error
                     (error "Failed to deduce a rank --- this is required for parsing the pattern")
                     '_)))))
         (dimensions-spec
          (match dimensions
            ;; supports only a limited kind of forms
            ((or (list 'quote (and (type list) dimensions)) (list* 'list (and (type list) dimensions)))
             (when (integerp rank) (assert (= rank (length dimensions))))
             (when (integerp rank2) (assert (= rank2 (length dimensions))))
             (substitute-if-not '* #'integerp dimensions))
            ((or (and (type integer) n) (list 'quote (and (type integer) n)))
             (when (integerp rank) (assert (= rank n)))
             (when (integerp rank2) (assert (= rank2 n)))
             (mapcar (constantly '*) (iota n)))
            (_
             (cond
               ((integerp rank) (mapcar (constantly '*) (iota rank)))
               ((integerp rank2) (mapcar (constantly '*) (iota rank2)))
               (t '*)))))
         (total-size
          (match total-size
            ((integer)
             (when (listp dimensions-spec)
               (when (every #'integerp dimensions-spec)
                 (assert (= total-size (apply #'* dimensions-spec)))))
             (trivia.skip:skip))
            (_
             (if (and (listp dimensions-spec)
                      (every #'integerp dimensions-spec))
                 `(and ,(apply #'* dimensions-spec) ,total-size)
                 total-size)))))
    (values dimensions-spec total-size rank2)))

(defpattern array (&key
                   element-type
                   adjustable
                   has-fill-pointer
                   displaced-to
                   displaced-index-offset
                   dimensions
                   rank
                   total-size
                   (contents nil))
  "Matches against an array, its contents, and its meta-level information
such as size, element-type.

* CONTENTS is a matrix notation of patterns, i.e., a tree of patterns.
  For example, :contents ((A _ _) (_ B _) (_ _ C)) matches against
  (AREF X 0 0), (AREF X 1 1) (AREF X 2 2)
  of an array X and binds them to A, B, C respectively.
* DIMENSIONS should be either
  * a quoted list of integers (e.g. '(5 4 4)),
  * an integer specifying a 1-dimensional array (e.g. 256),
  * or a list pattern (e.g. (list 5 A _)).
* RANK should be an integer (e.g. 3) or a variable pattern (e.g. A, _).
* The rank of the array should be deduced from DIMENSIONS or RANK itself,
  and the deduced rank and the specified RANK should be consistent when both are present.
  Otherwise, the compilation fails. Rank information is used to parse the subpatterns.
* TOTAL-SIZE should be consistent with DIMENSIONS when all dimensions are fully specified
  (e.g. when DIMENSIONS = '(5 4 4) and TOTAL-SIZE is a variable pattern,
        then it is bound to 80. When TOTAL-SIZE is an integer, it should be 80
        or it signals an error.)
* ELEMENT-TYPE is * unless specified.
* If ADJUSTABLE, HAS-FILL-POINTER, DISPLACED-TO are all NIL, then
  it is a SIMPLE-ARRAY. Otherwise it's an ARRAY.
"
  (let ((array-type-spec (array-type-spec adjustable has-fill-pointer displaced-to))
        (element-type-spec (element-type-spec element-type)))
    (multiple-value-bind (dimensions-spec total-size deduced-rank) (common-specs dimensions rank total-size contents)
      (with-gensyms (a)
        `(guard1 (,a :type (,array-type-spec ,element-type-spec ,dimensions-spec))
                 (typep ,a '(,array-type-spec ,element-type-spec ,dimensions-spec))
                 (array-element-type       ,a) ,element-type
                 (array-has-fill-pointer-p ,a) ,has-fill-pointer
                 (array-dimensions         ,a) ,(match dimensions
                                                  ((or (and x (integer))
                                                       (list 'quote (and x (integer))))
                                                   ;; When the DIMENSIONS pattern was given in an integer,
                                                   ;; it should be replaced with a list pattern,
                                                   ;; otherwise it fails to match.
                                                   `(list ,@(mapcar (constantly '_) (iota x))))
                                                  (_
                                                   ;; Enhancement. Balland2006 optimizer is able to merge these declarations
                                                   #+(or)
                                                   `(and (list ,@(mapcar
                                                                  (constantly `(type (integer 1 ,array-dimension-limit)))
                                                                  (iota deduced-rank)))
                                                         ,dimensions)
                                                   dimensions))
                 (array-rank               ,a) ,rank
                 (array-total-size         ,a) ,total-size
                 (adjustable-array-p       ,a) ,adjustable
                 (nth-value 0 (array-displacement ,a))
                 ,displaced-to
                 (nth-value 1 (array-displacement ,a))
                 ,displaced-index-offset
                 ,@(labels ((parse-array-body (rank contents r-subscripts)
                              (if (= rank 0)
                                  `((aref ,a ,@(reverse r-subscripts)) ,contents)
                                  (mappend (lambda (content index)
                                             (parse-array-body (1- rank) content (cons index r-subscripts)))
                                           contents
                                           (iota (length contents))))))
                     (when contents
                       (parse-array-body deduced-rank contents nil))))))))

(defpattern simple-array (&rest args &key element-type dimensions rank total-size contents)
  "Matches against a simple-array, its contents, and its meta-level information such as size, element-type.
This is an alias to the base ARRAY pattern.
"
  (declare (ignorable element-type dimensions rank total-size contents))
  `(array :adjustable nil  :has-fill-pointer nil :displaced-to nil ,@args))

(defpattern row-major-array* (&key element-type
                                   adjustable
                                   has-fill-pointer
                                   displaced-to
                                   displaced-index-offset
                                   dimensions
                                   rank
                                   total-size
                                   contents)
  "This is a soft-match variant of ROW-MAJOR-ARRAY pattern
i.e. the total length of CONTENTS (subpatterns) can be less than the actual size of the pattern."
  (let ((array-type-spec (array-type-spec adjustable has-fill-pointer displaced-to))
        (element-type-spec (element-type-spec element-type)))
    (multiple-value-bind (dimensions-spec total-size) (common-specs dimensions rank total-size)
      (with-gensyms (a)
        `(guard1 (,a :type (,array-type-spec ,element-type-spec ,dimensions-spec))
                 (typep ,a '(,array-type-spec ,element-type-spec ,dimensions-spec))
                 (array-element-type       ,a) ,element-type
                 (array-has-fill-pointer-p ,a) ,has-fill-pointer
                 (array-dimensions         ,a) ,(match dimensions
                                                  ((or (and x (integer))
                                                       (list 'quote (and x (integer))))
                                                   ;; When the DIMENSIONS pattern was given in an integer,
                                                   ;; it should be replaced with a list pattern,
                                                   ;; otherwise it fails to match.
                                                   `(list ,@(mapcar (constantly '_) (iota x))))
                                                  (_ dimensions))
                 (array-rank               ,a) ,rank
                 (array-total-size         ,a) ,total-size
                 (adjustable-array-p       ,a) ,adjustable
                 (multiple-value-list (array-displacement ,a)) (list ,displaced-to ,displaced-index-offset)
                 ,@(mappend (lambda (content index)
                              `((row-major-aref ,a ,index) ,content))
                            contents
                            (iota (length contents))))))))

(defpattern row-major-array (&rest args
                                   &key element-type
                                   adjustable
                                   has-fill-pointer
                                   displaced-to
                                   displaced-index-offset
                                   dimensions
                                   rank
                                   total-size
                                   contents)
  "Same as ARRAY pattern, but it uses row-major-array to access the elements.
 CONTENTS is a list of patterns (just like in VECTOR pattern),
 rather than a matrix notation of the patterns in ARRAY pattern."
  (declare (ignorable element-type adjustable has-fill-pointer displaced-to displaced-index-offset dimensions
                      rank total-size contents))
  (remf args :total-size)
  `(row-major-array* :total-size (and ,(length contents) ,total-size) ,@args))

;; (defpattern vector (&rest args)
;;   `(array :element-type t :total-size ,(length args) :dimensions (,(length args))

(defun set-vector-matcher (name &optional (ref 'aref) need-type soft)
  (let* ((level2p (find-package :trivia.level2))
         (name2 (if soft
                    (intern (concatenate 'string (symbol-name name) "*") level2p)
                    name)))
    (when soft
      (export name2 level2p))
    (setf (documentation name2 'pattern)
          (if soft
              (format nil
                      "Lambda-List: (&REST ELEMENTS)~%  ~
                       Soft-match variant of ~a pattern.~%  ~
                       Matches against a ~a by specifying each element, like a LIST pattern.~%  ~
                       The matched ~a can contain more elements than the number of subpatterns."
                      name name name)
              (format nil
                      "Matches against a ~a by specifying each element, like a LIST pattern."
                      name)))

    (setf (symbol-pattern name2)
          (lambda (&rest patterns)
            (with-gensyms (it)
              (let* ((len (length patterns))
                     (type `(,name
                             ,@(when need-type '(*))
                             ,(if soft '* len))))
                `(guard1 (,it :type ,type)
                         (typep ,it ',type)
                         ,@(mappend (lambda (pattern i)
                                      `(,(if soft
                                             `(when (< ,i (array-total-size ,it))
                                                (,ref ,it ,i))
                                             `(,ref ,it ,i)) ,pattern))
                                    patterns (iota len)))))))))

(dolist (s '(string bit-vector base-string))
  ;; strict vector matching
  (set-vector-matcher s)
  ;; soft vector matching where the insufficient elements are given NIL
  (set-vector-matcher s 'aref nil t))
(dolist (s '(simple-string simple-bit-vector simple-base-string))
  (set-vector-matcher s 'aref nil)
  (set-vector-matcher s 'aref nil t))

(set-vector-matcher 'vector 'aref t)
(set-vector-matcher 'vector 'aref t t)
(set-vector-matcher 'simple-vector 'svref nil)
(set-vector-matcher 'simple-vector 'svref nil t)
