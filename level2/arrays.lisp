(in-package :trivia.level2.impl)

(defun-match* array-type-spec (adjustable has-fill-pointer displaced-to displaced-index-offset)
  ((nil nil nil 0) 'simple-array)
  (_ 'array))

(defun-match element-type-spec (element-type)
  (t t)
  ((list 'quote typespec) typespec)
  (_ '*))

(defun common-specs (dimensions rank total-size &optional rank-error)
  (let* ((rank2
          (match dimensions
            ;; supports only a limited kind of forms
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
                     (error "Rank cannot be determined --- required to parse the contents")
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
                   (displaced-index-offset 0)
                   dimensions
                   rank
                   total-size
                   (contents nil))
  ;; deduce the array type
  (check-type displaced-index-offset (and fixnum (integer 0)))
  (let ((array-type-spec (array-type-spec adjustable has-fill-pointer displaced-to displaced-index-offset))
        (element-type-spec (element-type-spec element-type)))
    (multiple-value-bind (dimensions-spec total-size rank2) (common-specs dimensions rank total-size t)
      (with-gensyms (a)
        `(guard1 (,a :type (,array-type-spec ,element-type-spec ,dimensions-spec))
                 (typep ,a '(,array-type-spec ,element-type-spec ,dimensions-spec))
                 (array-element-type       ,a) ,element-type
                 (array-has-fill-pointer-p ,a) ,has-fill-pointer
                 (array-dimensions         ,a) ,(match dimensions
                                                  ((or (and x (integer))
                                                       (list 'quote (and x (integer))))
                                                   `(list ,@(mapcar (constantly '_) (iota x))))
                                                  (_ dimensions))
                 (array-rank               ,a) ,rank
                 (array-total-size         ,a) ,total-size
                 (adjustable-array-p       ,a) ,adjustable
                 (multiple-value-list (array-displacement ,a))
                 (list ,displaced-to ,displaced-index-offset)
                 ,@(labels ((parse-array-body (rank contents r-subscripts)
                              (if (= rank 0)
                                  `((aref ,a ,@(reverse r-subscripts)) ,contents)
                                  (mappend (lambda (content index)
                                             (parse-array-body (1- rank) content (cons index r-subscripts)))
                                           contents
                                           (iota (length contents))))))
                     (parse-array-body rank2 contents nil)))))))

(defpattern simple-array (&rest args &key element-type dimensions rank total-size contents)
  (declare (ignorable element-type dimensions rank total-size contents))
  `(array :adjustable nil  :has-fill-pointer nil :displaced-to nil :displaced-index-offset 0 ,@args))

(defpattern row-major-array* (&key element-type
                                   adjustable
                                   has-fill-pointer
                                   displaced-to
                                   displaced-index-offset
                                   dimensions
                                   rank
                                   total-size
                                   contents)
  (let ((array-type-spec (array-type-spec adjustable has-fill-pointer displaced-to displaced-index-offset))
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
  (declare (ignorable element-type adjustable has-fill-pointer displaced-to displaced-index-offset dimensions
                      rank total-size contents))
  (remf args :total-size)
  `(row-major-array* :total-size (and ,(length contents) ,total-size) ,@args))

;; (defpattern vector (&rest args)
;;   `(array :element-type t :total-size ,(length args) :dimensions (,(length args))

(defun set-vector-matcher (name &optional (ref 'aref) need-type soft)
  (let* ((level2p (find-package :trivia.level2))
         (name* (intern (format nil "~a*" name) level2p)))
    (export name* level2p)
    (setf (symbol-pattern (if soft name* name))
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
