;;; prelude

(in-package :trivia.balland2006)

(defvar *trace-optimization* nil "if non-nil, prints the debug information")

(defoptimizer :balland2006 (clauses &key types &allow-other-keys)
  (balland2006 clauses
               (or types
                   (make-list (reduce #'max
                                      (mapcar
                                       (compose #'length #'first)
                                       clauses))
                              :initial-element t))))

#|

 debugging memo: to debug the optimizer, set *trace-optimization* to t, and
 disable one of 4 optimization step below to see which part has the problem.

|#

(defparameter *optimization-timeout* 3 "Optimization timeout in seconds. It matters for a very complex pattern")
(defparameter *grounding-clause-limit* 300 "The limit for grounding the OR patterns. If the grounding
resulted in clauses more than this limit, it undoes the grounding.")
(defparameter *optimization-report-threshold* 1 "Prints the runtime of optimization passes when it exceeds this limit")

(defun timeout-p (start-time)
  (let ((result (< *optimization-timeout*
                   (- (get-universal-time) start-time))))
    (when result
      (format t "~&~<; ~@;Optimization timed out! Return the current result~:>~%" nil))
    result))

(defun grounding-limit-reached-p (clauses)
  (let ((result (< *grounding-clause-limit* (length clauses))))
    (when result
      (format t "~&~<; ~@;Discarding the grounding result with ~a clauses; more than ~a clauses.~:>~%"
              (list (length clauses) *grounding-clause-limit*)))
    result))

(defun balland2006 (clauses types)
  (let ((% clauses)
        (start-time (get-universal-time)))
    (iter (for prev = %)
          (when *trace-optimization*
            (format t "~&~<; ~@;Current pattern~_ ~s~:>~%" (list %)))
          (until (timeout-p start-time))
          (let ((result (apply-or-grounding %)))
            (when *trace-optimization*
              (format t "~&~<; ~@;Grounding result~_ ~s~:>~%" (list result)))
            (until (grounding-limit-reached-p result))
            ;; it can't continue, since swapping cannot be applied to OR patterns
            (setf % result))
          (until (timeout-p start-time))
          (setf % (apply-swapping     % types))
          (when *trace-optimization*
            (format t "~&~<; ~@;Swapping result~_ ~s~:>~%" (list %)))
          (until (timeout-p start-time))
          (setf % (apply-fusion       % types))
          (when *trace-optimization*
            (format t "~&~<; ~@;Fusion result~_ ~s~:>~%" (list %)))
          (until (timeout-p start-time))
          (setf % (apply-interleaving % types))
          (when *trace-optimization*
            (format t "~&~<; ~@;Interleaving result~_ ~s~:>~%" (list %)))
          (until (equal % prev)))
    (let ((runtime (- (get-universal-time) start-time)))
      (when (< *optimization-report-threshold* runtime)
        (format t "~&~<; ~@;Optimization took ~a seconds. Resuting clauses: ~a clauses. ~:>~%" (list runtime (length %)))))
    %))

;;; or lifting

(defun apply-or-grounding (clauses)
  (let ((new (mappend #'ground-or clauses)))
    (if (not (equal clauses new))
        (apply-or-grounding new)
        clauses)))

(defun ground-or (clause)
  (ematch clause
    ((list* (list* (list* 'guard1 _) _) _)
     (list clause))
    ((list* (list* (list* 'or1 subpatterns) rest) body)
     (when *trace-optimization*
       (format t "~&~<; ~@;Grounding~_ ~s~:>" (list clause)))
     ;; overrides the default or1 compilation
     (mappend (lambda (x)
                ;; this inflates the code size, but let's ignore it for the sake of speed!
                (if rest
                    (mapcar (lambda-ematch
                              ((list* rest body)
                               (list* (list* x rest) body)))
                            (ground-or (list* rest body)))
                    (list (list* (list x) body))))
              subpatterns))))

;;; Fusion

(defun apply-fusion (clauses types)
  (mappend (rcurry #'fuse types) (divide-clauses clauses types)))

(defun divide-clauses (clauses types &aux (under (first types)))
  (ematch clauses
    ((list) nil)
    ((list c) (list (list c)))
    ((list* _ _)
     (iter (for c in clauses)
           (with tmp = nil)
           (with acc = nil)
           (if (emptyp tmp)
               (push c tmp)
               (if (fusiblep (car tmp) c under)
                   (push c tmp)
                   (progn (push (nreverse tmp) acc)
                          (setf tmp (list c)))))
           (finally
            (push (nreverse tmp) acc)
            (return (nreverse acc)))))))

(defun type-disjointp (t1 t2 &optional (under t))
  (subtypep `(and ,under ,t1 ,t2) nil))

(defun type-exhaustivep (t1 t2 &optional (under t))
  "Returns true for types under, t1, t2, if under = t1 + t2 and nothing more"
  (and (type-disjointp t1 t2 under)
       (subtypep under `(or ,t1 ,t2))))

(defun type-equal (t1 t2 &optional (under t))
  (type= `(and ,under ,t1) `(and ,under ,t2)))

(defun gensym* (name)
  (lambda (x)
    (declare (ignore x))
    (gensym name)))

(defun fusiblep (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* (list* ($guard1 s1 _ test1 _) _) _)
      (list* (list* ($guard1 s2 _ test2 _) _) _))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (and ok1 ok2
              (type-equal type1 type2 under)))))))

(defun gen-union (&optional x y)
  (union x y :test #'equal))

(defun fuse (clauses types)
  (unless (cdr clauses)
    (return-from fuse clauses))
  ;; assumes all clauses are fusible
  (with-gensyms (fusion)
    (labels ((sym  (c) (ematch c ((list* (list* ($guard1 x _ _ _) _) _) x)))
             (type (c) (ematch c ((list* (list* ($guard1 _ (property :type x) _ _) _) _) x)))
             (test (c) (ematch c ((list* (list* ($guard1 _ _ x _) _) _) x)))
             (more (c) (ematch c ((list* (list* ($guard1 _ _ _ x) _) _) x)))
             (pat* (c) (ematch c ((list* (list* ($guard1 _ _ _ _) x) _) x)))
             (body (c) (ematch c ((list* (list* ($guard1 _ _ _ _) _) x) x)))
             (generator-alist (x) (plist-alist (subst fusion (sym x) (more x)))))
      (let* ((c (first clauses))
             (more1 (mapcar #'generator-alist clauses))
             (generators (reduce #'gen-union (mapcar (curry #'mapcar #'car) more1)))
             (gen-tmps (mapcar (gensym* "GEN") generators))
             (more2 (mapcar #'cons generators (mapcar #'pattern-expand-all gen-tmps)))
             (pat*-tmps (mapcar (gensym* "PAT") (pat* c)))
             (pat*-pats (mapcar #'pattern-expand-all pat*-tmps))
             (pat** (mapcar (lambda (c) (subst fusion (sym c) (pat* c))) clauses)))
        (when *trace-optimization*
          (format t "~&~<; ~@;Fusing~_ ~{~4t~s~^, ~_~}~:>"
                  (list (mapcar (compose #'third #'first #'first) clauses))))
        ((lambda (result)
           #+nil
           result ;; this results in infinite recursion
           (if (every (curry #'eq t) (mapcar #'test clauses))
               ;; then level1 can handle it, and further fusion results in infinite recursion
               clauses result))
         `((((guard1 (,fusion :type ,(type c))
                     ,(subst fusion (sym c) (test c))
                     ,@(mappend (lambda (c) (list fusion `(guard1 (,(sym c) :type ,(type c)) t)))
                                (remove-duplicates clauses :key #'sym))
                     ,@(alist-plist more2))
             ,@pat*-pats)
            (match2*+ (,@gen-tmps ,@pat*-tmps)
                (,@(mapcar (constantly t) gen-tmps) ,@(rest types))
              ,@(mapcar (lambda (c m pat*)
                          `((,@(mapcar (lambda (gen)
                                         (or (cdr (assoc gen m :test #'equal)) '_))
                                       generators)
                               ,@pat*)
                            ,@(body c)))
                        clauses more1 pat**)
              ;; 
              ;;     +-- this (skip) makes the failure propagate upwards correctly
              ;;    / 
              (_ (skip))))))))))

;;; Interleaving

(defun apply-interleaving (clauses types &aux (under (first types)))
  ;; be more conservative than Balland 2006:
  ;; apply only once by each call
  (ematch clauses
    ((list) nil)
    ((list _) clauses)
    ((list* c1 (and rest1 (list* c2 rest2)))
     (if-let ((c12 (interleave c1 c2 types)))
       (progn (when *trace-optimization*
                (format t "~&~<; ~@;Interleaving ~_ ~s,~_ ~s~_ under ~s~:>"
                        (list (third (first (first c1)))
                              (third (first (first c2)))
                              under)))
              (cons c12 rest2))
       (cons c1 (apply-interleaving rest1 types))))))

(defun interleave (c1 c2 types &aux (under (first types)))
  (ematch* (c1 c2)
    (((list* (list* ($guard1 s1 o1 test1 more1) rest1) body1)
      (list* (list* ($guard1 s2 o2 test2 more2) rest2) body2))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (when (and ok1 ok2)
           (cond
             ((type-disjointp type1 under) c2) ; no possibility
             ((type-disjointp type2 under) c1) ; no possibility
             ((type-exhaustivep type1 type2 under)
              ;; exhaustive partition
              (with-gensyms (il)
                `(((guard1 (,il :type ,under) t))
                  (match2*+ (,il) ,types
                    (((guard1 ,(list* s1 o1) ,test1 ,@more1) ,@rest1) ,@body1)
                    (((guard1 ,(list* s2 o2) t      ,@more2) ,@rest2) ,@body2)
                    ;; 
                    ;;     +-- this (skip) makes the failure propagate upwards correctly
                    ;;    / 
                    (_ (skip)))))))))))))

;;; Swapping

(defun apply-swapping (clauses types &aux (under (first types)))
  ;; runs swap sort
  (let* ((v (coerce clauses 'vector))
         (len (length v)))
    (iter
      (while
          (iter (for i from 1 below len)
                (for j = (1- i))
                (when (swappable (aref v i) (aref v j) under)
                  (when *trace-optimization*
                    (format t "~&~<; ~@;Swapping~_ ~s,~_ ~s~_ under ~s~:>"
                            (list (third (first (first (aref v j))))
                                  (third (first (first (aref v i)))) under)))
                  (rotatef (aref v i) (aref v j))
                  (leave t)))))
    (coerce v 'list)))


(defun swappable (c1 c2 &optional (under t))
  (ematch* (c1 c2)
    (((list* patterns1 _)
      (list* patterns2 _))
     (swappable-rec patterns1 patterns2 under))))

(defun swappable-rec (patterns1 patterns2 under)
  (match* (patterns1 patterns2)
    (((list* ($guard1 s1 _ test1 _) rest1)
      (list* ($guard1 s2 _ test2 _) rest2))
     (multiple-value-bind (type1 ok1) (test-type (? s1 test1))
       (multiple-value-bind (type2 ok2) (test-type (? s2 test2))
         (and (< (sxhash type1) (sxhash type2))
              ok1 ok2
              (type-disjointp type1 type2 under)
              (swappable-rec rest1 rest2 under)))))))


