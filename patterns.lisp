(in-package :optima.level2.impl)

(defun ? (symbol test)
  "substitute symbol with '?, canonicalizing the test form amenable for comparison"
  (subst '? symbol test))

(defun pattern-compatible-p (p1 p2)
  "tests if two patterns share the constructor. only descends into or1 patterns"
  (match0 p1
    ((list* 'guard1 s1 t1 more1)
     (match0 p2
       ((list* 'guard1 s2 t2 more2)
        (test-compatible-p (? s1 t1)
                           (? s2 t2)))
       ((list* 'or1 or-subpatterns)
        (list* 'or1
               (some (curry #'pattern-compatible-p p1)
                     or-subpatterns)))))
    ((list* 'or1 or-subpatterns)
     (list* 'or1
            (some (curry #'pattern-compatible-p p2)
                  or-subpatterns)))))

;; (defun gsubpatterns (g)
;;   (match0 g
;;     ((list* 'guard1 _ _ more-patterns) more-patterns)
;;     (_ (error "[~a] called against non-guard1 pattern ~a" 'gsubpatterns g))))
;; 
;; (defun incompatible-p (g1 g2)
;;   (or (not (compatible g1 g2))
;;       (and (= (length g1) (length g2))
;;            (some #'incompatible-p
;;                  (mapcar #'cdr (plist-alist (gsubpatterns g1)))
;;                  (mapcar #'cdr (plist-alist (gsubpatterns g2)))))))

(defun find-exhaustive-test-form (g1 g2)
  (match0 g1
    ((list* 'guard1 s1 t1 _)
     (match0 g2
       ((list* 'guard1 s2 t2 _)
        `(and ,@(mapcar #'first
                        (mapcar #'type-tests
                                (exhaustive-union
                                 (list (test-type (? s1 t1))
                                       (test-type (? s2 t2))))))))
       (_ (error "[~a] called against non-guard1 pattern ~a" 'test= g2))))
    (_ (error "[~a] called against non-guard1 pattern ~a" 'test= g1))))


