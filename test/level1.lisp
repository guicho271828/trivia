
(defpackage :optima.level1.test
  (:use :cl :fiveam :optima.level1 :alexandria))
(in-package :optima.level1.test)

(def-suite :optima.level1)
(in-suite :optima.level1)

(defmacro is-match (arg pattern)
  `(is-true (match1 ,arg (,pattern t))))
(defmacro is-not-match (arg pattern)
  `(is-false (match1 ,arg (,pattern t))))

(test variables
  ;; in layer1, variable binding order is strictly defined,
  ;; so it should be compared with equal
  (is (equal '(x y)
             (variables `(guard1 x t (car x) (guard1 y t)))))


  ;; in layer1, or-pattern binding is strict, so the variable set should be set-equal
  (signals error
    (variables `(or1 (guard1 x t)
                     (guard1 y t))))

  (is (set-equal '(x y)
                 (variables `(or1 (guard1 x t (car x) (guard1 y t))
                                  (guard1 y t (car y) (guard1 x t))))))
  (is (set-equal '(x)
                 (variables `(or1 (guard1 x t)
                                  (guard1 x t)))))
  ;; to emulate soft binding (defaulted to nil), use below:
  (is (set-equal '(x y)
                 (variables `(or1 (guard1 x t nil (guard1 y t))
                                  (guard1 y t (car y) (guard1 x t)))))))

(test guard1
  (finishes
    (macroexpand `(match1 y ((guard1 x t) nil))))
  (signals error
    (macroexpand `(match1 y ((guard1 (and x y) t) nil))))
  (signals error
    (variables `(guard1 (and x y) t))))

(test match1

  (is-match '(1 2)
            (guard1 it (consp it)
                    (car it) (guard1 car (= 1 car))
                    (cadr it) (guard1 cadr (= 2 cadr))))

  (is-not-match '(1 2)
                (guard1 it (consp it)
                        (car it) (guard1 car (= 1 car))
                        (cadr it) (guard1 cadr (= 1 cadr))))

  (is-match '(1 2)
            (or1 (guard1 it (consp it)
                         (car it) (guard1 car (= 1 car))
                         (cadr it) (guard1 cadr (= 2 cadr)))
                 (guard1 it (consp it) ;;; this does not happen under level-2 optimization
                                     ;;; because checks for (consp it) is
                                     ;;; duplicated
                         (car it) (guard1 car (= 2 car))
                         (cadr it) (guard1 cadr (= 1 cadr)))))
  (is-match '(2 1)
            (or1 (guard1 it (consp it)
                         (car it) (guard1 car (= 1 car))
                         (cadr it) (guard1 cadr (= 2 cadr)))
                 (guard1 it (consp it)
                         (car it) (guard1 car (= 2 car))
                         (cadr it) (guard1 cadr (= 1 cadr)))))
  (is-not-match '(2 2)
                (or1 (guard1 it (consp it)
                             (car it) (guard1 car (= 1 car))
                             (cadr it) (guard1 cadr (= 2 cadr)))
                     (guard1 it (consp it)
                             (car it) (guard1 car (= 2 car))
                             (cadr it) (guard1 cadr (= 1 cadr)))))

  ;; when checking is optimized...
  (is-match '(2 1)
            (guard1 it (consp it)
                    it
                    (or1 (guard1 it t
                                 (car it) (guard1 car (= 1 car))
                                 (cadr it) (guard1 cadr (= 2 cadr)))
                         (guard1 it t
                                 (car it) (guard1 car (= 2 car))
                                 (cadr it) (guard1 cadr (= 1 cadr)))))))

(defun constant-fn ()
  ;; sbcl can optimize away everything in here, and can infer that the
  ;; following code returns T: since '(1 2) is a constant value
  (match1 '(1 2)
    ((or1 (guard1 it (consp it)
                  (car it) (guard1 car (= 1 car))
                  (cadr it) (guard1 cadr (= 2 cadr)))
          (guard1 it (consp it) ; this does not happen under level-2 ;
                                        ; optimization because checks for ; (consp
                                        ; it) is duplicated
                  (car it) (guard1 car (= 2 car))
                  (cadr it) (guard1 cadr (= 1 cadr))))
     t)))

(defun nonconstant-fn (thing)
  ;; of cource this is not the case if it is not a constant
  (match1 thing
    ((or1 (guard1 it (consp it)
                  (car it) (guard1 car (= 1 car))
                  (cadr it) (guard1 cadr (= 2 cadr)))
          (guard1 it (consp it) ; this does not happen under level-2
                                        ; optimization because checks for (consp it)
                                        ; is duplicated
                  (car it) (guard1 car (= 2 car))
                  (cadr it) (guard1 cadr (= 1 cadr))))
     t)))

(test match1*

  (match1* ('(1 2) '(2 1))
    (((guard1 it1 (consp it1)
              (car it1) (guard1 car1 (= 1 car1))
              (cadr it1) (guard1 cadr1 (= 2 cadr1)))
      (guard1 it2 (consp it2)
              (car it2) (guard1 car2 (= cadr1 car2))
              (cadr it2) (guard1 cadr2 (= car1 cadr2))))
     (pass))
    (((guard1 it t) (guard1 it t))
     (fail))))

(eval-when (:load-toplevel :execute)
  (run! :optima.level1))

