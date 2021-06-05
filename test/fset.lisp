(defpackage :trivia.fset.test
  (:use :cl :fiveam
   :trivia.level2 :trivia.fset))

(in-package :trivia.fset.test)

(in-suite :trivia.fset)

;; (defmacro is-match (arg pattern)
;;   `(is-true (match ,arg (,pattern t))))

(defmacro is-match (arg &body pattern)
  `(is-true (locally
                (declare (optimize (safety 3) (debug 3) (speed 0)))
              (match ,arg (,@pattern t)))
            ,(format nil "~<pattern ~a did not match against arg ~s~:@>" (list pattern arg))))

(defmacro is-not-match (arg pattern)
  `(is-false (match ,arg (,pattern t))))

(test (fset-equal? :compile-at :run-time)
  (is-match (fset:empty-map) (fset:equal? (fset:empty-map)))
  (is-match (fset:map (:x 1)) (fset:equal? (fset:map (:x 1))))
  (is-not-match (fset:map (:y 1)) (fset:equal? (fset:map (:x 1))))

  (is-match (fset:empty-set) (fset:equal? (fset:empty-set)))
  (is-match (fset:set 1 2) (fset:equal? (fset:set 1 2)))
  (is-not-match (fset:set 1 2) (fset:equal? (fset:set 1 3)))

  (is-match (fset:empty-seq) (fset:equal? (fset:empty-seq)))
  (is-match (fset:seq 1 2) (fset:equal? (fset:seq 1 2)))
  (is-not-match (fset:seq 1 2) (fset:equal? (fset:seq 1 3)))

  (is-match (fset:empty-bag) (fset:equal? (fset:empty-bag)))
  (is-match (fset:bag 1 2) (fset:equal? (fset:bag 1 2)))
  (is-not-match (fset:bag 1 2) (fset:equal? (fset:bag 1 3))))

(test (fset-map :compile-at :run-time)
  ;; constant match
  (is-match (fset:map (:x 1)) (fset:map (:x 1)))
  (is-not-match (fset:map (:x 1)) (fset:map (:x 0)))
  (is-match (fset:map (:x 1) (:y 2)) (fset:map (:y 2)))
  ;; capture
  (is-true (match (fset:map (:x 5) (:y 10))
             ((fset:map (:x x) (:y y))
              (and (= x 5) (= y 10)))))
  (is-false (match (fset:map (:y 5))
              ((fset:map (:x v)) v)))
  (is (eq :not-matched (match (fset:empty-map)
                         ((guard
                           (fset:map (:x v))
                           (not (null v)))
                          :matched)
                         ((fset:equal? (fset:empty-map))
                          :not-matched)))))

(test (fset-set :compile-at :run-time)
  ;; constant match
  (is-match (fset:set 1 2) (fset:set 1))
  (is-match (fset:set 1 2) (fset:set 1 2))
  (is-not-match (fset:set) (fset:set 1)))

(test (fset-seq :compile-at :run-time)
  ;; constant matches
  (is-match (fset:seq) (fset:seq _))
  (is-match (fset:seq 1 2) (fset:seq 1 2 _))
  (is-not-match (fset:seq 1 2 3) (fset:seq 1 2))
  ;; captures
  (is-true (match (fset:seq 1 2 3)
             ((fset:seq a b)
              (and (= a 1) (fset:equal? b (fset:seq 2 3))))))
  (is-true (match (fset:seq 1 2 3)
             ((fset:seq a b c)
              (and (= a 1) (= b 2) (fset:equal? c (fset:seq 3))))))
  (is-true (match (fset:seq 1 2)
             ((fset:seq a b c)
              (and (= a 1) (= b 2) (fset:empty? c))))))

;; (test (fset-seq :compile-at :run-time)
;;   ;; constant matches
;;   (is-match (fset:seq) (fset:seq))
;;   (is-match (fset:seq 1 2) (fset:seq 1 2))
;;   (is-not-match (fset:seq 1 2 3) (fset:seq 1 2))
;;   ;; capture
;;   (is-true (match (fset:seq 1 2)
;;              ((fset:seq 1 x) (= x 2))))
;;   (is-not-match (fset:seq 1 2 3) (fset:seq _ _)))

