(in-package :trivia.level2.impl)

#+example
(defpattern cons (a b)
  (with-gensyms (it)
    `(guard1 (,it :type cons) (consp ,it) (car ,it) ,a (cdr ,it) ,b)))

(defpattern < (upper-bound)
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (< ,it2 ,upper-bound)))))

(defpattern > (lower-bound)
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (< ,lower-bound ,it2)))))

(defpattern <= (upper-bound)
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (<= ,it2 ,upper-bound)))))

(defpattern => (lower-bound)
  (with-gensyms (it it2)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it2 (<= ,lower-bound ,it2)))))

(defpattern = (number)
  (with-gensyms (it it2)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it2 (= ,number ,it2)))))

(defpattern /= (number)
  (with-gensyms (it it2)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it2 (/= ,number ,it2)))))





