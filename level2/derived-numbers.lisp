(in-package :trivia.level2.impl)

#+example
(defpattern cons (a b)
  (with-gensyms (it)
    `(guard1 (,it :type cons) (consp ,it) (car ,it) ,a (cdr ,it) ,b)))

(defpattern < (upper-bound)
  (with-gensyms (it)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it (< ,it ,upper-bound)))))

(defpattern > (lower-bound)
  (with-gensyms (it)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it (< ,lower-bound ,it)))))

(defpattern = (number)
  (with-gensyms (it)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it
                     (= ,number ,it)))))

(defpattern /= (number)
  (with-gensyms (it)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it
                     (/= ,number ,it)))))





