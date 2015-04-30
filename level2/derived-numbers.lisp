(in-package :trivia.level2.impl)

#+example
(defpattern cons (a b)
  (with-gensyms (it)
    `(guard1 (,it :type cons) (consp ,it) (car ,it) ,a (cdr ,it) ,b)))

(defpattern < (lower &optional upper)
  (with-gensyms (it)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it
                     ,(if upper
                          `(< ,lower ,it ,upper)
                          `(< ,lower ,it))))))

(defpattern > (upper &optional lower)
  (with-gensyms (it)
    `(guard1 (,it :type real) (realp ,it)
             ,it
             (guard1 ,it
                     ,(if upper
                          `(< ,upper ,it ,lower)
                          `(< ,upper ,it))))))

(defpattern = (number)
  (with-gensyms (it)
    `(guard1 (,it :type number) (numberp ,it)
             ,it
             (guard1 ,it
                     (= ,number ,it)))))





