

(in-package :trivia.benchmark)
#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(defvar *optimization* nil)

;;; Fibonacci computes several times the 18th Fibonacci number.

(defun fibonacci-form (matcher)
  `(lambda (arg)
     (declare (optimize ,@*optimization*)
              #+sbcl
              (sb-ext:muffle-conditions sb-ext:compiler-note))
     (labels ((fib (x)
                (,matcher x
                          (0 0)
                          (1 1)
                          (n (+ (fib (- n 1))
                                (fib (- n 2)))))))
       (fib arg))))


;;; Eratosthene
;; (i.e. sieve of Eratosthenes
;; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) computes primes
;; numbers up to 1000, using associative list matching. The improvement
;; comes from the Inlining rules which avoids computing a substitution
;; unless the rule applies (i.e. the conditions are verified).

;;; Gomoku looks for five pawn on a go board, using list matching.
;; This example contains more than 40 patterns and illustrates the interest of test-sharing.

(defun gomoku-form (matcher)
  `(lambda (v)
     (declare (optimize ,@*optimization*)
              #+sbcl
              (sb-ext:muffle-conditions sb-ext:compiler-note))
     (,matcher v
      ((or (simple-vector
            1 _ _ _ _
            _ 1 _ _ _
            _ _ 1 _ _
            _ _ _ 1 _
            _ _ _ _ 1)
           (simple-vector
            _ _ _ _ 1
            _ _ _ 1 _
            _ _ 1 _ _
            _ 1 _ _ _
            1 _ _ _ _)
           (simple-vector
            1 _ _ _ _
            1 _ _ _ _
            1 _ _ _ _
            1 _ _ _ _
            1 _ _ _ _)
           (simple-vector
            _ 1 _ _ _
            _ 1 _ _ _
            _ 1 _ _ _
            _ 1 _ _ _
            _ 1 _ _ _)
           (simple-vector
            _ _ 1 _ _
            _ _ 1 _ _
            _ _ 1 _ _
            _ _ 1 _ _
            _ _ 1 _ _)
           (simple-vector
            _ _ _ 1 _
            _ _ _ 1 _
            _ _ _ 1 _
            _ _ _ 1 _
            _ _ _ 1 _)
           (simple-vector
            _ _ _ _ 1
            _ _ _ _ 1
            _ _ _ _ 1
            _ _ _ _ 1
            _ _ _ _ 1)
           (simple-vector
            1 1 1 1 1
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            1 1 1 1 1
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            1 1 1 1 1
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            1 1 1 1 1
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            1 1 1 1 1)) 1)
      ((or (simple-vector
            0 _ _ _ _
            _ 0 _ _ _
            _ _ 0 _ _
            _ _ _ 0 _
            _ _ _ _ 0)
           (simple-vector
            _ _ _ _ 0
            _ _ _ 0 _
            _ _ 0 _ _
            _ 0 _ _ _
            0 _ _ _ _)
           (simple-vector
            0 _ _ _ _
            0 _ _ _ _
            0 _ _ _ _
            0 _ _ _ _
            0 _ _ _ _)
           (simple-vector
            _ 0 _ _ _
            _ 0 _ _ _
            _ 0 _ _ _
            _ 0 _ _ _
            _ 0 _ _ _)
           (simple-vector
            _ _ 0 _ _
            _ _ 0 _ _
            _ _ 0 _ _
            _ _ 0 _ _
            _ _ 0 _ _)
           (simple-vector
            _ _ _ 0 _
            _ _ _ 0 _
            _ _ _ 0 _
            _ _ _ 0 _
            _ _ _ 0 _)
           (simple-vector
            _ _ _ _ 0
            _ _ _ _ 0
            _ _ _ _ 0
            _ _ _ _ 0
            _ _ _ _ 0)
           (simple-vector
            0 0 0 0 0
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            0 0 0 0 0
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            0 0 0 0 0
            _ _ _ _ _
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            0 0 0 0 0
            _ _ _ _ _)
           (simple-vector
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            _ _ _ _ _
            0 0 0 0 0)) 0)
      (_ 2))))

(defun gen-gomoku ()
  (let ((v (make-array 25 :initial-element -1))
        (total (+ 12 (random 13))))
    (iter (for n below total)
          (iter (for index = (random 25))
                (when (minusp (aref v index))
                  (setf (aref v index) (random 2))
                  (leave))))
    v))

(defparameter *gomoku-testset* (iter (repeat 2000000) (collect (gen-gomoku))))


;;; Langton is a program which computes the 1000th iteration of a cellular automaton, us-
;; ing pattern matching to implement the transition function. This example contains more
;; than 100 (ground) patterns. The optimized program is optimal in the sense that a pair

;; hmm seems difficult

;;; string-matching searches for a list of strings if a given string
;;; matches against one of them

(defun strmatch-form (matcher)
  `(lambda (string)
     (declare (optimize ,@*optimization*)
              #+sbcl
              (sb-ext:muffle-conditions sb-ext:compiler-note))
     (,matcher string
               ((or "Lorem" "ipsum" "dolor" "sit" "amet" "consectetur" "adipisicing"
                    "elit" "sed" "do" "eiusmod" "tempor" "incididunt" "ut" "labore" "et"
                    "dolore" "magna" "aliqua" "Ut" "enim" "ad" "minim" "veniam" "quis"
                    "nostrud" "exercitation" "ullamco" "laboris" "nisi" "ut" "aliquip" "ex"
                    "ea" "commodo" "consequat" "Duis" "aute" "irure" "dolor" "in"
                    "reprehenderit" "in" "voluptate" "velit" "esse" "cillum" "dolore" "eu"
                    "fugiat" "nulla" "pariatur" "Excepteur" "sint" "occaecat" "cupidatat"
                    "non" "proident" "sunt" "in" "culpa" "qui" "officia" "deserunt"
                    "mollit" "anim" "id" "est" "laborum")
                t))))

(defvar *longstr*
    (list "Sed" "ut" "perspiciatis" "unde" "omnis" "iste" "natus" "error" "sit"
          "voluptatem" "accusantium" "doloremque" "laudantium" "totam" "rem"
          "aperiam" "eaque" "ipsa" "quae" "ab" "illo" "inventore" "veritatis"
          "et" "quasi" "architecto" "beatae" "vitae" "dicta" "sunt"
          "explicabo" "Nemo" "enim" "ipsam" "voluptatem" "quia" "voluptas"
          "sit" "aspernatur" "aut" "odit" "aut" "fugit" "sed" "quia"
          "consequuntur" "magni" "dolores" "eos" "qui" "ratione" "voluptatem"
          "sequi" "nesciunt" "neque" "porro" "quisquam" "est" "qui" "dolorem"
          "ipsum" "quia" "dolor" "sit" "amet" "consectetur" "adipisci"
          "velit" "sed" "quia" "non" "numquam" "eius" "modi" "tempora"
          "incidunt" "ut" "labore" "et" "dolore" "magnam" "aliquam" "quaerat"
          "voluptatem" "Ut" "enim" "ad" "minima" "veniam" "quis" "nostrum"
          "exercitationem" "ullam" "corporis" "suscipit" "laboriosam" "nisi"
          "ut" "aliquid" "ex" "ea" "commodi" "consequatur""? Quis" "autem" "vel"
          "eum" "iure" "reprehenderit" "qui" "in" "ea" "voluptate" "velit"
          "esse" "quam" "nihil" "molestiae" "consequatur" "vel" "illum" "qui"
          "dolorem" "eum" "fugiat" "quo" "voluptas" "nulla" "pariatur"
          "At" "vero" "eos" "et" "accusamus" "et" "iusto" "odio" "dignissimos"
          "ducimus" "qui" "blanditiis" "praesentium" "voluptatum" "deleniti"
          "atque" "corrupti" "quos" "dolores" "et" "quas" "molestias"
          "excepturi" "sint" "obcaecati" "cupiditate" "non" "provident" "similique" "sunt" "in"
          "culpa" "qui" "officia" "deserunt" "mollitia" "animi" "id" "est" "laborum" "et" "dolorum"
          "fuga" "Et" "harum" "quidem" "rerum" "facilis" "est" "et" "expedita" "distinctio" "Nam"
          "libero" "tempore" "cum" "soluta" "nobis" "est" "eligendi" "optio" "cumque" "nihil"
          "impedit" "quo" "minus" "id" "quod" "maxime" "placeat" "facere" "possimus" "omnis"
          "voluptas" "assumenda" "est" "omnis" "dolor" "repellendus" "Temporibus" "autem"
          "quibusdam" "et" "aut" "officiis" "debitis" "aut" "rerum" "necessitatibus" "saepe"
          "eveniet" "ut" "et" "voluptates" "repudiandae" "sint" "et" "molestiae" "non"
          "recusandae" "Itaque" "earum" "rerum" "hic" "tenetur" "a" "sapiente" "delectus" "ut" "aut"
          "reiciendis" "voluptatibus" "maiores" "alias" "consequatur" "aut" "perferendis"
          "doloribus" "asperiores" "repellat"))

;;; benchmark maker

(defun run-benchmark (matcher name)
  (format t "~&-------------------- ~a ----------------------------------------~&" name)
  (list
   (block nil
     (let ((fn (handler-case (compile nil (fibonacci-form matcher))
                 (error (c)
                   (print c)
                   (return nil)))))
       (format t "~&Running Fibonacci")
       (time (iter (repeat 200)
                   (collect (funcall fn 32))))))

   (block nil
     (let ((fn (handler-case (compile nil (gomoku-form matcher))
                 (error (c)
                   (print c)
                   (return nil)))))
       (format t "~&Running Gomoku")
       (time
        (iter outer
              (repeat 100)
              (iter (for test in *gomoku-testset*)
                    (case (funcall fn test)
                      (0 (in outer (counting t into black)))
                      (1 (in outer (counting t into white)))))
              (finally
               (format t "~& Black : White : Notany = ~a : ~a : ~a "
                       black white
                       (- (* 30 (length *gomoku-testset*))
                          black white))
               (return-from outer (list black white)))))))
   #+nil
   (let ((fn (compile nil (eratosthenes-form matcher)))
         (input 100000))
     (format t "~&Running Eratosthenes-sieve with input=~a" input)
     (let ((res (time (funcall fn input))))
       (format t "~&~a primes" (length res))
       res))
   (block nil
     (let ((fn (handler-case (compile nil (strmatch-form matcher))
                 (error (c)
                   (print c)
                   (return nil)))))
       (format t "~&Running String-match")
       (time (let ((sum (iter (repeat 10000)
                              (summing
                               (iter (for word in *longstr*)
                                     (count (funcall fn word)))))))
               (format t "~& Matched ~a times" sum)
               sum))))))

(defun run-benchmarks (&optional *optimization*)
  (let ((balland (let ((*optimizer* :balland2006))
                  (run-benchmark 'trivia:match :balland)))
        (optima (run-benchmark 'optima:match :optima))
        (trivia (let ((*optimizer* :trivial))
                  (run-benchmark 'trivia:match :trivial))))
    (print balland)
    (print optima)
    (print trivia)))



#+nil
(print
 (let ((*optimizer* :balland2006))
   (sb-cltl2:macroexpand-all
    (gomoku-form 'trivia:match))))

;; (sb-cltl2:macroexpand-all
;;  (nimoku-form 'trivia:match))
