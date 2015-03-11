(asdf:defsystem :trivia.benchmark
  :depends-on (:trivia :optima :trivia.emilie2006 :iterate)
  :pathname "bench/"
  :components ((:file "definitions")))
