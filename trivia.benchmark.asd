(defsystem :trivia.benchmark
  :depends-on (:trivia :optima :trivia.emilie2006 :iterate)
  :pathname "bench/"
  :components ((:file "definitions"))
  :in-order-to ((test-op (load-op :trivia.benchmark.run))))
 
(defsystem :trivia.benchmark.run
  :depends-on (:trivia.benchmark)
  :pathname "bench/"
  :components ((:file "run")))
