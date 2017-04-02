(defsystem "trivia.benchmark"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Benchmarking system of trivia"
  :depends-on ("trivia" "optima" "trivia.balland2006" "iterate")
  :pathname "bench/"
  :components ((:file "package")
               (:file "definitions"))
  :in-order-to ((test-op (test-op "trivia.benchmark/run"))))

(defsystem "trivia.benchmark/run"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "System that runs the benchmark"
  :depends-on ("trivia.benchmark")
  :pathname "bench/"
  :components ((:file "run")))
