(defpackage :trivia.benchmark
  (:use :cl :alexandria :iterate :trivia :trivia.level1)
  (:export
   #:run-benchmarks)
  (:shadow :next))
