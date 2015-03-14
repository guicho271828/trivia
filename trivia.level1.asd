(asdf:defsystem :trivia.level1
  :description "Core patterns"
  :version "0.1"
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:trivia.level0)
  :pathname "level1/"
  :serial t
  :components ((:file "package")
               (:file "impl"))
  :in-order-to ((test-op (load-op :trivia.level1.test))))
