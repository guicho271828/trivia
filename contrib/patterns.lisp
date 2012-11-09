(in-package :optima.contrib)

(defpattern alist (&rest args)
  `(and ,@(loop for (key . value) in args
                collect `(assoc ,key ,value))))

(defpattern plist (&rest args)
  `(and ,@(loop for (key . value) in (plist-alist args)
                collect `(optima::passoc ,key ,value))))
