(defpackage :trivia.cffi
  (:import-from :cffi :null-pointer)
  (:export :-> :null-pointer))

(defpackage :trivia.cffi.impl
  (:use :cl :alexandria :cffi
        :trivia.level0
        :trivia.level1
        :trivia.level2
        :trivia.cffi))

(in-package :trivia.cffi.impl)

(defpattern -> (foreign-type &rest slots)
  "Accesses slot values in a foreign object pointer.
Example:

    (match (cudd-regular node) ; returns a pointer
      ((-> (:struct dd-node)
         type                             ; foreign-slot-value access (leaf pattern)
         (& type)                         ; explicit foreign-slot-pointer access
         (type type)                      ; implicit foreign-slot-pointer access (nested pattern)
         (type                            ; implicit foreign-slot-pointer access (nested pattern)
          (-> (:union dd-node/type)
            value)))                      ; leaf node: value access
       value))

Note: Above example does not actually compile because it rebinds multiple patterns to a same variable.
"
  (with-gensyms (obj)
    `(guard1 (,obj :type foreign-pointer)
             (typep ,obj 'foreign-pointer)
             ,@(mappend
                (lambda (slot)
                  (ematch slot
                    ((symbol)
                     `((foreign-slot-value ,obj ',foreign-type ',slot) ,slot))
                    ((list (symbol :name "&") slot-name)
                     `((foreign-slot-pointer ,obj ',foreign-type ',slot-name) ,slot-name))
                    ((list slot-name subpattern)
                     `((foreign-slot-pointer ,obj ',foreign-type ',slot-name) ,subpattern))))
                slots))))

(defpattern null-pointer ()
  "Matches when the given foreign pointer is null. "
  (with-gensyms (obj)
    `(guard1 (,obj :type foreign-pointer)
             (null-pointer-p ,obj))))
