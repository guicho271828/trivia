
(defpackage :trivia.level0
  (:use :cl)
  (:export #:match0
           #:ematch0
           #:lambda-match0
           #:lambda-ematch0))

(in-package :trivia.level0)

'once-only

(in-package :alexandria)

;; from alexandria, but adds `ignorable'
(defmacro trivia.level0::once-only (specs &body forms)
  (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                   gensyms names-and-forms))
          (declare (ignorable ,,@gensyms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

