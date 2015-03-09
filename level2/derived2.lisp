(in-package :trivia.level2.impl)

;; patterns defined using level2 matchers and patterns

;; matches against guard1 pattern (form) itself
(defpattern $guard1 (sym options test &rest more-patterns)
  (with-gensyms (symopts)
    `(list* 'guard1
            (guard1 ,symopts t
                    (preprocess-symopts ,symopts ,symopts)
                    (list* ,sym ,options))
            ,test ,@more-patterns)))

(defpattern $or1 (sym options test &rest more-patterns)
  (with-gensyms (symopts)
    `(list* 'or1
            (guard ,symopts t
                   (preprocess-symopts ,symopts ,symopts)
                   (list* ,sym ,options))
            ,test ,@more-patterns)))

