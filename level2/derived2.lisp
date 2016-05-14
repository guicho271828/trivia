(in-package :trivia.level2.impl)

;; patterns defined using level2 matchers and patterns

(defpattern $guard1 (sym options test more-patterns)
  "matches against guard1 pattern (form) itself"
  (with-gensyms (symopts)
    `(list* 'guard1
            (guard1 ,symopts t
                    (preprocess-symopts ,symopts ,symopts)
                    (list* ,sym ,options))
            ,test ,more-patterns)))

(defpattern $or1 (sym options test more-patterns)
  "matches against or1 pattern (form) itself"
  (with-gensyms (symopts)
    `(list* 'or1
            (guard ,symopts t
                   (preprocess-symopts ,symopts ,symopts)
                   (list* ,sym ,options))
            ,test ,more-patterns)))

(defpattern <> (pattern value &optional (var (gensym "BIND")))
  "The current matching value is bound to `var'.
The result of evaluating `value' using `var' is then matched against `pattern'.
`var' can be omitted."
  (assert (symbolp var))
  `(guard1 ,var t ,value ,pattern))
