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



(defpattern read (pattern)
  "The current matching object should be a string.
The result of reading the string by read-from-string is bound to the subpattern.
Useful for simple parsing.

END-OF-FILE and PARSE-ERROR are ignored and the matching will be successfull.
Subpattern is matched against NIL."
  #+nil `(access #'read-from-string ,pattern) ;; slow, untyped
  (with-gensyms (it)
    `(guard1 (,it :type string) (stringp ,it)
             (handler-case (read-from-string ,it)
               (end-of-file ())
               (parse-error ())) ,pattern)))

(defpattern last (subpattern &optional (n 1))
  "Matches against a list, and matches subpatterns against N last elements obtained by CL:LAST."
  (check-type n (integer 0))
  (with-gensyms (it)
    `(guard1 (,it :type list) (listp ,it)
             (last ,it ,n)
             ,subpattern)))


(defpattern member (list &rest args &key (key nil) (test 'eql))
  "Matches against an object O that satisfies (member O LIST <args>)."
  (with-gensyms (it)
    `(guard1 (,it :type ,(if (and (constantp list)
                                  (eq test 'eql)
                                  (eq key nil))
                             `(member ,@(eval list))
                             t))
             (member ,it ,list ,@args))))
