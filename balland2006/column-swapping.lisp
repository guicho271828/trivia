(in-package :trivia.balland2006)

;;; Column-swapping

;; Swapping rule in Balland2006 swaps two clauses on each application. This
;; is called a row swapping, in a sense. However, another kind of swapping
;; can be considered, called column-swapping. This is analogous to changing
;; the ordering of variable in Ordered-BDD, which severely affects the
;; quality of the decision tree (= size of the decision tree, number of
;; nodes in a decision tree, or a number of test-forms in a matching
;; tree). It does not increase the number of checks.

;; quote from Rice, Kulhari, "A Survey of Static Variable Ordering
;; Heuristics for Efficient BDD/MDD Construction" :

;;     The problem of finding an optimal variable ordering for Binary
;;     Decision Diagrams (BDD) or Multi-Valued Decision Diagrams (MDD) is
;;     widely known to be NP-Complete. ... in order to minimize the overall
;;     size of the resulting decision diagram.

;;; Dependency Between Patterns

;; One problem in applying these techniques to pattern matching is the
;; dependency between the test forms. For example, A check on a value of a
;; slot of an object should be done after the parent object is
;; checked. However, this itself is largely no problem since the
;; subpatterns of guard1 are considered dependent on the pattern.

;; The remaining degree of freedom is in the ordering of subpatterns, and
;; the ordering of multipatterns. Since subpatterns are compiled into
;; multipatterns when a fusion occurs, this can simply be reduced to a
;; problem of swapping and changing the ordering of multipatterns.

;; User-supplied multipatterns are not safe, since they
;; may contain references to the variables in the earlier patterns, e.g.,
;; 
;; (match* (1 2 3) (((list a b c) (eq a) (eq b)) c)) 

;; We check if the variables in (list a b c) appears in (eq a) and (eq b).
;; Now this only applies to the symbols used in the pattern. Since any
;; implicitly defined symbols are supposed to be created by gensyms, they
;; would not overwrap with each other, and thus, we can safely assume that
;; checking the value returned by `variables' is enough. Note that this
;; does not require any type information.

(defun pattern-dependent (p1 p2)
  "return true if p2 depends on p1"
  (some (lambda (symopt) (find-tree (car symopt) p2))
        (variables p1)))

(defun find-tree (obj tree &key (test #'eql))
  (labels ((rec (tree)
             (match tree
               ((list) nil)
               ((list* (and thing (type list)) rest)
                (or (funcall test obj thing)
                    (some #'rec thing)
                    (rec rest)))
               ((list* thing rest)
                (or (funcall test obj thing)
                    (rec rest))))))
    (rec tree)))

(defun pattern-dependencies (patterns)
  (let ((pv (coerce patterns 'vector)))
    (iter outer
          (for p1 in-vector pv with-index i)
          (iter (for p2 in-vector pv with-index j from (1+ i))
                (when (pattern-dependent p1 p2)
                  (in outer (collect (list i j))))))))

;;; Variable Ordering

;; Next, implement the ordering method that satisfy the above
;; dependency-based ordering.

;; Quote from Rice, Kulhari:

;;     The problem of finding an optimal variable ordering for constructing
;;     a minimum-size decision diagram (in terms of the number of nodes
;;     needed to encode the function) is known to be NP-complete [5].
;;
;;     The complexity of the variable ordering problem for decision diagram
;;     construction has forced researchers to establish many efficient
;;     heuristic and meta-heuristic approaches toward establishing
;;     near-optimal variable orderings for efficient BDD and
;;     MDD construction.
;;
;;     There are generally considered to be two classes of heuristic
;;     techniques for establishing efficient variable orderings for
;;     decision diagram construction: static variable ordering and dynamic
;;     variable ordering. Static variable ordering techniques attempt to
;;     establish the variable ordering prior to constructing the actual
;;     decision diagram, while dynamic variable ordering techiques attempt
;;     to adjust the ordering online during the actual construction of the
;;     decision diagram.

;;     Since static heuristics generate the final variable ordering before
;;     construction of the decision diagram even begins, there is no
;;     guarantee of good quality solutions from the resulting
;;     order. Alternatively, since dynamic variable ordering allows for
;;     adjusting the variable order during the actual construction of the
;;     decision diagram, they are generally considered more effective in
;;     pro- viding efficient orderings; however, they are also typically
;;     much more time consuming in practice than the simpler, static
;;     heuristics, and thus, are often considered less practical.

;; now in this implementation, we want to implement dynamic ordering of
;; course :) however, since that paper only describes static ordering, I
;; implement static ordering methods only.


;;; Static Ordering Methods


;;;; 2.1 Topological ordering
;; search from the exit node in breadth-first : work quite well on tree-like circuits.
;; simple, therefore "baseline technique for comparison"
;;;; 2.2 Influencial ordering
;; the most influential of the primary inputs to the function/circuit are
;; placed earlier on in the ordering.
;;;;; 2.2.1 Fanin Heuristics
;;;;; 2.2.2 Subgraph Complexity
;;;;; 2.2.3 dependent count
;;;; 2.3 Priority Ordering
;;;;; 2.3.1 Variable Appending
;;;;; 2.3.2 Variable Interleaving
;;;; 2.4 EVAL Meta-Heuristic
;;;; 2.5 Metric Optimization Heuristics
;;;;; 2.5.1 Smallest Communication Graph
;;;;; 2.5.2 Normalized Average Lifetime
;;;;; 2.5.3 Variable Weighting
;;;;; 2.5.4 Hybrid Metric Optimization
;;;;; 2.5.5 MINCE
;;;;; 2.5.6 FORCE
;;;;; 2.5.7 Weighted Event Span
