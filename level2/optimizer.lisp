;;; optimizer specification

(in-package :optima.level2.impl)

#|
The compatibility of test-forms of guard1 pattern is determined in
form-to-form basis. Two test-forms of a guard1 patterns are equivalent
if they are `equal' except for the specified symbol.

c.f.

   Le Fessant, Fabrice, and Luc Maranget. Optimizing pattern matching.
   ACM SIGPLAN Notices. Vol. 36. No. 10. ACM, 2001.

   Emilie Balland, Pierre-Etienne Moreau. Optimizing pattern matching compilation by program
   transformation. [Technical Report] 2006, pp.19. <inria-00001127v2>

In (Emillie 2006), their optimization rule reduces the number of
assignments (let) and tests (if).  However, since current state-of-the-art
common lisp implementations (namely, sbcl and ccl) eliminates unnecessary
assignments by default, so we do not focus on the assignments in our compiler.

The main focus is on reducing the number of conditional check, which may
involve a function call and is costly. Assignment optimization in Section
3.1, e.g., Constant Propagation, Dead Variable Elimination, Inlining,
Fusion (of let) are not considered.

Instead, we implement Section 3.2 Reducing the number of tests, which
describes: Fusion, Interleaving, Ifswapping.

|#

;;; If-Fusion

;; Consider the following match:

#+nil
(match1 what
  ((guard1 it (consp it)
           (car it) (guard1 x (= 1 x))
           (cdr it) (guard1 y (null y))) body1)
  ((guard1 it (consp it)
           (car it) (guard1 x (stringp x))
           (cdr it) (guard1 y (null y))) body2))

;; body1 has an environment where
;; 
;;  it <-- (consp it) <-- can be infered as type `cons'
;;  car <-- (= 1 car) <-- not inferred right now: an anonymous type e.g. #:ANON0
;;  cdr <-- (null y)  <-- type `null'
;; 
;; body2 has an environment where
;; 
;;  it <-- (consp it) <-- can be infered as type `cons'
;;  car <-- (stringp x) <-- can be infered as type `string'
;;  cdr <-- (null y)  <-- type `null'

;; since the two checks have type `cons' in common, the first check can be
;; merged. In the above case, the original code is compiled into:

#+nil
(match what
  ((guard1 it (consp it) (car it) #:car (cdr it) #:cdr)
   (match* (#:car #:cdr)
     (((guard x (= 1 x))     (guard y (null y))) body1)
     (((guard x (stringp x)) (guard y (null y))) body2))))

;;; Interleaving

;; Consider the following match is done under the environment in which `what' is known to be of type `list'.

#+nil
(match1 what
  ((guard1 it (consp it)) body1)
  ((guard1 it (null  it)) body2))

;; since `cons' and `null' are the exhaustive partition of type `list', this can be optimized into

#+nil
(match1 what
  ((guard1 it (consp it)) body1)
  (_                      body2))

;; to avoid checks.

;; Note: in (Emillie 2006), 2 variations of interleaving rule is proposed, one
;; general case, and the other specialized case if i'_1 and i'_2 being nop.
;; As a good news, in optima's context, i'_1 and i'_2 are always nop, and
;; exactly 1 clause should match at a time.

;; Note: In order to calculate the applicability of this rule, information about
;; the environment is essential.  however, we try not to use cltl2
;; environment as of now, since it is out of scope of optima: Conditional
;; expression may be removed using the outside environment, but we focus on
;; the removal of the tests inside optima.

;; Quoting (Emillie 2006):
;; 
;; IfInterleaving:
;;  if(c1,i1,i'1); if(c2,i2,nop) → if(c1,i1,i'1;if(c2,i2,nop)) IF c1⊥c2
;;  if(c1,i1,nop);if(c2,i2,i'2)  → if(c2,i2,if(c1,i1,nop);i'2) IF c1⊥c2
;; 
;; These two rules reduce the number of tests at run time because one of the tests is
;; moved into the “else” branch of the other. The second rule can be instantiated and used
;; to swap blocks. When i'1 and i'2 are reduced to the instruction nop, the second rule can be
;; simplified into:
;; 
;; if(c1,i1,nop);if(c2,i2,nop)→if(c2,i2,if(c1,i1,nop)) IF c1⊥c2

;;; Swapping

;; Above interleaving rule only applies when the two checks are
;; adjacsent. Therefore, we swap the order of patterns.

;; Quoting (Emillie 2006):
;; 
;;    After all, we obtain the following rule corresponding to the swapping of two conditional
;;    adjacent blocks. This rule does not optimize the number of tests but is useful to join blocks
;;    subject to be merged thanks to a smart strategy.
;; 
;;    IfSwapping: if(c1,i1,nop);if(c2,i2,nop)→if(c2,i2,nop);if(c1,i1,nop) IF c1⊥c2
