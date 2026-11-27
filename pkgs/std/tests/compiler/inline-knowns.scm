;;; Calls of procedures that (tests compiler inline-provider) exports as inline knowns. The
;;; importer gets copies of their bodies; each test checks that a copy computes what the call
;;; would, including when one body is spliced several times into one expression.
(import (rnrs) (srfi 64) (tests compiler inline-provider))

(test-begin "inline-knowns")

(test-equal "a leaf body" 6 (add1 5))
(test-equal "if and let, low" 0 (clamp -3 0 10))
(test-equal "if and let, high" 10 (clamp 99 0 10))
(test-equal "if and let, inside" 4 (clamp 4 0 10))
(test-equal "a body that calls an inline body" 7 (twice-sum 2 3))
(test-equal "the same body twice in one expression"
  '(2 . 11) (cons (add1 1) (add1 10)))
(test-equal "nested splices of a body with a let"
  5 (clamp (clamp 7 0 6) 5 5))
(test-equal "first-or of a pair" 'a (first-or '(a b) 'none))
(test-equal "first-or of an atom" 'none (first-or 42 'none))
(test-equal "pair-swap" '(2 . 1) (pair-swap (cons 1 2)))
(test-equal "a body with a private variable is called" 42 (uses-private 1))
(test-equal "assigned state is shared" '(1 2 3)
  (let* ([a (counter-next)] [b (counter-next)] [c (counter-next)]) (list a b c)))
(test-equal "make-box and box-ref" 'v (box-ref (make-box 'v)))

;; the arguments are evaluated once each, whatever the body does with them
(let ([evaluations 0])
  (define (arg v) (set! evaluations (+ evaluations 1)) v)
  (test-equal "clamp of side-effecting arguments" 3 (clamp (arg 3) (arg 0) (arg 9)))
  (test-equal "each argument evaluated once" 3 evaluations))

;; a procedure of an inline known still works as a value
(test-equal "an inlined procedure passed as a value" '(2 3 4) (map add1 '(1 2 3)))

(test-end "inline-knowns")
