;;; Procedures that importers of this library get as inline knowns: small bodies the compiler
;;; copies into the importer (see `inline-known-body` in pkgs/core/src/boot/units.scm), and
;;; ones it must not copy. tests/compiler/inline-knowns.scm checks that the copies behave like
;;; the calls.
(library (tests compiler inline-provider)
  (export add1 clamp twice-sum first-or pair-swap uses-private counter-next
    make-box box-ref)
  (import (rnrs))

  (define (add1 x) (+ x 1))

  ;; an `if` and a `let`
  (define (clamp x lo hi)
    (let ([y (if (< x lo) lo x)])
      (if (> y hi) hi y)))

  ;; calls another inline body
  (define (twice-sum a b) (+ (add1 a) (add1 b)))

  (define (first-or x default)
    (if (pair? x) (car x) default))

  (define (pair-swap p) (cons (cdr p) (car p)))

  ;; a private variable: the body is not copied, the call stays
  (define secret 41)
  (define (uses-private x) (+ x secret))

  ;; assigned state: must not be copied (each copy would see the same variable anyway, but
  ;; `set!` keeps the body out)
  (define count 0)
  (define (counter-next)
    (set! count (+ count 1))
    count)

  (define (make-box v) (vector 'box v))
  (define (box-ref b) (vector-ref b 1)))
