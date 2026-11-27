;; memv, memq and member on a short quoted list are expanded into a chain of
;; comparisons. The expansion must keep their meaning.
(import (srfi 64) (rnrs))

(test-begin "core compiler list search expansion")

(define calls 0)
(define (key x) (set! calls (+ calls 1)) x)

(define-syntax counting
  (syntax-rules ()
    [(_ expr) (begin (set! calls 0) (let ([v expr]) (list v calls)))]))

(test-equal "memv evaluates its key once" '((#\b #\c) 1) (counting (memv (key #\b) '(#\a #\b #\c))))
(test-equal "memv evaluates a missing key once" '(#f 1) (counting (memv (key #\z) '(#\a #\b #\c))))
(test-equal "memq evaluates its key once" '((b c) 1) (counting (memq (key 'b) '(a b c))))
(test-equal "member evaluates its key once" '(("b" "c") 1) (counting (member (key "b") '("a" "b" "c"))))
(test-equal "a one-element list" '((x) 1) (counting (memq (key 'x) '(x))))

(define (first-tail x) (memv x '(a b a)))
(test-equal "a repeated datum answers with its first tail" '(a b a) (first-tail 'a))
(test-equal "memq too" '(b c b) (let ([x 'b]) (memq x '(a b c b))))
(test-equal "numbers use eqv?" '(2.5 3) (let ([x 2.5]) (memv x '(1 2.5 3))))
(test-equal "no match" #f (let ([x 'q]) (memq x '(a b c))))

(test-end)
