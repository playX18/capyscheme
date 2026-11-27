(import (rnrs)
  (srfi 64)
  (srfi 197))

(test-begin "srfi 197")

(test-group "chain"
  (test-equal 1 (chain 1))
  (test-equal 3 (chain 1 (+ _ 2)))
  (test-equal "the value is the last argument without a placeholder" '(1 2 3)
    (chain 3 (list 1 2)))
  (test-equal '(a b c) (chain 'b (list 'a _) (append _ '(c))))
  (test-equal 6 (chain '(1 2 3) (apply + _)))
  (test-equal "placeholder in operator position" 5 (chain + (_ 2 3)))
  (test-equal "multiple values into several placeholders" '(1 2)
    (chain (values 1 2) (list _ _)))
  (test-equal "ellipsis takes the rest" '(0 1 2 3)
    (chain (values 1 2 3) (list 0 _ ...)))
  (test-equal "placeholder then ellipsis" '(1 x 2 3)
    (chain (values 1 2 3) (list _ 'x _ ...)))
  (test-equal "custom placeholder" 6 (chain 3 <> (* <> 2)))
  (test-equal "custom placeholder and ellipsis" '(1 2 3)
    (chain (values 1 2 3) <> ::: (list <> :::)))
  (test-equal "steps evaluate left to right" '(c b a)
    (let ([log '()])
      (chain 'a
        ((lambda (x) (set! log (cons x log)) 'b) _)
        ((lambda (x) (set! log (cons x log)) 'c) _)
        ((lambda (x) (set! log (cons x log)) log) _))))
  (test-equal "nested lists are not searched" '((_ 1) 2)
    (chain 2 (list '(_ 1) _))))

(test-group "chain-and"
  (test-equal 4 (chain-and 1 (+ _ 1) (* _ 2)))
  (test-equal #f (chain-and 1 (memv 5 '(1 2)) (car _)))
  (test-equal #f (chain-and #f (+ _ 1)))
  (test-equal 2 (chain-and '(1 2) (memv 2 _) (car _)))
  (test-equal 3 (chain-and 3))
  (test-equal "custom placeholder" 9 (chain-and 3 % (* % 3))))

(test-group "chain-when"
  (test-equal 4 (chain-when 1 (#t (+ _ 1)) (#f (* _ 10)) (#t (* _ 2))))
  (test-equal 1 (chain-when 1))
  (let ([add? #t] [double? #f])
    (test-equal 11 (chain-when 10 (add? (+ _ 1)) (double? (* _ 2)))))
  (test-equal '(x 1) (chain-when 1 ((odd? 1) (list 'x _)))))

(test-group "chain-lambda"
  (test-equal 7 ((chain-lambda (+ _ 1) (* _ 2) (- _ 1)) 3))
  (test-equal '(2 1) ((chain-lambda (list _ _) (reverse _)) 1 2))
  (test-equal 10 ((chain-lambda (+ _ ...)) 1 2 3 4))
  (test-equal '(1) ((chain-lambda (list))  1))
  (test-equal 6 ((chain-lambda <> (* <> <>)) 2 3)))

(test-group "nest"
  (test-equal '(1 (2 3)) (nest (list 1 _) (list 2 _) 3))
  (test-equal 3 (nest 3))
  (test-equal "nest substitutes syntax" 2
    (nest (let ([x 1]) _) (+ x 1)))
  (test-equal "custom placeholder" '(a (b c)) (nest <> (list 'a <>) (list 'b <>) 'c))
  (test-equal '(1 (2 3)) (nest-reverse 3 (list 2 _) (list 1 _)))
  (test-equal 5 (nest-reverse 5))
  (test-equal '(a (b c)) (nest-reverse 'c <> (list 'b <>) (list 'a <>))))

(test-end "srfi 197")
