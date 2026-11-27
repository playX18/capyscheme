(import (except (rnrs) string-hash string-ci-hash)
  (srfi 64)
  (srfi 69))

(test-begin "srfi 69")

(define (sorted-alist table)
  (list-sort (lambda (a b) (< (car a) (car b))) (hash-table->alist table)))

(test-group "construction"
  (test-assert (hash-table? (make-hash-table)))
  (test-assert (hash-table? (make-hash-table eq?)))
  (test-assert (hash-table? (make-hash-table string=? string-hash)))
  (test-assert (not (hash-table? '())))
  (test-equal 0 (hash-table-size (make-hash-table)))
  (let ([t (alist->hash-table '((1 . a) (2 . b) (1 . c)) eqv?)])
    (test-equal "first association wins" 'a (hash-table-ref t 1))
    (test-equal 2 (hash-table-size t)))
  (test-eq eq? (hash-table-equivalence-function (make-hash-table eq?)))
  (test-eq string=? (hash-table-equivalence-function (make-hash-table string=?)))
  (test-assert (procedure? (hash-table-hash-function (make-hash-table equal?)))))

(test-group "ref and set"
  (define t (make-hash-table))
  (hash-table-set! t 'a 1)
  (hash-table-set! t "b" 2)
  (hash-table-set! t '(c) 3)
  (test-equal 1 (hash-table-ref t 'a))
  (test-equal "equal? keys" 2 (hash-table-ref t (string #\b)))
  (test-equal 3 (hash-table-ref t (list 'c)))
  (test-error (hash-table-ref t 'missing))
  (test-equal 'none (hash-table-ref t 'missing (lambda () 'none)))
  (test-equal 10 (hash-table-ref t 'a (lambda () 'none) (lambda (v) (* v 10))))
  (test-equal 'd (hash-table-ref/default t 'missing 'd))
  (test-assert (hash-table-exists? t 'a))
  (test-assert (not (hash-table-exists? t 'z)))
  (hash-table-delete! t 'a)
  (test-assert (not (hash-table-exists? t 'a)))
  (hash-table-delete! t 'a)
  (test-equal 2 (hash-table-size t)))

(test-group "update"
  (define t (make-hash-table eqv?))
  (hash-table-set! t 1 10)
  (hash-table-update! t 1 (lambda (v) (+ v 1)))
  (test-equal 11 (hash-table-ref t 1))
  (test-error (hash-table-update! t 2 (lambda (v) v)))
  (hash-table-update! t 2 (lambda (v) (cons 'x v)) (lambda () '()))
  (test-equal '(x) (hash-table-ref t 2))
  (hash-table-update!/default t 3 (lambda (v) (+ v 1)) 0)
  (hash-table-update!/default t 3 (lambda (v) (+ v 1)) 0)
  (test-equal 2 (hash-table-ref t 3)))

(test-group "whole table"
  (define t (alist->hash-table '((1 . one) (2 . two) (3 . three))))
  (test-equal '(1 2 3) (list-sort < (hash-table-keys t)))
  (test-equal 3 (length (hash-table-values t)))
  (test-equal '((1 . one) (2 . two) (3 . three)) (sorted-alist t))
  (test-equal 6 (hash-table-fold t (lambda (k v acc) (+ k acc)) 0))
  (let ([sum 0])
    (hash-table-walk t (lambda (k v) (set! sum (+ sum k))))
    (test-equal 6 sum))
  (let ([c (hash-table-copy t)])
    (hash-table-set! c 4 'four)
    (test-equal "copy is mutable and separate" 3 (hash-table-size t))
    (test-equal 4 (hash-table-size c)))
  (let ([c (hash-table-copy t #f)])
    (test-error (hash-table-set! c 9 'nine)))
  (let ([a (alist->hash-table '((1 . a) (2 . b)))]
        [b (alist->hash-table '((2 . B) (3 . C)))])
    (test-eq a (hash-table-merge! a b))
    (test-equal '((1 . a) (2 . B) (3 . C)) (sorted-alist a))))

(test-group "string tables"
  (define t (make-hash-table string-ci=? string-ci-hash))
  (hash-table-set! t "Hello" 1)
  (test-equal 1 (hash-table-ref/default t "HELLO" #f))
  (define u (make-hash-table string=?))
  (hash-table-set! u "Hello" 1)
  (test-equal #f (hash-table-ref/default u "HELLO" #f)))

(test-group "custom hash"
  ;; Keys are compared modulo 10.
  (define t (make-hash-table (lambda (a b) (= (mod a 10) (mod b 10)))
              (lambda (k . bound) (mod k 10))))
  (hash-table-set! t 3 'three)
  (test-equal 'three (hash-table-ref t 13))
  (hash-table-set! t 23 'twenty-three)
  (test-equal 1 (hash-table-size t)))

(test-group "hash functions"
  (test-assert ((lambda (x) (and (integer? x) (exact? x) (>= x 0))) (hash '(1 2 3))))
  (test-equal (hash "abc") (hash (string #\a #\b #\c)))
  (test-assert (< (hash '(1 2 3) 17) 17))
  (test-assert (< (string-hash "abc" 5) 5))
  (test-equal (string-ci-hash "ABC") (string-ci-hash "abc"))
  (test-assert (< (hash-by-identity 'sym 7) 7))
  (test-equal (hash-by-identity 'sym) (hash-by-identity 'sym)))

(test-group "many keys"
  (define t (make-hash-table eqv?))
  (do ([i 0 (+ i 1)]) ((= i 10000)) (hash-table-set! t i (* i i)))
  (test-equal 10000 (hash-table-size t))
  (test-equal (* 9999 9999) (hash-table-ref t 9999))
  (do ([i 0 (+ i 2)]) ((>= i 10000)) (hash-table-delete! t i))
  (test-equal 5000 (hash-table-size t)))

(test-end "srfi 69")
