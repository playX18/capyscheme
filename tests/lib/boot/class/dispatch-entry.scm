(import (srfi 64)
        (capy))

(test-begin "boot class dispatch entry")

(define-class <de-a> () ((v #:init-keyword #:v #:init-value 1 #:accessor de-v)))
(define-class <de-b> (<de-a>) ())
(define-class <de-c> (<de-b>) ())

(define-generic de-depth)
(define-method (de-depth (o <de-a>)) 1)
(define-method (de-depth (o <de-b>)) (+ 1 (next-method)))
(define-method (de-depth (o <de-c>)) (+ 1 (next-method)))

(define-generic de-args)
(define-method (de-args (o <de-a>) x) (list 'a x))
(define-method (de-args (o <de-b>) x) (cons 'b (next-method)))
(define-method (de-args (o <de-c>) x) (cons 'c (next-method)))

(test-group "next-method through the in-place entry"
  (do ((i 0 (+ i 1))) ((= i 3))
    (test-equal 3 (de-depth (make <de-c>)))
    (test-equal 2 (de-depth (make <de-b>)))
    (test-equal 1 (de-depth (make <de-a>))))
  (test-equal '(c b a 7) (de-args (make <de-c>) 7))
  (test-equal '(b a 7) (de-args (make <de-b>) 7)))

(test-group "deep recursion keeps the stack consistent"
  (define-generic de-count)
  (define-method (de-count (n <fixnum>))
    (if (= n 0) 0 (+ 1 (de-count (- n 1)))))
  (test-equal 5000 (de-count 5000)))

(test-group "redefinition after warm-up"
  (define o (make <de-c>))
  (do ((i 0 (+ i 1))) ((= i 5)) (de-depth o))
  (define-method (de-depth (o <de-c>)) 100)
  (test-equal 100 (de-depth o))
  (test-equal 2 (de-depth (make <de-b>))))

(test-group "no applicable method and arity still raise"
  (test-error (de-depth 42))
  (test-error (de-args (make <de-a>))))

(test-group "make with a custom initialize"
  (define-class <de-init> () ((n #:init-keyword #:n #:init-value 0 #:accessor de-n)))
  (define o1 (make <de-init> #:n 5))
  (test-equal 5 (de-n o1))
  (define-method (initialize (o <de-init>) initargs)
    (set! (de-n o) (+ 1000 (de-n o)))
    o)
  (test-equal 1005 (de-n (make <de-init> #:n 5)))
  (test-equal 1000 (de-n (make <de-init>))))

(test-group "make argument errors"
  (test-error (make <de-a> #:nope 1))
  (test-error (make <de-a> '#:v))
  (test-error (make 42)))

(test-end "boot class dispatch entry")
