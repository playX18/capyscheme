;; Generic dispatch benchmark.
(import (rnrs) (scheme time) (capy))

(define-class <gd-a> () ())
(define-class <gd-b> (<gd-a>) ())
(define-class <gd-c> (<gd-a>) ())
(define-class <gd-d> (<gd-a>) ())

(define-generic gd-area)
(define-method (gd-area (o <gd-a>)) 1)
(define-method (gd-area (o <gd-b>)) 2)
(define-method (gd-area (o <gd-c>)) 3)
(define-method (gd-area (o <gd-d>)) 4)

(define-generic gd-pair)
(define-method (gd-pair (x <gd-a>) (y <gd-a>)) 1)
(define-method (gd-pair (x <gd-b>) (y <gd-c>)) 2)


;; A slot reader (): a plain accessor on a Scheme instance.
(define-class <gd-s> () ((v #:init-value 7 #:accessor gd-v)))
(define gd-slot-obj (make <gd-s>))

;; A next-method chain (/): every method calls next-method.
(define-class <gd-n> () ())
(define-class <gd-m> (<gd-n>) ())
(define-generic gd-chain)
(define-method (gd-chain (o <gd-n>)) 1)
(define-method (gd-chain (o <gd-m>)) (+ 1 (next-method)))

;; A sealed, final class with one method (/ candidate).
(define-class <gd-f> () () #:sealed #t)
(define-generic gd-sealed)
(define-method (gd-sealed (o <gd-f>)) 5)
(define gd-final-obj (make <gd-f>))

;; A generic over six classes (: a compare chain, past the four-shape arms).
(define-class <gd-s1> (<gd-a>) ())
(define-class <gd-s2> (<gd-a>) ())
(define-class <gd-s3> (<gd-a>) ())
(define-class <gd-s4> (<gd-a>) ())
(define-class <gd-s5> (<gd-a>) ())
(define-generic gd-six)
(define-method (gd-six (o <gd-a>)) 1)
(define-method (gd-six (o <gd-s1>)) 2)
(define-method (gd-six (o <gd-s2>)) 3)
(define-method (gd-six (o <gd-s3>)) 4)
(define-method (gd-six (o <gd-s4>)) 5)
(define-method (gd-six (o <gd-s5>)) 6)
(define gd-six-objs
  (vector (make <gd-a>) (make <gd-s1>) (make <gd-s2>) (make <gd-s3>)
          (make <gd-s4>) (make <gd-s5>)))

;; A sealed chain: a final class under a sealed one, a next-method chain.
(define-class <gd-sc0> () () #:sealed #t #:allow-subclasses #t)
(define-class <gd-sc1> (<gd-sc0>) () #:sealed #t)
(define-generic gd-sealed-chain)
(define-method (gd-sealed-chain (o <gd-sc0>)) 1)
(define-method (gd-sealed-chain (o <gd-sc1>)) (+ 1 (next-method)))
(define gd-sc-obj (make <gd-sc1>))

;; A subclass receiver (): the site profiles on a non-final class and sees
;; one of its subclasses, which has no method of its own.
(define-class <gd-p> () ())
(define-class <gd-q> (<gd-p>) ())
(define-generic gd-sub)
(define-method (gd-sub (o <gd-p>)) 6)
(define gd-sub-objs (vector (make <gd-p>) (make <gd-q>)))

;; A taking site: the call's value feeds `+` (the return point takes values).
(define calls 30000000)

;; One call site per case, so each site's profile holds its own shape.
(define (mono objs)
  (let ([o (vector-ref objs 0)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (loop (+ i 1) (+ sum (gd-area o)))))))

(define (poly objs)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (loop (+ i 1) (+ sum (gd-area (vector-ref objs (modulo i n)))))))))

(define (pair x y)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (gd-pair x y))))))

(define (slot-read o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (gd-v o))))))

(define (chain o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (gd-chain o))))))

(define (sealed o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (gd-sealed o))))))

(define (six objs)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (loop (+ i 1) (+ sum (gd-six (vector-ref objs (modulo i n)))))))))

;; The same six shapes at a site whose value is bound, not summed: the
;; return point takes one value, so the class-test chain serves it ().
(define (six-bound objs)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i calls)
          sum
          (let ([r (gd-six (vector-ref objs (modulo i n)))])
            (loop (+ i 1) (+ sum r)))))))

(define (slot-read-taking o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (+ 0 (gd-v o)))))))

(define (area-taking o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (+ 0 (gd-area o)))))))

(define (subclass-recv objs count)
  (let ([n (vector-length objs)])
    (let loop ([i 0] [sum 0])
      (if (= i count)
          sum
          (let ([r (gd-sub (vector-ref objs (modulo i n)))])
            (loop (+ i 1) (+ sum r)))))))

(define (sealed-chain o)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (gd-sealed-chain o))))))

(define objs
  (vector (make <gd-a>) (make <gd-b>) (make <gd-c>) (make <gd-d>)))

(define (timed label thunk)
  (thunk)
  (let* ([start (current-jiffy)]
         [result (thunk)]
         [elapsed (/ (exact->inexact (- (current-jiffy) start))
                     (jiffies-per-second))])
    (display (list label 'result result 'seconds elapsed))
    (newline)))

;; A Tier 2 unit compiled before a generic's method cache holds an entry
;; gets no method-cache arm, and nothing recompiles it. Each case first makes
;; its own calls directly, so its loop compiles against a warm cache.
(define (warm-case thunk)
  (do ((k 0 (+ k 1))) ((= k 50)) (thunk)))

(warm-case (lambda () (gd-area (vector-ref objs 0))))
(timed 'mono (lambda () (mono objs)))
(warm-case (lambda () (for-each (lambda (o) (gd-area o)) (vector->list objs))))
(timed 'poly4 (lambda () (poly objs)))
(warm-case (lambda () (gd-pair (vector-ref objs 1) (vector-ref objs 2))))
(timed 'pair (lambda () (pair (vector-ref objs 1) (vector-ref objs 2))))
(warm-case (lambda () (gd-v gd-slot-obj)))
(timed 'slot-read (lambda () (slot-read gd-slot-obj)))
(warm-case (lambda () (gd-chain (make <gd-m>))))
(timed 'chain (lambda () (chain (make <gd-m>))))
(warm-case (lambda () (gd-sealed gd-final-obj)))
(timed 'sealed (lambda () (sealed gd-final-obj)))
(warm-case (lambda () (for-each (lambda (o) (gd-six o)) (vector->list gd-six-objs))))
(timed 'six-shapes (lambda () (six gd-six-objs)))
(warm-case (lambda () (for-each (lambda (o) (gd-six o)) (vector->list gd-six-objs))))
(timed 'six-shapes-bound (lambda () (six-bound gd-six-objs)))
(warm-case (lambda () (gd-v gd-slot-obj)))
(timed 'slot-read-taking (lambda () (slot-read-taking gd-slot-obj)))
(warm-case (lambda () (gd-area (vector-ref objs 1))))
(timed 'area-taking (lambda () (area-taking (vector-ref objs 1))))
(warm-case (lambda () (gd-sealed-chain gd-sc-obj)))
(timed 'sealed-chain (lambda () (sealed-chain gd-sc-obj)))
;; Only the parent is warmed, by the same loop, so the unit compiles with a
;; one-class cache. The subclass instance is first dispatched in the timed
;; run, which the subclass arms () serve from that unit.
(subclass-recv (vector (vector-ref gd-sub-objs 0)) 2000000)
(timed 'subclass-recv (lambda () (subclass-recv gd-sub-objs calls)))
