(import (srfi 64)
        (rnrs)
        (capy))

(test-begin "boot class ic watchpoints")

;; Slot inline caches depend on the slot protocol staying plain. Adding a
;; `slot-ref-using-class` method after the caches warmed up must take effect
;; on every thread by the time `define-method` returns, including on a thread
;; that is spinning in a hot compiled `slot-ref` loop.

(define-class <wp-a> () ((val #:init-value 1)))
(define-class <wp-b> (<wp-a>) ())

(define a (make-instance <wp-a>))
(define b (make-instance <wp-b>))
(slot-set! b 'val 2)

(define (read-x o) (slot-ref o 'val))
(define (warm! o n) (do ([i 0 (+ i 1)]) ((= i n)) (read-x o)))

(warm! a 2000)
(warm! b 2000)

(test-equal "warm reads" '(1 2) (list (read-x a) (read-x b)))

;; A reader thread hammers the cached site and records what it saw last.
(define lock (make-mutex))
(define stop #f)
(define last-seen #f)
(define reads 0)

(define (stopped?)
  (mutex-acquire lock)
  (let ([s stop]) (mutex-release lock) s))

(fork-thread
 (lambda ()
   (let loop ([n 0])
     (let ([v (read-x a)])
       (mutex-acquire lock)
       (set! last-seen v)
       (set! reads (+ reads 1))
       (mutex-release lock))
     (unless (stopped?) (loop (+ n 1))))))

(define (wait-reads! n)
  (let loop ()
    (mutex-acquire lock)
    (let ([r reads]) (mutex-release lock)
      (when (< r n) (usleep 1000) (loop)))))

(wait-reads! 200)

(define-method (slot-ref-using-class (class <class>) (obj <wp-a>) slot . fallback)
  100)

;; The override is visible to the calling thread at once...
(test-equal "override seen by caller" 100 (read-x a))
(test-equal "override seen for subclass" 100 (read-x b))

;; ...and, because the grace period ran, the reader thread cannot still be
;; serving the old entry: its next read after this point is the override.
(define seen-before
  (begin (mutex-acquire lock) (let ([r reads]) (mutex-release lock) r)))
(wait-reads! (+ seen-before 50))
(test-equal "override seen by reader thread" 100
  (begin (mutex-acquire lock) (let ([v last-seen]) (mutex-release lock) v)))

(mutex-acquire lock) (set! stop #t) (mutex-release lock)

(test-end "boot class ic watchpoints")
