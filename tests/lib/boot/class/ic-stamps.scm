(import (srfi 64)
        (rnrs)
        (capy))

(test-begin "boot class ic stamps")

;; Slot caches check the layout stamp of the instance's storage, so a
;; redefinition must make an obsolete instance miss, migrate and read its
;; migrated value, while instances of other classes are unaffected.

(define-class <st-a> () ((p #:init-value 1) (q #:init-value 2)))
(define-class <st-b> () ((p #:init-value 10)))

(define a (make-instance <st-a>))
(define b (make-instance <st-b>))

(define (read-p o) (slot-ref o 'p))
(define (read-q o) (slot-ref o 'q))
(define (write-p! o v) (slot-set! o 'p v))
(define (warm! n)
  (do ([i 0 (+ i 1)]) ((= i n))
    (read-p a) (read-q a) (read-p b) (write-p! b (+ 10 (modulo i 3)))))

(warm! 3000)
(write-p! b 10)
(write-p! a 5)

(test-equal "warm reads" '(5 2 10) (list (read-p a) (read-q a) (read-p b)))

;; Redefine <st-a> with its slots swapped and one added: p moves from index
;; 0 to 1. A stale hit through the old index would return q's value.
(redefine-class! <st-a> '<st-a> '((q #:init-value 20) (p #:init-value 30) (r #:init-value 40)) '())

(test-equal "obsolete instance keeps p" 5 (read-p a))
(test-equal "obsolete instance keeps q" 2 (read-q a))
(test-equal "unrelated class still reads" 10 (read-p b))

(write-p! a 7)
(test-equal "write after migration lands in the new layout" 7 (read-p a))

;; A new instance of the redefined class works through the same sites.
(define a2 (make-instance <st-a>))
(test-equal "fresh instance has the added slot" 40 (slot-ref a2 'r))
(test-assert "fresh instance reads both old slots"
  (and (number? (read-p a2)) (number? (read-q a2))))

(warm! 3000)
(write-p! b 10)
(test-equal "after rewarm" '(7 2 10) (list (read-p a) (read-q a) (read-p b)))
(test-equal "added slot readable" 40 (slot-ref a 'r))

(test-end "boot class ic stamps")
