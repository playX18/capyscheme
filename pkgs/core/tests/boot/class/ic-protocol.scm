(import (srfi 64)
        (rnrs)
        (capy))

(test-begin "boot class ic protocol")

;; The slot protocol is decided per class: a user method on
;; `slot-ref-using-class` changes the classes it can apply to, and leaves
;; every other class's caches alone.

(define-class <pr-plain> () ((v #:init-value 1)))
(define-class <pr-base> () ())
(define-class <pr-logged> (<pr-base>) ((v #:init-value 2)))
(define-class <pr-other> () ((v #:init-value 3)))

(define plain (make-instance <pr-plain>))
(define logged (make-instance <pr-logged>))
(define other (make-instance <pr-other>))

(define (read-v o) (slot-ref o 'v))
(define (warm!)
  (do ([i 0 (+ i 1)]) ((= i 3000))
    (read-v plain) (read-v logged) (read-v other)))

(warm!)
(test-equal "warm reads" '(1 2 3) (list (read-v plain) (read-v logged) (read-v other)))

(define events '())

;; A method specialized on a class nothing is cached for fires nothing.
(define-class <pr-unused> () ((v #:init-value 0)))
(define before (%ic-dependents-cleared))
(define-method (slot-ref-using-class (class <class>) (obj <pr-unused>) slot . fallback)
  (next-method))
(test-equal "unrelated method clears no cache" before (%ic-dependents-cleared))

;; A method on <pr-base> affects <pr-logged> only.
(define-method (slot-ref-using-class (class <class>) (obj <pr-base>) slot . fallback)
  (set! events (cons (slot-definition-name slot) events))
  (next-method))
(test-assert "method clears the affected class's caches"
  (> (%ic-dependents-cleared) before))

(set! events '())
(test-equal "logged class runs the method" 2 (read-v logged))
(test-equal "method saw the read" '(v) events)
(set! events '())
(test-equal "plain class unaffected" 1 (read-v plain))
(test-equal "other class unaffected" 3 (read-v other))
(test-equal "no event for unaffected classes" '() events)

;; Hot again: still correct, and still going through the method.
(set! events '())
(do ([i 0 (+ i 1)]) ((= i 100)) (read-v logged))
(test-equal "logged class stays on the method" 100 (length events))

;; The cleared count does not move for reads of unaffected classes.
(define settled (%ic-dependents-cleared))
(warm!)
(test-equal "rewarming fires nothing" settled (%ic-dependents-cleared))

(test-end "boot class ic protocol")
