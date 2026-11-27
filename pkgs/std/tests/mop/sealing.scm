;; mop/sealing and mop/precompute: sealed and final classes, sealed generics,
;; and generic-precompute!.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature sealing")

(define-class sl-final () () #:final #t)
(define-class sl-sealed () () #:sealed #t)
(define-class sl-open () ())
(class-seal! sl-open)

(test-group "classes"
  (test-assert "a final class is sealed and final" (and (class-sealed? sl-final) (class-final? sl-final)))
  (test-assert "a sealed class is sealed" (class-sealed? sl-sealed))
  (test-assert "class-seal! seals a class" (class-sealed? sl-open))
  (test-error "a sealed class cannot be subclassed"
    (make-class 'sl-bad (list sl-open) '())))

(define-generic sl-gf :sealed #t)

(test-group "generics"
  (test-assert "a sealed generic reports sealed" (generic-sealed? sl-gf))
  (test-error "a sealed generic refuses methods"
    (add-method! sl-gf (car (generic-methods sl-pre))))
  (test-assert "generic-unseal! lifts the seal"
    (begin (generic-unseal! sl-gf) (not (generic-sealed? sl-gf)))))

(define-class sl-x () ())
(define-generic sl-pre)
(define-method (sl-pre (o sl-x)) 'pre)
(test-group "precomputation"
  (test-assert "generic-precompute! accepts the default classes"
    (begin (generic-precompute! sl-pre) #t))
  (test-equal "the generic answers after precomputation" 'pre
    (sl-pre (make sl-x)))
  (test-assert "generic-flush-cache! empties the caches"
    (begin (generic-flush-cache! sl-pre) #t))
  (test-equal "the generic answers after a flush" 'pre
    (sl-pre (make sl-x))))

(test-group "unsealing with compiled callers"
  (define-class <us-c> () ((v #:init-value 1)))
  (define-generic us-gf 1)
  (define-method (us-gf (x <us-c>)) 'old)
  (define (call-it o) (us-gf o))
  (define o (make <us-c>))
  ;; Warm the caller enough for the JIT to compile and inline it
  (do ([i 0 (+ i 1)]) ((= i 20000)) (call-it o))
  (generic-seal! us-gf)
  (test-eq "sealed generic still answers" 'old (call-it o))
  (generic-unseal! us-gf)
  (define-method (us-gf (x <us-c>)) 'new)
  (test-eq "the method added after unsealing is called" 'new (call-it o)))

(test-end "mop feature sealing")
