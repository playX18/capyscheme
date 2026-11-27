;; mop/resolved-keys: generics that specialize on funcallable classes dispatch
;; on resolved class ids, so a generic object and an invocable instance pick
;; the method of their own class.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature resolved keys")

(define rk-gf (make-invocable-class 'rk-callable '(procedure)))
(define rk-obj (make-instance rk-gf #:procedure (lambda (self) 'called)))

(define-generic rk-kind)
(define-method (rk-kind (o <top>)) 'other)
(define-method (rk-kind (o <generic>)) 'generic)
(define-method (rk-kind (o <number>)) 'number)

(test-group "resolved class keys"
  (test-equal "a generic object is dispatched as a generic" 'generic
    (rk-kind rk-kind))
  (test-equal "an unrelated object takes the top method" 'other
    (rk-kind 'symbol))
  (test-equal "numbers keep their method" 'number
    (rk-kind 4)))

(define-generic rk-invocable)
(define-method (rk-invocable (o <top>)) 'top)
(define-method (rk-invocable (o rk-gf)) 'callable)

(test-group "invocable instances"
  (test-equal "an instance of the invocable class takes its method" 'callable
    (rk-invocable rk-obj))
  (test-equal "a plain value takes the top method" 'top
    (rk-invocable 1)))

(test-end "mop feature resolved keys")
