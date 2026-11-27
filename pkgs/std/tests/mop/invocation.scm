;; mop/invocation: the invocation protocol generics exist with standard
;; methods, and they answer for a standard generic.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature invocation")

(define-class iv-a () ())
(define-generic iv-f)
(define-method (iv-f (o iv-a) (n <number>)) (* n 2))
(define-method (iv-f (o iv-a) (n <number>)) #:before 'before)

(test-group "protocol generics"
  (test-assert "compute-applicable-methods is a generic"
    (generic? compute-applicable-methods))
  (test-equal "compute-applicable-methods finds both methods"
    2
    (length (compute-applicable-methods iv-f (list (make iv-a) 1))))
  (test-assert "compute-applicable-methods-using-classes answers"
    (list? (compute-applicable-methods-using-classes iv-f (list iv-a <number>))))
  (test-assert "compute-effective-method-function is a procedure"
    (procedure? (compute-effective-method-function iv-f
                  (compute-applicable-methods iv-f (list (make iv-a) 1)))))
  (test-assert "make-method-lambda is a generic"
    (generic? make-method-lambda)))

(test-group "dispatch is unchanged by the protocol"
  (test-equal "the generic still answers natively" 6
    (iv-f (make iv-a) 3)))

(test-end "mop feature invocation")
