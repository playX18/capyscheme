;; mop/qualifiers: before, after and around methods, method-qualifiers and
;; remove-method!.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature qualifiers")

(define (method-with-qualifier gf qualifier)
  (let loop ([ms (generic-methods gf)])
    (cond
      [(null? ms) #f]
      [(equal? (method-qualifiers (car ms)) (list qualifier)) (car ms)]
      [else (loop (cdr ms))])))

(define-class q-a () ())
(define-class q-b (q-a) ())

(define-generic q-shape)
(define-method (q-shape (o q-a)) 'primary)
(define-method (q-shape (o q-b)) #:before 'before)
(define-method (q-shape (o q-a)) #:after 'after)
(define-method (q-shape (o q-b)) #:around (list 'around (next-method)))

(test-group "qualifiers"
  (test-equal "a before method keeps its qualifier"
    '(#:before)
    (method-qualifiers (method-with-qualifier q-shape #:before)))
  (test-equal "an unqualified method has no qualifiers" '()
    (method-qualifiers (car (generic-methods q-shape)))))

(test-group "combination"
  (test-equal "around wraps the primaries"
    '(around primary)
    (q-shape (make q-b))))

(define-generic q-plain)
(define-method (q-plain (o q-a)) 'primary)
(define-method (q-plain (o q-a)) #:before 'b)

(test-group "remove-method!"
  (test-equal "the primary answers before removal" 'primary
    (q-plain (make q-a)))
  (test-assert "remove-method! takes the before method out"
    (remove-method! q-plain (method-with-qualifier q-plain #:before)))
  (test-equal "the generic still answers with the primary" 'primary
    (q-plain (make q-a))))

(test-end "mop feature qualifiers")
