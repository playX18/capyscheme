;; mop/dfun and mop/custom-dfun: compute-discriminating-function, generic
;; function classes, set-funcallable-instance-function!, invocable instances.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature dfun")

(define dfun-trace '())
(define-class dfun-gf (<standard-generic-function>) ())
(define-method compute-discriminating-function ((gf dfun-gf))
  (let ([standard (next-method)])
    (lambda args
      (set! dfun-trace (cons args dfun-trace))
      (apply standard args))))

(define-generic dfun-describe #:generic-function-class dfun-gf)
(define-method (dfun-describe (n <number>)) (list 'number n))

(test-group "standard discriminating function"
  (test-equal "the standard dfun answers through the methods" '(number 3)
    (dfun-describe 3))
  (test-equal "a custom dfun saw the call" '((3)) dfun-trace)
  (test-assert "standard-discriminating-function is a procedure"
    (procedure? standard-discriminating-function)))

(test-group "installed procedure"
  (set-funcallable-instance-function! dfun-describe (lambda (x) (list 'replaced x)))
  (test-equal "an installed procedure answers instead" '(replaced 4)
    (dfun-describe 4))
  (set-funcallable-instance-function! dfun-describe #f)
  (test-equal "clearing the procedure restores dispatch" '(number 5)
    (dfun-describe 5)))

(test-group "ensure-generic-function"
  (define made (ensure-generic-function 'dfun-made dfun-gf))
  (define-method (made (n <number>)) (* n n))
  (test-equal "a generic made by ensure-generic-function uses the class" 9
    (made 3)))

(define labelled
  (make-invocable-class 'dfun-labelled '(procedure label)))
(test-group "invocable instances"
  (test-equal "an invocable instance calls its procedure" '(first 1)
    (let ([obj (make-instance labelled
                 #:procedure (lambda (self x) (list (slot-ref self 'label) x))
                 #:label 'first)])
      (obj 1))))

(test-end "mop feature dfun")
