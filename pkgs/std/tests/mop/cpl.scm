;; mop/cpl: class precedence lists follow the CLOS topological sort.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature cpl")

(define-class cpl-food () ())
(define-class cpl-spice (cpl-food) ())
(define-class cpl-cinnamon (cpl-spice) ())
(define-class cpl-fruit (cpl-food) ())
(define-class cpl-apple (cpl-fruit) ())
(define-class cpl-pie (cpl-apple cpl-cinnamon) ())

(test-group "CLHS pie example"
  (test-equal "the CLOS order, with the classes before <object>"
    (list cpl-pie cpl-apple cpl-fruit cpl-cinnamon cpl-spice cpl-food <object> <top>)
    (class-precedence-list cpl-pie)))

(define-class cpl-a () ())
(define-class cpl-b (cpl-a) ())
(define-class cpl-c (cpl-a) ())
(define-class cpl-d (cpl-b cpl-c) ())

(test-group "diamond"
  (test-equal "a shared superclass comes after both branches"
    (list cpl-d cpl-b cpl-c cpl-a <object> <top>)
    (class-precedence-list cpl-d)))

(define-class cpl-p () ())
(define-class cpl-q () ())
(define-class cpl-x (cpl-p cpl-q) ())
(define-class cpl-y (cpl-q cpl-p) ())

(test-group "inconsistent precedence"
  (test-error "conflicting local orders are rejected"
    &assertion-violation
    (make-class 'cpl-z '() (list cpl-x cpl-y))))

(test-end "mop feature cpl")
