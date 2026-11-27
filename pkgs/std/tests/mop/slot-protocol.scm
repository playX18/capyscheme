;; mop/slot-protocol: slot-unbind-using-class!, standard-instance-access, slot
;; reads through the protocol, shared slots, and slot caches past eight classes.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature slot protocol")

(define-class sp-point () ((x #:init-keyword #:x) (y #:init-value 0)))
(define-class sp-shared () ((count #:allocation #:class #:init-value 1)))

(test-group "slot references"
  (test-equal "slot-ref-using-class reads through the class"
    3
    (slot-ref-using-class sp-point (make sp-point #:x 3) 'x))
  (test-equal "slot-set-using-class! writes through the class"
    9
    (let ([p (make sp-point #:x 3)])
      (slot-set-using-class! sp-point p 'x 9)
      (slot-ref p 'x)))
  (test-assert "slot-bound-using-class? sees a bound slot"
    (slot-bound-using-class? sp-point (make sp-point #:x 3) 'x)))

(test-group "unbinding"
  (test-assert "slot-unbind-using-class! unbinds"
    (let ([p (make sp-point #:x 3)])
      (slot-unbind-using-class! sp-point p 'x)
      (not (slot-bound-using-class? sp-point p 'x))))
  (test-equal "standard-instance-access reads the raw index"
    3
    (standard-instance-access (make sp-point #:x 3) 0)))

(test-group "class allocation"
  (test-equal "a class slot is shared by instances"
    1
    (slot-ref (make sp-shared) 'count))
  (test-equal "a class slot write is seen by every instance"
    7
    (let ([a (make sp-shared)] [b (make sp-shared)])
      (slot-set! a 'count 7)
      (slot-ref b 'count))))

(define-class sp-c1 () ((v #:init-value 1)))
(define-class sp-c2 () ((v #:init-value 2)))
(define-class sp-c3 () ((v #:init-value 3)))
(define-class sp-c4 () ((v #:init-value 4)))
(define-class sp-c5 () ((v #:init-value 5)))
(define-class sp-c6 () ((v #:init-value 6)))
(define-class sp-c7 () ((v #:init-value 7)))
(define-class sp-c8 () ((v #:init-value 8)))
(define-class sp-c9 () ((v #:init-value 9)))
(define-generic sp-read)
(define-method (sp-read (o sp-c1)) (slot-ref o 'v))
(define-method (sp-read (o sp-c2)) (slot-ref o 'v))
(define-method (sp-read (o sp-c3)) (slot-ref o 'v))
(define-method (sp-read (o sp-c4)) (slot-ref o 'v))
(define-method (sp-read (o sp-c5)) (slot-ref o 'v))
(define-method (sp-read (o sp-c6)) (slot-ref o 'v))
(define-method (sp-read (o sp-c7)) (slot-ref o 'v))
(define-method (sp-read (o sp-c8)) (slot-ref o 'v))
(define-method (sp-read (o sp-c9)) (slot-ref o 'v))

(test-group "many classes"
  (test-equal "nine classes through one accessor site"
    '(1 2 3 4 5 6 7 8 9)
    (map sp-read (list (make sp-c1) (make sp-c2) (make sp-c3) (make sp-c4)
                       (make sp-c5) (make sp-c6) (make sp-c7) (make sp-c8)
                       (make sp-c9)))))

(test-end "mop feature slot protocol")
