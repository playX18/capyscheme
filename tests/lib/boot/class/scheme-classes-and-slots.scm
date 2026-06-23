(import (srfi 64)
        (capy))

(test-begin "boot class scheme classes and slots")

(test-group "scheme classes and slots"
  (define point (make-class 'point '(x y)))
  (define p (make-instance point #:x 10))

  (test-assert "class object" (class? point))
  (test-equal "class name" (class-name point) "point")
  (test-assert "class-of instance" (eq? (class-of p) point))
  (test-assert "scheme instance" (scheme-instance? p))
  (test-assert "non-instance" (not (scheme-instance? point)))

  (test-assert "initialized slot is bound" (slot-bound? p 'x))
  (test-equal "initialized slot value" (slot-ref p 'x) 10)
  (test-assert "uninitialized slot is unbound" (not (slot-bound? p 'y)))
  (test-equal "missing slot fallback" (slot-ref p 'z 'missing) 'missing)
  (test-equal "instance-slot-ref reads raw initialized slots" (instance-slot-ref p 0) 10)
  (test-equal "instance-slot-ref returns fallback for raw unbound slots"
    (instance-slot-ref p 1 'raw-missing)
    'raw-missing)

  (slot-set! p 'y 20)
  (test-assert "written slot is bound" (slot-bound? p 'y))
  (test-equal "written slot value" (slot-ref p 'y) 20)
  (instance-slot-set! p 1 30)
  (test-equal "instance-slot-set! writes raw slots" (slot-ref p 'y) 30)
  (test-error "instance-slot-ref rejects unbound raw slots without fallback"
    &assertion-violation
    (instance-slot-ref (make-instance point #:x 1) 1))
  (test-error "instance-slot-ref rejects out-of-bounds indexes"
    &assertion-violation
    (instance-slot-ref p 9))
  (test-error "instance-slot-set! rejects negative indexes"
    &assertion-violation
    (instance-slot-set! p -1 'bad))
  (test-error "instance-slot-ref rejects non-instances"
    &assertion-violation
    (instance-slot-ref point 0)))

(test-end "boot class scheme classes and slots")
