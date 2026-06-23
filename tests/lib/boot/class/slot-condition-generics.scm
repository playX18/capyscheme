(import (srfi 64)
        (capy))

(test-begin "boot class slot condition generics")

(test-group "slot condition generics"
  (define point (make-class 'slot-condition-point '(x)))
  (define p (make-instance point))

  (test-assert "slot-unbound is a generic"
    (generic? slot-unbound))
  (test-assert "slot-missing is a generic"
    (generic? slot-missing))
  (test-equal "slot-unbound has a default class method"
    (length (generic-methods slot-unbound))
    1)
  (test-equal "slot-missing has a default class method"
    (length (generic-methods slot-missing))
    1)
  (test-error "slot-unbound default method raises assertion violation"
    &assertion-violation
    (slot-unbound point p 'x))
  (test-error "slot-missing default method raises assertion violation"
    &assertion-violation
    (slot-missing point p 'missing))
  (define-method (slot-unbound (class <class>) obj slot)
    (list 'unbound slot))
  (define-method (slot-missing (class <class>) obj slot . value)
    (list 'missing slot value))
  (test-equal "slot-ref routes unbound slots through slot-unbound"
    (slot-ref p 'x)
    '(unbound x))
  (test-equal "slot-ref routes missing slots through slot-missing"
    (slot-ref p 'missing)
    '(missing missing ()))
  (test-equal "slot-ref missing fallback still bypasses slot-missing"
    (slot-ref p 'missing 'fallback)
    'fallback)
  (define-method (slot-unbound (class <class>) obj slot)
    (assertion-violation 'slot-unbound
      "slot is unbound"
      class
      slot))
  (define-method (slot-missing (class <class>) obj slot . value)
    (assertion-violation 'slot-missing
      "class has no slot"
      class
      slot)))

(test-end "boot class slot condition generics")
