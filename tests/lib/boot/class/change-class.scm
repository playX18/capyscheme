(import (srfi 64)
        (capy))

(test-begin "boot class change-class")

(test-group "change-class"
  (define source (make-class 'change-source '(kept removed)))
  (define target (make-class 'change-target '(added kept)))
  (define source-instance (make-instance source #:kept 10 #:removed 20))
  (define-class change-target-default ()
    ((added #:init-value 30)
     kept))
  (define defaulted-instance (make-instance source #:kept 11 #:removed 21))

  (test-equal "change-class returns the changed instance"
    (change-class source-instance target)
    source-instance)
  (test-equal "change-class updates class-of through object header"
    (class-of source-instance)
    target)
  (test-equal "change-class updates current-class-of"
    (current-class-of source-instance)
    target)
  (test-equal "change-class preserves same-name slot values"
    (slot-ref source-instance 'kept)
    10)
  (test-assert "change-class leaves added slots without defaults unbound"
    (not (slot-bound? source-instance 'added)))
  (change-class defaulted-instance change-target-default)
  (test-equal "change-class seeds added slots from target defaults"
    (slot-ref defaulted-instance 'added)
    30)
  (test-equal "change-class preserves same-name slots with defaulted targets"
    (slot-ref defaulted-instance 'kept)
    11)
  (test-equal "change-class drops removed slots"
    (slot-ref source-instance 'removed 'missing)
    'missing)
  (test-error "change-class rejects non-instances"
    &assertion-violation
    (change-class target target))
  (test-error "change-class rejects built-in target classes"
    &assertion-violation
    (change-class source-instance <class>)))

(test-end "boot class change-class")
