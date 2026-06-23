(import (srfi 64)
        (capy))

(test-begin "boot class class redefinition")

(test-group "class redefinition"
  (define record (make-class 'redef-record '(kept removed)))
  (define instance (make-instance record #:kept 1 #:removed 2))
  (define instance-for-touch (make-instance record #:kept 4))
  (define redefined (redefine-class! record 'redef-record-v2 '(added kept)))
  (define old-super (make-class 'redef-old-super '(old-base)))
  (define new-super (make-class 'redef-new-super '(new-base)))
  (define changing-subclass
    (make-class 'redef-changing-subclass '(child-slot) (list old-super)))
  (define changed-subclass #f)

  (test-assert "redefinition returns class object" (class? redefined))
  (test-equal "class-of resolves redefined class through table" (class-of instance) redefined)
  (test-equal "current-class-of exposes stale instance class before touch"
    (current-class-of instance)
    record)
  (test-assert "%class-redefined? reports stale instances"
    (%class-redefined? instance))
  (test-assert "%class-redefined? reports stale class descriptors"
    (%class-redefined? record))
  (test-assert "%class-redefined? rejects current class descriptors"
    (not (%class-redefined? redefined)))
  (test-assert "%class-redefined? rejects non-class values"
    (not (%class-redefined? 'not-a-class)))
  (test-equal "touch-instance! returns the touched instance"
    (touch-instance! instance-for-touch)
    instance-for-touch)
  (test-equal "touch-instance! updates stale instance class"
    (current-class-of instance-for-touch)
    redefined)
  (test-assert "%class-redefined? rejects touched instances"
    (not (%class-redefined? instance-for-touch)))
  (test-error "touch-instance! rejects non-instances"
    &assertion-violation
    (touch-instance! redefined))
  (test-equal "redefined class name" (class-name redefined) "redef-record-v2")
  (test-equal "preserved slot keeps value" (slot-ref instance 'kept) 1)
  (test-equal "current-class-of exposes touched instance class after access"
    (current-class-of instance)
    redefined)
  (test-equal "added slot initarg applies to new instances"
    (slot-ref (make-instance redefined #:added 3) 'added)
    3)
  (test-equal "removed slot is missing" (slot-ref instance 'removed 'missing) 'missing)
  (test-assert "old superclass initially tracks direct subclass"
    (memq changing-subclass (class-direct-subclasses old-super)))
  (set! changed-subclass
    (redefine-class!
      changing-subclass
      'redef-changing-subclass-v2
      '(child-slot)
      (list new-super)))
  (test-assert "redefinition removes old superclass direct subclass edge"
    (not (memq changing-subclass (class-direct-subclasses old-super))))
  (test-assert "redefinition adds new superclass direct subclass edge"
    (memq changed-subclass (class-direct-subclasses new-super)))
  (test-equal "redefinition recomputes inherited slots from new supers"
    (map slot-definition-name (class-slots changed-subclass))
    '(new-base child-slot)))

(test-end "boot class class redefinition")
