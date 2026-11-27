;; mop/class-protocol: ensure-class and ensure-class-using-class, the class
;; accessors, and finalization.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature class protocol")

(define-class cp-base () ((a #:init-keyword #:a)))
(define-class cp-derived (cp-base) ((b #:init-keyword #:b)))

(test-group "ensure-class"
  (test-assert "ensure-class-using-class is a generic"
    (generic? ensure-class-using-class))
  (test-equal "define-class lists its supers, then the root object class"
    (list cp-base <object>)
    (class-direct-supers cp-derived))
  (test-assert "ensure-class makes a class"
    (class? (ensure-class 'cp-made #:direct-superclasses (list cp-base))))
  (test-assert "ensure-class on the same name redefines it"
    (let ([again (ensure-class 'cp-made #:direct-superclasses (list cp-base))])
      (and (class? again) (memq cp-base (class-direct-supers again)) #t))))

(test-group "accessors"
  (test-equal "direct slots are the slots declared here"
    '(b)
    (map slot-definition-name (class-direct-slots cp-derived)))
  (test-equal "all slots include inherited ones"
    '(a b)
    (map slot-definition-name (class-slots cp-derived)))
  (test-assert "the superclass lists its subclass"
    (memq cp-derived (class-direct-subclasses cp-base))))

(test-group "finalization"
  (test-assert "a finished class is finalized" (class-finalized? cp-derived))
  (test-assert "finalize-inheritance on a finished class is harmless"
    (begin (finalize-inheritance cp-derived) (class-finalized? cp-derived))))

(test-end "mop feature class protocol")
