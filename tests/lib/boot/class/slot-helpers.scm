(import (srfi 64)
        (capy))

(test-begin "boot class slot convenience helpers")

(test-group "slot convenience helpers"
  (define-class slot-helper-record ()
    ((items #:init-value '())
     empty))
  (define record (make-instance slot-helper-record))

  (test-assert "slot-exists? reports present slots"
    (slot-exists? record 'items))
  (test-assert "slot-exists? rejects missing slots"
    (not (slot-exists? record 'missing)))
  (test-assert "slot-exists-using-class? reports present slots"
    (slot-exists-using-class? slot-helper-record record 'items))
  (test-assert "slot-exists-using-class? rejects missing slots"
    (not (slot-exists-using-class? slot-helper-record record 'missing)))
  (slot-push! record 'items 'first)
  (slot-push! record 'items 'second)
  (test-equal "slot-push! conses onto slot value"
    (slot-ref record 'items)
    '(second first))
  (test-equal "slot-pop! returns the head"
    (slot-pop! record 'items)
    'second)
  (test-equal "slot-pop! stores the tail"
    (slot-ref record 'items)
    '(first))
  (test-equal "slot-pop! returns default for unbound slot"
    (slot-pop! record 'empty 'fallback)
    'fallback)
  (test-equal "slot-pop! returns default for missing slot"
    (slot-pop! record 'missing 'fallback)
    'fallback)
  (test-error "slot-pop! rejects non-pair slot without default"
    (slot-pop! record 'empty)))

(test-end "boot class slot convenience helpers")
