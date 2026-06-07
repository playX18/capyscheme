(import (capy))

(define base-a
  (make-class 'base-a '(a-slot)))

(define base-b
  (make-class 'base-b '(b-slot)))

(define record
  (make-class 'record '(kept removed) (list base-a)))

(define before
  (make-instance record #:a-slot 'from-a #:kept 1 #:removed 2))

(display (map slot-definition-name (class-slots record)))
(newline)
(display (not (not (memq record (class-direct-subclasses base-a)))))
(newline)

(define record-v2
  (redefine-class!
    record
    'record-v2
    '(kept added)
    (list base-b)))

(display (class-name (class-of before)))
(newline)
(display (class-name (current-class-of before)))
(newline)
(display (%class-redefined? before))
(newline)
(display (%class-redefined? record))
(newline)
(display (eq? (touch-instance! before) before))
(newline)
(display (class-name (current-class-of before)))
(newline)
(display (%class-redefined? before))
(newline)

;; Same-name slots keep their values, removed slots disappear, and new
;; inherited/direct slots appear according to the replacement class shape.
(display (slot-ref before 'kept))
(newline)
(display (class-name (current-class-of before)))
(newline)
(display (slot-ref before 'removed 'missing))
(newline)
(display (slot-ref (make-instance record-v2 #:b-slot 'from-b #:added 3) 'added))
(newline)
(display (map slot-definition-name (class-slots record-v2)))
(newline)

(display (class-malleable? record-v2))
(newline)
(class-seal! record-v2)
(display (class-sealed? record-v2))
(newline)
(display (class-malleable? record-v2))
(newline)
(class-unseal! record-v2)
(display (class-malleable? record-v2))
(newline)

;; Direct-subclass metadata is weak and non-owning, but class redefinition still
;; updates the visible edges for live class objects when direct supers change.
(display (not (not (memq record (class-direct-subclasses base-a)))))
(newline)
(display (not (not (memq record-v2 (class-direct-subclasses base-b)))))
(newline)

(define-class named-record ()
  (kept removed))

(define named-before
  (make named-record #:kept 'kept #:removed 'removed))

(define named-record-old named-record)

(define-generic named-record-summary)

(define-method (named-record-summary (obj named-record))
  (list 'record (slot-ref obj 'kept)))

(display (named-record-summary named-before))
(newline)

(define-class named-record ()
  (kept added))

(display (not (eq? named-record named-record-old)))
(newline)
(display (slot-ref named-before 'kept))
(newline)
(display (slot-ref named-before 'removed 'missing))
(newline)
(display (slot-ref (make named-record #:added 'added) 'added))
(newline)
(display (named-record-summary named-before))
(newline)
(display (not (null? (class-direct-methods named-record))))
(newline)
