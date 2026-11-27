;; mop/default-initargs: #:default-initargs fills initargs a caller leaves
;; out, and class-prototype is built once.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature default initargs")

(define-class di-base ()
  ((x #:init-keyword #:x)
   (y #:init-keyword #:y))
  #:default-initargs ((#:x 10) (#:y 20)))

(define-class di-sub (di-base) ()
  #:default-initargs ((#:x 1)))

(test-group "defaults"
  (test-equal "defaults fill the missing initargs"
    '(10 20)
    (let ([o (make di-base)]) (list (slot-ref o 'x) (slot-ref o 'y))))
  (test-equal "a caller's value wins over the default"
    '(5 20)
    (let ([o (make di-base #:x 5)]) (list (slot-ref o 'x) (slot-ref o 'y))))
  (test-equal "a subclass default overrides the superclass default"
    '(1 20)
    (let ([o (make di-sub)]) (list (slot-ref o 'x) (slot-ref o 'y)))))

(test-group "introspection"
  (test-equal "class-direct-default-initargs lists this class's keys"
    '(#:x)
    (map car (class-direct-default-initargs di-sub)))
  (test-equal "class-default-initargs merges the superclass keys"
    '(#:x #:y)
    (map car (class-default-initargs di-sub)))

  (test-assert "class-prototype is an instance of its class"
    (is-a? (class-prototype di-base) di-base)))

(test-end "mop feature default initargs")
