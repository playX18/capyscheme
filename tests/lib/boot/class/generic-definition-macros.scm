(import (srfi 64)
        (capy))

(test-begin "boot class generic definition macros")

(test-group "generic definition macros"
  (define point (make-class 'macro-point '(x)))
  (define colored-point (make-class 'macro-colored-point '(color) (list point)))
  (define base (make-class 'macro-base '()))
  (define derived (make-class 'macro-derived '() (list base)))
  (define p (make-instance point #:x 9))
  (define cp (make-instance colored-point #:x 11 #:color 'green))
  (define base-instance (make-instance base))
  (define derived-instance (make-instance derived))

  (define-generic macro-describe)
  (define-generic macro-combine 2)
  (define-generic macro-next)
  (define-generic macro-locked)
  (define-generic macro-keyword-locked)
  (define-generic macro-split)
  (define-generic macro-keyword-split)
  (define-generic macro-options)
  (define-generic macro-ref)
  (define-generic (setter macro-ref))
  (define-generic macro-sealed)
  (define-generic macro-fallback-option
    :fallback (lambda (obj) (list 'fallback-option obj)))
  (define-generic macro-keyword-fallback-option
    #:fallback (lambda (obj) (list 'keyword-fallback-option obj)))
  (define-generic macro-sealed-option :sealed #t)
  (define-generic macro-option-dispatch 2
    :fallback (lambda (left right) (list 'fallback-pair left right)))
  (define-generic macro-option-ref)
  (define-generic (setter macro-option-ref)
    :fallback (lambda (obj value) (list 'setter-fallback obj value)))

  (define-method (macro-describe (obj point))
    (list 'point (slot-ref obj 'x)))
  (define-method (macro-describe (obj colored-point))
    (list 'colored (slot-ref obj 'color) (next-method-invoke next)))
  (define-method (macro-next (obj point))
    (list 'point (slot-ref obj 'x)))
  (define-method (macro-next (obj colored-point))
    (list 'colored (slot-ref obj 'color) (next-method)))
  (define-method (macro-combine (left base) (right base))
    '(base base))
  (define-method (macro-combine (left base) (right derived))
    '(base derived))
  (define-method (macro-describe obj)
    (list 'any obj))
  (define-method (macro-describe (obj point) prefix . rest)
    (list prefix (slot-ref obj 'x) rest))
  (define-method (macro-locked (obj point)) :locked
    (list 'locked (slot-ref obj 'x)))
  (define-method (macro-keyword-locked (obj point)) #:locked
    (list 'keyword-locked (slot-ref obj 'x)))
  (define-method macro-split :locked ((obj point))
    (list 'split (slot-ref obj 'x)))
  (define-method macro-keyword-split #:locked ((obj point))
    (list 'keyword-split (slot-ref obj 'x)))
  (define-method (macro-options (obj point) #:optional (prefix 'default) #:key (scale 1))
    (list prefix (* (slot-ref obj 'x) scale)))
  (define-method (macro-ref (obj point))
    (slot-ref obj 'x))
  (define-method ((setter macro-ref) (obj point) value)
    (slot-set! obj 'x value)
    value)
  (define-method (macro-sealed (obj point))
    (list 'sealed (slot-ref obj 'x)))

  (test-equal "define-generic creates invocable generic"
    (macro-describe p)
    '(point 9))
  (test-equal "define-method exposes next method binding"
    (macro-describe cp)
    '(colored green (point 11)))
  (test-equal "define-method exposes Gauche-style next-method procedure"
    (macro-next cp)
    '(colored green (point 11)))
  (test-equal "define-method handles multiple specializers"
    (macro-combine base-instance derived-instance)
    '(base derived))
  (test-equal "define-method handles unspecialized fallback methods"
    (macro-describe 'plain)
    '(any plain))
  (test-equal "define-method handles trailing unspecialized and rest args"
    (macro-describe p 'prefix 1 2 3)
    '(prefix 9 (1 2 3)))
  (test-equal "define-method accepts locked qualifier"
    (macro-locked p)
    '(locked 9))
  (test-assert "define-method records locked qualifier"
    (method-locked? (car (generic-methods macro-locked))))
  (test-assert "method-option exposes locked qualifier"
    (method-option (car (generic-methods macro-locked)) #:locked))
  (test-equal "define-method accepts keyword locked qualifier"
    (macro-keyword-locked p)
    '(keyword-locked 9))
  (test-assert "define-method records keyword locked qualifier"
    (method-locked? (car (generic-methods macro-keyword-locked))))
  (test-error "locked methods reject replacement"
    &assertion-violation
    (add-method! macro-locked
      (list point)
      1
      (lambda (next obj) (list 'replaced obj))))
  (test-equal "define-method accepts Gauche-style split form"
    (macro-split p)
    '(split 9))
  (test-equal "define-method accepts split keyword locked qualifier"
    (macro-keyword-split p)
    '(keyword-split 9))
  (test-assert "define-method records split keyword locked qualifier"
    (method-locked? (car (generic-methods macro-keyword-split))))
  (test-error "define-method rejects unsupported inline qualifiers"
    &syntax
    (eval '(define-method (macro-unsupported-inline (obj point)) :before obj)
      (current-module)))
  (test-error "define-method rejects unsupported split qualifiers"
    &syntax
    (eval '(define-method macro-unsupported-split :before ((obj point)) obj)
      (current-module)))
  (test-equal "define-method handles optional method arguments"
    (macro-options p)
    '(default 9))
  (test-equal "define-method handles keyword method arguments"
    (macro-options p 'scaled #:scale 3)
    '(scaled 27))
  (test-assert "define-generic installs setter generic"
    (generic? (setter macro-ref)))
  (test-assert "has-setter? reports setter generic"
    (has-setter? macro-ref))
  (test-equal "setter generic dispatches"
    ((setter macro-ref) p 42)
    42)
  (test-equal "setter generic updates slots"
    (macro-ref p)
    42)
  (test-equal "define-generic fallback option installs fallback procedure"
    (macro-fallback-option 'plain)
    '(fallback-option plain))
  (test-assert "generic-options expose supported option keys"
    (memq #:fallback (generic-options macro-fallback-option)))
  (test-assert "generic-option exposes fallback procedures"
    (procedure? (generic-option macro-fallback-option #:fallback)))
  (test-equal "generic-option exposes dispatch arg counts"
    (generic-option macro-option-dispatch ':required-dispatch-arg-count)
    2)
  (test-assert "generic-option accepts Gauche colon keys"
    (procedure? (generic-option macro-fallback-option ':fallback)))
  (test-equal "generic-option returns defaults for missing options"
    (generic-option macro-fallback-option ':unknown 'fallback)
    'fallback)
  (test-equal "define-generic keyword fallback option installs fallback procedure"
    (macro-keyword-fallback-option 'plain)
    '(keyword-fallback-option plain))
  (test-equal "define-generic fallback option honors dispatch arity"
    (macro-option-dispatch 'left 'right)
    '(fallback-pair left right))
  (test-assert "define-generic sealed option seals generic"
    (generic-sealed? macro-sealed-option))
  (test-error "define-generic sealed option rejects later methods"
    &assertion-violation
    (add-method! macro-sealed-option
      (list point)
      1
      (lambda (next obj) obj)))
  (test-equal "define-generic setter fallback option installs setter fallback"
    ((setter macro-option-ref) 'plain 99)
    '(setter-fallback plain 99))
  (test-error "define-generic rejects unsupported options"
    &syntax
    (eval '(define-generic macro-unsupported-generic-option :class <generic>)
      (current-module)))
  (test-assert "generic-sealed? starts false"
    (not (generic-sealed? macro-sealed)))
  (generic-seal! macro-sealed)
  (test-assert "generic-seal! marks generic sealed"
    (generic-sealed? macro-sealed))
  (test-error "sealed generics reject method replacement"
    &assertion-violation
    (add-method! macro-sealed
      (list point)
      1
      (lambda (next obj) (list 'replaced obj))))
  (test-equal "sealed generic keeps existing method"
    (macro-sealed p)
    '(sealed 42))
  (generic-unseal! macro-sealed)
  (test-assert "generic-unseal! clears sealed flag"
    (not (generic-sealed? macro-sealed)))
  (add-method! macro-sealed
    (list point)
    1
    (lambda (next obj) (list 'unsealed (slot-ref obj 'x))))
  (test-equal "unsealed generic accepts replacement"
    (macro-sealed p)
    '(unsealed 42))
  (test-error "define-method rejects non-prefix specializers"
    &syntax
    (eval '(define-method (macro-combine left (right derived)) '(bad))
      (environment '(capy)))))

(test-end "boot class generic definition macros")
