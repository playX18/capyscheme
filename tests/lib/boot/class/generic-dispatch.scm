(import (srfi 64)
        (capy))

(test-begin "boot class generic dispatch")

(test-group "generic dispatch"
  (define point (make-class 'generic-point '(x)))
  (define base (make-class 'generic-base '()))
  (define derived (make-class 'generic-derived '() (list base)))
  (define describe (make-generic 'describe))
  (define combine (make-generic 'combine 2))
  (define fallback (make-generic 'fallback))
  (define p (make-instance point #:x 5))
  (define base-instance (make-instance base))
  (define derived-instance (make-instance derived))

  (define describe-method
    (add-method! describe
      (list point)
      1
      (lambda (next obj)
        (list 'point (slot-ref obj 'x)))))

  (test-assert "class direct methods expose registered method metadata"
    (pair? (class-direct-methods point)))
  (test-assert "generic predicate accepts closure wrappers"
    (generic? describe))
  (test-equal "generic names are inspectable from wrappers"
    (generic-name describe)
    "describe")
  (test-equal "generic methods expose method descriptors"
    (generic-methods describe)
    (list describe-method))
  (test-equal "generic required dispatch arg count is inspectable"
    (generic-required-dispatch-arg-count combine)
    2)
  (test-assert "method predicate accepts method descriptors"
    (method? describe-method))
  (test-equal "method generic exposes its descriptor name"
    (generic-name (method-generic describe-method))
    "describe")
  (test-equal "method specializers expose class objects"
    (method-specializers describe-method)
    (list point))
  (test-equal "method required arg count is inspectable"
    (method-required-arg-count describe-method)
    1)
  (test-assert "method body is inspectable"
    (procedure? (method-body describe-method)))
  (test-assert "method-options expose supported option keys"
    (memq #:specializers (method-options describe-method)))
  (test-equal "method-option exposes specializers"
    (method-option describe-method #:specializers)
    (list point))
  (test-equal "method-option accepts Gauche colon keys"
    (method-option describe-method ':required-arg-count)
    1)
  (test-equal "method-option returns defaults for missing options"
    (method-option describe-method ':unknown 'fallback)
    'fallback)
  (test-assert "method-applicable-for-classes? accepts exact classes"
    (method-applicable-for-classes? describe-method point))
  (test-assert "method-applicable-for-classes? rejects unrelated classes"
    (not (method-applicable-for-classes? describe-method base)))
  (test-assert "method-applicable-for-classes? rejects missing required classes"
    (not (method-applicable-for-classes? describe-method)))
  (test-assert "dispatch returns method body"
    (procedure? (generic-dispatch describe p)))
  (test-equal "generic-invoke dispatches by class"
    (generic-invoke describe p)
    '(point 5))
  (test-equal "generic object is directly applicable"
    (describe p)
    '(point 5))

  (define combine-base-method
    (add-method! combine
      (list base base)
      2
      (lambda (next left right)
        '(base base))))
  (test-assert "method-applicable-for-classes? accepts subclass arguments"
    (method-applicable-for-classes? combine-base-method derived derived))
  (add-method! combine
    (list base derived)
    2
    (lambda (next left right)
      '(base derived)))
  (test-equal "generic dispatch handles multiple specializers"
    (combine base-instance derived-instance)
    '(base derived))
  (test-error "generic dispatch rejects missing required dispatch argument"
    &assertion-violation
    (combine base-instance))

  (add-method! describe
    (list point)
    1
    (lambda (next obj)
      (list 'replacement (slot-ref obj 'x))))
  (test-equal "method replacement invalidates dispatch"
    (generic-invoke describe p)
    '(replacement 5))

  (set-generic-fallback! fallback
    (lambda (obj)
      (list 'fallback obj)))
  (test-equal "fallback receives original args"
    (generic-invoke fallback 'value)
    '(fallback value)))

(test-end "boot class generic dispatch")
