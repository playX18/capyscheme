(import (srfi 64)
        (capy))

(test-begin "boot class next methods")

(test-group "next methods"
  (define point (make-class 'next-point '(x)))
  (define colored-point (make-class 'next-colored-point '(color) (list point)))
  (define describe (make-generic 'describe-next))
  (define cp (make-instance colored-point #:x 7 #:color 'blue))

  (define point-method
    (add-method! describe
      (list point)
      1
      (lambda (next obj)
        (list 'point (slot-ref obj 'x)))))
  (define colored-method
    (add-method! describe
      (list colored-point)
      1
      (lambda (next obj)
        (list 'colored (slot-ref obj 'color) (next-method-invoke next)))))
  (define next-chain (generic-next-methods describe cp))

  (test-equal "next-method advances to less specific method"
    (generic-invoke describe cp)
    '(colored blue (point 7)))
  (test-equal "compute-applicable-methods returns matching methods"
    (compute-applicable-methods describe (list cp))
    (list point-method colored-method))
  (test-equal "sort-applicable-methods orders by specificity"
    (sort-applicable-methods describe
      (compute-applicable-methods describe (list cp))
      (list cp))
    (list colored-method point-method))
  (test-equal "next-method-methods exposes sorted runtime method chains"
    (next-method-methods next-chain)
    (list colored-method point-method))
  (test-assert "next-method predicate accepts descriptors"
    (next-method? next-chain))
  (test-assert "next-method predicate rejects non-descriptors"
    (not (next-method? describe)))
  (test-equal "next-method-generic exposes owning generic"
    (generic-name (next-method-generic next-chain))
    "describe-next")
  (test-equal "next-method-args exposes dispatch args"
    (next-method-args next-chain)
    (list cp))
  (test-equal "next-method-index exposes current method index"
    (next-method-index next-chain)
    0)
  (test-assert "next-method-has-next? reports remaining methods"
    (next-method-has-next? next-chain))
  (test-assert "next-method-options expose supported option keys"
    (memq #:methods (next-method-options next-chain)))
  (test-equal "next-method-option accepts Gauche colon keys"
    (next-method-option next-chain ':index)
    0)
  (test-equal "next-method-option exposes next descriptor"
    (next-method-methods (next-method-option next-chain ':next))
    (list colored-method point-method))
  (test-equal "next-method-option returns defaults for missing options"
    (next-method-option next-chain ':unknown 'fallback)
    'fallback)
  (test-assert "method-more-specific? compares class CPL positions"
    (method-more-specific? colored-method point-method (list colored-point)))
  (test-equal "apply-generic invokes through applicable methods"
    (apply-generic describe (list cp))
    '(colored blue (point 7)))
  (test-equal "apply-methods invokes an explicit sorted method list"
    (apply-methods describe (list colored-method point-method) (list cp))
    '(colored blue (point 7))))

(test-end "boot class next methods")
