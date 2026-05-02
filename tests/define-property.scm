(import (core)
        (core syntax-case)
        (srfi 64))

(test-begin "define-property")

(define property-key)
(define other-key)
(define target "target-value")

(define-syntax get-property
  (lambda (stx)
    (lambda (lookup)
      (syntax-case stx ()
        [(_ id key)
         #`'#,(datum->syntax #'* (lookup #'id #'key))]))))

(define-property target property-key "top-property")

(test-equal "top-property" (get-property target property-key) "top-property")
(test-equal "missing-property" (get-property target other-key) #f)
(test-equal "normal-binding" target "target-value")

(let ()
  (define local-key)
  (define-property target local-key "local-property")
  (test-equal "local-property" (get-property target local-key) "local-property"))

(define hidden-key)

(let ()
  (define shadowed-target "shadowed")
  (define-property shadowed-target hidden-key "shadowed-property")
  (test-equal "shadowed-property" (get-property shadowed-target hidden-key) "shadowed-property"))

(test-equal "hidden-property-not-on-target" (get-property target hidden-key) #f)

(let ()
  (define target "inner-target")
  (test-equal "shadowed-target-has-no-outer-property" (get-property target property-key) #f)
  (test-equal "shadowed-target-value" target "inner-target"))

(let ()
  (define property-key "inner-key")
  (test-equal "shadowed-key-has-no-outer-property" (get-property target property-key) #f)
  (test-equal "shadowed-key-value" property-key "inner-key"))

(define-property target property-key "replacement-property")
(test-equal "replacement-property" (get-property target property-key) "replacement-property")

(let* ()
  (define-property target property-key "internal-replacement")
  (test-equal "internal-property-override" (get-property target property-key) "internal-replacement"))

(test-equal "replacement-property-restored" (get-property target property-key) "replacement-property")

(define expression-context-error?
  (guard (c [else #t])
    (eval '(let ([x (define-property target property-key "bad")]) x))
    #f))

(test-assert "expression-context" expression-context-error?)

(define unbound-define-property-error?
  (guard (c [else #t])
    (eval '(begin
            (define unbound-property-key #f)
            (define-property unbound-property-target unbound-property-key "bad")))
    #f))

(test-assert "unbound-define-property" unbound-define-property-error?)

(test-end "define-property")
