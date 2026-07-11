(import (core)
        (core syntax-case))

(define (check name ok?)
  (if ok?
    (begin (display "PASS ") (display name) (newline))
    (begin (display "FAIL ") (display name) (newline) (exit 1))))

(define property-key)
(define other-key)
(define target "target-value")

(define-syntax get-property
  (lambda (stx)
    (capture-lookup
      (lambda (lookup)
        (syntax-case stx ()
          [(_ id key)
           #`'#,(lookup #'id #'key)])))))

(define-property target property-key "top-property")

(check "top-level property lookup" (string=? (get-property target property-key) "top-property"))
(check "missing property lookup" (if (get-property target other-key) #f #t))
(check "property target remains a normal binding" (string=? target "target-value"))

(let ()
  (define local-key)
  (define-property target local-key "local-property")
  (check "internal definition property lookup" (string=? (get-property target local-key) "local-property")))

(let ()
  (define target "inner-target")
  (check "shadowed target does not inherit outer property" (if (get-property target property-key) #f #t))
  (check "shadowed target value is unchanged" (string=? target "inner-target")))

(define-property target property-key "replacement-property")
(check "top-level replacement property" (string=? (get-property target property-key) "replacement-property"))

(let* ()
  (define-property target property-key "internal-replacement")
  (check "internal replacement shadows top-level property" (string=? (get-property target property-key) "internal-replacement")))

(check "top-level property restored after internal body" (string=? (get-property target property-key) "replacement-property"))

(define expression-context-error?
  (guard (c [else #t])
    (eval '(let ([x (define-property target property-key "bad")]) x))
    #f))

(define unbound-define-property-error?
  (guard (c [else #t])
    (eval '(begin
            (define unbound-property-key #f)
            (define-property unbound-property-target unbound-property-key "bad")))
    #f))

(check "define-property is rejected in expression context" expression-context-error?)
(check "define-property rejects unbound target" unbound-define-property-error?)
