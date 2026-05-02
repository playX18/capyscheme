(import (core)
        (core syntax-case)
        (srfi 64)
        (srfi srfi-213)
        (tests define-property-provider)
        (prefix (tests define-property-reexport) re:))

(test-begin "define-property-imports")

(define-syntax get-property
  (lambda (stx)
    (capture-lookup
      (lambda (lookup)
        (syntax-case stx ()
          [(_ id key)
           #`'#,(lookup #'id #'key)])))))

(test-equal "imported-property" (get-property computer answer) 42)
(test-equal "imported-binding-value" computer "the-computer")

(let ([answer #f])
  (test-equal "imported-shadowed-key" (get-property computer answer) #f))

(let ([computer "inner"])
  (test-equal "imported-shadowed-target" (get-property computer answer) #f))

(test-equal "reexport-preserves-imported-property" (get-property re:computer re:answer) 42)
(test-equal "reexport-adds-property" (get-property re:computer re:extra-answer) 43)

(test-end "define-property-imports")
