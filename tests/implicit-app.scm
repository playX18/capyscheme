(import (tests implicit-app-reexport)
        (tests implicit-app-consumer)
        (core)
        (core syntax-case)
        (srfi 64))

(test-begin "implicit-app")

(define-syntax emit-call
  (lambda (stx)
    (syntax-case stx ()
      [(_)
       #'((lambda (x) x) 99)])))

(define local-result
  (let-syntax ([|#%app|
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ rator rand ...)
                     #'(quote local-app-intercepted)]))])
    (ignored-operator 1 2 3)))

(define empty-app-error?
  (guard (c [else #t])
    (eval '(|#%app|))
    #f))

(test-equal "default-primitive-call" (+ 1 2) 3)
(test-equal "default-lambda-call" ((lambda (x) x) 42) 42)
(test-equal "reexported-default-app" (|#%app| + 4 5) 9)
(test-equal "macro-output-call" (emit-call) 99)
(test-equal "local-override" local-result 'local-app-intercepted)
(test-equal "imported-override" imported-result 'imported-app-intercepted)
(test-equal "imported-override-primitive" imported-primitive-result 'imported-app-intercepted)
(test-assert "empty-app-error" empty-app-error?)

(test-end "implicit-app")
