(import (core)
        (core syntax-case)
        (tests implicit-app-consumer))

(define (check name ok?)
  (if ok?
    (begin (display "PASS ") (display name) (newline))
    (begin (display "FAIL ") (display name) (newline) (exit 1))))

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

(check "default primitive call still uses #%app" (= (+ 1 2) 3))
(check "default lambda call still uses #%app" (= ((lambda (x) x) 42) 42))
(check "default app can be invoked explicitly" (= (|#%app| + 4 5) 9))
(check "macro output call uses current #%app" (= (emit-call) 99))
(check "local override intercepts call syntax" (eq? local-result 'local-app-intercepted))
(check "imported override intercepts non-primitive call" (eq? imported-result 'imported-app-intercepted))
(check "imported override intercepts primitive call" (eq? imported-primitive-result 'imported-app-intercepted))
(check "empty #%app form is rejected" empty-app-error?)
