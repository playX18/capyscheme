(import (core)
        (core syntax-case)
        (tests implicit-app-reexport)
        (tests implicit-app-consumer))

(define failures '())

(define (record-failure name got expected)
  (set! failures (cons (list name got expected) failures)))

(define-syntax check
  (syntax-rules ()
    ((_ name got expected)
     (let ([actual got] [want expected])
       (unless (equal? actual want)
         (record-failure 'name actual want))))))

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

(check default-primitive-call (+ 1 2) 3)
(check default-lambda-call ((lambda (x) x) 42) 42)
(check reexported-default-app (|#%app| + 4 5) 9)
(check macro-output-call (emit-call) 99)
(check local-override local-result 'local-app-intercepted)
(check imported-override imported-result 'imported-app-intercepted)
(check imported-override-primitive imported-primitive-result 'imported-app-intercepted)
(check empty-app-error empty-app-error? #t)

(if (null? failures)
    (begin
      (display "implicit app tests passed")
      (newline))
    (begin
      (for-each
        (lambda (failure)
          (display "implicit app test failed: ")
          (write failure)
          (newline))
        (reverse failures))
      (exit 1)))
