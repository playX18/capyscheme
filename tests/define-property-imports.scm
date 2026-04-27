(import (core)
        (core syntax-case)
        (srfi srfi-213)
        (tests define-property-provider)
        (prefix (tests define-property-reexport) re:))

(define failures '())

(define (record-failure name got expected)
  (set! failures (cons (list name got expected) failures)))

(define-syntax check
  (syntax-rules ()
    ((_ name got expected)
     (let ([actual got] [want expected])
       (unless (equal? actual want)
         (record-failure 'name actual want))))))

(define-syntax get-property
  (lambda (stx)
    (capture-lookup
      (lambda (lookup)
        (syntax-case stx ()
          [(_ id key)
           #`'#,(lookup #'id #'key)])))))

(check imported-property (get-property computer answer) 42)
(check imported-binding-value computer "the-computer")

(let ([answer #f])
  (check imported-shadowed-key (get-property computer answer) #f))

(let ([computer "inner"])
  (check imported-shadowed-target (get-property computer answer) #f))

(check reexport-preserves-imported-property (get-property re:computer re:answer) 42)
(check reexport-adds-property (get-property re:computer re:extra-answer) 43)

(if (null? failures)
    (begin
      (display "define-property import tests passed")
      (newline))
    (begin
      (for-each
        (lambda (failure)
          (display "define-property import test failed: ")
          (write failure)
          (newline))
        (reverse failures))
      (exit 1)))
