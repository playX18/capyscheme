(import (core)
        (core syntax-case))

(define failures '())

(define (record-failure name got expected)
  (set! failures (cons (list name got expected) failures)))

(define-syntax check
  (syntax-rules ()
    ((_ name got expected)
     (let ([actual got] [want expected])
       (unless (equal? actual want)
         (record-failure 'name actual want))))))

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

(check top-property (get-property target property-key) "top-property")
(check missing-property (get-property target other-key) #f)
(check normal-binding target "target-value")

(let ()
  (define local-key)
  (define-property target local-key "local-property")
  (check local-property (get-property target local-key) "local-property"))

(define hidden-key)

(let ()
  (define shadowed-target "shadowed")
  (define-property shadowed-target hidden-key "shadowed-property")
  (check shadowed-property (get-property shadowed-target hidden-key) "shadowed-property"))

(check hidden-property-not-on-target (get-property target hidden-key) #f)

(let ()
  (define target "inner-target")
  (check shadowed-target-has-no-outer-property (get-property target property-key) #f)
  (check shadowed-target-value target "inner-target"))

(let ()
  (define property-key "inner-key")
  (check shadowed-key-has-no-outer-property (get-property target property-key) #f)
  (check shadowed-key-value property-key "inner-key"))

(define-property target property-key "replacement-property")
(check replacement-property (get-property target property-key) "replacement-property")

(let* ()
  (define-property target property-key "internal-replacement")
  (check internal-property-override (get-property target property-key) "internal-replacement"))

(check replacement-property-restored (get-property target property-key) "replacement-property")

(define expression-context-error?
  (guard (c [else #t])
    (eval '(let ([x (define-property target property-key "bad")]) x))
    #f))

(check expression-context expression-context-error? #t)

(define unbound-define-property-error?
  (guard (c [else #t])
    (eval '(begin
            (define unbound-property-key #f)
            (define-property unbound-property-target unbound-property-key "bad")))
    #f))

(check unbound-define-property unbound-define-property-error? #t)

(if (null? failures)
    (begin
      (display "define-property tests passed")
      (newline))
    (begin
      (for-each
        (lambda (failure)
          (display "define-property test failed: ")
          (write failure)
          (newline))
        (reverse failures))
      (exit 1)))
