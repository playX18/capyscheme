(import (core)
        (tests phase0 r6rs-smoke)
        (tests phase0 r7rs-smoke))

(define (check name ok?)
  (if ok?
    (begin (display "PASS ") (display name) (newline))
    (begin (display "FAIL ") (display name) (newline) (exit 1))))

(define r6rs-smoke-result (r6rs-smoke-call 'ok))
(define r7rs-smoke-result (r7rs-smoke-call 'ok))
(define r6rs-call-ok?
  (and (pair? r6rs-smoke-result)
       (eq? (car r6rs-smoke-result) 'phase0-r6rs)
       (pair? (cdr r6rs-smoke-result))
       (eq? (cadr r6rs-smoke-result) 'ok)
       (null? (cddr r6rs-smoke-result))))
(define r7rs-call-ok?
  (and (pair? r7rs-smoke-result)
       (eq? (car r7rs-smoke-result) 'phase0-r7rs)
       (pair? (cdr r7rs-smoke-result))
       (eq? (cadr r7rs-smoke-result) 'ok)
       (null? (cddr r7rs-smoke-result))))

(check "R6RS library imports" (eq? r6rs-smoke-value 'phase0-r6rs))
(check "R6RS exported procedure runs" r6rs-call-ok?)
(check "R7RS define-library imports" (eq? r7rs-smoke-value 'phase0-r7rs))
(check "R7RS exported procedure runs" r7rs-call-ok?)
