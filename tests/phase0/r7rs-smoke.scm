(define-library (tests phase0 r7rs-smoke)
  (import (scheme base))
  (export r7rs-smoke-value r7rs-smoke-call)

  (begin
    (define r7rs-smoke-value 'phase0-r7rs)

    (define (r7rs-smoke-call x)
      (list r7rs-smoke-value x))))
