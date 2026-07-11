#!r6rs

(library (tests phase0 r6rs-smoke)
  (export r6rs-smoke-value r6rs-smoke-call)
  (import (rnrs))

  (define r6rs-smoke-value 'phase0-r6rs)

  (define (r6rs-smoke-call x)
    (list r6rs-smoke-value x)))
