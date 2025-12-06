

(define-library
  (foo bar)
  (import (scheme base)
          (scheme write) (capy))
  (export hello)
  (include "bar.scm"))