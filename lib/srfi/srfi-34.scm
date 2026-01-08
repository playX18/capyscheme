(define-library (srfi srfi-34)
  (export with-exception-handler
    raise
    guard)
  (import (only (core exceptions) with-exception-handler
           raise
           guard)))
