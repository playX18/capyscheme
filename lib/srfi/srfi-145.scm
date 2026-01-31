(define-library (srfi srfi-145)
  (export assume)
  (import (scheme base))
  (begin 
    (define fatal-error error)
    (define-syntax assume
      (syntax-rules ()
        ((assume expression message ...)
         (or expression
             (fatal-error "invalid assumption" (quote expression) (list message ...))))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))))