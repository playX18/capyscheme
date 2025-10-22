
(define-library (scheme process-context)
  (import (rename (core primitives) (exit core:exit))
          (core optargs))
  (export command-line
          exit
          get-environment-variable
          get-environment-variables
          emergency-exit)
  (begin
    (define get-environment-variable getenv)
    (define get-environment-variables process-environment->alist)
    (define emergency-exit core:exit)
    (define exit core:exit)))
