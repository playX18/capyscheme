(define-library (srfi srfi-98)
  (export 
    get-environment-variable
    get-environment-variables)
  (import (only (scheme process-context)
                  get-environment-variable
                  get-environment-variables)))