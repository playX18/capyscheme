(define-library (scheme repl)
  (import (core primitives))
  (export interaction-environment)
  (begin
    ;; R7RS-small: the environment in which interaction-environment
    ;; evaluation takes place. Capy's read-eval-print-loop evaluates in
    ;; the current module, and eval accepts modules directly.
    (define (interaction-environment) (current-module))))
