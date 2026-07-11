(library (tests phase0 autoload-provider)
  (export autoloaded-value autoloaded-call)
  (import (core))

  (define autoloaded-value 'phase0-autoloaded)

  (define (autoloaded-call x)
    (list autoloaded-value x)))
