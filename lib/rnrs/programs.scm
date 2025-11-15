
(library (rnrs programs (6))
  (export command-line exit)
  (import (core primitives) (only (capy) @@))
  
  (define (command-line)
    ((@@ (capy) program-arguments))))
