
(library (rnrs programs (6))
  (export command-line exit)
  (import (core primitives))
  
  (define (command-line)
    ((@@ (capy) program-arguments))))
