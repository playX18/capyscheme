(import (capy keywords))

(define (foo x :y [y 0])
  (+ x y))

(foo 5)