

(define (main args)
  ((@@ (boot cli) enter) args))

(collect-garbage)
(dump-heap "capy.heap" main)
