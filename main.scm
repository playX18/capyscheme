
(import (capy compiler tree-il) (capy pretty-print))
(import (capy keywords))

(define f (lambda (x :woops w :y y) (+ x w y)))

(printf "f(2, 3) = ~a~%" (f 2 :woops 3 :y 4))
