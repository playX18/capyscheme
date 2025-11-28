
(import (capy compiler tree-il) (capy pretty-print))
(import (capy keywords))

(define f (lambda (x :y y :z [z 10]) (+ x y z)))

(printf "f(1 :y 2) = ~a~%" (f 1 :y 2)) ; -> 13
(printf "f(1 :y 2 :z 3) = ~a~%" (f 1 :y 2 :z 3)) ; -> 6
