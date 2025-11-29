(import (capy keywords))

(define (point :x [x 0] :y [y 0] :z [z 0]) (vector x y z))

(printf "point1: ~a~%" (point :y 10 :z 5))