(import (capy keywords))

(define (f :x [x 0]) x)

(printf "f: ~a\n" (f))
(printf "f: ~a\n" (f :x 42))