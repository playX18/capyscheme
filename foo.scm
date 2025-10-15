(import (scheme base))
(import (core control))

(define f 
    (case-lambda 
        [(x) (format #t "one ~%~!")]
        [(x y) (format #t "two ~%~!")]
        [(x y z) (format #t "three ~%~!")]
        [(x y z . r) (format #t "many ~%~!")]
        [() (format #t "none ~%~!")]))

(f 1 2 4 5 6)

(format #t "Hello, World!~%~!")