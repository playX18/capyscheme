(define (set-map s f) (map f s))
(define (set-for-each s f) (for-each f s))
(define (set-add s x) (if (member x s) s) (cons x s))
(define (set-remove s . xs) (remove* xs s))
