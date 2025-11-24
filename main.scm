

(define m 42)

(dump-heap "foo.heap" (lambda (args)
  (printf "matches: ~a~%" m)))