
(define (hello)
  (display "Hello world")
  (newline))

(printf "foo/bar.scm loaded, current-module: ~a~%" (current-module))
(printf "hello=~a~%" hello)