(define manifest-path (list-ref (command-line) 1))

(define (run-bootstrap-form form)
  (*raw-log* log:info '(capy bootstrap) 'bootstrap-batch "Evaluating ~a" form)
  (eval form (current-module)))

(call-with-input-file manifest-path
  (lambda (port)
    (let ([forms (read port)])
      (for-each run-bootstrap-form forms))))
