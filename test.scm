(define reader (make-reader (current-input-port) "stdin"))
(reader-mode-set! reader 'r6rs)
(define (repl)
    (let loop ()
        (with-exception-handler 
            (lambda (exn)
                        (display "error reading from stdin: ")
                        (displayln (condition-message exn))
                        (display "irritants: ")
                        (displayln (condition-irritants exn))
                        (loop))
            (lambda ()
            (display "> ")
        (let ((datum (read-datum reader)))
            (if (eof-object? datum)
                (begin
                    (newline)
                    (displayln "Goodbye!")
                    (exit))
                (begin
                    (with-exception-handler 
                      (lambda (exn)
                        (display "Error: ")
                        (displayln (condition-message exn))
                        (display "irritants: ")
                        (displayln (condition-irritants exn))
                        (loop))
                      (lambda () (let ([res (eval datum)])
                        (if (not (eq? res (unspecified)))
                            (begin (display "=> ")
                            (displayln res))))))
                    (loop))))))))



(repl)

