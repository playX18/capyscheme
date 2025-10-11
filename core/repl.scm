(library (core repl)
    (export read-eval-print-loop current-exception-printer default-exception-printer)
    
    (import (capy)
            (core parameters)
            (core exceptions))

(define continuation-to-exit (make-parameter #f))
(define dump-condition (make-parameter #f))
(define self-evaluating-vector-constants (make-parameter #t))
(define ellipsis/underscore-in-literal (make-parameter #t))
(define right-arrow-in-case (make-parameter #t))
(define repl-startup-version (make-parameter #f))


(define (default-exception-printer c . maybe-out)
  (define out (if (null? maybe-out) (current-error-port) (car maybe-out)))
  
  (define output (call-with-string-output-port 
    (lambda (port)
      (define (output-who-message)
        (format port "error")
        (and (who-condition? c)
             (format port " in ~a" (condition-who c))))
      (define (output-irritants)
        (and (irritants-condition? c)
             (format port " [~a]" (condition-irritants c))))
      (define (output-message)
        (and (message-condition? c)
             (format port ": ~a" (condition-message c))))

      (cond 
        [(undefined-violation? c)
          (format port "error: undefined variable ")
          (output-who-message)
          (format port ": ")
          (output-message)
          (output-irritants)]
        [else 
          (output-who-message)
          (output-irritants)
          (output-message)])
      )))
  (format out "~a~%" output))
(current-exception-printer default-exception-printer)
(define (read-eval-print-loop)
  (define input (current-input-port))

  (let loop () 
    (call/cc (lambda (continue)
      (with-exception-handler 
        (lambda (c)
          (flush-output-port (current-output-port))
          ((current-exception-printer) c)
          (continue #f))
        (lambda ()
          (format #t "> ~!")
          (let ([form (read input)])
            (if (eof-object? form)
              (begin 
                (format #t "Goodbye!~%")
                (flush-output-port (current-output-port))
                (exit 0))
              (begin 
                (let ([ans (eval form (current-module))])
                  (unless (eq? ans (unspecified))
                    (format #t "=> ~a~%~!" ans))))))))))
    (loop))))
