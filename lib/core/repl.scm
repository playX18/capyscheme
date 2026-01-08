(library (core repl)
  (export read-eval-print-loop current-exception-printer default-exception-printer)

  (import (capy)
    (core parameters)
    (core exceptions)
    (core control))

  (define dump-condition (make-parameter #f))
  (define self-evaluating-vector-constants (make-parameter #t))
  (define ellipsis/underscore-in-literal (make-parameter #t))
  (define right-arrow-in-case (make-parameter #t))
  (define repl-startup-version (make-parameter #f))

  (define (default-exception-printer c . maybe-out)
    (define out (if (null? maybe-out) (current-error-port) (car maybe-out)))
    (if (marks-condition? c)
      (stack-trace (condition-marks c) out))
    (print-condition c out))
  (current-exception-printer default-exception-printer)

  (define (read-eval-print-loop)
    (define (print-values vals)
      (cond
        [(null? vals) (unspecified)]
        [(null? (cdr vals))
          (unless (eq? (car vals) (unspecified))
            (format #t "=> ~a~%~!" (car vals)))]
        [else
          (let loop ([vals vals] [index 0])
            (unless (null? vals)
              (format #t "[~a] = ~a~%" index (car vals))
              (loop (cdr vals) (+ index 1))))
          (flush-output-port (current-output-port))]))

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
                          (receive ans (eval form (current-module))
                            (print-values ans)))))))))
      (loop))))
