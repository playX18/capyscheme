(library (core repl)
  (export read-eval-print-loop current-exception-printer default-exception-printer)

  (import (capy)
    (capy term tty)
    (core control)
    (core parameters)
    (core exceptions)
    (srfi 8))

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

  (define (module-name->string name)
    (let loop ([name name] [out ""])
      (cond
        [(null? name) out]
        [(string=? out "")
          (loop (cdr name) (symbol->string (car name)))]
        [else
          (loop (cdr name)
            (string-append out " " (symbol->string (car name))))])))

  (define (repl-prompt)
    (string-append "(" (module-name->string (module-name (current-module))) ")> "))

  (define (print-values vals)
    (cond
      [(null? vals) (unspecified)]
      [(null? (cdr vals))
        (unless (eq? (car vals) (unspecified))
          (format #t "~a~%" (car vals)))]
      [else
        (let loop ([vals vals] [index 0])
          (unless (null? vals)
            (format #t "[~a] = ~a~%" index (car vals))
            (loop (cdr vals) (+ index 1))))])
    (flush-output-port (current-output-port)))

  (define (simple-read-eval-print-loop)
    (let loop ()
      (display (repl-prompt))
      (flush-output-port (current-output-port))
      (let ([form (read (current-input-port))])
        (cond
          [(eof-object? form)
            (newline)
            (exit 0)]
          [else
            (guard (c [else
                       (flush-output-port (current-output-port))
                       ((current-exception-printer) c)])
              (receive ans (eval form (current-module))
                (print-values ans)))
            (loop)]))))

  (define (read-eval-print-loop)
    (if (and (tty? (current-input-port))
         (tty? (current-output-port)))
      (eval '((@ (core fancy-repl) read-eval-print-loop)) (current-module))
      (simple-read-eval-print-loop))))
