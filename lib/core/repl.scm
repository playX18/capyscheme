(library (core repl)
  (export read-eval-print-loop current-exception-printer default-exception-printer)

  (import (capy)
    (core control)
    (core parameters)
    (core exceptions))

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
    (eval '((@ (core fancy-repl) read-eval-print-loop)) (current-module))))
