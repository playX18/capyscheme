(library (srfi 78)
  ;; SRFI-78: lightweight testing.
  ;;
  ;;   (check (foo 1) 42)        ; => #t on success, #f on failure
  ;;   (check-report)            ; prints "checks: N, failures: M"
  ;;   (check-set-mode! 'report-failed)
  (export check check-reset! check-report check-set-mode!)
  (import (capy))

  (define %check-count (make-parameter 0))
  (define %failure-count (make-parameter 0))
  (define %report-mode (make-parameter 'summary))

  (define (check expr expected)
    (let ([actual expr])
      (%check-count (+ (%check-count) 1))
      (if (equal? actual expected)
        #t
        (begin
          (%failure-count (+ (%failure-count) 1))
          (when (memq (%report-mode) '(report-failed report-all))
            (format (current-error-port)
              "check failed:~%  got:      ~s~%  expected: ~s~%"
              actual expected))
          #f))))

  (define (check-reset!)
    (%check-count 0)
    (%failure-count 0))

  (define (check-report)
    (format (current-output-port)
      "checks: ~a, failures: ~a~%"
      (%check-count) (%failure-count)))

  (define (check-set-mode! mode)
    (unless (memq mode '(off summary report-failed report-all))
      (error 'check-set-mode! "invalid report mode: ~a" mode))
    (%report-mode mode)))
