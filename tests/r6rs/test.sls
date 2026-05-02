#!r6rs

(library (tests r6rs test)
  (export test
    test/approx
    test/alts
    test/exn
    test/values
    test/output
    test/unspec
    test/unspec-or-exn
    test/unspec-flonum-or-exn
    test/output/unspec
    run-test
    report-test-results)
  (import (rnrs)
    (core parameters)
    (prefix (srfi 64) test:)
    (only (capy) syntax-sourcev))

  (define-record-type err
    (fields err-c))

  (define-record-type expected-exception
    (fields))

  (define-record-type multiple-results
    (fields values))

  (define-record-type approx
    (fields value))

  (define-record-type alts
    (fields values))

  (define r6rs-test-runners '())
  (define r6rs-test-runner-failures '())
  (define r6rs-test-runner-final-reported '())
  (define r6rs-test-output-port (current-output-port))
  (define r6rs-test-output-suppressed? #f)
  (define r6rs-test-color-reset "\x1b;[0m")
  (define r6rs-test-color-bold "\x1b;[1m")
  (define r6rs-test-color-red "\x1b;[31m")
  (define r6rs-test-color-green "\x1b;[32m")
  (define r6rs-test-color-yellow "\x1b;[33m")
  (define r6rs-test-color-blue "\x1b;[34m")

  (define (r6rs-test-runner-known? runner)
    (exists (lambda (known-runner) (eq? known-runner runner))
      r6rs-test-runners))

  (define (result-source runner)
    (let ([file (test:test-result-ref runner 'source-file #f)]
          [line (test:test-result-ref runner 'source-line #f)])
      (if (and file line)
        (vector file line)
        #f)))

  (define (r6rs-test-runner-ensure-failures! runner)
    (let ([cell (assq runner r6rs-test-runner-failures)])
      (unless cell
        (set! r6rs-test-runner-failures
          (cons (cons runner '()) r6rs-test-runner-failures)))))

  (define (r6rs-test-runner-failures-ref runner)
    (let ([cell (assq runner r6rs-test-runner-failures)])
      (if cell (cdr cell) '())))

  (define (r6rs-test-runner-add-failure! runner failure)
    (set! r6rs-test-runner-failures
      (let loop ([failures r6rs-test-runner-failures])
        (cond
          [(null? failures)
            (list (cons runner (list failure)))]
          [(eq? runner (caar failures))
            (cons (cons runner (cons failure (cdar failures)))
              (cdr failures))]
          [else
            (cons (car failures) (loop (cdr failures)))]))))

  (define (r6rs-test-runner-final-reported? runner)
    (exists (lambda (known-runner) (eq? known-runner runner))
      r6rs-test-runner-final-reported))

  (define (r6rs-test-runner-final-reported! runner)
    (unless (r6rs-test-runner-final-reported? runner)
      (set! r6rs-test-runner-final-reported
        (cons runner r6rs-test-runner-final-reported))))

  (define (r6rs-record-failure! runner)
    (let ([kind (test:test-result-kind runner)])
      (when (memq kind '(fail xpass))
        (r6rs-test-runner-add-failure! runner
          (list (test:test-result-ref runner 'test-name #f)
            (test:test-result-ref runner 'actual-value #f)
            (test:test-result-ref runner 'expected-value #f)
            (result-source runner))))))

  (define (call-with-r6rs-test-output thunk)
    (unless r6rs-test-output-suppressed?
      (parameterize ([current-output-port r6rs-test-output-port])
        (thunk))))

  (define (register-r6rs-test-runner! runner)
    (unless (r6rs-test-runner-known? runner)
      (let ([on-test-end (test:test-runner-on-test-end runner)]
            [on-group-begin (test:test-runner-on-group-begin runner)]
            [on-group-end (test:test-runner-on-group-end runner)]
            [on-final (test:test-runner-on-final runner)]
            [on-bad-count (test:test-runner-on-bad-count runner)]
            [on-bad-end-name (test:test-runner-on-bad-end-name runner)])
        (test:test-runner-on-group-begin! runner
          (lambda (runner name count)
            (call-with-r6rs-test-output
              (lambda () (on-group-begin runner name count)))))
        (test:test-runner-on-group-end! runner
          (lambda (runner)
            (call-with-r6rs-test-output
              (lambda () (on-group-end runner)))))
        (test:test-runner-on-test-end! runner
          (lambda (runner)
            (r6rs-record-failure! runner)
            (call-with-r6rs-test-output
              (lambda () (on-test-end runner)))))
        (test:test-runner-on-final! runner
          (lambda (runner)
            (call-with-r6rs-test-output
              (lambda () (on-final runner)))
            (r6rs-test-runner-final-reported! runner)))
        (test:test-runner-on-bad-count! runner
          (lambda (runner count expected)
            (call-with-r6rs-test-output
              (lambda () (on-bad-count runner count expected)))))
        (test:test-runner-on-bad-end-name! runner
          (lambda (runner begin end)
            (call-with-r6rs-test-output
              (lambda () (on-bad-end-name runner begin end))))))
      (r6rs-test-runner-ensure-failures! runner)
      (set! r6rs-test-runners (cons runner r6rs-test-runners)))
    runner)

  (define (make-r6rs-test-runner)
    (register-r6rs-test-runner! (test:test-runner-simple)))

  (test:test-runner-factory make-r6rs-test-runner)

  (define (ensure-r6rs-test-runner)
    (let ([runner (test:test-runner-current)])
      (if runner
        (register-r6rs-test-runner! runner)
        (let ([runner (test:test-runner-create)])
          (test:test-runner-current runner)
          runner))))

  (define (source->line-info src)
    (if (vector? src)
      (list (cons 'source-file (vector-ref src 0))
        (cons 'source-line (vector-ref src 1)))
      '()))

  ;(define-syntax test
  ;  (syntax-rules ()
  ;    [(_ expr expected)
  ;     (begin
  ;       (run-test 'expr
  ;                 (catch-exns (lambda () expr))
  ;                 expected
  ;                 ))]))
  (define-syntax test
    (lambda (x)
      (syntax-case x ()
        [(_ expr expected)
          (with-syntax ([t (datum->syntax #'expr (syntax-sourcev #'expr))])
            #'(begin
               (run-test 'expr
                (catch-exns (lambda () expr))
                expected
                t)))])))

  (define (catch-exns thunk)
    (guard (c [#t (make-err c)])
      (call-with-values thunk
        (lambda x
          (if (= 1 (length x))
            (car x)
            (make-multiple-results x))))))

  (define-syntax test/approx
    (syntax-rules ()
      [(_ expr expected)
        (run-test 'expr
          (make-approx expr)
          (make-approx expected))]))

  (define-syntax test/alts
    (syntax-rules ()
      [(_ expr expected0 expected ...)
        (run-test 'expr
          expr
          (make-alts (list expected0 expected ...)))]))

  (define (good-enough? x y)
    ;; relative error should be with 0.1%, but greater
    ;; relative error is allowed when the expected value
    ;; is near zero.
    (cond ((not (number? x)) #f)
      ((not (number? y)) #f)
      ((or (not (real? x))
          (not (real? y)))
        (and (good-enough? (real-part x) (real-part y))
          (good-enough? (imag-part x) (imag-part y))))
      ((infinite? x)
        (= x (* 2.0 y)))
      ((infinite? y)
        (= (* 2.0 x) y))
      ((nan? y)
        (nan? x))
      ((> (magnitude y) 1e-6)
        (< (/ (magnitude (- x y))
            (magnitude y))
          1e-3))
      (else
        (< (magnitude (- x y)) 1e-6))))

  (define-syntax test/exn
    (syntax-rules ()
      [(_ expr condition)
        (test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                         (make-expected-exception)])
               expr)
          (make-expected-exception))]))

  (define-syntax test/values
    (syntax-rules ()
      [(_ expr val ...)
        (run-test 'expr
          (catch-exns (lambda () expr))
          (make-multiple-results (list val ...)))]))

  (define-syntax test/output
    (syntax-rules ()
      [(_ expr expected str)
        (run-test 'expr
          (capture-output
            (lambda ()
              (run-test 'expr
                (guard (c [#t (make-err c)])
                  expr)
                expected)))
          str)]))

  (define-syntax test/unspec
    (syntax-rules ()
      [(_ expr)
        (test (begin expr 'unspec) 'unspec)]))

  (define-syntax test/unspec-or-exn
    (syntax-rules ()
      [(_ expr condition)
        (test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                         'unspec])
               (begin expr 'unspec))
          'unspec)]))

  (define-syntax test/unspec-flonum-or-exn
    (syntax-rules ()
      [(_ expr condition)
        (test (guard (c [((condition-predicate (record-type-descriptor condition)) c)
                         'unspec-or-flonum])
               (let ([v expr])
                 (if (flonum? v)
                   'unspec-or-flonum
                   (if (eq? v 'unspec-or-flonum)
                     (list v)
                     v))))
          'unspec-or-flonum)]))

  (define-syntax test/output/unspec
    (syntax-rules ()
      [(_ expr str)
        (test/output (begin expr 'unspec) 'unspec str)]))

  (define (capture-output thunk)
    (if (file-exists? "tmp-catch-out")
      (delete-file "tmp-catch-out"))
    (dynamic-wind
      (lambda () 'nothing)
      (lambda ()
        (let ([old-suppressed? r6rs-test-output-suppressed?])
          (dynamic-wind
            (lambda () (set! r6rs-test-output-suppressed? #t))
            (lambda ()
              (with-output-to-file "tmp-catch-out"
                thunk))
            (lambda () (set! r6rs-test-output-suppressed? old-suppressed?))))
        (call-with-input-file "tmp-catch-out"
          (lambda (p)
            (get-string-n p 1024))))
      (lambda ()
        (if (file-exists? "tmp-catch-out")
          (delete-file "tmp-catch-out")))))

  (define (same-result? got expected)
    (cond
      [(and (real? expected) (nan? expected))
        (and (real? got) (nan? got))]
      [(expected-exception? expected)
        (expected-exception? got)]
      [(approx? expected)
        (and (approx? got)
          (good-enough? (approx-value expected)
            (approx-value got)))]
      [(multiple-results? expected)
        (and (multiple-results? got)
          (= (length (multiple-results-values expected))
            (length (multiple-results-values got)))
          (for-all same-result?
            (multiple-results-values expected)
            (multiple-results-values got)))]
      [(alts? expected)
        (exists (lambda (e) (same-result? got e))
          (alts-values expected))]
      [else (equal? got expected)]))

  (define (run-test expr got expected . src?)
    (ensure-r6rs-test-runner)
    (test:test-compare expr
      (lambda (expected actual)
        (same-result? actual expected))
      expected
      got
      (if (null? src?) '() (source->line-info (car src?)))))

  (define (write-result prefix v)
    (cond
      [(multiple-results? v)
        (for-each (lambda (v)
                   (write-result prefix v))
          (multiple-results-values v))]
      [(approx? v)
        (display prefix)
        (display "approximately ")
        (write (approx-value v))]
      [(alts? v)
        (write-result (string-append prefix "   ")
          (car (alts-values v)))
        (for-each (lambda (v)
                   (write-result (string-append prefix "OR ")
                     v))
          (cdr (alts-values v)))]
      [else
        (display prefix)
        (write v)]))

  (define (runner-test-count runner)
    (+ (test:test-runner-pass-count runner)
      (test:test-runner-fail-count runner)
      (test:test-runner-xpass-count runner)
      (test:test-runner-xfail-count runner)
      (test:test-runner-skip-count runner)))

  (define (sum-runner-count selector)
    (let loop ([runners r6rs-test-runners] [count 0])
      (if (null? runners)
        count
        (loop (cdr runners) (+ count (selector (car runners)))))))

  (define (all-test-count)
    (sum-runner-count runner-test-count))

  (define (write-count label value color)
    (display label)
    (display color)
    (display value)
    (display r6rs-test-color-reset)
    (newline))

  (define (report-total-test-results)
    (let ([pass-count (sum-runner-count test:test-runner-pass-count)]
          [fail-count (sum-runner-count test:test-runner-fail-count)]
          [xfail-count (sum-runner-count test:test-runner-xfail-count)]
          [xpass-count (sum-runner-count test:test-runner-xpass-count)]
          [skip-count (sum-runner-count test:test-runner-skip-count)]
          [total-count (all-test-count)])
      (display (string-append r6rs-test-color-blue r6rs-test-color-bold
                 "%%%% Total test results" r6rs-test-color-reset))
      (newline)
      (write-count "# of total tests          " total-count r6rs-test-color-blue)
      (write-count "# of total PASS           " pass-count r6rs-test-color-green)
      (write-count "# of total FAIL           " fail-count r6rs-test-color-red)
      (write-count "# of total IGNORE         " skip-count r6rs-test-color-yellow)
      (write-count "# of expected failures    " xfail-count r6rs-test-color-green)
      (write-count "# of unexpected successes " xpass-count r6rs-test-color-red)))

  (define (all-test-failures)
    (let loop ([runners r6rs-test-runners] [failures '()])
      (if (null? runners)
        failures
        (loop (cdr runners)
          (append failures (r6rs-test-runner-failures-ref (car runners)))))))

  (define (report-unfinalized-runners)
    (for-each
      (lambda (runner)
        (unless (r6rs-test-runner-final-reported? runner)
          (test:test-on-final-simple runner)
          (r6rs-test-runner-final-reported! runner)))
      (reverse r6rs-test-runners)))

  (define (report-test-results)
    (let ([checked (all-test-count)]
          [failures (all-test-failures)])
      (report-unfinalized-runners)
      (report-total-test-results)
      (unless (null? failures)
        (begin
          (display (length failures))
          (display " tests failed:\n\n")
          (for-each (lambda (t)
                     (display "Expression:\n ")
                     (write (car t))
                     (if (list-ref t 3)
                       (begin
                         (display "\nSource location:\n ")
                         (let* ([sourcev (list-ref t 3)] [file (vector-ref sourcev 0)] [line (vector-ref sourcev 1)])
                           (display file)
                           (display ":")
                           (display line))))
                     (display "\nResult:")
                     (if (err? (cadr t))
                       (begin
                         (display "\n  Exception: ")
                         (write (err-err-c (cadr t)))))
                     (write-result "\n " (cadr t))
                     (display "\nExpected:")
                     (write-result "\n " (caddr t))
                     (display "\n\n"))
            (reverse failures))
          (display (length failures))
          (display " of ")
          (display checked)
          (display " tests failed.\n"))))))
