#!r6rs

(import (rnrs)
        (rnrs io simple)
        (rnrs files)
        (capy)
        (capy lsp analysis)
        (capy lsp worker)
        (srfi 180))

(define root "/tmp/capy-lsp-refresh-test")
(define dep-path "/tmp/capy-lsp-refresh-test/dep.sls")
(define consumer-path "/tmp/capy-lsp-refresh-test/consumer.sls")

(define (ensure-directory path)
  (guard (_ [else (unspecified)])
    (create-directory path)))

(define (write-file path text)
  (when (file-exists? path)
    (delete-file path))
  (call-with-output-file path
    (lambda (port)
      (display text port))))

(define (alist-ref key alist)
  (cond
    [(assq key alist) => cdr]
    [else #f]))

(define (completion-label? facts label)
  (let ((completions (alist-ref 'completions facts)))
    (and (vector? completions)
         (let loop ((i 0))
           (and (< i (vector-length completions))
                (or (let ((entry (vector-ref completions i)))
                      (and (list? entry)
                           (let ((label-entry (assq 'label entry)))
                             (and label-entry
                                  (string=? (cdr label-entry) label)))))
                    (loop (+ i 1))))))))

(define (check condition message)
  (unless condition
    (assertion-violation 'incremental-refresh message)))

(define (json->string obj)
  (let ((port (open-output-string)))
    (json-write obj port)
    (get-output-string port)))

(define (dispatch-response request)
  (json-read (open-input-string (dispatch-json (json->string request)))))

(define (analyze-consumer version)
  (analyze-document "file:///tmp/capy-lsp-refresh-test/consumer"
                    (call-with-input-file consumer-path get-string-all)
                    version
                    consumer-path))

(ensure-directory root)
(set! %load-path (cons root %load-path))

(write-file dep-path
            "(library (dep) (export dep-a dep-b) (import (rnrs)) (define dep-a 1) (define dep-b 2))\n")
(write-file consumer-path
            "(library (consumer) (export dep-use) (import (rnrs) (only (dep) dep-a)) (define dep-use 1))\n")

(let ((initial (analyze-consumer 1)))
  (check (completion-label? initial "dep-a") "selected imported export missing")
  (check (not (completion-label? initial "dep-b"))
         "unselected imported export was visible"))

(let* ((response
         (dispatch-response
           `((id . 10)
             (method . "invalidate-files")
             (params . ((paths . ,(list->vector (list dep-path))))))))
       (error (alist-ref 'error response)))
  (check (not (alist-ref 'ok response)) "invalidate-files unexpectedly succeeded")
  (check (and (list? error)
              (let ((code (alist-ref 'code error)))
                (and (string? code)
                     (string=? code "unknown-method"))))
         "invalidate-files did not report unknown method"))

(display "lsp module resolution ok\n")
