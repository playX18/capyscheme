#!r6rs

(import (rnrs)
        (rnrs io simple)
        (rnrs files)
        (capy)
        (capy lsp analysis)
        (capy lsp worker)
        (srfi 180))

(define root "/tmp/capy-lsp-refresh-test")
(define bar-path "/tmp/capy-lsp-refresh-test/bar.sls")
(define baz-path "/tmp/capy-lsp-refresh-test/baz.sls")
(define dep-path "/tmp/capy-lsp-refresh-test/dep.sls")
(define consumer-path "/tmp/capy-lsp-refresh-test/consumer.sls")
(define foo-path "/tmp/capy-lsp-refresh-test/foo.sls")

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

(define (module-invalidated? result name)
  (let ((modules (alist-ref 'invalidatedModules result)))
    (and (vector? modules)
         (let loop ((i 0))
           (and (< i (vector-length modules))
                (or (string=? (vector-ref modules i) name)
                    (loop (+ i 1))))))))

(define (check condition message)
  (unless condition
    (assertion-violation 'incremental-refresh message)))

(define (json->string obj)
  (let ((port (open-output-string)))
    (json-write obj port)
    (get-output-string port)))

(define (dispatch-result request)
  (let ((response (json-read (open-input-string (dispatch-json (json->string request))))))
    (check (alist-ref 'ok response) "worker request failed")
    (alist-ref 'result response)))

(ensure-directory root)
(set! %load-path (cons root %load-path))
(write-file bar-path
            "(library (bar) (export old) (import (rnrs)) (define old 1))\n")
(write-file foo-path
            "(library (foo) (export probe) (import (rnrs) (bar)) (define probe old))\n")

(let ((initial (analyze-document "file:///tmp/capy-lsp-refresh-test/foo"
                                 (call-with-input-file foo-path get-string-all)
                                 1
                                 foo-path)))
  (check (completion-label? initial "old") "initial import export missing")
  (check (not (completion-label? initial "new")) "new export visible before invalidation"))

(write-file bar-path
            "(library (bar) (export new) (import (rnrs)) (define new 2))\n")

(let ((invalidation (invalidate-files (list bar-path) '())))
  (check (module-invalidated? invalidation "bar") "bar was not invalidated")
  (check (module-invalidated? invalidation "foo") "dependent foo was not invalidated")
  (check (not (resolve-module '(bar) #f #f)) "bar module remained loaded after invalidation")
  (check (not (resolve-module '(foo) #f #f)) "foo module remained loaded after invalidation")
  (check (alist-ref 'restartRequired invalidation)
         "loaded module invalidation did not request restart"))

(let ((worker-invalidation
        (dispatch-result
          `((id . 10)
            (method . "invalidate-files")
            (params . ((paths . ,(list->vector (list baz-path)))
                       (openDocuments
                         . ,(list->vector
                              (list `((path . ,baz-path)
                                      (text . "(library (baz) (export baz-value) (import (rnrs)) (define baz-value 3))\n")))))))))))
  (check (module-invalidated? worker-invalidation "baz")
         "worker openDocuments invalidation override was not used")
  (check (not (alist-ref 'restartRequired worker-invalidation))
         "unloaded override invalidation requested restart"))

(write-file dep-path
            "(library (dep) (export dep-old) (import (rnrs)) (define dep-old 1))\n")
(write-file consumer-path
            "(library (consumer) (export dep-use) (import (rnrs) (dep)) (define dep-use dep-old))\n")
(let ((consumer (analyze-document "file:///tmp/capy-lsp-refresh-test/consumer"
                                  (call-with-input-file consumer-path get-string-all)
                                  1
                                  consumer-path)))
  (check (completion-label? consumer "dep-old") "imported dependency completion missing"))
(write-file dep-path "(library (dep)\n")
(let ((invalidation (invalidate-files (list dep-path) '())))
  (check (module-invalidated? invalidation "dep")
         "unparsed imported dependency file did not invalidate module")
  (check (module-invalidated? invalidation "consumer")
         "unparsed imported dependency file did not invalidate dependent")
  (check (alist-ref 'restartRequired invalidation)
         "unparsed loaded dependency invalidation did not request restart"))

(write-file bar-path "(library (bar)\n")
(invalidate-files (list bar-path) '())

(display "incremental refresh ok\n")
