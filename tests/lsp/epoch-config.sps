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
(define config-root "/tmp/capy-lsp-worker-config-test")
(define config-dep-path "/tmp/capy-lsp-worker-config-test/wcdep.workertest")

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

(define (diagnostic-code? facts code)
  (let ((diagnostics (alist-ref 'diagnostics facts)))
    (and (vector? diagnostics)
         (let loop ((i 0))
           (and (< i (vector-length diagnostics))
                (or (let ((entry (vector-ref diagnostics i)))
                      (and (list? entry)
                           (let ((code-entry (assq 'code entry)))
                             (and code-entry
                                  (string=? (cdr code-entry) code)))))
                    (loop (+ i 1))))))))

(define (import-fact facts name)
  (let ((imports (alist-ref 'imports facts)))
    (and (vector? imports)
         (let loop ((i 0))
           (and (< i (vector-length imports))
                (let ((entry (vector-ref imports i)))
                  (if (and (list? entry)
                           (let ((name-entry (assq 'name entry)))
                             (and name-entry
                                  (string=? (cdr name-entry) name))))
                      entry
                      (loop (+ i 1)))))))))

(define (range-nonzero? range)
  (let* ((start (and (list? range) (alist-ref 'start range)))
         (end (and (list? range) (alist-ref 'end range)))
         (start-line (and (list? start) (alist-ref 'line start)))
         (start-character (and (list? start) (alist-ref 'character start)))
         (end-line (and (list? end) (alist-ref 'line end)))
         (end-character (and (list? end) (alist-ref 'character end))))
    (and (number? start-line)
         (number? start-character)
         (number? end-line)
         (number? end-character)
         (or (not (= start-line end-line))
             (not (= start-character end-character))))))

(define (call-graph-edge? facts caller callee)
  (let* ((call-graph (alist-ref 'callGraph facts))
         (edges (and (list? call-graph) (alist-ref 'edges call-graph))))
    (and (vector? edges)
         (let loop ((i 0))
           (and (< i (vector-length edges))
                (or (let ((edge (vector-ref edges i)))
                      (and (list? edge)
                           (let ((caller-entry (assq 'caller edge))
                                 (callee-entry (assq 'callee edge)))
                             (and caller-entry
                                  callee-entry
                                  (string=? (cdr caller-entry) caller)
                                  (string=? (cdr callee-entry) callee)))))
                    (loop (+ i 1))))))))

(define (check condition message)
  (unless condition
    (assertion-violation 'epoch-config message)))

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
(ensure-directory config-root)
(set! %load-path (cons root %load-path))

(write-file dep-path
            "(library (dep) (export dep-a dep-b) (import (rnrs)) (define dep-a 1) (define dep-b 2))\n")
(write-file consumer-path
            "(library (consumer) (export dep-use) (import (rnrs) (only (dep) dep-a)) (define dep-use 1))\n")
(write-file config-dep-path
            "(library (wcdep) (export wc-value) (import (rnrs)) (define wc-value 7))\n")

(let ((initial (analyze-consumer 1)))
  (check (completion-label? initial "dep-a") "selected imported export missing")
  (check (not (completion-label? initial "dep-b"))
         "unselected imported export was visible")
  (let ((import (import-fact initial "dep")))
    (check import "dep import fact missing")
    (check (range-nonzero? (alist-ref 'range import))
           "dep import fact range was not populated")
    (check (alist-ref 'resolved import)
           "dep import fact was not marked resolved")
    (check (not (string? (alist-ref 'error import)))
           "resolved dep import reported an error")))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/calls.scm"
                               "(define (foo x) (bar x))\n(define (bar y) (+ y 1))\n"
                               1
                               "/tmp/capy-lsp-refresh-test/calls.scm")))
  (check (call-graph-edge? facts "foo" "bar") "call graph edge foo -> bar missing")
  (check (call-graph-edge? facts "bar" "+") "call graph primitive edge bar -> + missing"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/bad.scm"
                               "(define x 1\n"
                               1
                               "/tmp/capy-lsp-refresh-test/bad.scm")))
  (check (diagnostic-code? facts "syntax") "syntax diagnostic missing"))

(let* ((response
         (dispatch-response
           `((id . 10)
             (method . "analyze-document")
             (params . ((uri . "file:///tmp/capy-lsp-refresh-test/consumer")
                        (path . ,consumer-path)
                        (version . 2)
                        (text . ,(call-with-input-file consumer-path get-string-all))
                        (workspaceEpoch . 42)
                        (configFingerprint . 99)
                        (configRoot . ,root)
                        (loadPath . ,(list->vector (list root)))
                        (compiledLoadPath . ,(list->vector '()))
                        (extensions . ,(list->vector (list "sls" "scm")))
                        (defaultModule . ,(list->vector (list "capy" "user"))))))))
       (result (alist-ref 'result response)))
  (check (alist-ref 'ok response) "epoch/config analysis request failed")
  (check (= (alist-ref 'workspaceEpoch result) 42)
         "worker did not echo workspace epoch")
  (check (= (alist-ref 'configFingerprint result) 99)
         "worker did not echo config fingerprint"))

(let* ((response
         (dispatch-response
           `((id . 11)
             (method . "analyze-document")
             (params . ((uri . "file:///tmp/capy-lsp-worker-config-test/use.scm")
                        (path . "/tmp/capy-lsp-worker-config-test/use.scm")
                        (version . 1)
                        (text . "(import (only (wcdep) wc-value))\n(define local wc-value)\n")
                        (workspaceEpoch . 77)
                        (configFingerprint . 123)
                        (loadPath . ,(list->vector (list config-root)))
                        (compiledLoadPath . ,(list->vector '()))
                        (extensions . ,(list->vector (list "workertest"))))))))
       (result (alist-ref 'result response)))
  (check (alist-ref 'ok response) "worker config load-path analysis failed")
  (check (completion-label? result "wc-value")
         "worker did not apply request loadPath/extensions"))

(let* ((response
         (dispatch-response
           `((id . 12)
             (method . "run-action")
             (params . ((uri . "file:///tmp/capy-lsp-refresh-test/action.scm")
                        (path . "/tmp/capy-lsp-refresh-test/action.scm")
                        (version . 1)
                        (text . "(+ 1 2)\n")
                        (action . "capy.lsp.action.expand")
                        (workspaceEpoch . 88)
                        (configFingerprint . 456))))))
       (result (alist-ref 'result response)))
  (check (alist-ref 'ok response) "epoch/config action request failed")
  (check (= (alist-ref 'workspaceEpoch result) 88)
         "worker action did not echo workspace epoch")
  (check (= (alist-ref 'configFingerprint result) 456)
         "worker action did not echo config fingerprint"))

(display "lsp module resolution ok\n")
