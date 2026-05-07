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
(define origin-path "/tmp/capy-lsp-refresh-test/origin.sls")
(define reexport-path "/tmp/capy-lsp-refresh-test/reexport.sls")
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

(define (completion-fact facts label)
  (let ((completions (alist-ref 'completions facts)))
    (and (vector? completions)
         (let loop ((i 0))
           (and (< i (vector-length completions))
                (let ((entry (vector-ref completions i)))
                  (if (and (list? entry)
                           (let ((label-entry (assq 'label entry)))
                             (and label-entry
                                  (string=? (cdr label-entry) label))))
                      entry
                      (loop (+ i 1)))))))))

(define (completion-source? facts label source-module source-name)
  (let ((entry (completion-fact facts label)))
    (and entry
         (let ((module-entry (assq 'sourceModule entry))
               (name-entry (assq 'sourceName entry)))
           (and module-entry
                name-entry
                (string=? (cdr module-entry) source-module)
                (string=? (cdr name-entry) source-name))))))

(define (string-contains? text needle)
  (let ((text-len (string-length text))
        (needle-len (string-length needle)))
    (let loop ((i 0))
      (cond
        [(> (+ i needle-len) text-len) #f]
        [(string=? (substring text i (+ i needle-len)) needle) #t]
        [else (loop (+ i 1))]))))

(define (completion-detail-contains? facts label text)
  (let ((entry (completion-fact facts label)))
    (and entry
         (let ((detail-entry (assq 'detail entry)))
           (and detail-entry
                (string? (cdr detail-entry))
                (string-contains? (cdr detail-entry) text))))))

(define (completion-documentation-contains? facts label text)
  (let ((entry (completion-fact facts label)))
    (and entry
         (let ((documentation-entry (assq 'documentation entry)))
           (and documentation-entry
                (string? (cdr documentation-entry))
                (string-contains? (cdr documentation-entry) text))))))

(define (symbol-fact facts label)
  (let ((symbols (alist-ref 'symbols facts)))
    (and (vector? symbols)
         (let loop ((i 0))
           (and (< i (vector-length symbols))
                (let ((entry (vector-ref symbols i)))
                  (if (and (list? entry)
                           (let ((name-entry (assq 'name entry)))
                             (and name-entry
                                  (string=? (cdr name-entry) label))))
                      entry
                      (loop (+ i 1)))))))))

(define (range-start-at? range line character)
  (let* ((start (and (list? range) (alist-ref 'start range)))
         (start-line (and (list? start) (alist-ref 'line start)))
         (start-character (and (list? start) (alist-ref 'character start))))
    (and (number? start-line)
         (number? start-character)
         (= start-line line)
         (= start-character character))))

(define (symbol-start-at? facts label line character)
  (let ((symbol (symbol-fact facts label)))
    (and symbol
         (range-start-at? (alist-ref 'selectionRange symbol) line character))))

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

(define (vector-string? value expected)
  (and (vector? value)
       (let loop ((i 0))
         (and (< i (vector-length value))
              (or (and (string? (vector-ref value i))
                       (string=? (vector-ref value i) expected))
                  (loop (+ i 1)))))))

(define (vector-string-contains? value expected)
  (and (vector? value)
       (let loop ((i 0))
         (and (< i (vector-length value))
              (or (and (string? (vector-ref value i))
                       (string-contains? (vector-ref value i) expected))
                  (loop (+ i 1)))))))

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

(define (analyze-text name text)
  (analyze-document (string-append "file:///tmp/capy-lsp-refresh-test/" name)
                    text
                    1
                    (string-append root "/" name)))

(define (check-dep-import facts message)
  (let ((import (import-fact facts "dep")))
    (check import (string-append message ": dep import fact missing"))
    (check (range-nonzero? (alist-ref 'range import))
           (string-append message ": dep import fact range was not populated"))
    (check (alist-ref 'resolved import)
           (string-append message ": dep import fact was not marked resolved"))
    (check (not (string? (alist-ref 'error import)))
           (string-append message ": resolved dep import reported an error"))))

(define (check-only-dep-a facts message)
  (check (completion-label? facts "dep-a")
         (string-append message ": selected imported export missing"))
  (check (not (completion-label? facts "dep-b"))
         (string-append message ": unselected imported export was visible"))
  (check (completion-source? facts "dep-a" "dep" "dep-a")
         (string-append message ": dep-a completion source metadata missing"))
  (check-dep-import facts message))

(ensure-directory root)
(ensure-directory config-root)
(set! %load-path (cons root %load-path))

(write-file dep-path
            "(library (dep)\n  (export dep-a dep-b)\n  (import (rnrs))\n  (define dep-a 1)\n  (define dep-b 2))\n")
(write-file origin-path
            "(library (origin) (export origin-a) (import (rnrs)) (define origin-a 1))\n")
(write-file reexport-path
            "(library (reexport) (export origin-a) (import (rnrs) (origin)))\n")
(write-file consumer-path
            "(library (consumer) (export dep-use) (import (rnrs) (only (dep) dep-a)) (define dep-use 1))\n")
(write-file config-dep-path
            "(library (wcdep) (export wc-value) (import (rnrs)) (define wc-value 7))\n")

(let ((initial (analyze-consumer 1)))
  (check-only-dep-a initial "top-level only import"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/dep.sls"
                               (call-with-input-file dep-path get-string-all)
                               1
                               dep-path)))
  (check (symbol-start-at? facts "dep-a" 3 10)
         "imported module dep-a definition range did not point at binding name")
  (check (symbol-start-at? facts "dep-b" 4 10)
         "imported module dep-b definition range did not point at binding name"))

(check-only-dep-a
  (analyze-text "library-import.sls"
                "(library (consumer library-import) (export dep-use) (import (rnrs) (only (dep) dep-a)) (define dep-use dep-a))\n")
  "library only import")

(check-only-dep-a
  (analyze-text "define-library-import.sld"
                "(define-library (consumer define-library-import) (export dep-use) (import (only (dep) dep-a)) (begin (define dep-use dep-a)))\n")
  "define-library only import")

(let ((facts (analyze-text "except-import.scm"
                           "(import (except (dep) dep-a))\n(define local dep-b)\n")))
  (check (completion-label? facts "dep-b") "except import exposed dep-b")
  (check (not (completion-label? facts "dep-a")) "except import exposed excluded dep-a")
  (check (completion-source? facts "dep-b" "dep" "dep-b")
         "except import source metadata missing")
  (check-dep-import facts "except import"))

(let ((facts (analyze-text "prefix-import.scm"
                           "(import (prefix (dep) p-))\n(define local p-dep-a)\n")))
  (check (completion-label? facts "p-dep-a") "prefix import missing p-dep-a")
  (check (completion-label? facts "p-dep-b") "prefix import missing p-dep-b")
  (check (completion-source? facts "p-dep-a" "dep" "dep-a")
         "prefix import p-dep-a source metadata missing")
  (check (completion-source? facts "p-dep-b" "dep" "dep-b")
         "prefix import p-dep-b source metadata missing")
  (check-dep-import facts "prefix import"))

(let ((facts (analyze-text "rename-import.scm"
                           "(import (rename (dep) (dep-a renamed-a)))\n(define local renamed-a)\n")))
  (check (completion-label? facts "renamed-a") "rename import missing renamed-a")
  (check (not (completion-label? facts "dep-a")) "rename import exposed renamed-away dep-a")
  (check (completion-label? facts "dep-b") "rename import removed unrelated dep-b")
  (check (completion-source? facts "renamed-a" "dep" "dep-a")
         "rename import source metadata missing")
  (check-dep-import facts "rename import"))

(check-only-dep-a
  (analyze-text "for-import.scm"
                "(import (for (only (dep) dep-a) run))\n(define local dep-a)\n")
  "for import")

(check-only-dep-a
  (analyze-text "library-wrapper-import.scm"
                "(import (only (library (dep)) dep-a))\n(define local dep-a)\n")
  "library wrapper import")

(check-only-dep-a
  (analyze-text "begin-import.scm"
                "(begin (import (only (dep) dep-a)))\n(define local dep-a)\n")
  "begin-wrapped import")

(check-only-dep-a
  (analyze-text "macro-import.scm"
                "(define-syntax add-dep-import (syntax-rules () [(_) (import (only (dep) dep-a))]))\n(add-dep-import)\n(define local dep-a)\n")
  "macro-expanded import")

(let ((facts (analyze-text "reexport-import.scm"
                           "(import (reexport))\n(define local origin-a)\n")))
  (check (completion-label? facts "origin-a") "re-export import missing origin-a")
  (check (completion-source? facts "origin-a" "origin" "origin-a")
         "re-export import did not report original source module")
  (check (completion-detail-contains? facts "origin-a" "defined in origin")
         "re-export import detail did not report defining module")
  (check (completion-detail-contains? facts "origin-a" "re-exported from reexport")
         "re-export import detail did not report re-exporting module"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/calls.scm"
                               "(define (foo x) (bar x))\n(define (bar y) (+ y 1))\n"
                               1
                               "/tmp/capy-lsp-refresh-test/calls.scm")))
  (check (symbol-start-at? facts "foo" 0 9)
         "local function foo definition range did not point at binding name")
  (check (symbol-start-at? facts "bar" 1 9)
         "local function bar definition range did not point at binding name")
  (check (completion-detail-contains? facts "foo" "(foo x)")
         "local function foo completion detail did not include signature")
  (check (call-graph-edge? facts "foo" "bar") "call graph edge foo -> bar missing")
  (check (call-graph-edge? facts "bar" "+") "call graph primitive edge bar -> + missing"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/local-doc.scm"
                               "(define (documented value)\n  \"Local function docs.\"\n  (+ value 1))\n(documented 41)\n"
                               1
                               "/tmp/capy-lsp-refresh-test/local-doc.scm")))
  (check (symbol-start-at? facts "documented" 0 9)
         "documented local function definition range did not point at binding name")
  (check (completion-detail-contains? facts "documented" "(documented value)")
         "documented local function completion detail did not include signature")
  (check (completion-documentation-contains? facts "documented" "Local function docs.")
         "documented local function completion documentation missing"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/body-local-doc.scm"
                               "(let ()\n  (define (inner x) \"Inner docs.\" (+ x 1))\n  (inner 1))\n"
                               1
                               "/tmp/capy-lsp-refresh-test/body-local-doc.scm")))
  (check (symbol-start-at? facts "inner" 1 11)
         "body-local function definition range did not point at binding name")
  (check (completion-detail-contains? facts "inner" "(inner x)")
         "body-local function completion detail did not include signature")
  (check (completion-documentation-contains? facts "inner" "Inner docs.")
         "body-local function completion documentation missing"))

(let ((facts (analyze-document "file:///tmp/capy-lsp-refresh-test/record.scm"
                               "(import (rnrs records syntactic))\n(define-record-type point (fields x y))\n(make-point 42 42)\n"
                               1
                               "/tmp/capy-lsp-refresh-test/record.scm")))
  (check (completion-label? facts "point") "record type completion missing")
  (check (completion-label? facts "make-point") "record constructor completion missing")
  (check (completion-label? facts "point?") "record predicate completion missing")
  (check (completion-label? facts "point-x") "record accessor x completion missing")
  (check (completion-label? facts "point-y") "record accessor y completion missing"))

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
           `((id . 13)
             (method . "load-path")
             (params . ((loadPath . ,(list->vector (list config-root)))
                        (compiledLoadPath . ,(list->vector '()))
                        (extensions . ,(list->vector (list "workertest"))))))))
       (result (alist-ref 'result response)))
  (check (alist-ref 'ok response) "worker load-path request failed")
  (check (vector-string? (alist-ref 'loadPath result) config-root)
         "worker load-path result did not include requested loadPath")
  (check (vector-string-contains? (alist-ref 'loadPath result) "/lib")
         "worker load-path result did not include worker %load-path")
  (check (vector-string? (alist-ref 'extensions result) "workertest")
         "worker load-path result did not include worker extensions"))

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
