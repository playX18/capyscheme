;;; -*- mode: scheme; coding: utf-8; -*-
;;; Boot initialization
(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations)
  (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys)
  (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports)
  (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print)
  (boot format) (boot log) (boot match-syntax) (boot units) (boot environments)
  (boot library-manager) (boot condition-printer) (boot psyntax-exp) (boot str2num)
  (boot num2str) (boot reader) (boot eval) (boot namespace) (boot time) (boot linker))
(|#%provide| all-defined)

(let ()
  (define (boot-trace-report port n)
    (when linker-trace?
      (let* ([log (linker-trace-entries)]
             [own (apply + (map caddr log))]
             [sorted (list-sort (lambda (a b) (> (caddr a) (caddr b))) log)])
        (format port ";; boot: ~a libraries, ~a ms in loading them~%" (length log) (quotient own 1000))
        (let loop ([l sorted] [i 0])
          (when (and (pair? l) (< i n))
            (format port ";;   ~a us  ~a~%" (caddr (car l)) (car (car l)))
            (loop (cdr l) (+ i 1)))))))

  ;; Loads the source of a boot file with psyntax: a file already compiled is run, otherwise it
  ;; is compiled, and the unit is run by the linker.
  (define primitive-load
    (lambda (filename)
      (let ()
        (cond
          ;; when bootstrapping we rely on tree-sitter to parse Scheme code
          ((%bootstrapping?)
            (let ([thunk (load-thunk-in-vicinity-k filename
                            (lambda (exps env source)
                              ; the hash of the source is in the decl of its libraries, as when
                              ; the file is compiled by `compile-file`
                              (call-with-source-hash (%file-hash source) source
                                (lambda () (compile-tree-il exps env filename))))
                            (expansion-env) #f)])
              (with-exception-handler
                (lambda (exn)
                  (format #t ";; (primitive) Error loading file '~a'~%" filename)
                  (raise exn))
                (lambda () (linker-run-thunk thunk)))))
          (else
            (let ([thunk-or-path (try-load-compiled filename #f)])
              (cond
                [(procedure? thunk-or-path)
                  (with-exception-handler
                    (lambda (exn)
                      (format (current-error-port) ";; (primitive) Error loading file '~a'~%" filename)
                      (when (and (condition? exn) (serious-condition? exn))
                        ((current-exception-printer) exn (current-error-port)))
                      (raise exn))
                    (lambda ()
                      (*raw-log* log:info
                        '(capy)
                        'primitive-load
                        "Loading file ~a"
                        filename)
                      (if (%time-loads?)
                        ($as-time-goes-by
                          (list 'load filename)
                          (lambda () (linker-run-thunk thunk-or-path)))
                        (linker-run-thunk thunk-or-path))))]
                [else
                  (with-exception-handler
                    (lambda (exn)
                      (format (current-error-port) ";; (primitive) Error compiling file '~a'~%" filename)
                      (when (and (condition? exn) (serious-condition? exn))
                        ((current-exception-printer) exn (current-error-port)))
                      (raise exn))
                    (lambda ()
                      (define filename (list-ref thunk-or-path 0))
                      (define full-filename (list-ref thunk-or-path 1))
                      (define compiled-path (list-ref thunk-or-path 2))
                      (*raw-log* log:info
                        '(capy)
                        'primitive-load
                        "Compiling file ~a"
                        filename)
                      (linker-run-thunk (compile-file full-filename compiled-path (expansion-env) #t))))])))))))

  ;; The ports are initialized once they are all loaded.
  (initialize-io-system)

  ;; A libmap that cannot be used is an error when it was asked for, and is not used otherwise.
  (let ([problem (linker-libmap-problem)])
    (when (and problem %libmap-explicit?)
      (error 'libmap (format #f "cannot use the libmap ~a: ~a" %libmap-path problem))))

  ;; The boot files from here on are expanded by psyntax.
  (linker-use-psyntax! primitive-load)

  (when (equal? (getenv "CAPY_TIME_LOADS") "1")
    (%time-loads? #t))

  ; A library found in both an R6RS and an R7RS file: say which one the mode took.
  (linker-set-ambiguous-source-hook!
    (lambda (key chosen shadowed)
      (warning 'import
        (string-append "library defined in both R6RS and R7RS files; using the one of the "
          (symbol->string (capy:execution-mode)) " mode")
        key chosen shadowed)))

  (linker-require! '(boot base))
  (linker-require! '(core intrinsics))
  (linker-require! '(boot compiler))
  (linker-require! '(boot cli))

  ; The REPL namespace: the core bindings. It is the expansion environment.
  (let ([ns (make-namespace '(capy user) default-namespace-imports)])
    (current-namespace ns)
    (current-expansion-env ns))

  (boot-trace-report (current-error-port) 25))
