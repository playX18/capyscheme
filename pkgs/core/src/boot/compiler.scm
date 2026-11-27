(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot units) (boot psyntax-exp) (boot str2num) (boot num2str) (boot reader) (boot environments) (boot eval) (boot namespace) (boot time) (boot base))
(|#%provide| all-defined)


(import
  (core compiler tree-il fix-letrec)
  (core compiler tree-il assignment-elimination)
  (core compiler tree-il well-known-procs)
  (core compiler tree-il primitives)
  (core compiler tree-il terms)
  (core pretty-print)
  (core compiler tree-il))


;; Per-pass timing for the Scheme-side compile pipeline.
(define (%profile-phase name thunk)
  (let ([flag (getenv "CAPY_PROFILE_PASSES")])
    (if (and flag (not (equal? flag "")))
      (let ([t0 (microsecond)])
        (call-with-values
          thunk
          (lambda results
            (let ([t1 (microsecond)])
              (display
                (string-append
                  ";; PERF (capy) phase=" name " elapsed_ms="
                  (number->string (quotient (- t1 t0) 1000))
                  "\n")
                (current-error-port))
              (apply values results)))))
      (thunk))))


(define pipeline-key '(core compiler pipeline))

;; The compiler that was installed before this one (the seed in a bootstrap runtime, see
;; `%%file-compiler` in eval.scm). It compiles the libraries of the pipeline itself.
(define fallback-file-compiler (%%file-compiler))

(define resolving-pipeline? #f)

(define (pipeline-instance)
  (library-instance (current-library-registry) pipeline-key))

(define (pipeline-ref name)
  (instance-ref (pipeline-instance) name))

;; Makes the pipeline available: #t when it is, #f when it is being loaded right now (the
;; files compiled meanwhile are the libraries of the pipeline, for the fallback compiler).
(define pipeline-ready? #f)

(define (ensure-pipeline!)
  (cond
    [pipeline-ready? #t]
    [resolving-pipeline? #f]
    [else
      (dynamic-wind
        (lambda () (set! resolving-pipeline? #t))
        (lambda () (resolve-env pipeline-key))
        (lambda () (set! resolving-pipeline? #f)))
      (unless (pipeline-instance)
        (error 'compile-file "the compiler library is not available" pipeline-key))
      (set! pipeline-ready? #t)
      #t]))


(define (split-on-commas str)
  (let loop ([chars (string->list str)] [cur '()] [acc '()])
    (define (flush) (if (null? cur) acc (cons (list->string (reverse cur)) acc)))
    (cond
      [(null? chars) (reverse (flush))]
      [(char=? (car chars) #\,) (loop (cdr chars) '() (flush))]
      [else (loop (cdr chars) (cons (car chars) cur) acc)])))

;; => list of the selected dump names ("ir" "gcps" "ssa" "noopt"), "all" expanded
(define (dump-selection selections)
  (let* ([env (getenv "CAPY_COMPILE_DUMP")]
         [words (append (if env (split-on-commas env) '())
                        (map (lambda (s) (if (symbol? s) (symbol->string s) s)) selections))]
         [noopt? (let ([v (getenv "CAPY_COMPILE_DUMP_NOOPT")])
                   (and v (member v '("1" "true" "yes" "on")) #t))])
    (let loop ([ws words] [acc '()])
      (cond
        [(null? ws) (if noopt? (cons "noopt" acc) acc)]
        [(member (car ws) '("all" "1")) (loop (cdr ws) (append '("ir" "gcps" "ssa" "noopt") acc))]
        [(string=? (car ws) "graph") (loop (cdr ws) (cons "gcps" acc))]
        [else (loop (cdr ws) (cons (car ws) acc))]))))

(define (dump-artifact-path destination suffix)
  (let* ([dir (let ([d (or (getenv "CAPY_COMPILE_DUMP_DIR") (getenv "CAPY_DUMP_DIR"))])
                (if (and d (not (equal? d ""))) d "capy-dumps"))]
         [_ (create-directory* dir)]
         [slash (let loop ([i (- (string-length destination) 1)])
                  (cond [(< i 0) #f] [(char=? (string-ref destination i) #\/) i] [else (loop (- i 1))]))]
         [stem (if slash (substring destination (+ slash 1) (string-length destination)) destination)])
    (string-append dir "/" stem suffix)))

(define (write-dump destination suffix proc)
  (let ([path (dump-artifact-path destination suffix)])
    (call-with-output-file path proc)
    (format (current-error-port) ";; compile: dump -> ~a~%" path)))

(define (write-il-dump destination suffix code)
  (write-dump destination suffix
    (lambda (port)
      (pretty-print (tree-il->scheme code) port))))

;; Writes the dumps selected by SELECTED of the compilation of CODE to DESTINATION.
(define (dump-compilation! selected destination code)
  (when (member "ir" selected)
    (write-il-dump destination ".ir.scm" code))
  (when (or (member "gcps" selected) (member "ssa" selected))
    (let* ([dumps ((pipeline-ref 'compile-preprocessed-il) code #t)]
           [text (lambda (key) (let ([p (assq key dumps)]) (and p (cdr p))))])
      (when (and (member "gcps" selected) (text 'gcps))
        (write-dump destination ".gcps.txt" (lambda (port) (display (text 'gcps) port))))
      (when (text 'gcps-opt)
        (write-dump destination ".gcps.opt.txt" (lambda (port) (display (text 'gcps-opt) port))))
      (when (and (member "ssa" selected) (text 'ssa))
        (write-dump destination ".ssa.txt" (lambda (port) (display (text 'ssa) port)))))))

;; The extension of the compiled artifact, as `%compile` named it.
(define (artifact-name path)
  (let loop ([i (- (string-length path) 1)])
    (cond
      [(< i 0) (string-append path ".fasl")]
      [(char=? (string-ref path i) #\/) (string-append path ".fasl")]
      [(char=? (string-ref path i) #\.) (string-append (substring path 0 i) ".fasl")]
      [else (loop (- i 1))])))

;; Compiles the preprocessed Tree IL CODE to the artifact OUTPUT-FILE, and returns the load
;; thunk of the program, or the path of the artifact when LOAD-THUNK? is false.
(define (compile-code-to-artifact code output-file load-thunk? dump-options)
  (let ([selected (dump-selection dump-options)])
    (when (pair? selected)
      (dump-compilation! selected output-file code)))
  (let* ([bytes (%profile-phase "scheme.compile_il"
                  (lambda () ((pipeline-ref 'compile-il-to-bytevector) code)))]
         [path (%profile-phase "scheme.write_artifact"
                 (lambda () (%store-compiled-image bytes output-file)))])
    (if load-thunk?
      (%load-program-bytevector bytes)
      (or path (artifact-name output-file)))))

(define (scheme-file-compiler filename compiled-path env load-thunk? . maybe-dump-options)
    (define dump-options
      (if (null? maybe-dump-options) '() (car maybe-dump-options)))
    (define (read-all in)
      (%profile-phase
        "scheme.read_all"
        (lambda ()
          (let lp ([exps '()])
            (let ([exp (%runtime-stats-timed-reader (lambda () (read-syntax in)))])
              (cond
                [(eof-object? exp) (reverse exps)]
                [else (lp (cons exp exps))]))))))
    (define output-file (or compiled-path (compiled-file-name filename)))
    (define module (or env (current-namespace) (expansion-env)))
    (*raw-log* log:debug
      '(capy)
      'compile-file
      "Compiling file ~a to ~a"
      filename
      output-file)
    (dynamic-wind
      (lambda ()
        (%runtime-stats-begin-compilation))
      (lambda ()
        (call-with-source-hash (source-file-hash filename) filename
         (lambda ()
         (call-with-input-file filename
          (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
              (let* ([code (%profile-phase "scheme.compile_tree_il"
                             (lambda () (compile-tree-il exps module filename)))]
                     [code (%profile-phase "scheme.resolve_primitives"
                             (lambda () (resolve-primitives code #f)))]
                     [code (%profile-phase "scheme.expand_primitives"
                             (lambda () (expand-primitives code)))]
                     [code (%profile-phase "scheme.fix_letrec"
                             (lambda () (fix-letrec code)))]
                     ;; the machine-independent form `.wpo` files keep, see `write-wpo!`
                     [wpo-code (and (compile-wpo?) code)]
                     [code (%profile-phase "scheme.expand_well_known_procs"
                             (lambda () (expand-well-known-procs code)))]
                     [code (%profile-phase "scheme.assignment_elimination"
                             (lambda () (eliminate-assignments code)))]
                     [result (compile-code-to-artifact code output-file load-thunk? dump-options)])
                (when wpo-code
                  (write-wpo! wpo-code output-file))
                result)))))))
      (lambda ()
        (%runtime-stats-end-compilation))))

(%%file-compiler
  (lambda (filename compiled-path env load-thunk? . maybe-dump-options)
    (if (ensure-pipeline!)
      (apply scheme-file-compiler filename compiled-path env load-thunk? maybe-dump-options)
      (apply fallback-file-compiler filename compiled-path env load-thunk? maybe-dump-options))))
