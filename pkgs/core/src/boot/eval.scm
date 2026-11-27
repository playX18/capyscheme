(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot units) (boot environments) (boot library-manager) (boot linker) (boot psyntax-exp) (boot str2num) (boot num2str) (boot reader) (boot time))
(|#%provide| all-defined)

(define *compile-backtrace-key* (list 'backtrace))

(define (%runtime-stats-timed-reader thunk)
  (let ([token #f])
    (dynamic-wind
      (lambda ()
        (set! token (%runtime-stats-begin-reader)))
      thunk
      (lambda ()
        (%runtime-stats-end-reader token)))))

(define (%runtime-stats-timed-psyntax thunk)
  (let ([token #f])
    (dynamic-wind
      (lambda ()
        (set! token (%runtime-stats-begin-psyntax)))
      thunk
      (lambda ()
        (%runtime-stats-end-psyntax token)))))

(define (file-unit-key filename)
  (let* ([n (string-length filename)]
         [start (let loop ([i (- n 1)])
                  (cond
                    [(< i 0) 0]
                    [(char=? (string-ref filename i) #\/) (+ i 1)]
                    [else (loop (- i 1))]))]
         [end (let loop ([i (- n 1)])
                (cond
                  [(<= i start) n]
                  [(char=? (string-ref filename i) #\.) i]
                  [else (loop (- i 1))]))])
    (list 'boot (string->symbol (substring filename start end)))))


(define (linklets->il linklets)
  (cond
    [(null? linklets) (make-void #f)]
    [(null? (cdr linklets)) (car linklets)]
    [else
      (make-application #f (make-primref #f 'make-linklet-directory) linklets)]))

;; The linklet terms of the code CODE of a file, the inverse of `linklets->il`.
(define (il->linklets code)
  (cond
    [(linklet-term? code) (list code)]
    [(and (application? code) (primref? (application-operator code))
        (eq? (primref-prim (application-operator code)) 'make-linklet-directory))
      (application-operands code)]
    [else '()]))

(define current-program-load (make-parameter #f))

(define (call-with-program-load program? thunk)
  (let ([saved #f])
    (dynamic-wind
      (lambda ()
        (set! saved (current-program-load))
        (current-program-load program?))
      thunk
      (lambda ()
        (current-program-load saved)))))

(define (form-head-is? form heads)
  (let ([d (syntax->datum form)])
    (and (pair? d) (memq (car d) heads) #t)))

(define (sps-file? filename)
  (let ([n (string-length filename)])
    (and (> n 4) (string=? (substring filename (- n 4) n) ".sps"))))


;; The import sets of an `(import ...)` form, as syntax objects when the form is one, so
;; that diagnostics on them keep their source spans.
(define (import-form-specs f)
  (let ([e (if (syntax? f) (syntax-expression f) f)])
    (if (and (pair? e) (list? (cdr e)))
      (cdr e)
      (cdr (syntax->datum f)))))

(define (program-forms forms filename)
  (and (current-program-load) filename
    (not (let loop ([fs forms])
           (and (pair? fs)
             (or (form-head-is? (car fs) '(library define-library)) (loop (cdr fs))))))
    (let ([imports (filter (lambda (f) (form-head-is? f '(import))) forms)])
      (and (or (pair? imports) (sps-file? filename))
        (list
          (cons 'library
            (cons (list '%program (string->symbol filename))
              (cons '(export)
                (cons (cons 'import
                        (apply append (map import-form-specs imports)))
                  (filter (lambda (f) (not (form-head-is? f '(import)))) forms))))))))))

;; The linklet terms (a list, see `expand-file-units`) of the forms X0 of the file FILENAME
;; (#f for none) expanded in the environment E, with the errors the expander recovers from
;; collected in SINK, in the mode MODE (`collect`, see `expansion-error-mode`).
(define (expand-forms-to-terms x0 e filename mode sink)
  (define key (and filename (file-unit-key filename)))
  (define x (or (and filename (program-forms x0 filename)) x0))
  (define declared? #f)
  (call-with-expansion-errors mode sink
    (lambda ()
      (expand-file-units x e key
        (lambda (exp)
          (if (not declared?)
            (begin
              (set! declared? #t)
              (unit-predeclare! (current-unit) x syntax->datum)))
          (%runtime-stats-timed-psyntax
            (lambda ()
              (macroexpand exp 'c '(compile load eval)))))))))

(define (compile-tree-il x0 e . maybe-filename)
  (define sink (make-diagnostic-sink))
  ; Errors the expander can recover from are collected, and raised together once the file is
  ; expanded, see `raise-collected-errors`.
  (define il
    (linklets->il
      (expand-forms-to-terms x0 e (and (pair? maybe-filename) (car maybe-filename)) 'collect sink)))
  (raise-collected-errors sink il)
  ; what was reported but does not fail the compilation (warnings, see `sink-warnings`) goes to
  ; the handler of the caller, if it has one
  (let ([handler (compile-warning-handler)])
    (when handler
      (let ([warnings (sink-warnings sink)])
        (when (pair? warnings)
          (handler warnings)))))
  il)

;; #f, or a procedure that is called with the list of diagnostics (see (core diagnostics)) that a
;; compilation reported without failing, when there are some. `capyc` prints them.
(define compile-warning-handler (make-parameter #f))

;; The diagnostics of the errors the expander collects come from `condition->diagnostic`.
(set-condition-diagnostician!
  (lambda (c)
    (let ([key '(core diagnostics)])
      (if (not (library-instance (current-library-registry) key))
        (resolve-env key))
      ((instance-ref (library-instance (current-library-registry) key) 'condition->diagnostic) c))))

;; The help of the diagnostics of unbound variables comes from `(core suggest)`, loaded when
;; there is one to explain.
(set-unbound-suggester!
  (lambda (sym visible)
    (let ([key '(core suggest)])
      (if (not (library-instance (current-library-registry) key))
        (resolve-env key))
      ((instance-ref (library-instance (current-library-registry) key) 'suggest-unbound)
        sym visible))))

;; Where the compiler leaves the compiled form of FILE when it is not told: the file of the
;; compile cache the loader looks in (see `try-load-compiled`).
(define (compiled-file-name file)
  (%cache-artifact-path (canonicalize-path-string file)))

;;; An active compiler for files.
;;; Must be overridden by the implementation depending on runtime
;;; status.
(define %%file-compiler (make-parameter #f))

;; First, the initial compiler: the seed of a bootstrap runtime. `(boot compiler)` replaces it by
;; the compiler written in Scheme once that is available.
(%%file-compiler
  (lambda (filename compiled-path env load-thunk? . maybe-dump-options)
    (define dump-options
      (if (null? maybe-dump-options) '() (car maybe-dump-options)))
    (define (read-all in)
      (let lp ([exps '()])
        (let ([exp (%runtime-stats-timed-reader (lambda () (read-syntax in)))])
          (cond
            [(eof-object? exp) (reverse exps)]
            [else (lp (cons exp exps))]))))
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
              (let ([code (compile-tree-il exps module filename)])
                (unless (%bootstrapping?)
                  (error 'compile-file
                    "no compiler is installed: (boot compiler) did not load (core compiler pipeline)"
                    filename))
                ;; The seed emitter, with the Tree IL passes of Rust (`preprocessed` is #f). It
                ;; compiles the libraries of the compiler itself in stage-0.
                (%compile-seed code output-file load-thunk? dump-options #f))))))))
      (lambda ()
        (%runtime-stats-end-compilation)))))

;; Compile `filename` and put output into `compiled-path`.
;; Env is a module where the file is being compiled.
;; load-thunk? indicates whether to return a thunk to initialize compiled
;; file or just compile and return. If its #f use load-thunk-in-vicinity
(define (compile-file file compiled-path env load-thunk? . maybe-dump-options)
  ; The compiled file names its source (spans, backtraces) by its canonical path, however the
  ; file was named by whoever compiles it (`capyc lib/x.scm`, or the loader finding it).
  ;; A file under the working directory is named relative to it, so that what is compiled does
  ;; not depend on where the tree is, nor on whether the file was found through a relative or an
  ;; absolute name (the toolchain stages are byte-identical, see ./bootstrap.sh).
  (define filename
    (if (file-exists? file)
      (let* ([abs (canonicalize-path-string file)]
             [cwd (canonicalize-path-string (current-directory))]
             [prefix (string-append cwd "/")]
             [n (string-length prefix)])
        (if (and (> (string-length abs) n) (string=? (substring abs 0 n) prefix))
          (substring abs n (string-length abs))
          abs))
      file))
  (define (compile)
    (capy:call-with-file-language filename (capy:execution-mode)
      (lambda ()
        (call-with-own-imports env
          (lambda ()
            ((%%file-compiler)
              filename
              compiled-path
              env
              load-thunk?
              (if (null? maybe-dump-options) '() (car maybe-dump-options))))))))
  (if (%time-loads?)
    ($as-time-goes-by (list 'compile-file filename) compile)
    (compile)))

;;; Compiled files and their freshness
;;;
;;; The compiled form of a source file is kept in the compiled load path (installed libraries)
;;; or in the compile cache. The loader does not decide by file times whether one is fresh:
;;; `fasl-needs-recompile?` (environments.scm) compares the hashes of its decls, with the
;;; source and with the libraries it imports. The libraries it imports are found by
;;; `library-fasl-resolver`, which makes them current, so the interface hash it reads is the
;;; one the library has now, see below.

;; Library key -> path of the current compiled file of the library, for the libraries
;; checked in the extent of the outermost load.
(define current-fasls (make-core-hash-equal))
(define current-fasls-depth 0)
;; The keys of the libraries being checked, to stop a cycle.
(define checking-fasls '())

;; Calls THUNK as a part of a load. The libraries found current in the extent of the outermost
;; load are remembered, since a library imported by many is checked once.
(define (call-with-fasl-epoch thunk)
  (dynamic-wind
    (lambda ()
      (if (= current-fasls-depth 0)
        (core-hash-clear! current-fasls))
      (set! current-fasls-depth (+ current-fasls-depth 1)))
    thunk
    (lambda ()
      (set! current-fasls-depth (- current-fasls-depth 1)))))

;; The first of the compiled files CANDIDATES of the source file SOURCE that is fresh, or #f.
;; RESOLVER finds the compiled files of the libraries SOURCE imports (`library-fasl-resolver`
;; unless it is given).
(define (fresh-compiled-candidate source candidates . maybe-resolver)
  (let ([resolver (if (pair? maybe-resolver) (car maybe-resolver) library-fasl-resolver)])
    (let loop ([candidates candidates])
      (cond
        [(null? candidates) #f]
        [(fasl-needs-recompile? (car candidates) source resolver)
          (loop (cdr candidates))]
        [else (car candidates)]))))

;; The resolver of `fasl-needs-recompile?`: the path of the current compiled file of the
;; library KEY, compiling the library first when it has no fresh one, or #f when the library
;; has no file. The library has to be current for its interface hash to say what an importer
;; is compiled against: whether the importer is stale depends on what the library is now
;; (and not whether its file is), and a library that changed in its body only keeps the
;; interface hash it had.
(define (library-fasl-resolver requested)
  (let ([key (linker-canonical-key requested)])
    (cond
      [(core-hash-ref current-fasls key #f)]
      [(member key checking-fasls) #f]
      [else
        (let ([saved checking-fasls])
          (dynamic-wind
            (lambda () (set! checking-fasls (cons key checking-fasls)))
            (lambda ()
              (let ([path (current-library-fasl key)])
                (if path (core-hash-put! current-fasls key path))
                path))
            (lambda () (set! checking-fasls saved))))])))

;; The libmap is trusted: a library it lists is current as it is.
(define (current-library-fasl key)
  (or (linker-libmap-fasl key)
    (let* ([file (linker-library-source key)]
           [found (try-load-thunk-in-vicinity file #t)])
      (cond
        [(procedure? found) #f]
        [(not found) #f]
        [else
          (let* ([source (list-ref found 1)]
                 [destination (list-ref found 2)]
                 [fresh (fresh-compiled-candidate source (list-ref found 3))])
            (or fresh
              (begin
                (compile-file source destination (script-env) #f)
                destination)))]))))

;;; The `.wpo` file of a compiled library
;;;
;;; Next to `foo.fasl` the compiler can leave `foo.wpo` (with `--wpo`, `CAPY_WPO=1` or the
;;; parameter `compile-wpo?`), after the Chez Scheme file of that name. It is a FASL data image
;;; with the machine-independent tree-IL of every library linklet of the file, taken after
;;; the primitives are expanded and the letrecs fixed, and before the closures are converted
;;; and the code is generated. The static linker merges it instead of expanding the source
;;; again. The loader never opens it.
;;;
;;; The image holds `#(#%wpo 1 compile-id linklets)`, where LINKLETS are the `term->datum` of
;;; the linklet terms. Its directory header has the `(name imports exports meta)` entries of
;;; the linklets, as the one of the `.fasl`, so `fasl-library-decls` reads the decls of both.
;;; COMPILE-ID (`library-compile-id`) is derived from the decls, the hash of the source, the
;;; compiler and the interface hashes of the imported libraries. A `.wpo` whose id differs from
;;; the one of the `.fasl` was left by another compilation of the file and is ignored.

(define compile-wpo?
  (make-parameter
    (let ([flag (getenv "CAPY_WPO")])
      (and flag (not (equal? flag "")) (not (equal? flag "0"))))))

;; The path of the `.wpo` file of the compiled file FASL-PATH
(define (wpo-path fasl-path)
  (let ([n (string-length fasl-path)])
    (if (and (> n 5) (string=? (substring fasl-path (- n 5) n) ".fasl"))
      (string-append (substring fasl-path 0 (- n 5)) ".wpo")
      (string-append fasl-path ".wpo"))))

;; Writes the `.wpo` file of the compiled file FASL-PATH from CODE, the code of the file
;; before the closures are converted. Files that define neither libraries nor boot
;; linklets have none.
;;
;; A boot file has no decl to say which compilation a `.wpo` file belongs to. Its id is
;; `(boot source-hash fasl-hash)`: the hash of the source it was expanded from and the
;; hash of the compiled file written by the same compilation, which is current when the
;; build says the boot file is (see `boot-wpo-linklets`).
(define (write-wpo! code fasl-path)
  (let* ([linklets (il->linklets code)]
         [decls (map linklet-term-meta linklets)]
         [directory (map (lambda (t)
                           (list (linklet-term-name t) (linklet-term-imports t)
                             (linklet-term-exports t) (linklet-term-meta t)))
                      linklets)])
    (cond
      [(and (pair? decls) (for-all library-decl? decls))
        (fasl-write
          (vector '|#%wpo| 1 (library-compile-id decls) (map term->datum linklets))
          (wpo-path fasl-path) directory)]
      [(and (pair? decls) (for-all boot-meta? decls) (current-source-hash))
        (fasl-write
          (vector '|#%wpo| 1 (boot-compile-id (current-source-hash) (%file-hash fasl-path))
            (map term->datum linklets))
          (wpo-path fasl-path) directory)])))

(define (boot-meta? meta) (and (pair? meta) (eq? (car meta) '|#%boot|)))

(define (boot-compile-id source-hash fasl-hash) (list 'boot source-hash fasl-hash))

;; The linklet terms of the boot file SOURCE-PATH, which the library KEY names, kept in the
;; `.wpo` file of its compiled file, or #f: when there is no compiled file or `.wpo` file,
;; when the `.wpo` file was left by another compilation than the one of the compiled file,
;; or when the source changed since.
(define (boot-wpo-linklets key source-path)
  (let* ([fasl (linker-libmap-fasl key)]
         [wpo (and fasl (wpo-path fasl))])
    (and wpo (file-exists? wpo) (file-exists? source-path)
      (call/cc
        (lambda (escape)
          (with-exception-handler
            (lambda (cause) (escape #f))
            (lambda ()
              (let ([image (fasl-read wpo)])
                (and (vector? image)
                  (= (vector-length image) 4)
                  (eq? (vector-ref image 0) '|#%wpo|)
                  (eqv? (vector-ref image 1) 1)
                  (equal? (vector-ref image 2)
                    (boot-compile-id (source-file-hash source-path) (%file-hash fasl)))
                  (cons wpo (map datum->term (vector-ref image 3))))))))))))

;; The linklet terms in the `.wpo` file of the compiled file FASL-PATH, or #f when there is no
;; such file, when it was left by another compilation of the file than the one of FASL-PATH, or
;; when it cannot be read.
(define (wpo-linklets fasl-path . maybe-wpo)
  (let ([path (if (pair? maybe-wpo) (car maybe-wpo) (wpo-path fasl-path))]
        [decls (fasl-library-decls fasl-path)])
    (and decls (pair? decls) path (file-exists? path)
      (call/cc
        (lambda (escape)
          (with-exception-handler
            (lambda (cause) (escape #f))
            (lambda ()
              (let ([image (fasl-read path)])
                (and (vector? image)
                  (= (vector-length image) 4)
                  (eq? (vector-ref image 0) '|#%wpo|)
                  (eqv? (vector-ref image 1) 1)
                  (equal? (vector-ref image 2) (library-compile-id decls))
                  (map datum->term (vector-ref image 3)))))))))))

;; Like `library-fasl-resolver`, but never compiles: the path of the compiled file of the
;; library KEY when there is one that is fresh, and so are the compiled files of the libraries
;; it imports, otherwise #f.
(define (library-fasl-lookup requested)
  (let ([key (linker-canonical-key requested)])
    (cond
      [(core-hash-ref current-fasls key #f)]
      [(member key checking-fasls) #f]
      [else
        (let ([saved checking-fasls])
          (dynamic-wind
            (lambda () (set! checking-fasls (cons key checking-fasls)))
            (lambda ()
              (let ([path (existing-library-fasl key)])
                (if path (core-hash-put! current-fasls key path))
                path))
            (lambda () (set! checking-fasls saved))))])))

(define (existing-library-fasl key)
  (or (linker-libmap-fasl key)
    (let* ([file (linker-library-source key)]
           [found (try-load-thunk-in-vicinity file #t)])
      (and (pair? found)
        (fresh-compiled-candidate (list-ref found 1) (list-ref found 3) library-fasl-lookup)))))

;; The linklet terms of the compiled form of the library KEY kept in its `.wpo` file, and the
;; path of the file, or #f: when the library has no fresh compiled file, no `.wpo` file, or one
;; left by another compilation. Nothing is compiled to find out (`library-fasl-lookup`), the
;; caller expands the source of the library instead.
(define (library-wpo-linklets key)
  (call-with-fasl-epoch
    (lambda ()
      (let* ([fasl (library-fasl-lookup key)]
             ; The libmap says whether a library has a current `.wpo` file.
             [wpo (cond
                    [(not fasl) #f]
                    [(linker-libmap-entry key) (linker-libmap-wpo key)]
                    [else (wpo-path fasl)])])
        (and wpo
          (let ([terms (wpo-linklets fasl wpo)])
            (and terms (cons wpo terms))))))))

;; Like `try-load-thunk-in-vicinity`, the thunk of the compiled form of FILENAME, but only
;; when a compiled file of it is fresh; otherwise `(source full-source compiled-path)`, where
;; the file is to be compiled to.
(define (try-load-compiled filename resolve-relative . vicinity)
  (let ([found (apply try-load-thunk-in-vicinity filename resolve-relative vicinity)])
    (if (procedure? found)
      found
      (let ([fresh (fresh-compiled-candidate (list-ref found 1) (list-ref found 3))])
        (if fresh
          (load-thunk-in-vicinity fresh #t)
          (list (list-ref found 0) (list-ref found 1) (list-ref found 2)))))))

(define (load-in-vicinity filename directory)
  (call-with-fasl-epoch (lambda () (load-in-vicinity* filename directory))))

(define load-in-vicinity*
  (lambda (filename directory)
    (let ()
      (define thunk-or-path (try-load-compiled filename #t directory))
      (cond
        [(procedure? thunk-or-path)
          (capy:call-with-file-language filename (capy:execution-mode)
            (lambda () (linker-run-thunk thunk-or-path (script-env))))]
        [else
          (let ([filename (list-ref thunk-or-path 0)]
                [full-filename (list-ref thunk-or-path 1)]
                [compiled-path (list-ref thunk-or-path 2)])
            (capy:call-with-file-language full-filename (capy:execution-mode)
              (lambda ()
                (linker-run-thunk
                  (compile-file full-filename compiled-path (expansion-env) #t)
                  (script-env)))))]))))

; The namespace a script loads into: the current namespace, when there is one.
(define (script-env)
  (or (current-namespace) (expansion-env)))

; Calls THUNK in the current namespace, if there is one, so the script's top-level
; references and definitions go there.
(define (in-script-namespace thunk)
  (if (current-namespace)
    (call-with-expansion-env (current-namespace) thunk)
    (thunk)))

;; Runs the file FILENAME as a program (`-s`, `capy file`): a file with an `import` form is
;; compiled as a closed unit, see `program-forms`. Anything else is `load`.
(define (load-program filename)
  (%load filename #t))

(define (load filename)
  (%load filename #f))

(define (%load filename program?)
  (call-with-fasl-epoch (lambda () (%load* filename program?))))

(define %load*
  (lambda (filename program?)
    (if (and (string? filename) (file-exists? filename) (%static-bundle-file? filename))
      ; A static bundle is a whole program image: run it in a child capy.
      (exit (%run-static-bundle filename '()))
    (let ()
       (in-script-namespace (lambda ()
        (define thunk-or-path (try-load-compiled filename #t))
        (cond
          [(procedure? thunk-or-path)
            (capy:call-with-file-language filename (capy:execution-mode)
              (lambda ()
                (with-exception-handler
                  (lambda (exn)
                    (format (current-error-port) ";; Error loading file '~a'~%" filename)
                    (raise exn))
                  (lambda ()
                    (*raw-log* log:info
                      '(capy)
                      'load
                      "Loading file ~a"
                      filename)
                    (if (%time-loads?)
                      ($as-time-goes-by
                        (list 'load filename)
                        (lambda () (linker-run-thunk thunk-or-path (script-env))))
                      (linker-run-thunk thunk-or-path (script-env)))))))]
          [else
            (let* ([filename (list-ref thunk-or-path 0)]
                   [full-filename (list-ref thunk-or-path 1)]
                   [compiled-path (list-ref thunk-or-path 2)])
              (capy:call-with-file-language full-filename (capy:execution-mode)
                (lambda ()
                  (define unit
                    (with-exception-handler
                      (lambda (exn)
                        (format (current-error-port) ";; Error compiling file '~a'~%" filename)
                        (raise exn))
                      (lambda ()
                        (*raw-log* log:info
                          '(capy)
                          'load
                          "Compiling file ~a"
                          filename)
                        (call-with-program-load program?
                          (lambda ()
                            (compile-file full-filename compiled-path (script-env) #t))))))
                  (with-exception-handler
                    (lambda (exn)
                      (format (current-error-port) ";; Error loading file '~a'~%" filename)
                      (raise exn))
                    (lambda ()
                      (linker-run-thunk unit (script-env)))))))]))))))
    )

;; Whether the datum X is a `library` or `define-library` form.
(define (library-form? x)
  (and (pair? x) (memq (car x) '(library define-library)) #t))

;; Evaluates X in the environment M (the script environment by default).
(define (eval x . m)
  (let ([env (if (or (null? m) (not (car m))) (script-env) (car m))])
    (if (and (namespace? env)
          (env-library-registry env)
          (not (eq? (env-library-registry env) (current-library-registry))))
      ; A namespace of another registry: its libraries are the ones the code sees.
      (call-with-current-namespace env (lambda () (eval x env)))
      (%eval x env))))

(define (%eval x env)
  ; `eval` raises the errors it finds, whatever compiles the code that calls it
  (call-with-expansion-errors 'raise #f (lambda () (%eval* x env))))

(define (%eval* x env)
  (begin
    (if (library-form? x)
      (begin
        (for-each
          (lambda (term) (linker-run-thunk (lambda () (primitive-eval term)) (script-env)))
          (expand-file-units (list x) (script-env) #f
            (lambda (exp) (macroexpand exp 'c '(compile load eval)))))
        (if #f #f))
      (call-with-unit (make-file-unit env #f 'e)
        (lambda ()
          (primitive-eval (macroexpand x 'e '(eval))))))))

;; The library manager is defined before the ports and `load`, so they are handed to it here.
($set-library-services! open-input-file read-char close-port load)
