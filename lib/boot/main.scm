;; Boot library. Loads all the functionality necessary to run R6RS code
;; including macro-expander. 

;; `primitive-load` is defined here because we do not depend on its overloaded
;; version from `boot/eval.scm`.

(let ()
;; The meta of a boot linklet is `(#%boot key late-key ...)`, KEY being #f when the file is
;; expanded in the core environment. 
(define ($boot-meta? meta)
  (and (pair? meta) (eq? (car meta) '|#%boot|)))

(define ($boot-key? key)
  (and (pair? key) (eq? (car key) 'boot)))

;; The import instances of a linklet in the registry REG, one per import group. Written by
;; hand because `map` is not defined yet at this point.
(define ($linklet-import-instances reg imports)
  (if (null? imports)
      '()
      (let ([key (car (car imports))])
        ; A boot key that is not instantiated yet is a late import: 
        ; it gets the placeholder instance, which the linklet of the
        ; key takes over when it is instantiated. Other keys are loaded from the compile
        ; cache, which did not expand, so their imports may not be loaded yet:
        ; autoloading the library of the key loads and registers them.
        (let ([inst (or (library-instance reg key)
                        (if ($boot-key? key)
                            (library-instance-or-placeholder reg key)
                            (begin
                              (ensure-library-loaded! key)
                              (or (library-instance reg key)
                                  (begin (resolve-library key) (library-instance reg key))))))])
          (if inst
              (cons inst ($linklet-import-instances reg (cdr imports)))
              (error '$boot-run-unit-thunk "missing library instance" key))))))

;; Whether environments.scm is loaded. Until it is, linklets are instantiated
;; without environments, and their exports are kept in `$early-exports` to be put
;; into the core environment once it exists.
(define $envs-ready? #f)
(define $early-exports '())

;; The registry linklets are instantiated in: the one of the current namespace, and the
;; root registry until the environments (and so the namespaces) exist.
(define ($current-registry)
  (if $envs-ready?
      (current-library-registry)
      (root-library-registry)))

;; Runs the body of a linklet with the expansion environment of `meta` current, after
;; the exports are put into that environment (see `$instantiate-linklet-values`).
(define ($run-body-in-env meta exports inst body slots)
  (let ([env (let ([key (if ($boot-meta? meta) (car (cdr meta)) meta)])
               (if (pair? key) (ensure-env key) (expansion-env)))]
        [saved-env (current-expansion-env)])
    (current-expansion-env env)
    (env-mirror-exports! env inst exports)
    (apply body slots)
    (current-expansion-env saved-env)))

;; Runs the body of a library linklet. The library gets no expansion environment here:
;; when psyntax built one while compiling the library in this process, it is given the
;; instance variables, and otherwise it is built from the meta when something expands
;; against the library
(define ($run-library-body meta exports inst body slots)
  (let ([env (env-library-instantiated! meta inst exports)])
    (if env
        (let ([saved-env (current-expansion-env)])
          (current-expansion-env env)
          (apply body slots)
          (current-expansion-env saved-env))
        (apply body slots))))

;; Runs a linklet
(define ($instantiate-linklet-values name imports exports body meta)
  (let* ([reg ($current-registry)]
         [lk (make-linklet name imports exports body meta)]
         [inst (or (take-placeholder-instance! reg name) (make-linklet-instance name #f))])
    (let ([slots (linklet-slots reg lk ($linklet-import-instances reg imports) inst)])
      (if $envs-ready?
          (if (library-meta? meta)
              ($run-library-body meta exports inst body slots)
              ($run-body-in-env meta exports inst body slots))
          (begin
            (set! $early-exports (cons (cons inst exports) $early-exports))
            (apply body slots))))
    (register-library-instance! reg name inst)))

;; The import instances of the groups of a namespace linklet. 
(define ($namespace-import-instances reg imports ns)
  (let ([own (env-instance ns)])
    (for-each (lambda (sym) (instance-ensure-variable! own sym)) (cdr (car imports)))
    (cons own ($linklet-import-instances reg (cdr imports)))))

;; Runs a namespace linklet in namespace NS.
(define ($run-namespace-linklet ns name imports exports body meta)
  (let* ([reg ($current-registry)]
         [lk (make-linklet name imports exports body meta)]
         [insts ($namespace-import-instances reg imports ns)]
         [slots (linklet-slots reg lk insts (env-instance ns))])
    ; The imports of the unit are applied to the namespace when it is first used to
    ; expand something (`eval`, the REPL), not now.
    (env-add-pending-imports! ns (cdr (cdr meta)))
    (apply body slots)))

;; Runs the values of a unit thunk. 
(define ($run-unit-values vals ns)
  (cond
    [(and (pair? vals) (eq? (car vals) '|#%linklet|) (= (length vals) 6))
      (if (let ([meta (car (cdr (cdr (cdr (cdr (cdr vals))))))])
            (and (pair? meta) (eq? (car meta) '|#%namespace|)))
          (apply $run-namespace-linklet (or ns (current-namespace) (expansion-env)) (cdr vals))
          (apply $instantiate-linklet-values (cdr vals)))]
    [(and (pair? vals) (eq? (car vals) '|#%linklets|))
      (let loop ([thunks (cdr vals)] [result (if #f #f)])
        (if (null? thunks)
            result
            (loop (cdr thunks) ($boot-run-unit-thunk (car thunks) ns))))]
    [else (apply values vals)]))

(define ($boot-run-unit-thunk thunk . maybe-ns)
  (receive vals (thunk)
    ($run-unit-values vals (and (pair? maybe-ns) (car maybe-ns)))))

(define ($primitive-load filename)
  "Loads file by searching only load path or by its absolute path."
  ($boot-run-unit-thunk (load-thunk-in-vicinity filename #t)))

; load file containing base macros
(define primitive-load
  (lambda (filename)
    (let ()
      (cond
        ;; when bootstrapping we rely on tree-sitter to parse Scheme code
        ((%bootstrapping?)
          (let ([thunk (load-thunk-in-vicinity-k filename
                          (lambda (exps env) (compile-tree-il exps env filename))
                          (expansion-env) #f)])
            (with-exception-handler
              (lambda (exn)
                (format #t ";; (primitive) Error loading file '~a'~%" filename)
                (raise exn))
              (lambda () ($boot-run-unit-thunk thunk)))))
        (else
          (let ([thunk-or-path (try-load-thunk-in-vicinity filename #f)])
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
                        (lambda () ($boot-run-unit-thunk thunk-or-path)))
                      ($boot-run-unit-thunk thunk-or-path))))]
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
                    ($boot-run-unit-thunk (compile-file full-filename compiled-path (expansion-env) #t))))])))))))

(define $boot-rust-expander? #t)
(define $boot-requiring '())

(define ($boot-key-file key)
  (string-append "boot/" (symbol->string (car (cdr key))) ".scm"))

(define ($boot-spec-key spec)
  (let ([key (if (and (pair? spec) (eq? (car spec) 'only)) (car (cdr spec)) spec)])
    (if (and (pair? key) (not (eq? (car key) 'late)) ($boot-key? key))
        key
        #f)))

(define ($boot-load file)
  (if $boot-rust-expander?
      ($primitive-load file)
      (primitive-load file)))

(define ($boot-requiring? key)
  (let loop ([keys $boot-requiring])
    (and (pair? keys) (or (equal? (car keys) key) (loop (cdr keys))))))

(define ($string-suffix? s suffix)
  (let ([n (string-length s)] [m (string-length suffix)])
    (and (>= n m) (string=? (substring s (- n m) n) suffix))))

(define ($boot-memberq-equal? key keys)
  (and (pair? keys) (or (equal? (car keys) key) ($boot-memberq-equal? key (cdr keys)))))

(define ($boot-require-imports! imports late)
  (let loop ([groups imports])
    (if (pair? groups)
        (let ([key (car (car groups))])
          (if (and ($boot-key? key) (not ($boot-memberq-equal? key late)))
              ($boot-require! key))
          (loop (cdr groups))))))

(define ($boot-require! key)
  (cond
    [(library-instance (root-library-registry) key) (if #f #f)]
    [($boot-requiring? key)
      (error '$boot-require! "cyclic boot requires" key $boot-requiring)]
    [else
      (let* ([file ($boot-key-file key)]
             [found (%search-load-path file)]
             [path (and found (car (cdr found)))])
        (if (not path)
            (error '$boot-require! "no boot file for key" key file))
        (set! $boot-requiring (cons key $boot-requiring))
        (if ($string-suffix? path ".fasl")
            (let* ([thunk (load-thunk-in-vicinity file #t)]
                   [vals (receive vals (thunk) vals)])
              (if (and (pair? vals) (eq? (car vals) '|#%linklet|) (= (length vals) 6))
                  (let ([meta (car (cdr (cdr (cdr (cdr (cdr vals))))))])
                    ($boot-require-imports! (car (cdr (cdr vals)))
                      (if ($boot-meta? meta) (cdr (cdr meta)) '()))))
              ($run-unit-values vals #f))
            (begin
              (let loop ([specs (or (%boot-header-requires path) '())])
                (if (pair? specs)
                    (let ([dep ($boot-spec-key (car specs))])
                      (if dep ($boot-require! dep))
                      (loop (cdr specs)))))
              ($boot-load file)))
        (set! $boot-requiring (cdr $boot-requiring)))]))

;; The environments: they come first
($boot-require! '(boot library-manager))
; The exports of the linklets instantiated before the environments are put into the
; core environment now, and environments are used from here on.
(for-each (lambda (entry)
            (env-mirror-exports! the-core-env (car entry) (cdr entry)))
  (reverse $early-exports))
(set! $early-exports '())
(set! $envs-ready? #t)

;; The ports are initialized once they are all loaded.
($boot-require! '(boot log))
(initialize-io-system)

($boot-require! '(boot namespace))
($boot-require! '(boot time))
(set! $boot-rust-expander? #f)

(when (equal? (getenv "CAPY_TIME_LOADS") "1")
  (%time-loads? #t))

($set-unit-runner! $boot-run-unit-thunk)

($boot-require! '(boot base))
(primitive-load "boot/libraries.scm")
($boot-require! '(boot compiler))
(primitive-load "boot/cli.scm")

; The REPL namespace: the core bindings. It is the expansion environment.
(let ([ns (make-namespace '(capy user) '((capy)))])
  (current-namespace ns)
  (current-expansion-env ns)))
