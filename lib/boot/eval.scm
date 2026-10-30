(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot units) (boot environments) (boot library-manager) (boot psyntax-exp) (boot str2num) (boot num2str) (boot reader) (late (boot time) $as-time-goes-by %time-loads?))
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
      (make-values #f
        (cons (make-constant #f '|#%linklets|)
          (map (lambda (lk) (make-proc #f '() lk '() '())) linklets)))]))


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
                        (apply append (map (lambda (f) (cdr (syntax->datum f))) imports)))
                  (filter (lambda (f) (not (form-head-is? f '(import)))) forms))))))))))

(define (compile-tree-il x0 e . maybe-filename)
  (define key (and (pair? maybe-filename) (file-unit-key (car maybe-filename))))
  (define x (or (and (pair? maybe-filename) (program-forms x0 (car maybe-filename))) x0))
  (define declared? #f)
  (values
    (linklets->il
      (expand-file-units x e key
        (lambda (exp)
          (if (not declared?)
            (begin
              (set! declared? #t)
              (unit-predeclare! (current-unit) x syntax->datum)))
          (%runtime-stats-timed-psyntax
            (lambda ()
              (macroexpand exp 'c '(compile load eval)))))))
    #f #f))

(define (compiled-file-name file)
  (define (canonical->suffix canon)
    (cond
      ((string-prefix? "/" file) canon)
      ((and (> (string-length canon) 2)
          (eqv? (string-ref canon 1) #\:))
        (string-append "/" (substring canon 0 1) (substring canon 2)))
      (else canon)))

  (define (compiled-extension) (string-append "." %native-extension))
  (and %compile-fallback-path
    (let ((f (string-append
              %compile-fallback-path
              (canonical->suffix (canonicalize-path-string file))
              (compiled-extension))))
      (and (create-directory* (dirname f))
        f))))

;;; An active compiler for files.
;;; Must be overridden by the implementation depending on runtime
;;; status.
(define %%file-compiler (make-parameter #f))

;; First, initial compiler is the default one.
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
        (call-with-input-file filename
          (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
              (receive (code mod new-mod) (compile-tree-il exps module filename)
                (%compile code output-file load-thunk? dump-options))))))
      (lambda ()
        (%runtime-stats-end-compilation)))))

;; Compile `filename` and put output into `compiled-path`.
;; Env is a module where the file is being compiled.
;; load-thunk? indicates whether to return a thunk to initialize compiled
;; file or just compile and return. If its #f use load-thunk-in-vicinity
(define (compile-file filename compiled-path env load-thunk? . maybe-dump-options)
  (define (compile)
    ((%%file-compiler)
      filename
      compiled-path
      env
      load-thunk?
      (if (null? maybe-dump-options) '() (car maybe-dump-options))))
  (if (%time-loads?)
    ($as-time-goes-by (list 'compile-file filename) compile)
    (compile)))

(define load-in-vicinity
  (lambda (filename directory)
    (let ()
      (define thunk-or-path (try-load-thunk-in-vicinity filename #t directory))
      (cond
        [(procedure? thunk-or-path) ($run-unit-thunk thunk-or-path (script-env))]
        [else
          (let ([filename (list-ref thunk-or-path 0)]
                [full-filename (list-ref thunk-or-path 1)]
                [compiled-path (list-ref thunk-or-path 2)])
            ($run-unit-thunk (compile-file full-filename compiled-path (expansion-env) #t) (script-env)))]))))

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

(define %load
  (lambda (filename program?)
    (let ()
       (in-script-namespace (lambda ()
        (define thunk-or-path (try-load-thunk-in-vicinity filename #t))
        (cond
          [(procedure? thunk-or-path)
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
                    (lambda () ($run-unit-thunk thunk-or-path (script-env))))
                  ($run-unit-thunk thunk-or-path (script-env)))))]
          [else
            (let* ([filename (list-ref thunk-or-path 0)]
                   [full-filename (list-ref thunk-or-path 1)]
                   [compiled-path (list-ref thunk-or-path 2)]
                   [unit
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
                             (compile-file full-filename compiled-path (script-env) #t)))))])
              (with-exception-handler
                (lambda (exn)
                  (format (current-error-port) ";; Error loading file '~a'~%" filename)
                  (raise exn))
                (lambda ()
                  ($run-unit-thunk unit (script-env)))))]))))))

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
  (begin
    (if (library-form? x)
      (begin
        (for-each
          (lambda (term) ($run-unit-thunk (lambda () (primitive-eval term)) (script-env)))
          (expand-file-units (list x) (script-env) #f
            (lambda (exp) (macroexpand exp 'c '(compile load eval)))))
        (if #f #f))
      (call-with-unit (make-file-unit env #f 'e)
        (lambda ()
          (primitive-eval (macroexpand x 'e '(eval))))))))
