(define *compile-backtrace-key* (list 'backtrace))

(define (compile-tree-il x e)
    (define (join exps)
        (cond
            [(null? exps) (make-void #f)]
            [else
                (if (null? (cdr exps))
                    (car exps)
                    (make-sequence #f (car exps) (join (cdr exps))))]))

    (save-module-excursion
        (lambda ()
            (current-module e)
            (let lp ([exps x] [out '()])
                (cond
                    [(null? exps) (values (join (reverse out)) (current-module) (current-module))]
                    [else
                        (let ([exp (macroexpand (car exps) 'c '(compile load eval))])
                            (lp (cdr exps) (cons exp out)))])))))

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

;; Compile `filename` and put output into `compiled-path`.
;; Env is a module where the file is being compiled.
;; load-thunk? indicates whether to return a thunk to initialize compiled
;; file (dlopen it) or just compile and return. If its #f use load-thunk-in-vicinity
(define (compile-file filename compiled-path env load-thunk?)
    (define (read-all in)
        (let lp ([exps '()])
            (let ([exp (read-syntax in)])
                (cond
                    [(eof-object? exp) (reverse exps)]
                    [else (lp (cons exp exps))]))))
    (define output-file (or compiled-path (compiled-file-name filename)))
    (define module (or env (resolve-r6rs-interface '(capy user))))
    (*raw-log* log:debug
               '(capy)
               'compile-file
               "Compiling file ~a to ~a" filename output-file)
    (call-with-input-file filename
        (lambda (in)
            (define exps (read-all in))
            (define reader (get-port-reader in #f))
            (with-continuation-mark *compile-backtrace-key* (not (reader-nobacktrace? reader))
                (receive (code mod new-mod) (compile-tree-il exps module)
                    (%compile code output-file mod load-thunk?))))))

(define load-in-vicinity
    (lambda (filename directory)
        (save-module-excursion
            (lambda ()
                (define thunk-or-path (try-load-thunk-in-vicinity filename #t directory))
                (cond
                    [(procedure? thunk-or-path) (thunk-or-path)]
                    [else
                        (let ([filename (list-ref thunk-or-path 0)]
                              [full-filename (list-ref thunk-or-path 1)]
                              [compiled-path (list-ref thunk-or-path 2)])
                          ((compile-file full-filename compiled-path (current-module) #t)))])))))


(define load
    (lambda (filename)
        (save-module-excursion
            (lambda ()
                (define thunk-or-path (try-load-thunk-in-vicinity filename #t))
                (cond
                    [(procedure? thunk-or-path)
                     (with-exception-handler
                        (lambda (exn)
                            (format (current-error-port) ";; Error loading file '~a'~%" filename)
                            ((current-exception-printer) exn (current-error-port))
                            (flush-output-port (current-error-port))
                            (raise exn))
                        (lambda ()
                            (*raw-log* log:info
                                       '(capy)
                                       'load
                                       "Loading file ~a" filename)
                            (thunk-or-path)))]
                    [else
                        (with-exception-handler 
                            (lambda (exn)
                                (format (current-error-port) ";; Error compiling file '~a'~%" filename)
                                ((current-exception-printer) exn (current-error-port))
                                (flush-output-port (current-error-port))
                                (raise exn))
                            (lambda ()
                                (define filename (list-ref thunk-or-path 0))
                                (define full-filename (list-ref thunk-or-path 1))
                                (define compiled-path (list-ref thunk-or-path 2))
                                (*raw-log* log:info
                                        '(capy)
                                        'load
                                        "Compiling file ~a" filename)
                                ((compile-file full-filename compiled-path (current-module) #t))))])))))


(define (eval x . m)
    (save-module-excursion (lambda ()
        (let* ([m (if (null? m) (current-module) (car m))]
           [code (macroexpand x 'e '(eval))])
            (primitive-eval code)))))

(define (try-resolve-module name autoload ensure)
    (with-exception-handler
        (lambda exn #f)
        (lambda ()
            (resolve-module name autoload ensure))))
