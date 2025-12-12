(library (boot cli)
    (export enter eval-string)
    (import (capy)
            (core repl)
            (capy args)
            (capy args argparser))
    

(define (eval-string str)
    (call-with-input-string 
        str 
        (lambda (in)
            (define reader (make-reader in "stdin"))
            (let lp () 
                (let ([exp (read-datum reader)])
                    (if (not (eof-object? exp))
                        (begin 
                            (eval exp (current-module))
                            (lp))))))))

(define (enter args)
    "Main entrypoint of CapyScheme CLI."
    (define arg0 "capy")
    (define interactive? #t)
    (define entrypoint #f)
    (define load-path '())
    (define load-compiled-path '())
    (define extensions '())

    (define parser (argparser))
   
    (define (error fmt . args)
        (apply format #t fmt args)
        (exit 1))
    
    (define (parse args out)
        (cond 
            [(null? args) (finish args out)]
            [else 
                (let ([arg (car args)] [args (cdr args)])
                    (cond 
                        [(not (string-prefix? "-" arg))
                            (set! arg0 arg)
                            (set! interactive? #f)
                            (finish args 
                                (cons `((@@ (capy) load) ,arg0) out))]
                        [(string=? arg "-s")
                            (if (null? args)
                                (error "missing argument for -s"))
                            (set! arg0 (car args))
                            (set! interactive? #f)
                            (finish (cdr args)
                                (cons `((@@ (capy) load) ,arg0) out))]
                        [(string=? arg "-c")
                            (if (null? args)
                                (error "missing argument for -c"))
                            (set! arg0 (car args))
                            (set! interactive? #f)
                            (finish (cdr args)
                                (cons `((@@ (boot cli) eval-string) ,arg0) out))]
                        [(string=? arg "--")
                            (finish args out)]
                        [(string=? arg "-l")
                            (if (null? args)
                                (error "-l: missing a file to load"))
                            (parse (cdr args)
                                (cons `((@ (capy) load), (car args)) out))]
                        [(string=? arg "-L")
                            (if (null? args)
                                (error "-L: missing a directory to add to load path"))
                            (set! load-path (cons (car args) load-path))
                            (parse (cdr args) out)]
                        [(string=? arg "-C")
                            (if (null? args)
                                (error "-C: missing a directory to add to compiled load path"))
                            (set! load-compiled-path (cons (car args) load-compiled-path))
                            (parse (cdr args) out)]
                        [(string=? arg "-x")
                            (if (null? args)
                                (error "-x: missing an extension to load"))
                            (set! extensions (cons (car args) extensions))
                            (parse (cdr args) out)]
                        [(string=? arg "-e")
                            (if (null? args)
                                (error "-e: missing entrypoint name"))
                            (let* ([port (open-input-string (car args))]
                                   [arg (read port)])
                                (set! entrypoint arg))
                            (parse (cdr args) out)]
                        [(string=? arg "-fresh-auto-compile")
                            (set! %fresh-auto-compile! #t)
                            (parse args out)]
                        [(string=? arg "-log:trace")
                            (log:set-max-level! log:trace)
                            (parse args out)]
                        [(string=? arg "-log:info")
                            (log:set-max-level! log:info)
                            (parse args out)]
                        [(string=? arg "-log:debug")
                            (log:set-max-level! log:debug)
                            (parse args out)]
                        [(string=? arg "-log:warn")
                            (log:set-max-level! log:warn)
                            (parse args out)]
                        [(string=? arg "-log:error")
                            (log:set-max-level! log:error)
                            (parse args out)]
                        [else (error "unknown argument: ~a" arg)]))]))

    (define (finish args out)
        (set-program-arguments! (cons arg0 args))
        (if (not (zero? (log:max-level)))
            (log:set-logger! *simple-logger*))
        (with-exception-handler 
            (lambda (c)
                (flush-output-port (current-output-port))
                ((current-exception-printer) c)
                (exit 1))
            (lambda ()
                (eval `(begin 
                    ,@(reverse out)
                    ,(if interactive? 
                        `((@ (core repl) read-eval-print-loop))
                        '(exit 0)))))))
    (define (run)
        (with-exception-handler 
            (lambda (c)
                (format #t "Failed to parse command line arguments: ~a~%" (condition-message c))
                (exit 1))
            (lambda ()
                (define res (parse-args parser (cdr args)))
                
                (if (arg-results-ref res "log-warn")
                    (log:set-max-level! log:warn))
                (if (arg-results-ref res "log-error")
                    (log:set-max-level! log:error))
                (if (arg-results-ref res "log-info")
                    (log:set-max-level! log:info))
                (if (arg-results-ref res "log-debug")
                    (log:set-max-level! log:debug))
                (if (arg-results-ref res "log-trace")
                    (log:set-max-level! log:trace))
                (if (not (zero? (log:max-level)))
                    (log:set-logger! *simple-logger*))
                (when (arg-results-ref res "help")
                    (format #t "CapyScheme ~a~%" (implementation-version))
                    (format #t "Usage:~%~a~%" (argparser-usage parser))
                    (exit 0))
                (set! entrypoint (arg-results-ref res "entrypoint"))
                (set! %load-path (append (reverse (arg-results-ref res "load-path")) %load-path))
                (set! %load-path (append %load-path (reverse (arg-results-ref res "append-load-path"))))
                (set! %load-compiled-path (append (reverse (arg-results-ref res "compiled-load-path")) %load-compiled-path))
                (set! %load-extensions (append (reverse (arg-results-ref res "extensions")) %load-extensions))
                (if (arg-results-ref res "fresh-auto-compile")
                    (set! %fresh-auto-compile #t))
                (define out '())
                
                (when (arg-results-ref res "script")
                    (set! interactive? #f)
                    (set! out (cons `((@@ (capy) load) ,(arg-results-ref res "script")) out)))
                (when (arg-results-ref res "command")
                    (set! interactive? #f)
                    (set! out (cons `((@@ (boot cli) eval-string) ,(arg-results-ref res "command")) out)))
                (finish (arg-results-rest res) out))))
    (add-flag! parser 
        "help"
        (help "Show this help message and exit"))
    (add-option! parser 
        "script"
        (abbreviation "s")
        (help "Run specified file as a script")
        (value-help "FILE"))
    (add-option! parser 
        "command"
        (abbreviation "c")
        (help "Evaluate a command string")
        (value-help "STRING"))
    (add-option! parser 
        "load"
        (abbreviation "l")
        (help "Load specified file before entering REPL or running script")
        (value-help "FILE"))
    (add-multi-option! parser 
        "load-path"
        (abbreviation "L")
        (defaults-to '())
        (value-help "DIR")
        split-commas
        (help "Add a directory to the load path"))
    (add-multi-option! parser 
        "append-load-path"
        (abbreviation "A")
        (defaults-to '())
        (value-help "DIR")
        split-commas 
        (help "Append a directory to the load path"))
    (add-multi-option! parser 
        "compiled-load-path"
        (abbreviation "C")
        (defaults-to '())
        (value-help "DIR")
        split-commas
        (help "Add a directory to the compiled load path"))
    (add-multi-option! parser 
        "extensions"
        (abbreviation "x")
        (defaults-to '())
        (value-help "EXTENSION")
        split-commas
        (help "Add specified extension to list of extensions"))                    
    (add-option! parser 
        "entrypoint"
        (abbreviation "e")
        (defaults-to #f)
        (help "Specify the entrypoint function to call"))
    (add-flag! parser 
        "fresh-auto-compile"
        (defaults-to #f)
        (help "Enable fresh auto compilation of loaded files"))
    (argparser-add-separator! parser "Logging options:")
    (add-flag! parser 
        "log-trace"          
        (help "Enable trace logging"))
    (add-flag! parser 
        "log-info"
        (help "Enable info logging"))
    (add-flag! parser 
        "log-debug"
        (help "Enable debug logging"))
    (add-flag! parser 
        "log-warn"
        (help "Enable warn logging"))
    (add-flag! parser 
        "log-error"
        (help "Enable error logging"))

    (run)))