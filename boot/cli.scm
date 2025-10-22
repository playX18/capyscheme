(library (boot cli)
    (export enter eval-string)
    (import (capy)
            (core repl))
    

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
                (unless (null? load-path)
                    (set! %load-path (append (reverse load-path) %load-path)))
                (eval `(begin 
                    ,@(reverse out)
                    ,(if interactive? 
                        `((@ (core repl) read-eval-print-loop))
                        '(exit 0)))))))
    (if (pair? args)
        (begin 
            (set! arg0 (car args))
            (parse (cdr args) '()))
        (parse args '()))))