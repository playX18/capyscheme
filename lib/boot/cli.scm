(library (boot cli)
    (export enter eval-string enter-compiler)
    (import (capy)
            (core control)
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

(define (canonicalize-paths paths)
  (map canonicalize-path-string paths))

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
                (format #t "Failed to parse command line arguments: ~a ~a ~a ~%" (condition-message c) (condition-irritants c) (condition-who c))
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
                (set! %load-path (append (canonicalize-paths (reverse (arg-results-ref res "load-path"))) %load-path))
                (set! %load-path (append %load-path (canonicalize-paths (reverse (arg-results-ref res "append-load-path")))))
                (set! %load-compiled-path (append (canonicalize-paths (reverse (arg-results-ref res "compiled-load-path"))) %load-compiled-path))
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

(define (enter-compiler args)
  "Entrypoint for `capyc` compiler CLI."
  (define arg0 "capyc")
  (define parser (argparser))

  (define (error fmt . args)
      (apply format #t fmt args)
      (exit 1))

  (define (print-help)
    (format #t "CapyScheme Compiler ~a~%" (implementation-version))
    (format #t "Usage:~%~a~%" (argparser-usage parser))
    (exit 0))

  (define (parse-module str)
    (define parts (string-split str #\space))
    (map string->symbol parts))
  
  (define (run)
    (define res (with-exception-handler 
                    (lambda (c)
                      (format #t "Failed to parse command line arguments: ~a~%" (condition-message c))
                      (exit 1))
                    (lambda ()
                      (parse-args parser (cdr args)))))
    
    (define load-path (arg-results-ref res "load-path"))
    (define append-load-path (arg-results-ref res "append-load-path"))
    (define compiled-load-path (arg-results-ref res "compiled-load-path"))
    (define extensions (arg-results-ref res "extensions"))
    (set! %load-path (append (canonicalize-paths load-path) %load-path))
    (set! %load-path (append %load-path (canonicalize-paths append-load-path)))
    (set! %load-compiled-path (append (canonicalize-paths (reverse compiled-load-path)) %load-compiled-path))
    (set! %load-extensions (append (reverse extensions) %load-extensions))
    (define out-file (arg-results-ref res "output"))
    (define r7rs-mode (arg-results-ref res "r7rs"))
    (define r6rs-mode (arg-results-ref res "r6rs"))
    (define verbose (arg-results-ref res "verbose"))
    (if (and r7rs-mode r6rs-mode)
        (error "Cannot specify both --r7rs and --r6rs modes"))
    (define source-files (arg-results-rest res))
    (if (null? source-files)
        (print-help))
    
    (when (arg-results-ref res "help")
      (print-help))
  
    (define module-name (or (parse-module (arg-results-ref res "module"))
                            '(capy user)))

    (when (and out-file 
            (or (null? source-files)
                (not (null? (cdr source-files)))))
      (error #f "--output can only be used when compiling a single file"))
    
    (with-exception-handler 
      (lambda (exn)
        (print-condition exn (current-output-port))
        (format #t "Compilation failed.~%")
        (exit 1))
      (lambda ()
        (for-each 
          (lambda (file)
            (with-exception-handler 
              (lambda (exn)
                (print-condition exn (current-output-port))
                (format #t ";; Compilation of file ~a failed.~%" file)
                (when (marks-condition? exn)
                  (stack-trace (condition-marks exn) (current-output-port)))
                (exit 1))
              (lambda () 
                (when verbose 
                  (format #t ";; Compiling file ~a~%" file))
                (define out (compile-file 
                  file 
                  out-file 
                  (or (and module-name (resolve-module module-name #t #t)) #f)
                  #f))
                (when verbose 
                  (format #t ";; Compiled ~a -> ~a~%" file out)))))
          source-files))))

  (add-option! parser 
    "output"
    (abbreviation "o")
    (help "Specify output file"))
  
  (add-flag! parser 
    "r7rs"
    (help "Compile in R7RS mode"))
  (add-flag! parser 
    "r6rs"
    (help "Compile in R6RS mode"))
  
  (add-flag! parser 
    "verbose"
    (abbreviation "v")
    (help "Enable verbose output"))

  (add-multi-option! parser 
    "load-path"
    (abbreviation "L")
    (help "Add a directory to the load path")
    (value-help "DIR")
    split-commas)
  (add-multi-option! parser 
    "compiled-load-path"
    (abbreviation "C")
    (help "Add a directory to the compiled load path")
    (value-help "DIR")
    split-commas)
  
  (add-multi-option! parser 
    "extensions"
    (abbreviation "x")
    (help "Add specified extension to list of extensions")
    (value-help "EXTENSION")
    split-commas)
  (add-multi-option! parser 
    "append-load-path"
    (abbreviation "A")
    (help "Append a directory to the load path")
    (value-help "DIR")
    split-commas)
  
  (add-option! parser 
    "module"
    (abbreviation "m")
    (help "Specify the module to compile source file in")
    (value-help "MODULE-NAME"))

  (add-flag! parser 
    "help"
    (help "Show this help message and exit"))


  (run))