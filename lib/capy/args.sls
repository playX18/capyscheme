#!r6rs 

(library (capy args)
    (export
        arg-results-parser 
        arg-results-parsed
        arg-results-name
        arg-results-command
        arg-results-rest
        arg-results-arguments
        arg-results-ref
        argparser
        argparser-add-command!
        argparser-add-flag!
        argparser-add-option!
        argparser-add-multi-option!
        argparser-add-separator!
        argparser-find-by-name-or-alias
        argparser-find-by-abbreviation
        argparser-options
        argparser-commands
        argparser-allow-trailing-options?
        arparser-usage
        parse-args
        add-option! 
        add-multi-option!
        add-flag!)
    (import 
        (rnrs)
        (capy args argparser)
        (capy args option)
        (capy args parser)
        (capy args results))


    (define (parse-args parser args)
        (define p (make-parser #f parser args #f))
        (parser-parse p))

    (define p (argparser))
    
    ;(argparser-add-flag! p "verbose" #f "Enable verbose output")
    ;(argparser-add-multi-option! p "load-path" "L" "Add specified path to load path")
    ;(argparser-add-option! p "gc-plan" #f "Select GC plan" #f '("semispace" "genimmix" "concurrentimmix") #f "genimmix")
    ;(displayln (argparser-usage p))

    ;(define res (parse-args p '("--load-path=/usr/local/lib" "-L" "/opt/lib")))

    ;(format #t "verbose: ~a~%" (arg-results-ref res "verbose"))
    ;(format #t "load-path: ~a~%" (arg-results-ref res "load-path"))

    (define-syntax add-flag! 
        (lambda (x)
            
            (define (process-clauses clauses)
                (define %abbr #f)
                (define %help #f)
                (define %defaults-to #f)
                (define %negatable? #f)
                (define %callback #f)
                (define %hide #f)
                (define %hide-negated-usage #f)
                (define %aliases '())

                (let lp ([clauses clauses])
                    (syntax-case clauses 
                        (abbreviation help defaults-to negatable callback hide hide-negated-usage aliases)
                        [((abbreviation abbr) rest ...)
                            (begin (set! %abbr #'abbr)
                            (lp #'(rest ...)))]
                        [((help help-str) rest ...) 
                            (begin (set! %help #'help-str)
                            (lp #'(rest ...)))]
                        [((defaults-to dt) rest ...)
                            (begin (set! %defaults-to #'dt)
                            (lp #'(rest ...)))]
                        [(negatable rest ...)
                            (begin (set! %negatable? #t)
                            (lp #'(rest ...)))]
                        [((callback cb) rest ...)
                            (begin (set! %callback #'cb)
                            (lp #'(rest ...)))]
                        [(hide rest ...)
                            (begin (set! %hide #t)
                            (lp #'(rest ...)))]
                        [(hide-negated-usage rest ...)
                            (begin (set! %hide-negated-usage #t)
                            (lp #'(rest ...)))]
                        [((aliases alist) rest ...)
                            (begin (set! %aliases #'alist)
                            (lp #'(rest ...)))]
                        
                        [() (list 
                            %abbr 
                            %help
                            %defaults-to
                            %negatable?
                            %callback
                            %hide
                            %hide-negated-usage
                            %aliases)]
                        
                        )))
            (syntax-case x ()
                [(_ parser name clause ...)
                    (let* ([clauses (process-clauses #'(clause ...))])
                        #`(argparser-add-flag! 
                            parser
                            name 
                            #,@clauses)
                    )])))


    (define-syntax add-option! 
        (lambda (x)
            
            (define (process-clauses clauses)
                (define %abbr #f)
                (define %help #f)
                (define %value-help #f)
                (define %allowed #f)
                (define %allowed-help #f)
                (define %defaults-to #f)
                (define %callback #f)
                (define %mandatory #f)
                (define %hide #f)
                (define %aliases '())

                (let lp ([clauses clauses])
                    (syntax-case clauses 
                        (abbreviation help value-help allowed allowed-help defaults-to callback mandatory hide aliases)
                        [((abbreviation abbr) rest ...)
                            (begin (set! %abbr #'abbr)
                            (lp #'(rest ...)))]
                        [((help help-str) rest ...) 
                            (begin (set! %help #'help-str)
                            (lp #'(rest ...)))]
                        [(mandatory rest ...)
                            (begin (set! %mandatory #t)
                            (lp #'(rest ...)))]
                        [((value-help vh) rest ...)
                            (begin (set! %value-help #'vh)
                            (lp #'(rest ...)))]
                        [((allowed alist) rest ...)
                            (begin (set! %allowed #'alist)
                            (lp #'(rest ...)))]
                        [((allowed-help ah) rest ...)
                            (begin (set! %allowed-help #'ah)
                            (lp #'(rest ...)))]
                        [((defaults-to dt) rest ...)
                            (begin (set! %defaults-to #'dt)
                            (lp #'(rest ...)))]
                        [((callback cb) rest ...)
                            (begin (set! %callback #'cb)
                            (lp #'(rest ...)))]
                        [(hide rest ...)
                            (begin (set! %hide #t)
                            (lp #'(rest ...)))]
                        [((aliases alist) rest ...)
                            (begin (set! %aliases #'alist)
                            (lp #'(rest ...)))]
                        
                        [() (list 
                            %abbr 
                            %help
                            %value-help
                            %allowed
                            %allowed-help
                            %defaults-to
                            %callback
                            %mandatory
                            %hide
                            %aliases)])))
            (syntax-case x ()
                [(_ parser name clause ...)
                    (let* ([clauses (process-clauses #'(clause ...))])
                        #`(argparser-add-option! 
                            parser
                            name 
                            #,@clauses)
                    )])))

    (define-syntax add-multi-option! 
        (lambda (x)

            (define (process-clauses clauses)
                (define %abbr #f)
                (define %help #f)
                (define %value-help #f)
                (define %allowed #f)
                (define %allowed-help #f)
                (define %defaults-to #f)
                (define %callback #f)
                (define %split-commas? #f)
                (define %mandatory #f)
                (define %hide #f)
                (define %aliases '())

                (let lp ([clauses clauses])
                 
                    (syntax-case clauses 
                        (abbreviation help value-help allowed allowed-help defaults-to callback mandatory hide aliases split-commas)
                        [((abbreviation abbr) rest ...)
                            (begin (set! %abbr #'abbr)
                            (lp #'(rest ...)))]
                        [((help help-str) rest ...) 
                            (begin (set! %help #'help-str)
                            (lp #'(rest ...)))]
                        [(mandatory rest ...)
                            (begin (set! %mandatory #t)
                            (lp #'(rest ...)))]
                        [((value-help vh) rest ...)
                            (begin (set! %value-help #'vh)
                            (lp #'(rest ...)))]
                        [((allowed alist) rest ...)
                            (begin (set! %allowed #'alist)
                            (lp #'(rest ...)))]
                        [((allowed-help ah) rest ...)
                            (begin (set! %allowed-help #'ah)
                            (lp #'(rest ...)))]
                        [((defaults-to dt) rest ...)
                            (begin (set! %defaults-to #'dt)
                            (lp #'(rest ...)))]
                        [((callback cb) rest ...)
                            (begin (set! %callback #'cb)
                            (lp #'(rest ...)))]
                        [(split-commas rest ...)
                            (begin (set! %split-commas? #t)
                            (lp #'(rest ...)))]
                        [(hide rest ...)
                            (begin (set! %hide #t)
                            (lp #'(rest ...)))]
                        [((aliases alist) rest ...)
                            (begin (set! %aliases #'alist)
                            (lp #'(rest ...)))]

                        [() (list 
                            %abbr 
                            %help
                            %value-help
                            %allowed
                            %allowed-help
                            %defaults-to
                            %callback
                            %split-commas?
                            %mandatory
                            %hide
                            %aliases)])))
            (syntax-case x ()
                [(_ parser name clause ...)
                    (let* ([clauses (process-clauses #'(clause ...))])
                         #`(argparser-add-multi-option! 
                            parser
                            name 
                            #,@clauses)
                    )])))

)