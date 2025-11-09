#!r6rs 

(library (args parser)
    (export
        make-parser 
        parser-parse)
    (import 
        (core optargs)
        (rnrs) 
        (rnrs records syntactic)
        (core hashtables)
        (args argparser)
        (args option)
        (args results)
        (srfi 13))

    (define-record-type 
        (parser %make-parser parser?)
        (fields 
            command-name
            parent 
            grammar 
            (mutable args)
            (mutable rest)
            (mutable results)))
    
    (define (make-parser command-name grammar args parent)    
        (%make-parser command-name parent grammar args '() (make-string-hashtable)))

    (define (validate c message . rest)
        (let-optionals rest ([args #f] [source '()] [offset 0])
            (unless c
                (assertion-violation 
                    'arg-parse 
                    message 
                    args 
                    source 
                    offset))))

    (define (validate-allowed opt value arg)
        (cond 
            [(option-allowed opt) 
                => (lambda (allowed)
                    (validate (member value allowed) (format #f "~a is not allowed value for option '~a'" value arg)))]
            [else #t]))
    (define (letter-or-digit? code-unit)
        (or (and (char>=? code-unit #\a) (char<=? code-unit #\z))
            (and (char>=? code-unit #\A) (char<=? code-unit #\Z))
            (and (char>=? code-unit #\0) (char<=? code-unit #\9))))
    
    (define (letter-or-digit-hypher-or-underscore? code-unit)
        (or (letter-or-digit? code-unit)
            (char=? code-unit #\_)
            (char=? code-unit #\-)))
    
    (define (set-flag results arg value)
        (hashtable-set! results (option-name arg) value))
    (define (set-option results option value arg)
        (cond 
            [(option-single? option)
                (validate-allowed option value arg)
                (hashtable-set! results (option-name option) value)]
            [else 
                (let ([ls (hashtable-ref results (option-name option) '())])
                    (cond 
                        [(option-split-commas? option)
                            (for-each 
                                (lambda (v)
                                    (validate-allowed option v arg)
                                    (set! ls (cons v ls)))
                                (string-split value #\,))]
                        [else 
                            (validate-allowed option value arg)
                            (set! ls (cons value ls))])
                    (hashtable-set! results (option-name option) ls))]))



    (define (current this)
        (car (parser-args this)))

    (define (advance! this)
        (define cur (current this))
        (parser-args-set! this (cdr (parser-args this)))
        cur)

    (define (read-next-as-value this option arg)
        (validate (not (null? (parser-args this))) (format #f "Missing argument for '~a'" arg) arg)
        (set-option (parser-results this) option (current this) arg)
        (advance! this))
    
    (define (handle-long-option this name value)
        (define option (argparser-find-by-name-or-alias (parser-grammar this) name))
      
        (cond 
            [option 
                (advance! this)
                (cond 
                    [(option-flag? option)
                        (validate (not value) (format #f "Flag option '~a' does not take a value" name) name)
                        (set-flag (parser-results this) option #t)]
                    [value 
                        (set-option (parser-results this) option value (string-append "--" name))]
                    [else 
                        (read-next-as-value this option (string-append "--" name))])]
            [(string-prefix? "no-" name)
                (let* ([positive-name (substring name 3)]
                       [option (argparser-find-by-name-or-alias (parser-grammar this) positive-name)])
                    (cond 
                        [(not option)
                            (validate (parser-parent this) (format #f "Unknown option '--~a'" name) (string-append "--" name))
                            (handle-long-option (parser-parent this) name value)]
                        [else 
                            (advance! this)
                            (validate (option-flag? option) (format #f "Cannot negate non-flag option '--~a" positive-name) (string-append "--" name))
                            (validate (option-negatable? option) (format #f "Option '--~a' is not negatable" positive-name) (string-append "--" name))
                            (set-flag (parser-results this) option #f)]))]
            [else 
                (validate (parser-parent this) (format #f "Unknown option '--~a'" name) (string-append "--" name))
                (handle-long-option (parser-parent this) name value)])
        #t)

    (define (parse-long-option this)
        (cond 
            [(not (string-prefix? "--" (current this))) #f]
            [else 
                (let* ([index (string-index (current this) #\=)]
                       [name (if index 
                                (substring (current this) 2 index)
                                (substring (current this) 2))])
                    (cond 
                        [(string-every letter-or-digit-hypher-or-underscore? name)
                            (let ([value (if index 
                                            (substring (current this) (+ index 1))
                                            #f)])
                                (cond 
                                    [(and value (string-any (lambda (c) (char=? #\newline c)) value))
                                        #f]
                                    [else 
                                        (handle-long-option this name value)
                                        #t]))]
                        [else #f]))]))
    
    (define (parse-short-flag this c)
        (define option (argparser-find-by-abbreviation (parser-grammar this) c))
        (cond 
            [(not option) 
                (validate (parser-parent this) (format #f "Unknown option '-~a'" c) (string-append "-" c))
                (parse-short-flag (parser-parent this) c)]
            [else 
                (validate (option-flag? option) (format #f "Option '-~a' is not a flag" c) (string-append "-" c))
                (set-flag (parser-results this) option #t)]))

    (define (handle-abbreviation this in rest innermost)
        (define c (substring in 0 1))
        (define first (argparser-find-by-abbreviation (parser-grammar this) c))
        (cond 
            [(not first)
                (validate (parser-parent this) (format #f "Unknown option '-~a'" c) (string-append "-" c))
                (handle-abbreviation (parser-parent this) in rest innermost)]
            [(not (option-flag? first))
                (let ([value (format #f "~a~a" (substring in 1) rest)])
                    (set-option (parser-results innermost) first value (string-append "-" c)))]
            [else 
                (validate 
                    (string=? rest "")
                    (format #f "Option '-~a' is a flag and cannot have a value" c)
                    (string-append "-" c))
            
                (string-for-each 
                    (lambda (ch)
                        (parse-short-flag innermost (string ch)))
                    in)])
        (advance! this)
        #t)
    (define (parse-abbreviation this innermost)
        (cond 
            [(< (string-length (current this)) 2) #f]
            [(not (string-prefix? "-" (current this))) #f]
            [else 
                (let lp ([index 1])
                    (cond 
                        [(and (< index (string-length (current this)))
                              (letter-or-digit? (string-ref (current this) index)))
                            (lp (+ index 1))]
                        [(= index 1) #f]
                        [else 
                            (let ([in (substring (current this) 1 index)]
                                  [rest (substring (current this) index)])
                                (handle-abbreviation this in rest innermost))]))]))

    (define (handle-solo-option this opt)
        (define option (argparser-find-by-abbreviation (parser-grammar this) opt))
        (cond 
            [(not option)
                (validate (parser-parent this) (format #f "Unknown option '-~a'" opt) (string-append "-" opt))
                (handle-solo-option (parser-parent this) opt)]
            [else 
                (advance! this)
                (cond 
                    [(option-flag? option)
                        (set-flag (parser-results this) option #t)]
                    [else 
                        (read-next-as-value this option (string-append "-" opt))])
                #t]))

    (define (parse-solo-option this)
        (cond 
            [(or (< (string-length (current this)) 2)
                 (not (string-prefix? "-" (current this)))) #f]
            [else 
                (let ([optx (string-ref (current this) 1)])
                    (cond 
                        [(not (letter-or-digit? optx)) #f]
                        [else (handle-solo-option this (string optx))]))]))
    
    (define (parser-parse this)
        (define arguments (parser-args this))
        (define command-results #f)
        (let lp ()
            (cond 
                [(null? (parser-args this)) #f]
                [(string=? (current this) "--")
                    (advance! this)
                    #f]
                [else  
                    (let ([command (assq (current this) (argparser-commands (parser-grammar this)))])
                        (cond 
                            [command
                                (validate 
                                    (null? (parser-rest this)) 
                                    (format #f "Cannot have subcommand '~a' after other arguments" (current this)) 
                                    (current this))
                                (let* ([command-name (advance! this)]
                                       [command-parser (make-parser command-name (cdr command) (parser-args this) this)])
                                    (set! command-results (parser-parse command-parser))
                                    (parser-rest-set! this '()))]
                            [else   
                                (cond
                                    [(parse-solo-option this) (lp)]
                                    [(parse-abbreviation this this) (lp)]
                                    [(parse-long-option this) (lp)]
                                    [else
                                        (cond 
                                            [(argparser-allow-trailing-options? (parser-grammar this))
                                                (parser-rest-set! this (cons (current this) (parser-rest this)))
                                                (lp)]
                                            [else #f]) 
                                        ])

                                ]))]))
        
        (for-each 
            (lambda (opt)
                (define name (car opt))
                (define option (cdr opt))
                (define callback (option-callback option))
                (define parsed-option (hashtable-ref (parser-results this) name #f))
                (cond 
                    [(not callback) #f]
                    [else 
                        (if (and (option-mandatoy? option)
                                 (not parsed-option))
                            (error 'arg-parse (format #f "Mandatory option '~a' not provided" name)))
                        (callback (option-value-or-default option parsed-option))]))
            (argparser-options (parser-grammar this)))
        (parser-rest-set! this (append (parser-args this) (parser-rest this)))
        (parser-args-set! this '())
        (make-arg-results (parser-grammar this)
                          (parser-results this)
                          (parser-command-name this)
                          command-results 
                          (parser-rest this)
                          arguments)))