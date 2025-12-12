#!r6rs 

(library (capy args argparser)
    (export 
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
        argparser-usage
        argparser-options-and-separators)
    (import 
        (capy)
        (rnrs)
        (srfi 1)
        (srfi 13)
        (capy args option)
        (core optargs))

    (define-record-type 
        (argparser %make-argparser argparser?)
        (fields 
            (mutable options)
            (mutable commands)
            (mutable aliases)
            (mutable options-and-separators)
            (mutable allow-trailing-options?)
            (mutable allows-anything?)))

    (define (argparser . rest)
        (let-optionals rest 
            ([allow-trailing-options? #f])
            (%make-argparser 
                '()
                '() 
                '() 
                '() 
                allow-trailing-options? 
                #f)))

    (define (argparser-add-command! this name . rest)
        "Defines a subcommand"
        (let-optionals rest 
            ([parser (argparser)])
           
            (when (assq name (argparser-commands parser))
                (error 'add-command! "command already exists" name))  
            (set-argparser-commands!
                this 
                (cons (cons name parser) (argparser-commands this)))
            parser))
    
    (define (argparser-add-flag! this name . rest)

        (let-optionals rest 
            ([abbreviation #f]
             [help #f]
             [defaults-to #f]
             [negatable? #t]
             [callback #f]
             [hide? #f]
             [hide-negated-usage? #f]
             [aliases '()])
            (%argparser-add-option!
                this 
                name 
                abbreviation
                help 
                #f 
                #f 
                #f
                defaults-to
                callback
                'flag
                negatable?
                #f ; split-commans?
                #f ; mandatory?
                hide?
                hide-negated-usage?
                aliases)))
    (define (%argparser-add-option! this name . rest)
        (let-optionals rest 
            ([abbreviation #f]
             [help #f]
             [value-help #f]
             [allowed #f]
             [allowed-help #f]
             [defaults-to #f]
             [callback #f]
             [type option-flag]
             [negatable? #f]
             [split-commas? #f]
             [mandatory? #f]
             [hide? #f]
             [hide-negated-usage? #f]
             [aliases '()])
            (when (or (argparser-find-by-name-or-alias this name)
                      (any (lambda (alias)
                              (argparser-find-by-name-or-alias this alias))
                            aliases))
                (error 'add-option! "option or alias already exists" name))
            (when (and abbreviation (argparser-find-by-abbreviation this abbreviation))
                (error 'add-option! "option abbreviation already exists" abbreviation))
            
            (when (and mandatory? defaults-to)
                (error 'add-option! "option cannot be both mandatory and have a default value" name))
            (when (and (not negatable?) hide-negated-usage?)
                (error 'add-option! "option cannot hide negated usage if it is not negatable" name))
            
            (let ([opt
                (make-option 
                    name
                    type 
                    abbreviation
                    help
                    value-help
                    allowed
                    allowed-help
                    defaults-to
                    callback
                    negatable?
                    split-commas?
                    mandatory?
                    hide?
                    hide-negated-usage?
                    aliases)])
                (argparser-options-set! 
                    this 
                    (cons (cons name opt) (argparser-options this)))
                (argparser-options-and-separators-set!
                    this 
                    (cons opt (argparser-options-and-separators this)))
                (for-each (lambda (alias)
                            (argparser-aliases-set!
                                this 
                                (cons (cons alias name) (argparser-aliases this))))
                          aliases))))

    (define (argparser-find-by-abbreviation this abbr)
        (let loop ([opts (argparser-options this)])
            (cond 
                [(null? opts) #f]
                [else 
                    (let ([opt (car opts)]
                          [opts (cdr opts)])
                        (cond 
                            [(and (option-abbreviation (cdr opt))
                                  (string=? abbr (option-abbreviation (cdr opt))))
                                (cdr opt)]
                            [else (loop opts)]))])))

    (define (argparser-find-by-name-or-alias this name)
        (cond 
            [(assoc name (argparser-options this)) => cdr]
            [(assoc name (argparser-aliases this)) => (lambda (opt-name)
                (cond 
                    [(assoc opt-name (argparser-options this)) => cdr]
                    [else (error 'find-by-name-or-alias "no such option" name)]))]
            [else #f]))

    (define (argparser-add-option! this name . rest)
       
        (let-optionals rest 
            ([abbreviation #f]
             [help #f]
             [value-help #f]
             [allowed #f]
             [allowed-help #f]
             [defaults-to #f]
             [callback #f]
             [mandatory? #f]
             [hide? #f]
             [aliases '()])
            (%argparser-add-option!
                this 
                name 
                abbreviation
                help 
                value-help
                allowed
                allowed-help
                defaults-to
                callback
                'single
                #f
                #f
                mandatory?
                hide?
                #f
                aliases)))
    
    (define (argparser-add-multi-option! this name . rest)
        (let-optionals rest 
            ([abbreviation #f]
             [help #f]
             [value-help #f]
             [allowed #f]
             [allowed-help #f]
             [defaults-to #f]
             [callback #f]
             [split-commas? #t]
             [mandatory? #f]
             [hide? #f]
             [aliases '()])
            (%argparser-add-option!
                this 
                name 
                abbreviation
                help 
                value-help
                allowed
                allowed-help
                defaults-to
                callback
                'multiple
                #f
                split-commas?
                mandatory?
                hide?
                #f
                aliases)))
    
    (define (argparser-add-separator! this separator)
        (argparser-options-and-separators-set!
            this 
            (cons separator (argparser-options-and-separators this))))
    

    (define (generate-usage opts-and-seps)
        (define column-count 3)
        (define newlines-needed 0)
        (define current-column 0)
       
        (define (calculate-column-widths)

            (let loop ([opts opts-and-seps]
                       [abbr 0]
                       [title 0])
                (cond 
                    [(null? opts) (vector abbr (+ 4 title))]
                    [(and (option? (car opts)) 
                        (not (option-hide? (car opts))))
                        (let* ([opt (car opts)]
                               [abbr (max abbr (string-length (abbreviation opt)))]
                               [title (max title
                                (+ (string-length (long-option opt))
                                   (string-length (mandatory-option opt))))])
                            (loop (cdr opts) abbr title))]
                    [else (loop (cdr opts) abbr title)])))
        (define (mandatory-option opt)
            (if (option-mandatory? opt) " (mandatory) " ""))

        (define (abbreviation opt)
            (if (not (option-abbreviation opt))
                ""
                (string-append "-" (option-abbreviation opt) ", ")))

        (define (long-option opt)
            (define result 
                (cond 
                    [(and (option-negatable? opt)
                        (not (option-hide-negated-usage? opt)))
                        (string-append "--[no-]" (option-name opt))]
                    [else
                        (string-append "--" (option-name opt))]))
            
            (if (option-value-help opt)
                (string-append result "=<" (option-value-help opt) ">")
                result))
        (define (newline)
            (set! newlines-needed (+ 1 newlines-needed))
            (set! current-column 0))
            
       
        (define column-widths (calculate-column-widths))
        
        (call-with-string-output-port 
            (lambda (out)
                (define (write-separator separator)
                    (format out "~%~%~a" separator)
                    (set! newlines-needed 1))

                (define (write-line column text)
                    (let lp ()
                        (when (> newlines-needed 0)
                            (format out "~%")
                            (set! newlines-needed (- newlines-needed 1))
                            (lp)))
                    
                    (let lp ()
                        (when (not (= current-column column))
                            (if (< current-column (- column-count 1))
                                (format out "~a" 
                                    (make-string 
                                        (vector-ref column-widths current-column) 
                                        #\space))
                                (format out "~%"))
                            (set! current-column (mod (+ 1 current-column) column-count))
                            (lp)))
                    
                    (if (< column 2)
                        (format out "~a" (string-pad-right text (vector-ref column-widths column)))
                        (format out "~a" text))
                    (set! current-column (mod (+ 1 current-column) column-count))

                    (if (= column (- column-count 1))
                        (set! newlines-needed (+ 1 newlines-needed))))
                (define (write column text)
                    (define lines (string-split text #\newline))
                    (for-each 
                        (lambda (line) 
                            (unless (zero? (string-length line))
                                (write-line column line)))
                        lines))
                (define (write-option opt)
                    (write 0 (abbreviation opt))
                    (write 1 (string-append (long-option opt) (mandatory-option opt)))
                    (when (option-help opt)
                        (write 2 (option-help opt)))
                    (cond 
                        [(option-allowed-help opt)
                            (newline)
                            (for-each
                                (lambda (allowed)
                                    (write 1 (allowed-title opt (car allowed)))
                                    (write 2 (cdr allowed)))
                                (option-allowed-help opt))
                            (newline)]
                        [(option-allowed opt)
                            (write 2 (build-allowed-list opt))]
                        [(eq? (option-type opt) 'flag)
                            (if (option-defaults-to opt)
                                (write 2 "(defaults to on)"))]
                        [(and (eq? (option-type opt) 'multiple)
                            (list? (option-defaults-to opt))
                            (not (null? (option-defaults-to opt))))
                            (write 2 (format #f "(defaults to ~a)" (option-defaults-to opt)))]
                        [(option-defaults-to opt)
                            (write 2 (format #f "(defaults to ~a)" (option-defaults-to opt)))]))
                
                (define (allowed-title opt allowed)
                    (define defaults-to (option-defaults-to opt))
                    (define default? (cond
                        [(list? defaults-to) (assq allowed defaults-to)]
                        [else (equal? allowed defaults-to)]))
                    (format 
                        #f "      [~a]~a"
                        allowed
                        (if default? " (default)" "")))

                (define (build-allowed-list opt)
                    (define defaults-to (option-defaults-to opt))
                    (define default? (cond 
                        [(list? defaults-to) (lambda (x) (assq x defaults-to))]
                        [else (lambda (x) (equal? x defaults-to))]))
                    (call-with-string-output-port 
                        (lambda (buf)
                            (define first #t)
                            (format buf "[")
                            (for-each 
                                (lambda (item)
                                    (unless first 
                                        (format buf ", "))
                                    (format buf "~a" item)
                                    (when (default? item)
                                        (format buf " (default)"))
                                    (set! first #f))
                                (option-allowed opt))
                            (format buf "]")
                                )))
                

                (for-each 
                    (lambda (item)
                        (cond 
                            [(and (option? item)
                                (not (option-hide? item)))
                                (write-option item)]
                            [(string? item)
                                (write-separator item)]))
                    opts-and-seps))))

    (define (argparser-usage this)
        (generate-usage (reverse (argparser-options-and-separators this))))
)
