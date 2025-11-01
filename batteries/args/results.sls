#!r6rs 
(library (args results)
    (export
        make-arg-results 
        arg-results?
        arg-results-parser 
        arg-results-parsed
        arg-results-name
        arg-results-command
        arg-results-rest
        arg-results-arguments
        arg-results-ref)
    (import
        (rnrs)
        (args option)
        (args argparser))

    (define-record-type 
        (arg-results make-arg-results arg-results?)
        (fields 
            parser
            parsed 
            name 
            command
            rest
            arguments))
    (define (arg-results-ref results name)
        (cond 
            [(assq name (argparser-options (arg-results-parser results)))
             => (lambda (opt)
                (if (and (option-mandatory? (cdr opt))
                         (not (hashtable-contains? (arg-results-parsed results) name)))
                    (error 'arg-results-ref (format #f "mandatory option --~a not provided" name)))
                (option-value-or-default (cdr opt) 
                    (hashtable-ref (arg-results-parsed results) name)))]
            [else (error 'arg-parser (format #f "no such option: --~a" name))]))


)