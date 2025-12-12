#!r6rs 

(library (capy args option)
    (export
        option
        %make-option 
        option?
        make-option
        option-name
        option-abbreviation
        option-help
        option-value-help
        option-allowed
        option-allowed-help
        option-defaults-to
        option-callback
        option-negatable?
        option-split-commas?
        option-mandatory?
        option-hide?
        option-hide-negated-usage?
        option-aliases
        option-type
        option-multiple?
        option-single?
        option-flag?
        option-value-or-default)
    (import (rnrs) (core optargs) (capy))

    (define option-multiple 'multiple)
    (define option-single 'single)
    (define option-flag 'flag)

    (define-record-type 
        (option %make-option option?)
        (fields 
            name 
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
            aliases
            type))
    
    

    (define (make-option name type . rest)
        (let-optionals rest 
            ([abbreviation #f]
             [help #f]
             [value-help #f]
             [allowed #f]
             [allowed-help #f]
             [defaults-to #f]
             [callback #f]
             [negatable? #f]
             [split-commas? #f]
             [mandatory? #f]
             [hide? #f]
             [hide-negated-usage? #f]
             [aliases '()])
            
            (%make-option name abbreviation help value-help allowed 
                           allowed-help defaults-to callback negatable? 
                           split-commas? mandatory? hide? hide-negated-usage? 
                           aliases type)))

    (define (option-single? opt)
        (eq? (option-type opt) option-single))
    (define (option-multiple? opt)
        (eq? (option-type opt) option-multiple))
    (define (option-flag? opt)
        (eq? (option-type opt) option-flag))

    (define (option-value-or-default opt val)
        (cond 
            [val val]
            [(option-multiple? opt) (or (option-defaults-to opt) '())]
            [else (option-defaults-to opt)])))