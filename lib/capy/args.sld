(define-library (capy args)
  (import (scheme base)
          (capy args grammar)
          (capy args option)
          (capy args parser)
          (capy args results))
  (export
    argparser
    argparser-add-command!
    argparser-add-flag!
    argparser-add-option!
    argparser-add-multi-option!
    argparser-add-separator!
    argparser-find-by-name-or-alias
    argparser-find-by-abbreviation
    argparser-usage
    parse-args

    arg-results-rest
    arg-results-ref

    add-flag!
    add-option!
    add-multi-option!
    abbreviation
    help
    value-help
    allowed
    allowed-help
    defaults-to
    callback
    aliases
    split-commas
    mandatory
    negatable
    hide
    hide-negated-usage

    make-grammar
    grammar-parse
    grammar-usage
    argument-results-rest)
  (begin
    (define argparser make-grammar)
    (define argparser-add-command! grammar-add-command!)
    (define argparser-add-flag! grammar-add-flag!)
    (define argparser-add-option! grammar-add-option!)
    (define argparser-add-multi-option! grammar-add-multi-option!)
    (define argparser-add-separator! grammar-add-separator!)
    (define argparser-find-by-name-or-alias grammar-find-by-name-or-alias)
    (define argparser-find-by-abbreviation grammar-find-by-abbr)
    (define argparser-usage grammar-usage)
    (define parse-args grammar-parse)

    (define arg-results-rest argument-results-rest)

    (define (arg-results-ref results name)
      (let ((option (grammar-find-by-name-or-alias
                      (argument-results-grammar results)
                      name)))
        (cond
          ((not option)
            (error (string-append "No option named: '--" name "'")))
          ((and (option-mandatory? option)
                (not (argument-results-was-parsed? results name)))
            (error (string-append "Mandatory option '--" name "' not provided")))
          (else
            (option-value
              option
              (cond
                ((assoc (option-name option)
                        (argument-results-parsed results)) => cdr)
                (else #f)))))))

    (define (option-clauses clauses)
      (if (null? clauses)
        '()
        (apply append clauses)))

    (define (add-flag! parser name . clauses)
      (apply grammar-add-flag! parser name (option-clauses clauses)))

    (define (add-option! parser name . clauses)
      (apply grammar-add-option! parser name (option-clauses clauses)))

    (define (add-multi-option! parser name . clauses)
      (apply grammar-add-multi-option! parser name (option-clauses clauses)))

    (define (abbreviation value) (list 'abbr: value))
    (define (help value) (list 'help: value))
    (define (value-help value) (list 'value-help: value))
    (define (allowed value) (list 'allowed: value))
    (define (allowed-help value) (list 'allowed-help: value))
    (define (defaults-to value) (list 'defaults-to: value))
    (define (callback value) (list 'callback: value))
    (define (aliases value) (list 'aliases: value))
    (define split-commas (list 'split-commas?: #t))
    (define mandatory (list 'mandatory?: #t))
    (define negatable (list 'negatable?: #t))
    (define hide (list 'hide?: #t))
    (define hide-negated-usage (list 'hide-negated-usage?: #t))))
