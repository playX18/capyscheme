(define-library (capy args results)
  (import (scheme base)
          (capy args help optional)
          (capy args grammar)
          (capy args option))

  (export
    argument-results
    argument-results?
    argument-results-grammar
    argument-results-parsed
    argument-results-name
    argument-results-command
    argument-results-rest
    argument-results-arguments
    argument-results-flags
    argument-results-options
    argument-results-multi-options
    argument-results-was-parsed?
    argument-results-has-option?)

  (begin
    (define-record-type <argument-results>
      (%make-argument-results
        grammar
        parsed
        name
        command
        rest
        arguments)
      argument-results?

      (grammar argument-results-grammar)
      (parsed argument-results-parsed)
      (name argument-results-name)
      (command argument-results-command)
      (rest argument-results-rest)
      (arguments argument-results-arguments))

    (define (argument-results grammar parsed name command rest arguments)
      (%make-argument-results
        grammar
        parsed
        name
        command
        rest
        arguments))

    (define (argument-results-find-option results name)
      (grammar-find-by-name-or-alias (argument-results-grammar results) name))

    (define (argument-results-parsed-value results option)
      (cond
        ((assoc (option-name option) (argument-results-parsed results)) => cdr)
        (else #f)))

    (define (argument-results-flags results)
      (lambda (flag)
        (define option (argument-results-find-option results flag))
        (cond
          ((not option)
            (error (string-append "No flag named: '--" flag "'")))
          ((not (option-flag? option))

            (error (string-append "Option '--" flag "' is not a flag option")))
          (else
            (option-value option (argument-results-parsed-value results option))))))

    (define (argument-results-options results)
      (lambda (name)
        (define option (argument-results-find-option results name))
        (cond
          ((not option)
            (error (string-append "No option named: '--" name "'")))
          ((not (option-single? option))
            (error (string-append "Option '--" name "' is not a single-value option")))
          ((and (option-mandatory? option)
                (not (argument-results-was-parsed? results name)))
            (error (string-append "Mandatory option '--" name "' not provided")))
          (else
            (option-value option (argument-results-parsed-value results option))))))

    (define (argument-results-multi-options results)
      (lambda (name)
        (define option (argument-results-find-option results name))
        (cond
          ((not option)
            (error (string-append "No option named: '--" name "'")))
          ((not (option-multi? option))
            (error (string-append "Option '--" name "' is not a multi-value option")))
          (else
            (option-value option (argument-results-parsed-value results option))))))

    (define (argument-results-was-parsed? results name)
      (define option (argument-results-find-option results name))
      (cond
        ((not option) #f)
        ((assoc (option-name option) (argument-results-parsed results)) => (lambda (pair) #t))
        (else #f)))

    (define (argument-results-has-option? results name)
      (argument-results-was-parsed? results name))))
