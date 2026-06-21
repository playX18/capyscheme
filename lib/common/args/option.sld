;; Extracted from Chibi Scheme. Thanks to Alex Shinn.

(define-library (args option)
  (import (scheme base)
          (args help optional))
  (export
    make-option
    option?
    option-flag?
    option-single?
    option-multi?

    option-name
    option-abbr
    option-help
    option-value-help
    option-allowed
    option-allowed-help
    option-defaults-to
    option-negatable?
    option-hide-negated-usage?
    option-callback
    option-type
    option-split-commas?
    option-mandatory?
    option-hide?
    option-aliases
    option-value)

  (begin
    (define-record-type <option>
      (%make-option
        name
        abbr
        help
        value-help
        allowed
        allowed-help
        defaults-to
        negatable?
        hide-negated-usage?
        callback
        type
        split-commas?
        mandatory?
        hide?
        aliases)
      option?

      (name option-name)
      (abbr option-abbr)
      (help option-help)
      (value-help option-value-help)
      (allowed option-allowed)
      (allowed-help option-allowed-help)
      (defaults-to option-defaults-to)
      (negatable? option-negatable?)
      (hide-negated-usage? option-hide-negated-usage?)
      (callback option-callback)
      (type option-type)
      (split-commas? option-split-commas?)
      (mandatory? option-mandatory?)
      (hide? option-hide?)
      (aliases option-aliases))

    (define (option-flag? opt)
      (eq? (option-type opt) 'flag))

    (define (option-single? opt)
      (eq? (option-type opt) 'single))

    (define (option-multi? opt)
      (eq? (option-type opt) 'multi))

    (define unspecified-key (cons 'unspecified '()))

    (define (make-option name . args)
      (let-keywords* args
        ((abbr #f)
         (help #f)
         (value-help #f)
         (allowed #f)
         (allowed-help #f)
         (defaults-to #f)
         (negatable? #f)
         (hide-negated-usage? #f)
         (callback #f)
         (type 'multi)
         (split-commas? unspecified-key)
         (mandatory? #f)
         (hide? #f)
         (aliases '()))

        (let ((split-commas? (if (eq? split-commas? unspecified-key)
                                  (if (eq? type 'multi) #t #f)
                                  split-commas?)))
          (unless (string? name)
            (error "Option name must be a string" name))

          (%make-option name abbr help value-help allowed allowed-help
                        defaults-to negatable? hide-negated-usage?
                        callback type split-commas? mandatory? hide? aliases))))

    (define (option-value opt . rest)
      (let-optionals rest ((value #f))
        (cond
          (value value)
          ((option-multi? opt)
            (or (option-defaults-to opt) '()))
          (else (option-defaults-to opt)))))))