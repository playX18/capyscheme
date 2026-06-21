;; Extracted from Chibi Scheme. Thanks to Alex Shinn.

(define-library (capy args option)
  (import (scheme base)
          (capy args help optional))
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
    (define (plist-ref args key default)
      (let loop ((args args))
        (cond
          ((null? args) default)
          ((not (pair? (cdr args))) default)
          ((eq? (car args) key) (cadr args))
          (else (loop (cddr args))))))

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
      (let* ((abbr (plist-ref args 'abbr: #f))
             (help (plist-ref args 'help: #f))
             (value-help (plist-ref args 'value-help: #f))
             (allowed (plist-ref args 'allowed: #f))
             (allowed-help (plist-ref args 'allowed-help: #f))
             (defaults-to (plist-ref args 'defaults-to: #f))
             (negatable? (plist-ref args 'negatable?: #f))
             (hide-negated-usage? (plist-ref args 'hide-negated-usage?: #f))
             (callback (plist-ref args 'callback: #f))
             (type (plist-ref args 'type: 'multi))
             (split-commas? (plist-ref args 'split-commas?: unspecified-key))
             (mandatory? (plist-ref args 'mandatory?: #f))
             (hide? (plist-ref args 'hide?: #f))
             (aliases (plist-ref args 'aliases: '()))
             (split-commas? (if (eq? split-commas? unspecified-key)
                              (if (eq? type 'multi) #t #f)
                              split-commas?)))
        (unless (string? name)
          (error "Option name must be a string" name))

        (%make-option name abbr help value-help allowed allowed-help
                      defaults-to negatable? hide-negated-usage?
                      callback type split-commas? mandatory? hide? aliases)))

    (define (option-value opt . rest)
      (let-optionals rest ((value #f))
        (cond
          (value value)
          ((option-multi? opt)
            (or (option-defaults-to opt) '()))
          (else (option-defaults-to opt)))))))
