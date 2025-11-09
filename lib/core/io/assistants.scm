
(library (core io assistants)

  (export port-type
          port-direction
          port-lookup-file-option-code
          port-lookup-buffer-mode-code
          port-lookup-codec-code
          port-lookup-eol-style-code
          port-lookup-error-handling-mode-code
          port-reverse-lookup-codec-code
          port-reverse-lookup-eol-style-code
          port-reverse-lookup-error-handling-mode-code
          make-file-options)

  (import (core primitives) (core enums))

  (define direction-codes
    '((input . 1) (output . 2) (input/output . 3)))

  (define type-codes
    '((file . 1) (bytevector . 2) (custom . 3)))

  (define file-option-codes
    '((no-create . 1) (no-fail . 2) (no-truncate . 4)))

  (define buffer-mode-codes
    '((none . 1) (line . 2) (block . 3)))

  (define codec-codes
    '((latin-1 . 1) (utf-8 . 2) (utf-16 . 3)))

  (define eol-style-codes
    '((none . 1) (lf . 2) (cr . 3) (crlf . 4) (nel . 5) (crnel . 6) (ls . 7)))

  (define error-handling-mode-codes
    '((ignore . 1) (raise . 2) (replace . 3)))

  (define flip (lambda (lst) (map (lambda (e) (cons (cdr e) (car e))) lst)))

  (define flipped-codec-codes (flip codec-codes))

  (define flipped-eol-style-codes (flip eol-style-codes))

  (define flipped-error-handling-mode-codes (flip error-handling-mode-codes))

  (define lookup (lambda (obj alist) (cond ((assq obj alist) => cdr) (else #f))))

  (define-syntax port-type
    (lambda (x)
      (syntax-case x ()
        ((_ type)
         (datum->syntax
          #'k
          (cond ((assq (syntax->datum (syntax type)) type-codes) => cdr)
                (else
                 (syntax-violation 'port-type "invalid port type" x)))))
        (_
         (syntax-violation 'port-type "invalid port type" x)))))

  (define-syntax port-direction
    (lambda (x)
      (syntax-case x (input output)
        ((_ input)
         (datum->syntax #'k (lookup 'input direction-codes)))
        ((_ output)
         (datum->syntax #'k (lookup 'output direction-codes)))
        ((_ input output)
         (datum->syntax #'k (lookup 'input/output direction-codes)))
        (_
         (syntax-violation 'port-direction "invalid port direction" x)))))

  (define port-lookup-file-option-code (lambda (obj) (lookup obj file-option-codes)))
  (define port-lookup-buffer-mode-code (lambda (obj) (lookup obj buffer-mode-codes)))
  (define port-lookup-codec-code (lambda (obj) (lookup obj codec-codes)))
  (define port-lookup-eol-style-code (lambda (obj) (lookup obj eol-style-codes)))
  (define port-lookup-error-handling-mode-code (lambda (obj) (lookup obj error-handling-mode-codes)))
  (define port-reverse-lookup-codec-code (lambda (obj) (lookup obj flipped-codec-codes)))
  (define port-reverse-lookup-eol-style-code (lambda (obj) (lookup obj flipped-eol-style-codes)))
  (define port-reverse-lookup-error-handling-mode-code (lambda (obj) (lookup obj flipped-error-handling-mode-codes)))

  (define make-file-options (enum-set-constructor (make-enumeration (map car file-option-codes))))

  ) ;[end]