(define-library (r7rs-drafts macro-fascicle)
  (export
    symbolic-identifier=?
    identifier-defined?
    syntax-rules
    syntax-case
    quote-syntax
    syntax
    datum->syntax
    syntax->datum
    with-syntax
    with-ellipsis
    identifier-syntax
    make-variable-transformer

    define-syntax-parameter
    syntax-parameterize

    syntax-error
    erroneous-syntax

    (rename (letrec-syntax splicing-letrec-syntax))
    (rename (let-syntax splicing-let-syntax))

    unwrap-syntax

    quasisyntax
    unsyntax
    unsyntax-splicing)
  (import (capy))
  (begin
    (define-syntax syntax-error
      (lambda (stx)
        (syntax-case stx ()
          ((_ message irritant ...)
            (string? (syntax->datum #'message))
            (raise
              (condition
                (make-syntax-violation #f #f)
                (make-message-condition (syntax->datum #'message))
                (make-irritants-condition #'(irritant ...))))))))

    (define (erroneous-syntax . message?)
      (define message (if (null? message?) "bad syntax" (car message?)))
      (lambda (stx)
        (raise (condition
                (make-syntax-violation stx #f)
                (make-message-condition message)))))

    (define (symbolic-identifier=? id1 id2)
      (eq? (syntax->datum id1) (syntax->datum id2)))

    (define (identifier-defined? id)
      "Checks if ID is defined. Checks definition inside the environment of
      `(syntax-module id)` or in the expansion environment if no syntax module is
      associated with ID."
      ; The module of a syntax object is (hygiene . key), as psyntax makes it.
      (define smod (syntax-module id))
      (define env (if smod (resolve-env (cdr smod) #f #f) (expansion-env)))
      (define var (and env (env-variable env (syntax->datum id))))
      (and var (variable-bound? var)))

    (define (unwrap-syntax stx)
      (syntax-case stx ()
        [(first . rest) (cons (unwrap-syntax #'first)
                         (unwrap-syntax #'rest))]
        [() '()]
        [#(val ...)
          (list->vector (unwrap-syntax #'(val ...)))]
        [id
          (identifier? #'id)
          #'id]
        [val (syntax->datum #'val)]))))
