(define-library (r7rs-drafts macro-fascicle)
  (export 
    symbolic-identifier=? identifier-defined?
    syntax-rules syntax-case 
    quote-syntax syntax datum->syntax syntax->datum
    with-syntax with-ellipsis identifier-syntax
    make-variable-transformer
    
    define-syntax-parameter
    syntax-parameterize
    
    syntax-error
    erroneus-syntax

    (rename (letrec-syntax splicing-letrec-syntax))
    (rename (let-syntax splicing-let-syntax))

    unwrap-syntax
    
    quasisyntax unsyntax unsyntax-splicing
    
    )
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

    (define (erroneus-syntax . message?)
      (define message (if (null? message?) "bad syntax" (car message?)))
      (lambda (stx)
        (raise (condition
          (make-syntax-violation stx #f)
          (make-message-condition message)))))

    

    (define (symbolic-identifier=? id1 id2)
      (eq? (syntax->datum id1) (syntax->datum id2)))
    
    (define (identifier-defined? id)
      "Checks if ID is defined. Checks definition inside the `(syntax-module id)` or in
      (current-module) if no syntax module is associated with ID."
      (define smod (or (syntax-module id) (current-module)))
      (define mod (resolve-module smod #f #f))
      (cond 
        [(not mod) #f]
        [else 
          (define var (module-variable mod (syntax->datum id)))
          (and var (variable-bound? var))]))
    
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