;; normalize `define` forms into something understood by psyntax. 
;; Can also be used to implement custom `define`-like forms.

#!r6rs 
(library (capy internals norm-define)
    (export normalize-definition)
    (import (capy) (core control) (capy pretty-print))

(define-for-syntax (stx-pair? stx)
    (cond 
        [(syntax? stx) (stx-pair? (syntax-expression stx))]
        [(pair? stx) #t]
        [else #f]))

(define-for-syntax (syntax->list s)
  (define l
    (let loop ([s s])
      (cond
       [(pair? s) (cons (car s) (loop (cdr s)))]
       [(syntax? s) (loop (syntax-e s))]
       [else s])))
  (and (list? l)
       l))

(define-for-syntax (stx-list? stx)
    (cond 
        [(syntax? stx) (stx-list? (syntax-expression stx))]
        [(list? stx) #t]
        [else #f]))

(define-for-syntax (normalize-definition/mk-rhs stx lambda-stx check-context? allow-key+opt? err-no-body?)
    
    (syntax-case stx ()
        [(_ id expr)
            (and  (identifier? #'id))
            (values #'id values #'expr)]
        [(_ id . rest)
            (and  (identifier? #'id))
            (syntax-violation 
                #f 
                (syntax-case stx ()
                    [(_ id expr0 expr ...)
                        "bad syntax (multiple expression after identifier)"]
                    [(_ id)
                        "bad syntax (missing expression after identifier)"]
                    [(_ id . rest)
                        "bad syntax (illegal use of `.`)"])
                stx)]
        [(_ something . rest)
            (not (stx-pair? #'something))
            (syntax-violation
                #f 
                "bad syntax"
                stx 
                #'something)]
        [(_ proto . body)
            (let-values ([(id mk-rhs)
                (letrec ([simple-proto 
                    ;; check the args and set up a proc-maker; we return
                    ;;  a proc maker instead of a final proc to enable
                    ;;  left-to-right checking of the function protos
                    (lambda (proto)
                        (let-values ([(args rets mk-rhs)
                            (syntax-case proto ()
                                [(id arg ...)
                                    (values 
                                        (syntax->list #'(arg ...))
                                        '()
                                        (lambda (body)
                                            #`(#,lambda-stx (arg ...)
                                                . #,body)))]
                                [(id arg ... . rest)
                                    (values (syntax->list #'(arg ...))
                                            (list #'rest)
                                            (lambda (body)
                                                #`(#,lambda-stx (arg ... . rest)
                                                    . #,body)))])])
                          
                            mk-rhs))]
                    [general-proto
                        (lambda (proto)
                           
                            (syntax-case proto ()
                                [(id . rest)
                                    (and (identifier? #'id))                     
                                    (values #'id
                                        (simple-proto proto))]
                                [((something . more) . rest)
                                    
                                    (let-values ([(id mk-rhs) (general-proto #'(something . more))])
                                        (let ([mk-inner (simple-proto proto)])
                                            (values 
                                                id
                                                (lambda (body)
                                                    (mk-rhs (list (mk-inner body)))))))]
                                [(other . rest)
                                    (syntax-violation 
                                        #f
                                        "bad syntax (not an identifier for procedure name, and not a nested procedure form)"
                                        stx 
                                        #'other)]))])

                (general-proto #'proto))])
            (values id mk-rhs #'body))]))
(define-for-syntax normalize-definition 
    (case-lambda 
     [(stx lambda-stx check-context? allow-key+opt?)
      (let-values ([(id mk-rhs body)
                    (normalize-definition/mk-rhs stx lambda-stx check-context? allow-key+opt? #t)])
        (values id (mk-rhs body)))]
     [(stx lambda-stx check-context?) (normalize-definition stx lambda-stx check-context? #f)]
     [(stx lambda-stx) (normalize-definition stx lambda-stx #t #f)]))






)