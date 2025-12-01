;; Keyword arguments implementation based on https://www2.ccs.neu.edu/racket/pubs/scheme2009-fb.pdf and 
;; Racket's `kw.rkt` code. Our implementation differs in not using structures (or records) to represent
;; keyword procedures. Instead we attach keyword information directly to procedure properties 
;; thus it makes calls slightly faster (no structure accesses needed). 

#!r6rs 

(library (capy keywords impl)
    (export 
        new-lambda
        new-app
        new-define
        keyword-procedure?
        parse-lambda
        keyword?)
    (import 
        (capy)
        (core control)
        (capy pretty-print)
        (capy internals norm-define)
        (capy compiler tree-il))

(eval-when (expand load eval)
    (define (keyword? str)
        (cond 
            [(identifier? str) (keyword? (syntax-expression str))]
            [(symbol? str) (string-prefix? ":" (symbol->string str))]
            [else #f]))
    (define (keyword<? kw1 kw2)
        (unless (and (keyword? kw1) (keyword? kw2))
            (error 'keyword<? "not keywords" kw1 kw2))
        (string<? 
            (substring (symbol->string kw1) 1)
            (substring (symbol->string kw2) 1))))

(define-syntax in-range?/static 
    (lambda (stx)
        (syntax-case stx () 
            [(_ v min #f)
                #'(>= v min)]
            [(_ v min max)
                (if (equal? (syntax-e #'min) (syntax-e #'max))
                    #'(= v min)
                    #'(and (>= v min) (<= v max)))])))

(define andmap
  (lambda (f . lists)
    (cond ((null? (car lists)) (and))
          ((null? (cdr (car lists)))
           (apply f (map car lists)))
          (else
           (and (apply f (map car lists))
                (apply andmap f (map cdr lists)))))))
(define (subset? l1 l2)
    ;; l1 and l2 are sorted
    (cond
        [(null? l1) #t]
        [(null? l2) #f]
        [(eq? (car l1) (car l2)) (subset? (cdr l1) (cdr l2))]
        [else (subset? l1 (cdr l2))]))

(define (subsets? l1 l2 l3)
    ;; l1, l2, and l3 are sorted, and l1 is a subset of l3
    (cond
        [(null? l1) (subset? l2 l3)]
        [(null? l2) #f]
        [(null? l3) #f]
        [else (let ([v2 (car l2)])
                (cond
                [(eq? (car l1) v2) (subsets? (cdr l1) (cdr l2) (cdr l3))]
                [(eq? v2 (car l3)) (subsets? l1 (cdr l2) (cdr l3))]
                [else (subsets? l1 l2 (cdr l3))]))]))

(define-syntax subset?/static 
    (lambda (stx)
        (syntax-case stx (quote) 
            [(_ l1-expr '()) #'(null? l1-expr)]
            [(_ '() l2-expr) #'t]
            [(_ l1-expr '(kw . kws))
                (with-syntax ([l1 (datum->syntax #f (gensym 'l1))])
                    #'(let ([l1 l1-expr])
                        (let ([l1 (if (null? l1)
                                    l1 
                                    (if (eq? (car l1) 'kw)
                                        (cdr l1)
                                        l1))])
                            (subset?/static l1 'kws))))]
            [(_ l1-expr l2-expr) #'(subset? l1-expr l2-expr)])))

(define-syntax subsets?/static 
    (lambda (stx)
        (syntax-case stx (quote)
            [(_ 'l1-elems l2-expr 'l3-elems)
                (if (equal? (map syntax-e (syntax->list #'l1-elems))
                            (map syntax-e (syntax->list #'l3-elems)))
                    #'(equal?/static 'l1-elems l2-expr)
                    #'(subsets? 'l1-elems l2-expr 'l3-elems))]
            [(_ '() l2-expr l3-expr)
                #'(subset?/static l2-expr l3-expr)]
            [(_ l1-expr l2-expr '())
                #'(subset?/static l1-expr l2-expr)]
            )))

(define-syntax equal?/static 
    (lambda (stx)
        (syntax-case stx (quote)
            [(_ '() l2-expr) #'(null? l2-expr)]
            [(_ '(kw . kw-rest) l2-expr)
                (with-syntax ([l2 (datum->syntax #f (gensym 'l2))])
                    #'(let ([l2 l2-expr])
                        (and (pair? l2)
                             (eq? (car l2) 'kw)
                             (equal?/static 'kw-rest (cdr l2)))))])))

(define-for-syntax (wrap-default-check arg-id expr)
    (with-syntax ([arg-id arg-id])
        (with-syntax ([tst (if (immediate-default? expr) #f #'(eq? arg-id (unspecified)))] [expr expr])
            #'(if tst expr arg-id))))

(define-for-syntax null '())
(define-for-syntax (syntax-e stx) 
    (syntax-expression stx))


(define-syntax begin0 
    (syntax-rules ()
      ((begin0 e0 e1 ...)
       (let ([temp e0])
         (begin e1 ... temp)))))

(define-for-syntax (simple-identifier? id)
    (and (identifier? id)
         (not (keyword? (syntax-expression id)))))

(define-for-syntax (simple-args? args)
    (cond 
        [(identifier? args) (simple-identifier? args)]
        [(pair? args) (and (simple-identifier? (car args))
                           (simple-args? (cdr args)))]
        [(syntax? args) (simple-args? (syntax-expression args))]
        [(null? args) #t]
        [else 
              #f]))

(define-for-syntax (sort lst less?)
    (if (null? lst)
        '()
        (let* ([pivot (car lst)]
               [less (filter (lambda (x) (less? x pivot)) (cdr lst))]
               [greater-eq (filter (lambda (x) (not (less? x pivot))) (cdr lst))])
            (append (sort less less?) (list pivot) (sort greater-eq less?)))))

(define-for-syntax (parse-formals stx args)
  (let* ([kw-ht (make-core-hash-eq)]
           [check-kw (lambda (kw)
                       (when (core-hash-ref kw-ht (syntax-e kw) #f)
                         (syntax-violation
                          #f
                          "duplicate keyword for argument"
                          stx
                          kw))
                       (core-hash-set! kw-ht (syntax-e kw) #t))])
      (let loop ([args args] [needs-default? #f])
        (syntax-case args ()
          [id
           (simple-identifier? (syntax id))
           #'(() () () () () (id))]
          [()
           #'(() () () () () ())]
          [(id . rest)
           (simple-identifier? (syntax id))
           (begin
             (when needs-default?
               (syntax-violation
                #f "default-value expression missing" stx (syntax id)))
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #f)])
               #'((id . plain) opt-ids ([id #f :plain] . opts) kws need-kw rest)))]
          [([id default] . rest)
           (simple-identifier? (syntax id))
           (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest #t)])
             #'(plain ([id default] . opt-ids) ([id default :opt] . opts) kws need-kw rest))]
          [(kw id . rest)
           (and (simple-identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id #f :kw-req] . opts) ([kw id #t #f] . kws) (kw . need-kw) rest)))]
          [(kw [id default] . rest)
           (and (simple-identifier? #'id)
                (keyword? (syntax-e #'kw)))
           (begin
             (check-kw #'kw)
             (with-syntax ([(plain opt-ids opts kws need-kw rest) (loop #'rest needs-default?)])
               #'(plain opt-ids ([id default :kw-opt] . opts) ([kw id #f default] . kws) need-kw rest)))]
          [(kw)
           (keyword? (syntax-e #'kw))
           (begin
             (check-kw #'kw)
             (syntax-violation
              #f
              "missing argument identifier after keyword"
              stx
              #'kw))]
          [(kw bad . rest)
           (keyword? (syntax-e #'kw))
           (syntax-violation
            #f
            "after keyword, not an identifier or identifier with default"
            stx
            (syntax bad))]
          [(bad . rest)
           (syntax-violation
            #f
            "not an identifier, identifier with default, or keyword"
            stx
            (syntax bad))]
          [else
           (syntax-violation
            #f "bad argument sequence" stx args)]))))  

(define-for-syntax (syntax->list s)
    (define l
        (let loop ([s s])
            (cond
                [(pair? s) (cons (car s) (loop (cdr s)))]
                [(syntax? s) (loop (syntax-e s))]
                [else s])))

  (and (list? l)
       l))


(define-for-syntax (immediate-default? expr)
    (define (immediate-literal? v)
        (or (boolean? v)
            (number? v)
            (char? v)
            (string? v)
            (bytevector? v)))
    (or (immediate-literal? (syntax-e expr))
        (syntax-case expr (quote unspecified null eof)
            [(quote s-exp)
                (let ([v (syntax-e #'s-exp)])
                    (or (symbol? v)
                        (null? v)
                        (immediate-literal? v)))]
            [(unspecified) #t]
            [null #t]
            [eof #t]
            [_ #f])))

(define-for-syntax (parse-lambda stx local-name non-kw-k kw-k)
    (syntax-case stx () 
        [(_ args body1 body ...)
            (if (simple-args? #'args)
                (non-kw-k
                    #'(lambda args body1 body ...))
                ;; handle keyword and optional arguments
                (with-syntax ([((plain-id ...)
                           ([opt-id pos-opt-expr] ...)
                           ([id opt-expr kind] ...)
                           ([kw kw-id kw-req kw-opt-expr] ...)
                           need-kw
                           rest)
                          (parse-formals stx #'args)])
                    (let* ([kws (syntax->list #'(kw ...))]
                            [opts (syntax->list #'(opt-id ...))]
                            [ids (syntax->list #'(id ...))]
                            [plain-ids (syntax->list #'(plain-id ...))]
                            [kw-reqs (syntax->list #'(kw-req ...))]
                            [kw-args (generate-temporaries kws)]     ; supplied value
                            [kw-arg?s (generate-temporaries kws)]    ; temporary to indicate whether supplied
                            [opt-args (generate-temporaries opts)]   ; supplied value
                            [get-not-supplieds (lambda (opt-exprs)
                                                (map (lambda (opt-expr)
                                                        (if (immediate-default? opt-expr)
                                                            opt-expr
                                                            #'(unspecified)))
                                                    opt-exprs))]
                            [opt-not-supplieds (get-not-supplieds (syntax->list #'(pos-opt-expr ...)))]
                            [kw-not-supplieds (get-not-supplieds (syntax->list #'(kw-opt-expr ...)))]
                            [needed-kws (sort (syntax->list #'need-kw)
                                            (lambda (a b) (keyword<? (syntax-e a) (syntax-e b))))]
                            [sorted-kws (sort (map list kws kw-args kw-arg?s kw-reqs kw-not-supplieds)
                                            (lambda (a b) (keyword<? (syntax-e (car a))
                                                                    (syntax-e (car b)))))]
                            
                            
                            [annotate-method (lambda (stx) stx)]
                            [flatten-keywords (lambda (kws)
                                                (let loop ([kws kws])
                                                (cond
                                                [(null? kws) null]
                                                [else
                                                    (cons (cadar kws) (loop (cdr kws)))])))]
                            [local-name #f]
                            [add-local-name (lambda (stx) stx)])
                        (with-syntax ([(kw-arg ...) kw-args]
                             [kws-sorted sorted-kws]
                             [(opt-arg ...) opt-args]
                             [(opt-not-supplied ...) opt-not-supplieds]
                             [(new-plain-id  ...) (generate-temporaries #'(plain-id ...))]
                             [new-rest (if (null? #'rest)
                                           '()
                                           '(new-rest))]
                             [(rest-id) (if (null? #'rest)
                                            '(())
                                            #'rest)]
                             [rest-empty (if (null? #'rest)
                                             '()
                                             '(null))]
                             [fail-rest (if (null? #'rest)
                                            '(null)
                                            #'rest)]
                             [make-okp 
                                           #'make-optional-keyword-procedure]
                             [kp/ae #'keyword-procedure/arity-error]
                             [with-kw-min-args (+ 2 (length plain-ids))]
                             [with-kw-max-arg (if (null? #'rest)
                                                  (+ 2 (length plain-ids) (length opts))
                                                  #f)]
                             [core (datum->syntax #f 'core)]
                             [unpack (datum->syntax #f 'unpack)])
                        (let ([mk-core
                            (lambda (kw-core?)
                                ;; body of procedure, where all optional
                                ;; arguments (both keyword and positional)
                                ;; come in as a pair of arguments (value and
                                ;; whether the value is valid):
                                ;; the arguments are in the following order:
                                ;; - optional kw; `unsafe-undefined` for not-supplied
                                ;; - mandatory kw
                                ;; - mandatory positional
                                ;; - optional positional; `unsafe-undefined` for not-supplied
                                ;; - rest arguments
                                #`(lambda (#,@(if kw-core? 
                                                (flatten-keywords sorted-kws)
                                                '())
                                            new-plain-id ...
                                            opt-arg ...
                                            . new-rest)
                                    (let-maybe ([id opt-expr kind] ... . rest)
                                               (kw-arg ...)
                                               (opt-arg ...)
                                               (new-plain-id ... . new-rest)
                                               ;; the original body, finally:
                                               body1 body ...)        ))]
                                 [mk-unpack 
                                    (lambda ()
                                        ;; like core, but keywords must be unpacked
                                        #`(lambda (given-kws given-args new-plain-id ... opt-arg ... . new-rest)
                                            (let-kws given-kws given-args kws-sorted
                                                (core #,@(flatten-keywords sorted-kws)
                                                      new-plain-id ...
                                                      opt-arg ...
                                                      . new-rest))))]
                                 [mk-no-kws 
                                    (lambda (kw-core?) 
                                        ;; entry point without keywords
                                        #`(opt-cases #,(if kw-core? 
                                                        #'(unpack '() '())
                                                        #'(core))
                                                     ([opt-id opt-arg opt-not-supplied] ...) (plain-id ...)
                                                     () ()
                                                     (rest-empty rest-id . rest) ()))]
                                 [mk-with-kws 
                                    (lambda ()
                                        ;; entry point with keywords 
                                        
                                        #'(opt-cases (unpack) ([opt-id opt-arg opt-not-supplied] ...) (given-kws given-args plain-id ...)
                                                    () ()
                                                    (rest-empty rest-id . rest) ()))]
                                 [mk-kw-arity-stub 
                                    (lambda () 
                                        #'(fail-opt-cases (missing-kw) (opt-id ...) (self plain-id ...)
                                            ()
                                            (rest-id . fail-rest) ()))]
                                 [kw-k*
                                    (lambda (impl kwimpl wrap)
                                        (kw-k impl kwimpl wrap #'core #'unpack 
                                              (length plain-ids)
                                              opt-not-supplieds
                                              (not (null? #'rest))
                                              needed-kws
                                              ;; split not-supplied defaults from the rest of keyword information,
                                              ;; because the not-supplied terms will need to be syntax-quoted
                                              sorted-kws))])
                            (cond 
                                [(null? kws)
                                    ;; just the no-kw part
                                    (non-kw-k
                                        #`(let ([core #,(mk-core #f)])
                                            #,(mk-no-kws #f)))]
                                [(null? needed-kws)
                                    ;; both parts dispatch to core
                                    (kw-k* 
                                        (mk-core #t)
                                        (mk-unpack)
                                        (with-syntax ([kws (map car sorted-kws)]
                                                      [no-kws (mk-no-kws #t)]
                                                      [with-kws (mk-with-kws)])
                                            #`(make-okp 
                                                (lambda (given-kws given-argc)

                                                    (and 
                                                        (in-range?/static given-argc with-kw-min-args with-kw-max-arg)
                                                        (subset?/static given-kws 'kws)))
                                                with-kws
                                                '()
                                                'kws
                                                no-kws)))]
                                [else 
                                    ;; just the keywords part dispatches to core,
                                    ;; and the other part dispatches to failure
                                    (kw-k* 
                                        (mk-core #t)
                                        (mk-unpack)
                                        (with-syntax ([kws (map car sorted-kws)]
                                                      [needed-kws needed-kws]
                                                      [with-kws (mk-with-kws)]
                                                      [(_ mk-id . _)
                                                        (with-syntax ([n 'unknown]
                                                                      [call-fail (mk-kw-arity-stub)])
                                                            #'(make-required* kp/ae 'n #f mk-id))])
                                            #`(mk-id 
                                                (lambda (given-kws given-argc)
                                                    
                                                    (and (in-range?/static given-argc with-kw-min-args with-kw-max-arg)
                                                         (subsets?/static 'needed-kws given-kws 'kws)))
                                                with-kws 
                                                'needed-kws
                                                'kws)))]))))))]))



(define-syntax make-required* 
    (lambda (stx)
        (syntax-case stx () 
            [(_ struct:km/ae name fail-proc)
                42
            ]
        )
    )
)

(define-syntax new-lambda 
    (lambda (stx)
        (parse-lambda stx #f
            (lambda (e) 
                e)
            (lambda (impl kwimpl wrap core-id unpack-id 
                n-req
                opt-not-supplieds
                rest?
                req-kws
                all-kws) 

                (define x #`(let ([#,core-id #,impl])
                    (let ([#,unpack-id #,kwimpl])
                        #,wrap)))
                x))))

(define-for-syntax (hide-binding-name stx)
    stx)

(define-for-syntax ormap
    (lambda (f . lists)
    (cond ((null? (car lists)) (or))
          ((null? (cdr (car lists)))
           (apply f (map car lists)))
          (else
           (or (apply f (map car lists))
               (apply ormap f (map cdr lists)))))))


(define-for-syntax (parse-app stx check-arity generate-direct)
    (define (check-keywords args)
        (define kw-ht (make-core-hash-eq))
        (let lp ([args args])
            (syntax-case args () 
                [() '()]
                [(kw . rest)
                    (keyword? (syntax-e #'kw))
                    (begin 
                        (when (core-hash-ref kw-ht (syntax-e #'kw) #f)
                            (syntax-violation 
                                'application
                                "duplicate keyword in application"
                                stx 
                                #'kw))
                        (core-hash-set! kw-ht (syntax-e #'kw) #t)
                        (syntax-case #'rest () 
                            [() 
                                (syntax-violation 
                                    'application
                                    "missing argument expression after keyword"
                                    stx 
                                    #'kw)]
                            [(arg . rest)
                                (keyword? (syntax-e #'arg))
                                (syntax-violation 
                                    'application 
                                    "keyword in expression position (immediately after another keyword)"
                                    stx 
                                    #'arg)]
                            [(arg . rest)
                                (cons #'arg (lp #'rest))]))]
                [(arg . rest)
                    (cons #'arg (lp #'rest))])))

    (define (syntax-length args)
        (let loop ([i 0] [args args])
            (cond 
                [(null? args) i]
                [(syntax? args) (loop i (syntax-e args))]
                [(pair? args) (loop (+ i 1) (cdr args))])))

    (define (syntax-cdr x)
        (cond 
            [(syntax? x) (cdr (syntax-e x))]
            [else (cdr x)]))
    
    (define (syntax-car x)
        (cond 
            [(syntax? x) (car (syntax-e x))]
            [else (car x)]))

    (define (collect-args ids args)
        
        (let loop ([l args]
                   [ids ids]
                   [bind-accum '()]
                   [arg-accum '()]
                   [kw-pairs '()])
        
            (syntax-case l ()
                [()
                    (let* ([args (reverse arg-accum)]
                           [sorted-kws (sort kw-pairs 
                                            (lambda (a b) (keyword<? (syntax-e (car a))
                                                                     (syntax-e (car b)))))]
                           [cnt (+ 1 (syntax-length args))])
                        #`(let #,(reverse bind-accum)
                            #,(generate-direct 
                                (syntax-cdr args) sorted-kws #t 
                                #`((checked-procedure-check-and-extract #,(car args) keyword-procedure-extract '#,(map car sorted-kws) #,cnt)
                                            '#,(map car sorted-kws)
                                            (list #,@(map syntax-cdr sorted-kws))
                                            . #,(syntax-cdr args)))))]
                [(kw arg . rest)
                    (keyword? (syntax-e #'kw))
                    (begin
                        (printf "kw arg: ~a: ~a~%" #'kw #'arg)
                    (loop (syntax-cdr #'rest)
                          (cdr ids)
                          (cons (list (car ids) #'arg)
                                bind-accum)
                          arg-accum
                          (cons (cons #'kw (car ids))
                                kw-pairs)))]
                [(arg . rest)
                    (loop #'rest
                          (cdr ids)
                          (cons (list (car ids) #'arg)
                                bind-accum)
                          (cons (car ids) arg-accum)
                          kw-pairs)])))

    (define (keyword-application args)
        (define exprs (check-keywords args))
        (define name #f)
        (define ids (cons 
            (datum->syntax #f 'procedure)
            (generate-temporaries exprs)))
        

        (printf ";; keyword application ids: ~a~%" ids)
        (collect-args ids exprs))


    (syntax-case stx ()
        ;; simple or erroneus application:
        [(self proc arg ...)
            (not (ormap (lambda (x) (keyword? (syntax-e x))) #'(proc arg ...)))
            (generate-direct 
                #'(arg ...)
                '()
                #f 
                (datum->syntax #f (cons '(@@ (capy) |#%app|) #'(proc arg ...))))]
        ;; keyword application
        [(self arg ...)
            (keyword-application #'(arg ...))]
        [_ (syntax-violation 'application "invalid application" stx)]))

(define-for-syntax (parse-app- stx check-arity generate-direct)
    (define l (syntax->list stx))
    (printf "parse-app: ~a~%" stx)
    (if (not (and l
                  (pair? (cdr l))
                  (not (keyword? (syntax-e (cadr l))))
                  (ormap (lambda (x) (keyword? (syntax-e x))) l)))
        ;; simple or erroneous app:
        (if (identifier? stx)
            (syntax-violation #f "illegal use" stx)
            (if (and (pair? l)
                     (null? (cdr l)))
                (syntax-violation #f "missing procedure expression;\n probably originally (), which is an illegal empty application" stx)
                (begin 
                    (printf ";; simple app: ~a~%" (syntax->datum stx))
                    (when l
                        (check-arity (- (length l) 2)))
                    (let ([args (cdr (syntax-e stx))])
                        (define new-stx (datum->syntax #f (cons '(@@ (capy) |#%app|) (datum->syntax stx args))))
                        (generate-direct 
                            (if l 
                                (cdr (if (pair? args) args (syntax-e args)))
                                '())
                            '()
                            #f
                            new-stx)))))    
        ;; keyword application
        (let ([exprs 
                (let ([kw-ht (make-core-hash-eq)])
                    (let loop ([l (cddr l)])
                      
                        (cond 
                            [(null? l) '()]
                            [(keyword? (syntax-e (car l)))
                                (when (core-hash-ref kw-ht (syntax-e (car l)) #f)
                                    (syntax-violation 
                                        'application
                                        "duplicate keyword in application"
                                        stx 
                                        (car l)))
                                (core-hash-set! kw-ht (syntax-e (car l)) #t)
                                (cond 
                                    [(null? (cdr l))
                                        (syntax-violation 
                                            'application
                                            "missing argument expression after keyword"
                                            stx
                                            (car l))]
                                    [(keyword? (cadr l))
                                        (syntax-violation 
                                            'application
                                            "keyword in expression position (immediately after another keyword)"
                                            stx
                                            (cadr l))]
                                    [else 
                                        (cons (cadr l)
                                            (loop (cddr l)))])]
                            [else
                                (cons (car l)
                                    (loop (cdr l)))])))])
            (let* ([name #f]
                   [ids (cons 
                    (if (and name (or (identifier? name) (symbol? name)))
                        (if (syntax? name) name (datum->syntax #f name))
                        (datum->syntax #f 'procedure))
                    (generate-temporaries exprs))])
                (printf ";; keyword application ids: ~a, l=~a~%" (map syntax-e ids) l)
                (let loop ([l (cdr l)]
                           [ids ids]
                           [bind-accum '()]
                           [arg-accum '()]
                           [kw-pairs '()])
                    (cond 
                        [(null? l)
                            (let* ([args (reverse arg-accum)]
                                   [sorted-kws (sort kw-pairs 
                                                    (lambda (a b) (keyword<? (syntax-e (car a))
                                                                            (syntax-e (car b)))))]
                                   [cnt (+ 1 (length args))])
                                (check-arity (- cnt 2))
                                (printf "Bind-accum: ~a~%" bind-accum)
                                #`(let #,(reverse bind-accum)
                                    #,(generate-direct 
                                        (cdr args) sorted-kws #t
                                        #`((checked-procedure-check-and-extract #,(car args) keyword-procedure-extract '#,(map car sorted-kws) #,cnt)
                                            '#,(map car sorted-kws)
                                            (list #,@(map cdr sorted-kws))
                                            . #,(cdr args)))))]
                        [(keyword? (syntax-e (car l)))
                            (loop (cddr l)
                                  (cdr ids)
                                  (cons (list (car ids) (hide-binding-name (cadr l)))
                                        bind-accum)
                                  arg-accum
                                  (cons (cons (car l) (car ids))
                                        kw-pairs))]
                        [else (loop (cdr l)
                                    (cdr ids)
                                    (cons (list (car ids) (hide-binding-name (car l)))
                                          bind-accum)
                                    (cons (car ids) arg-accum)
                                    kw-pairs)]))))))




(define-syntax new-app
    (lambda (stx)
        (define (dd x)
            
            ;(pretty-print (syntax->datum x))
            x)
        (dd (datum->syntax stx (parse-app stx 
            (lambda args #t)
            (lambda (args kw-args lifted? orig) 
                
                orig))))))

(define-for-syntax (make-keyword-syntax get-ids n-req opt-not-supplieds rest? req-kws all-kws)
    "Build a `define-syntax` form that defines a keyword procedure and optimistically expands it to a direct procedure call"
    (make-variable-transformer
        (lambda (stx)
            (define (dd x)
                ;(pretty-print (syntax->datum x))
                x)
            (define (dd1 x)
                (pretty-print (syntax->datum x))
                x)
            (define-values (impl-id wrap-id) (get-ids))
            
            (dd (syntax-case stx (set!)
                [(set! self rhs)
                    #`(set! #,wrap-id rhs)]
                [(self arg ...)
                (let () 
                    (define n-opt (length opt-not-supplieds))
                    (define (convert-default-expr e)
                        (if (eq? e 'unspecified)
                            #'(unspecified)
                            (datum->syntax #'here e)))
                    
                     
                    (if (free-identifier=? #'new-app (datum->syntax stx '|#%app|))
                        (dd1 (datum->syntax stx (parse-app 
                            (datum->syntax #f (cons #'new-app stx))
                            (lambda (n)
                                (when (or (< n n-req)
                                          (and (not rest?)
                                               (> n (+ n-req n-opt))))
                                    ;; todo: print warning?
                                    #f))
                            (lambda (args kw-args lifted? orig)
                                (let* ([args (syntax->list (datum->syntax #f args))]
                                       [n (length args)]
                                       [lift-args (lambda (k)
                                            (if (not lifted?)
                                                (let ([ids (generate-temporaries args)])
                                                    #`(let #,(map list ids
                                                                  (map hide-binding-name args))
                                                        #,(k ids)))
                                                (k args)))])
                                   
                                    (or 
                                        (and (not (or (< n n-req)
                                                 (and (not rest?)
                                                      (> n (+ n-req n-opt)))))
                                        (let loop ([kw-args kw-args] [req-kws req-kws] [all-kws all-kws])
                                            (cond 
                                                [(null? kw-args)
                                                    (or (null? req-kws)
                                                        ;; todo: print warning
                                                        (printf ";; warning: missing required keyword: ~a~%" (car req-kws))
                                                        #f)]
                                                [else (let* ([kw (syntax-e (caar kw-args))]
                                                             [all-kws (let loop ([all-kws all-kws])
                                                                        (cond 
                                                                            [(null? all-kws) '()]
                                                                            [(keyword<? (caar all-kws) kw)
                                                                                (loop (cdr all-kws))]
                                                                            [else all-kws]))])
                                                        (cond 
                                                            [(or (null? all-kws)
                                                                (not (eq? kw (caar all-kws))))
                                                                (printf ";; warning: keyword not recognized: ~a~%" kw)
                                                                #f]
                                                            [(and (pair? req-kws)
                                                                  (eq? kw (car req-kws)))
                                                                (loop (cdr kw-args) (cdr req-kws) (cdr all-kws))]
                                                            [(and (pair? req-kws)
                                                                (keyword<? (car req-kws) (caar all-kws)))
                                                                (printf ";; warning: missing required keyword: ~a~%" (car req-kws))
                                                                #f]
                                                            [else
                                                                (loop (cdr kw-args) req-kws (cdr all-kws))]))]))
                                        (lift-args 
                                            (lambda (args)
                                              
                                                #`(if #t
                                                    ((@@ (capy) |#%app|) 
                                                        #,impl-id
                                                        ;; keyword arguments
                                                        #,@(let loop ([kw-args kw-args]
                                                                      [all-kws all-kws])
                                                                (cond 
                                                                    [(null? all-kws) '()]
                                                                    [(and (pair? kw-args)
                                                                          (eq? (syntax-e (caar kw-args)) (caar all-kws)))
                                                                        (cons (datum->syntax stx (cdar kw-args))
                                                                              (loop (cdr kw-args) (cdr all-kws)))]
                                                                    [else 
                                                                        (cons (convert-default-expr 
                                                                                (list-ref (car all-kws) 4))
                                                                              (loop kw-args (cdr all-kws)))]))
                                                        ;; required arguments
                                                        #,@(let loop ([i n-req] [args args])
                                                            (if (zero? i)
                                                                '()
                                                                (cons (car args)
                                                                      (loop (- i 1) (cdr args)))))
                                                        ;; optional arguments
                                                        #,@(let loop ([opt-not-supplieds opt-not-supplieds]
                                                                      [args (list-tail args n-req)])
                                                                (cond 
                                                                    [(null? opt-not-supplieds) '()]
                                                                    [(null? args)
                                                                        (cons (convert-default-expr (car opt-not-supplieds))
                                                                              (loop (cdr opt-not-supplieds) '()))]
                                                                    [else 
                                                                        (cons (car args) (loop (cdr opt-not-supplieds) (cdr args)))]))
                                                        ;; rest arguments
                                                        #,@(if rest?
                                                            #`((list #,@(list-tail args (min (length args) (+ n-req n-opt)))))
                                                            '()))

                                                    #,(if lifted? 
                                                        orig
                                                        #`((@@ (capy) |#%app|) #,wrap-id . #,args))))))
                                    orig))))))
                        (datum->syntax stx (cons wrap-id #'(arg ...)))))]
                [self 
                    (begin 
                        (printf "self-call : ~a = ~a~%" #'self wrap-id)
                        #t 
                    )
                    wrap-id])))))


(define-syntax new-define 
    (lambda (stx)
        (define (dd x)

            x)
        (define-values (id rhs) (normalize-definition stx #'new-lambda #t #t))
        (define (plain rhs)
           
            #`(define #,id #,rhs))
        (define (can-opt? lam-id)
            (and (identifier? lam-id)
                 (free-identifier=? lam-id #'new-lambda)
                 #t))
        (define (opt lam-id rhs core-wrap plain)
            (parse-lambda
                rhs 
                id
                (lambda (new-rhs)
                    (plain new-rhs))
                (lambda (impl kwimpl wrap core-id unpack-id n-req opt-not-supplieds rest? req-kws all-kws)
                    (with-syntax ([proc (car (generate-temporaries (list id)))])
                        #`(begin 
                            (define-syntax #,id
                                (make-keyword-syntax (lambda () 
                                                        (values (quote-syntax #,core-id)
                                                                (quote-syntax proc)))
                                                     #,n-req
                                                     (quote #,opt-not-supplieds)
                                                     #,rest?
                                                     '#,req-kws
                                                     '#,all-kws))

                            (define #,core-id #,(core-wrap impl))
                            (define #,unpack-id #,kwimpl)
                            (define proc #,wrap))))))
     
        (dd (syntax-case rhs (begin quote)
            [(lam-id . _)
                (can-opt? #'lam-id)
                (opt #'lam-id rhs values plain)]
            [(begin (quote sym) (lam-id . _))
                (and (can-opt? #'lam-id)
                     (identifier? #'sym))
                (syntax-case rhs () 
                    [(_ _ sub-rhs)
                        (let ([wrap (lambda (stx) #`(begin (quote sym) #,stx))])
                            (opt #'lam-id
                                 #'sub-rhs
                                 wrap 
                                 (lambda (rhs) (plain (wrap rhs)))))])]
            [_ (plain rhs)]))))

;; Helper macro:
;; Builds up a `case-lambda' to handle the arities
;; possible due to optional arguments. Each clause
;; jumps directory to `core', where each optional
;; argument is split into two: a boolean argument that
;; indicates whether it was supplied, and an argument 
;; for the value (if supplied).

  (define-syntax opt-cases 
    (lambda (stx)
    (syntax-case stx ()
      [(_ (core ...) () (base ...)
          () ()
          (rest-empty rest-id . rest) ())
       ;; This case only happens when there are no optional arguments
       (syntax
         (case-lambda
           [(base ... . rest-id)
            (core ... base ... . rest)]))]
      [(_ (core ...) ([opt-id opt-arg not-supplied-val]) (base ...)
          (done-id ...) (done-not-supplied ...)
          (rest-empty rest-id . rest) clauses)
       ;; Handle the last optional argument and the rest args (if any)
       ;; at the same time.
       (syntax
         (case-lambda
           [(base ...) (core ... base ... done-not-supplied ... not-supplied-val . rest-empty)]
           [(base ... done-id ... opt-arg . rest-id)
            (core ... base ... done-id ... opt-arg . rest)]
           . clauses))]
      [(_ (core ...) ([opt-id opt-arg not-supplied-val] [more-id more-arg more-not-supplied] ...) (base ...)
          (done-id ...) (done-not-supplied ...)
          (rest-empty rest-id . rest) clauses)
       ;; Handle just one optional argument, add it to the "done" sequence,
       ;; and continue generating clauses for the remaining optional arguments.
       (syntax
         (opt-cases (core ...) ([more-id more-arg more-not-supplied] ...) (base ...)
                    (done-id ... opt-id) (done-not-supplied ... not-supplied-val)
                    (rest-empty rest-id . rest)
                    ([(base ... done-id ... opt-arg)
                      (core ... base ...
                            done-id ... opt-arg more-not-supplied ... . rest-empty)]
                     . clauses)))])))

;; Helper macro:
;; Walks through all arguments in order, shifting supplied
;; optional values into the user-supplied binding, and
;; evaluating default-value expressions when the optional
;; value is not available. The binding order here is
;; consistent with the original order of the arguments
;; (where, e.g., an optional keyword argument might
;; precede a required argument, so the required argument
;; cannot be used to compute the default).
(define-syntax let-maybe
    (lambda (stx)
        (syntax-case stx (required :plain :opt :kw-req :kw-opt)
            [(_ () () () () . body)
                #'(let () . body)]
            [(_ ([id ignore :plain] . more) kw-args opt-args (req-id . req-ids) . body)
                #'(let ([id req-id])
                    (let-maybe more kw-args opt-args req-ids . body))]
            [(_ ([id expr :opt] . more)  kw-args (opt-arg . opt-args) req-ids . body)
                #`(let ([id #,(wrap-default-check #'opt-arg #'expr)])
                    (let-maybe more kw-args opt-args req-ids . body))]
            [(_ ([id expr :kw-req] . more)  (kw-arg . kw-args) opt-args req-ids . body)
                #'(let ([id kw-arg])
                    (let-maybe more kw-args opt-args req-ids . body))]
            [(_ ([id expr :kw-opt] . more) (kw-arg . kw-args) opt-args req-ids . body)
                #`(let ([id #,(wrap-default-check #'kw-arg #'expr)])
                    (let-maybe more kw-args opt-args req-ids . body))]
            [(_ (id) () () (req-id) . body)
                #'(let ([id req-id]) . body)])))


;; Helper macro:
;; Steps through the list bound to `kw-args', extracting
;; the available values. For each keyword, this binds one
;; id to say whether the value is present, and one id
;; to the actual value (if present); if the keyword isn't
;; available, then the corresponding `req' is applied, which
;; should signal an error if the keyword is required.
(define-syntax let-kws 
    (lambda (stx)
        (syntax-case stx ()
            [(_ kws kw-args () . body)
            #'(begin . body)]
            [(_ kws kw-args ([kw arg arg? #f not-supplied-val]) . body)
            ;; last optional argument doesn't need to check as much or take as many cdrs
            #'(let ([arg? (pair? kws)])
                (let ([arg (if arg? (car kw-args) not-supplied-val)])
                    . body))]
            [(_ kws kw-args ([kw arg arg? #f not-supplied-val] . rest) . body)
            (with-syntax ([next-kws (datum->syntax #f (gensym 'kws))]
                          [next-kw-args (datum->syntax #f (gensym 'kw-args))])
                #'(let ([arg? (and (pair? kws)
                                    (eq? 'kw (car kws)))])
                    (let ([arg (if arg? (car kw-args) not-supplied-val)]
                        [next-kws (if arg? (cdr kws) kws)]
                        [next-kw-args (if arg? (cdr kw-args) kw-args)])
                    (let-kws next-kws next-kw-args rest . body))))]
            [(_ kws kw-args ([kw arg arg? #t _]) . body)
            ;; last required argument doesn't need to take cdrs
            #'(let ([arg (car kw-args)])
                . body)]
            [(_ kws kw-args ([kw arg arg? #t _] . rest) . body)
            (with-syntax ([next-kws (datum->syntax stx (gensym 'kws))]
                          [next-kw-args (datum->syntax stx (gensym 'kw-args))])
                #'(let ([arg (car kw-args)]
                        [next-kws (cdr kws)]
                        [next-kw-args (cdr kw-args)])
                    (let-kws next-kws next-kw-args rest . body)))])))

(define (make-keyword-procedure check proc required allowed)
    (define checked-proc (lambda args
        (apply proc args)))
    (set-procedure-property! 'kwd (tuple check proc required allowed))
    checked-proc)


(define (keyword-procedure/arity-error checker proc required allowed)
    (set-procedure-property! proc 'kwd (tuple checker proc required allowed #f))
    proc)

(define (keyword-procedure? proc)
    (and (procedure? proc)
         (procedure-property proc 'kwd)
         #t))

(define (keyword-procedure-checker proc)
    (and (keyword-procedure? proc)
         (tuple-ref (procedure-property proc 'kwd) 0)))

(define (keyword-procedure-proc proc)
    (and (keyword-procedure? proc)
         (tuple-ref (procedure-property proc 'kwd) 1)))

(define (keyword-procedure-required proc)
    (and (keyword-procedure? proc)
         (tuple-ref (procedure-property proc 'kwd) 2)))

(define (keyword-procedure-allowed proc)
    (and (keyword-procedure? proc)
         (tuple-ref (procedure-property proc 'kwd) 3)))

(define (checked-procedure-check-and-extract v alt-proc v1 v2)
    (if (and (keyword-procedure? v)
             ((keyword-procedure-checker v) v1 v2))
        (keyword-procedure-proc v)
        (alt-proc v v1 v2)))


(define (keyword-procedure-extract/method kws n p method-n)
    "Extracts the procedure using the keyword-argument protocol.
    If `p` doesn't accept keywords, make up a procedure that reports 
    an error"
    (if (and (keyword-procedure? p)
             ((keyword-procedure-checker p) kws n))
        (keyword-procedure-proc p)
        (lambda (kws kw-args . args)
            (receive (missing-kw extra-kw)
                (if (keyword-procedure? p)
                    (check-kw-args p kws)
                    (values #f (car kws)))
                (let ([n (let ([method-n (+ method-n 0)])
                            (if (>= n method-n) (- n method-n) n))]
                      [name/val (cond 
                                    [(or (keyword-procedure? p)
                                         (procedure? p)
                                         (not extra-kw))
                                        p]
                                    [else p])])
                    (raise-wrong-kws name/val (keyword-procedure? p) (procedure? p)
                        n kws kw-args missing-kw extra-kw args))))))

(define (keyword-procedure-extract p kws n)
    (keyword-procedure-extract/method kws n p 0))

(define (raise-wrong-kws name/val kw-proc? proc? n kws kw-args missing-kw extra-kw args)
    (define proc-name (if (procedure? name/val)
                          (procedure-name name/val)
                          #f))

    (raise 
        (condition 
            (make-assertion-violation)
            (make-message-condition
                (if extra-kw
                    (if kw-proc? 
                        (format "procedure does not expect an argument with given keyword: ~a" extra-kw)
                        (if proc?
                            (format "procedure does not expect keyword arguments; got keyword: ~a" extra-kw)
                            (format "value is not a procedure; got keyword: ~a" extra-kw)))
                    (if missing-kw
                        (format "required keyword argument missing: ~a" missing-kw)
                        (format "no case matching ~a non-keyword argument~a" (- n 2) (if (= 1 (- n 2)) "" "s")))))
            (if proc-name  
                (make-who-condition proc-name)
                (make-who-condition 'application))
            (make-irritants-condition args)
            (make-marks-condition (current-continuation-marks)))))

;; Checks given kws against expected. Result is
;; (values missing-kw extra-kw), where both are #f if
;; the arguments are ok.
(define (check-kw-args p kws)
    (let loop ([kws kws]
               [required (keyword-procedure-required p)]
               [allowed (keyword-procedure-allowed p)])
        (cond
            [(null? kws)
                (if (null? required)
                    (values #f #f)
                    (values (car required) #f))]
            [(and (pair? required)
                    (eq? (car required) (car kws)))
                (loop (cdr kws) (cdr required) (and allowed (cdr allowed)))]
            [(not allowed) ; => all keywords are allowed
                (loop (cdr kws) required #f)]
            [(pair? allowed)
                (if (eq? (car allowed) (car kws))
                    (loop (cdr kws) required (cdr allowed))
                    (loop kws required (cdr allowed)))]
            [else (values #f (car kws))])))


(define (make-optional-keyword-procedure check with-kws required allowed no-kws)
    "Same as keyword-procedure/arity-error except return procedure is `no-kws`
    and on application with keywords `with-kws` is used"
    (set-procedure-property! no-kws 'kwd (tuple check with-kws required allowed no-kws))
    no-kws)



)
