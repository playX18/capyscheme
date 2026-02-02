(library (capy generator)
  (export 
    generator 
    yield 
    return 
    make-generator
    generator?
    generator->list
    list->generator)
  (import 
          (srfi 259) ; procedure tags
          (rnrs)
          (rnrs syntax-case)
          (only (capy) syntax-parameterize define-syntax-parameter)) ; syntax parameters
           

  (define-procedure-tag generator-fn! generator? generator-fn-acc)

  
  (define-syntax-parameter yield 
    (syntax-rules () 
      [(_ . rest) (syntax-error "yield used outside of generator")]))

  (define-syntax-parameter return
    (syntax-rules () 
      [(_ . rest) (syntax-error "return used outside of generator")]))

  (define-syntax generator 
    (lambda (stx)
      "Define a generator lambda with ARGS and BODY. Inside BODY, the yield procedure can be
      called to yield values from the generator, and the return procedure can be called to
      terminate the generator and return a final value explicitly. 
      
      When generator is exhausted, it returns the eof object."
      (syntax-case stx () 
        [(_ args body ...)
          #'(make-generator 
            (lambda (k ret . args)
              (syntax-parameterize 
                ([yield (syntax-rules () [(_ . x) (k . x)])]
                 [return (syntax-rules () [(_ . x) (ret . x)])])
                body ...)))])))

  (define (make-generator proc)
    "Make a generator procedure from PROC. PROC should accept two arguments: yield and return,
    followed by any arguments passed to the generator. The yield procedure can be called to
    yield values from the generator, and the return procedure can be called to terminate the
    generator and return a final value explicitly."
    (generator-fn! #t proc)
    (let ((gen-cont #f)   
          (main-cont #f)
          (done #f))  
      (define (yield . vals)
        (call/cc (lambda (k)
                  (set! gen-cont k) 
                  (apply main-cont vals))))
      
      (define (return . vals)
        (set! done #t)
        (set! gen-cont #f)
        (apply main-cont vals))
      
      (let ([thunk (lambda args
        (if done
            (eof-object)
            (call/cc (lambda (k)
                      (set! main-cont k)     
                      (if gen-cont
                          (apply gen-cont args)   
                          (return (apply proc yield return args)))))))])
          (generator-fn! #t thunk)
          thunk)))
          
  (define (generator->list gen)
    "Convert a generator GEN to a list by exhausting it. 
    
    GEN must not take any arguments."
    (let loop ((result '()))
      (let ((val (gen)))
       
        (if (eq? val (eof-object))
          (reverse result)
          (loop (cons val result))))))

  (define (list->generator list)
    "Convert a LIST to a generator that yields each element of the list in order."
    (generator ()
      (let loop ([ls list]) 
        (cond 
          [(and (pair? ls) (null? (cdr ls)))
            (car ls)]
          [else 
            (yield (car ls))
            (loop (cdr ls))])))))
