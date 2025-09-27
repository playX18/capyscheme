

(define-syntax else
  (lambda (x)
    (syntax-violation 'else "bad use of 'else' syntactic keyword" x x)))

(define-syntax =>
  (lambda (x)
    (syntax-violation '=> "bad use of '=>' syntactic keyword" x x)))

(define-syntax ...
  (lambda (x)
    (syntax-violation '... "bad use of '...' syntactic keyword" x x)))

(define-syntax _
  (lambda (x)
    (syntax-violation '_ "bad use of '_' syntactic keyword" x x)))

(define-syntax or
    (lambda (stx)
        (syntax-case stx ()
            [(_ e1 e2 e3 ...) 
                #'(let ([tmp e1]) (if tmp tmp (or e2 e3 ...)))]
            [(_ e) #'e]
            [(_) #'#f])))

(define-syntax and
    (lambda (stx)
        (syntax-case stx ()
            [(_ e1 e2 e3 ...) 
                #'(if e1 (and e2 e3 ...) #f)]
            [(_ e) #'e]
            [(_) #'#t])))

(define-syntax syntax-rules
  (lambda (xx)
    (define (expand-clause clause)
      ;; Convert a 'syntax-rules' clause into a 'syntax-case' clause.
      (syntax-case clause (syntax-error)
        ;; If the template is a 'syntax-error' form, use the extended
        ;; internal syntax, which adds the original form as the first
        ;; operand for improved error reporting.
        (((keyword . pattern) (syntax-error message arg ...))
         (string? (syntax->datum #'message))
         #'((dummy . pattern) #'(syntax-error (dummy . pattern) message arg ...)))
        ;; Normal case
        (((keyword . pattern) template)
         #'((dummy . pattern) #'template))))
    (define (expand-syntax-rules dots keys docstrings clauses)
      (with-syntax
          (((k ...) keys)
           ((docstring ...) docstrings)
           ((((keyword . pattern) template) ...) clauses)
           ((clause ...) (map expand-clause clauses)))
        (with-syntax
            ((form #'(lambda (x)
                       docstring ...        ; optional docstring
                       #((macro-type . syntax-rules)
                         (patterns pattern ...)) ; embed patterns as procedure metadata
                       (syntax-case x (k ...)
                         clause ...))))
          (if dots
              (with-syntax ((dots dots))
                #'(with-ellipsis dots form))
              #'form))))
    (syntax-case xx ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (expand-syntax-rules #f #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ (k ...) docstring ((keyword . pattern) template) ...)
       (string? (syntax->datum #'docstring))
       (expand-syntax-rules #f #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) ((keyword . pattern) template) ...)
       (identifier? #'dots)
       (expand-syntax-rules #'dots #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) docstring ((keyword . pattern) template) ...)
       (and (identifier? #'dots) (string? (syntax->datum #'docstring)))
       (expand-syntax-rules #'dots #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...))))))

(define-syntax define-module 
    (lambda (x)
        (syntax-case x () 
            [(_ (name name* ...) body ...)
                #'(begin 
                    (eval-when (expand load eval)
                        (let ([m (define-module* '(name name* ...))])
                            (current-module m)))
                    body ...)])))

(define-syntax export 
    (lambda (stx)
        (define (quotify-specs args out)
            (syntax-case args (rename)
                ; order of exports does not matter
                [() out]
                [((rename from to) . args)
                    (quotify-specs #'args (cons (cons #'from #'to) out))]
                [(sym . args)
                    (if (not (identifier? #'sym))
                        (syntax-violation 'export "expected identifier or (rename from to)" #'sym #'args))
                    (quotify-specs #'args (cons #'sym out))]))

        (syntax-case stx ()
            [(_ spec spec* ...)
                (with-syntax (((specs ...) (quotify-specs #'(spec spec* ...) '())))
                    #'(eval-when (expand load eval) (module-export! (current-module) '(specs ...))))])))

(define-syntax import 
    (lambda (stx)
        (define (check-select names)
            (syntax-case names ()
                [() #t]
                [(name . names)
                    (syntax-case #'name ()
                        [(orig . new)
                            (if (not (and (identifier? #'orig) (identifier? #'new)))
                                (syntax-violation 'import "expected identifiers in (orig . new) pair" #'name #'names))
                            (check-select #'names)]
                        [name 
                            (if (not (identifier? #'name))
                                (syntax-violation 'import "expected identifier in select list" #'name #'names))
                            (check-select #'names)])]
                
                [(_ . _) 
                    (syntax-violation 'import "expected identifiers or (orig . new) pairs in select list" #'names)]))
        (define (check-hide names)
            (syntax-case names ()
                [() #t]
                [(name . names)
                    (if (not (identifier? #'name))
                        (syntax-violation 'import "expected identifier in hide list" #'name #'names))
                    (check-hide #'names)]
                [(_ . _) 
                    (syntax-violation 'import "expected identifiers in hide list" #'names)]))

        (define (quotify-spec spec xselect xhide xprefix)
            (syntax-case spec (select hide prefix)
                [(select (spec spec* ...) name ...)
                    (check-select #'(name ...))
                    (quotify-spec #'(spec spec* ...) #'(name ...) xhide xprefix)]
                [(hide spec name ...)
                    (check-hide #'(name ...))
                    (quotify-spec #'spec xselect #'(name ...) xprefix)]
                [(prefix spec id)
                    (if (not (identifier? #'id))
                        (syntax-violation 'import "expected identifier in prefix spec" #'spec #'id))
                    (quotify-spec #'spec xselect xhide #'id)]
                [(name name* ...)    
                    (list #'(name name* ...) xselect xhide xprefix)]
                [() (syntax-violation 'import "empty import spec" #'spec)]))

        (define (quotify-specs specs out)
            (syntax-case specs ()
                [() (reverse out)]
                [(spec . specs)
                    (quotify-specs #'specs (cons (quotify-spec #'spec #f '() #f) out))]))

        (syntax-case stx ()
            [(_ spec spec* ...)
                (with-syntax (((specs ...) (quotify-specs #'(spec spec* ...) '())))
                    #'(eval-when (expand load eval) (process-use-modules '(specs ...))))])))
