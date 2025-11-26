(define (resolve-module-path p enclosing)
    (cond 
        [(and (list? p)
              (= (length p) 2)
              (eq? 'quote (car p))
              (symbol? (cadr p)))
         (cadr p)]
        [(and (list? p)
            (eq? 'submod (car p))
            (equal? ".." (cadr p)))
            (foldl (lambda (enclosing s)
                (build-module-name s enclosing p))
                enclosing
                (cdr p))]
        [(and (list? p)
              (eq? 'submod (car p))
              (equal? "." (cadr p)))
            (foldl (lambda (enclosing s)
                (build-module-name s enclosing p))
                enclosing 
                (cdr p))]
        [(and (list? p)
              (eq? 'submod (car p)))
            (let ([base (resolve-module-path (cadr p) enclosing)])
              (foldl (lambda (enclosing s)
                  (build-module-name s enclosing p))
                  base
                  (cddr p)))]
        [else (syntax-violation #f "not supported module path" p)]))

(define (build-module-name name enclosing-module-name . rest)
    (define orig-name (if (null? rest) name (car rest)))
    (cond 
        [(equal? name "..")
            (cond 
                [(or (symbol? enclosing-module-name)
                     (not enclosing-module-name))
                    (syntax-violation #f "too many \"..\"s" orig-name)]
                [(= 2 (length enclosing-module-name))
                    (car enclosing-module-name)]
                [else (drop-right enclosing-module-name 1)])]
        [(not enclosing-module-name) name]
        [(symbol? enclosing-module-name) (list enclosing-module-name name)]
        [else (append enclosing-module-name (list name))]))

        