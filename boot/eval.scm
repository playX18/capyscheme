(define (make-r7rs-toplevel-environment prefix)
      (make-toplevel-environment
       (lambda (id)
	 (string->symbol
	  (string-append
	   prefix
	   (symbol->string id))))))



(define (make-library-object environment exports) (tuple 'type:library environment exports))
(define (library-object? obj) (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:library)))
(define (library-object-environment lib) (tuple-ref lib 1))
(define (library-object-exports lib) (tuple-ref lib 2))
(define (set-library-object-exports! lib exports) (tuple-set! lib 2 exports))

(define library-table '())

(define (assoc-library spec) (assoc spec library-table))
(define current-library
    (make-parameter #f))
    
(define (with-library spec thunk)
    (let ([old (current-library)])
        (dynamic-wind
            (lambda () (current-library lib))
            (lambda ()
                (let ([env (library-environment (current-library))])
                    (with-toplevel-environment env thunk)))
            (lambda () (current-library old)))))

(define (make-library spec)
    (define (mangle-library-spec spec)
        (let rec ([spec spec])
            (if (null? spec)
                ""
                (string-append 
                    (cond 
                        [(symbol? (car spec)) (symbol->string (car spec))]
                        [(number? (car spec)) (number->string (car spec))])
                    (if (null? (cdr spec))
                        ":"
                        (string-append
                            "."
                            (rec (cdr spec))))))))
    (let ([env (make-r7rs-toplevel-environment (mangle-library-spec spec))])
        (let ([obj (make-library-object env '())])
            (set! library-table (acons spec obj library-table)))))

(define (library-environment spec)
    (let ([x (assoc-library spec)])
        (if x
            (library-object-environment (cdr x))
            (assertion-violation #f "unknown library" spec))))

(define (library-exports spec)
    (let ([x (assoc-library spec)])
        (if x
            (library-object-exports (cdr x))
            (assertion-violation #f "unknown library" spec))))

(define (library-exists? spec)
    (and (assoc-library spec) #t))


(define (expand-library form)
    (let ([spec (cadr form)])
        (make-library spec)
        (with-library spec 
            (lambda ()
                (let ([decls (cddr form)])
                    (let ([forms (append-map interpret-library-declaration decls)])
                        (expand-toplevel forms)))))))

(define (interpret-library-declaration decl)
    (let ([x (car decl)])
      (cond 
        [(eq? x 'begin) (cdr decl)]
        [(eq? x 'export) (for-each library-export (cdr decl)) '()]
        [(eq? x 'import) (for-each library-import (cdr decl)) '()])))

(define (library-import spec)
    (let ([name-map (make-name-map spec)])
        (let ([env (current-toplevel-environment)])
            (for-each 
                (lambda (c)
                    (install-toplevel-binding! (car c) (cdr c) env))
                name-map ))))

(define (make-name-map spec)
    (let ([exports (library-exports spec)]
          [env (library-environment spec)])
        (map (lambda (cell)
            (let ([nickname (car cell)] [id (cdr cell)])
                (let ([name (cdr (assq-environment id env))])
                    (cons nickname name)))) exports)))

(define (library-export id)
    (let ([obj (assoc-library (current-library))])
        (let ([exports (library-object-exports obj)])
            (set-library-object-exports! obj (acons id id exports)))))
(define (expand-toplevel forms)
    
    (let ((forms (map expand forms)))
    (let ((forms (let flatten ((form (cons 'begin forms)))
            (if (and (pair? form) (eq? (car form) 'begin))
                (append-map flatten (cdr form))
                (list form)))))
        (let ((forms (map (lambda (form) (post-expand form #t)) forms)))
    (if (= (length forms) 1)
        (car forms)
        (cons 'begin forms))))))

(define (post-expand form allow-definition?)
	  (cond
	   ((symbol? form)
	    form)
	   ((vector? form)
        form)
	   ((expander? form)
	    (error "invalid use of keyword" form))
	   ((not (list? form))
	    form)
	   (else
	    (let ([head (car form)])
            (cond 
                [(eq? head 'quote) form]
                [(eq? head 'begin) 
                    (if (null? (cdr form)
                        (syntax-violation #f "empty begin" form))
                    (cons 'begin (map (lambda (f) (post-expand f #f)) (cdr form))))]
                [(eq? head 'define)
                    (if (not allow-definition?)
                        (syntax-violation #f "definition not allowed here" form))
                    (cons 'define (cons (caddr form) (post-expand (caddr form) #f)))]
                [(eq? head 'lambda)
                    (cons 'lambda (cons (cadr form) (map (lambda (f) (post-expand f #f)) (cddr form))))]
                [else (map (lambda (f) (post-expand f #f)) form)])))))

(define (expand-repl form env)
    (print "expand repl " form " " env)
      (with-toplevel-environment env
	(lambda ()
      
	  (cond
	   ((and (list? form) (eq? (car form) 'import)) 
	    (for-each library-import (cdr form))
	    '(begin))
	   (else
	    (expand-toplevel (list form)))))))


