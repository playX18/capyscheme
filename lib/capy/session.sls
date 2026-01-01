(library (capy session)
    (export 
        module-name->filename
        find-bindings
        fold-bindings
        fold-exported-bindings
        fold-all-bindings
        make-fold-module
        root-modules)
    (import (core primitives)
            (core files)
            (common pregexp)
            (rnrs hashtables)
            (srfi 1))

    (define (module-name->filename n)
        (define name (map symbol->string n))
        (define reverse-name (reverse name))
        (define leaf (car reverse-name))
        (define dir-hint-mname (reverse (cdr reverse-name)))
        (define dir-hint (apply string-append
                          (map (lambda (elt)
                                 (string-append elt "/"))
                               dir-hint-mname)))
        (cond 
            [(%search-load-path (in-vicinity dir-hint leaf)) => (lambda (ls) (list-ref ls 1))]
            [else #f]))

    (define (fold-bindings proc init pat folder)
        (define match (pregexp pat))
        (define recorded (make-eq-hashtable))
        (define (fold-module module data)
            (define (obarray-filter name val data)
                (if (and (pregexp-match match (symbol->string name))
                         (not (hashtable-contains? recorded name)))
                    (begin 
                        (hashtable-set! recorded name #t)
                        (folder name val data))
                    data))
            (define (module-filter binding data)
                (if (variable-bound? (cdr binding))
                    (obarray-filter (car binding)
                                    (variable-ref (cdr binding))
                                    data)
                    data))
            (cond 
                [module
                    (fold module-filter data (core-hash->list (module-obarray module)))]
                [else data]))
        (folder fold-module init))
    (define (make-fold-module init-thunk traverse extract)
        (lambda (fold-module init)
            (define table (make-eq-hashtable))
            (define (first? obj)
                (cond 
                    [(hashtable-contains? table obj) #f]
                    [else 
                        (hashtable-set! table obj #t)
                        #t]))
            (define modules (init-thunk))
            (define (rec data)
                (do ((modules modules (cdr modules))
	                 (data data (if (first? (car modules))
			        (rec (fold-module (extract (car modules)) data)
				        (traverse (car modules)))
			            data)))
	                ((null? modules) data)))
            (rec init)))
    
    (define (root-modules)
        (submodules (resolve-module '() #f #f)))

    (define (submodules mod)
        (map (lambda (binding)
            (cdr binding)
        ) (core-hash->list (module-submodules mod))))
    (define fold-exported-bindings
        (make-fold-module root-modules submodules module-public-interface))

    (define fold-all-bindings
        (make-fold-module root-modules submodules (lambda (x) x)))

    (define (find-bindings pat module)
        (fold-bindings 
            (lambda (module name var data)
                (cons name data))
            '()
            pat
            (fold-accessible module)))

    (define (fold-accessible module)
        (make-fold-module (lambda () (list module))
            module-uses
            (lambda (x) x)))

)
