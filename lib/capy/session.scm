(library (capy session)
  (export
    module-name->filename
    find-bindings
    fold-bindings)
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

  (define (fold-bindings proc init pat ns)
    (define match (pregexp pat))
    (define recorded (make-eq-hashtable))
    (fold
      (lambda (binding data)
        (let ([name (car binding)]
              [var (cdr binding)])
          (if (and (variable-bound? var)
               (pregexp-match match (symbol->string name))
               (not (hashtable-contains? recorded name)))
            (begin
              (hashtable-set! recorded name #t)
              (proc ns name (variable-ref var) data))
            data)))
      init
      (namespace-bindings ns)))

  (define (find-bindings pat ns)
    (fold-bindings
      (lambda (ns name val data)
        (cons name data))
      '()
      pat
      ns))
)
