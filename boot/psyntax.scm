
(let ([syntax? (module-ref (current-module) 'syntax?)]
      [make-syntax (module-ref (current-module) 'make-syntax)]
      [syntax-expression (module-ref (current-module) 'syntax-expression)]
      [syntax-wrap (module-ref (current-module) 'syntax-wrap)]
      [syntax-module (module-ref (current-module) 'syntax-module)]
      [syntax-sourcev (module-ref (current-module) 'syntax-sourcev)])
    (define (toplevel-eval exp env) (primitive-eval exp))
    (define (local-eval exp env) (primitive eval exp))

    (define (global-extend type sym val)
      (module-define! (current-module) sym (make-syntax-transformer sym type val)))
    
    (define (sourcev-filename s) (vector-ref s 0))
    (define (sourcev-line s) (vector-ref s 1))
    (define (sourcev-column s) (vector-ref s 2))
    (define sourcev->alist
             (lambda (sourcev)
               (letrec* ((maybe-acons (lambda (k v tail) (if v (acons k v tail) tail))))
                 (and sourcev
                      (maybe-acons
                       'filename
                       (sourcev-filename sourcev)
                       (list (cons 'line (sourcev-line sourcev)) (cons 'column (sourcev-column sourcev))))))))
                       
    (print (sourcev->alist (vector "psyntax.scm" 10 20))))