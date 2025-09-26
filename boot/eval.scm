(define (compile-tree-il x e)
    (save-module-excursion
        (lambda ()
            (current-module e)
            (let* ([x (if (= (length x) 1) (car x) (cons 'begin x))]
                   [x (macroexpand x 'c '(compile load eval))]
                   [_ (pretty-print-ir x)]
                   [cenv (current-module)])
                (values x cenv cenv)))))


(set! load
    (lambda (filename)
        (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module))])
            (thunk))))

(set! load-in-vicinity 
    (lambda (filename directory)
        (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) directory)])
            (thunk))))

; load file containing base macros
(load "boot/quasisyntax.scm")
(load "boot/base.scm")