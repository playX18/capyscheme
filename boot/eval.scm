


(define (compile-tree-il x e)
    (define (join exps)
        (cond 
            [(null? exps) (make-void #f)]
            [else 
                (if (null? (cdr exps))
                    (car exps)
                    (make-sequence #f (car exps) (join (cdr exps))))]))
   
    (save-module-excursion
        (lambda ()
            (current-module e)
            (let lp ([exps x] [out '()])
                (cond 
                    [(null? exps) (values (join (reverse out)) (current-module) (current-module))]
                    [else 
                        (let ([exp (macroexpand (car exps) 'c '(compile load eval))])
                            (print "expansion in " (module-name (current-module)))
                            (pretty-print-ir exp)
                            (lp (cdr exps) (cons exp out)))])))))


(set! load
    (lambda (filename)
        (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module))])
            (thunk))))

(set! load-in-vicinity 
    (lambda (filename directory)
        (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) directory)])
            (thunk))))


(define (eval x . m)
    (let* ([m (if (null? m) (current-module) (car m))]
           [code (macroexpand x 'e '(eval))])
        (primitive-eval code)))
; load file containing base macros
(load "boot/quasisyntax.scm")
(load "boot/base.scm")
(load "test.scm")