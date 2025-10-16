


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
                            (lp (cdr exps) (cons exp out)))])))))



(define (compile-file filename compiled-path env)
    (define (read-all in)
        (let lp ([exps '()])
            (let ([exp (read-syntax in)])
                (cond 
                    [(eof-object? exp) (reverse exps)]
                    [else (lp (cons exp exps))]))))

    (call-with-input-file filename 
        (lambda (in)
            (define exps (read-all in))
            (receive (code mod new-mod) (compile-tree-il exps env)
                (%compile code compiled-path mod)))))

(set! load-in-vicinity 
    (lambda (filename directory)
        (save-module-excursion 
            (lambda () 
                (define thunk-or-path (try-load-thunk-in-vicinity filename #t directory))
                (cond 
                    [(procedure? thunk-or-path) (thunk-or-path)]
                    [else 
                        ((compile-file (car thunk-or-path) (cdr thunk-or-path) (current-module)))])))))


(set! load
    (lambda (filename)
        (save-module-excursion 
            (lambda () 
                (define thunk-or-path (try-load-thunk-in-vicinity filename #t))
                (cond 
                    [(procedure? thunk-or-path) (thunk-or-path)]
                    [else 
                        ((compile-file (car thunk-or-path) (cdr thunk-or-path) (current-module)))])))))

        ;(save-module-excursion (lambda () (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) #t directory)])
        ;    (thunk))))))

(set! primitive-load
    (lambda (filename)
        (save-module-excursion (lambda () (let ([thunk (load-thunk-in-vicinity-k filename compile-tree-il (current-module) #f)])
            (thunk))))))

(define (eval x . m)
    (save-module-excursion (lambda () 
        (let* ([m (if (null? m) (current-module) (car m))]
           [code (macroexpand x 'e '(eval))])
            (primitive-eval code)))))

(define (try-resolve-module name autoload ensure)
    (with-exception-handler 
        (lambda exn #f)
        (lambda () 
            (resolve-module name autoload ensure))))
; load file containing base macros

(primitive-load "boot/base.scm")
(primitive-load "boot/libraries.scm")
(primitive-load "boot/match.scm")
(primitive-load "boot/cli.scm")