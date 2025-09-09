(print "loading records.scm")
(let ([m (define-module* 
    '(boot records)
    "records.scm"
    '()
    '()
    '()
    '()
    '()
    '())])
   (current-module m))
(module-export! (current-module) '(my-var))

(define my-var 123)
(define (my-func) my-var)

(let ([f my-func])
    (let ([m (define-module* 
        '(boot records2)
        "records.scm"
        '()
        '()
        '()
        '()
        '()
        '())])
    (current-module m)
    (print "module name: " (module-name (current-module)))
    (print "wow " (f))))