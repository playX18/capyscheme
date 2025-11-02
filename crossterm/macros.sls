(library (crossterm macros)
    (export csi ocs execute)
    (import (rnrs))

    (define-syntax csi 
        (lambda (stx)
            (syntax-case stx () 
                [(_ seq)
                    (string? (syntax->datum #'seq))
                    (datum->syntax stx (string-append "\x1b;[" (syntax->datum #'seq)))])))
    
    (define-syntax osc 
        (lambda (stx)
            (syntax-case stx () 
                [(_ seq)
                    (string? (syntax->datum #'seq))
                    (datum->syntax stx (string-append "\x1b;]" (syntax->datum #'seq) "\x1B;\\"))])))
    
    (define-syntax execute 
        (syntax-rules () 
            [(_ port cmd ...)
                (begin 
                    (cmd port) ...
                    (flush-output-port port))]))
)