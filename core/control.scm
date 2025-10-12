(library (core control)
    (export 
        call/cc
        call-with-current-continuation
        dynamic-wind)
    (import)

    ;; *cms* : (listof (list-of (cons key value)))
    ;; Manipulated by call-with-continuation-mark in malcode.mal
    ;; It is important for continuation/mutation safety that the 
    ;; structure of the continuation mark stack is never mutated; all
    ;; changes must be done by creating a new structure and mutating
    ;; only the variable.
    (define *cms* (make-thread-local-fluid '()))

    (define (replace-mark/call-thunk key mark thunk)
        (define (alist-replace alist key mark)
            (cond 
                [(pair? alist)
                    (let ((p0 (car alist)))
                        (if (eq? key (car p0))
                            (cons (cons key mark) (cdr alist))
                            (cons p0 (alist-replace (cdr alist) key mark))))]
                [else (list (cons key mark))]))
        ;  (if (not (pair? *cms*))
        ;      (error "sys$replace-mark/call-thunk: *cms* not a pair"))
        (let ((frame0 (car (fluid-ref *cms*))))
            (fluid-set! *cms* (cons (alist-replace frame0 key mark) (cdr (fluid-ref *cms*))))
            ;; Tail call--call/cm frame is still at top of stack
            (thunk)))

    (define dynamic-wind 
        (let ([dynamic-wind (@ (capy) dynamic-wind)])
                (lambda (before thunk after)
                    (define cms (fluid-ref *cms*))
                    (dynamic-wind 
                        (lambda ()
                            (fluid-set! *cms* (cons '() cms))
                            (before))
                        thunk
                        (lambda ()
                            (after)
                            (fluid-set! *cms* cms))))))
    
    (define call-with-current-continuation 
        (let ([call/cc (@ (capy) call/cc)])
            (lambda (proc)
                (define cms (fluid-ref *cms*))
                (call/cc 
                    (lambda (k)
                        (proc (lambda args
                            (fluid-set! *cms* cms)
                            (apply k args))))))))
    (define call/cc call-with-current-continuation)
)