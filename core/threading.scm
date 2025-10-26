(library (core threading)
    (export 
        make-thread-parameter
        with-mutex
        make-condition
        thread-condition?
        condition-wait
        condition-signal
        condition-broadcast
        make-mutex
        mutex?
        mutex-acquire
        mutex-release
        call-with-new-thread
        join-thread)
    (import (core primitives)
            (core hashtables))

    ;; weak hashtable which keeps thread data until thread object is alive
    (define %thread-results (make-weak-hashtable))
    (define %thread-join-data (make-weak-hashtable))

    (define (call-with-new-thread thunk)
        (define cv (make-condition))
        (define mutex (make-mutex))
        (define thread #f)
        (fork-thread 
            (lambda ()
                (call-with-values 
                    (lambda ()
                        (with-mutex mutex 
                            (set! thread (current-thread))
                            (hashtable-set! %thread-join-data thread (cons cv mutex))
                            (condition-signal cv))
                        (with-exception-handler 
                            (lambda (exn) exn)
                            thunk))
                    (lambda vals 
                        (with-mutex mutex 
                            (hashtable-set! %thread-results cv vals)
                            (condition-broadcast cv)))))))

    (define (join-thread thread)
        (define data (hashtable-ref %thread-join-data thread #f))

        (unless data 
            (error 'join-thread "thread is not joinable" thread))
        (let ([cv (car data)]
              [mutex (cdr data)])
            (with-mutex mutex 
                (let loop () 
                    (cond 
                        [(hashtable-ref %thread-results cv #f) => 
                            (lambda (res)
                                (apply values res))]
                        [else 
                            (condition-wait cv mutex)
                            (loop)]))))))
