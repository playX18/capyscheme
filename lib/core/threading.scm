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
    join-thread
    &uncaught-exception
    uncaught-exception?
    uncaught-exception-reason
    &terminated-thread-exception
    terminated-thread-exception?
    terminated-thread-exception-thread)
  (import (core primitives)
          (core hashtables)
          (core conditions)
          (core records))

  (define-condition-type &uncaught-exception &error 
    make-uncaught-exception
    uncaught-exception?
    (reason uncaught-exception-reason))

  (define-condition-type terminated-thread-exception &error 
    make-terminated-thread-exception
    terminated-thread-exception?
    (thread terminated-thread-exception-thread))

  (define-record-type uncaught
    (fields 
      reason))

  ;; weak hashtable which keeps thread data until thread object is alive
  (define %thread-results (make-weak-hashtable))
  (define %thread-join-data (make-weak-hashtable))

  (define (call-with-new-thread thunk)
    (define cv (make-condition))
    (define mutex (make-mutex))
    (define thread #f)

    (let ([t (fork-thread
              (lambda ()
                (call/cc 
                  (lambda (return)
                    (call-with-values
                      (lambda ()
                        (with-mutex mutex
                          (set! thread (current-thread))
                          (hashtable-set! %thread-join-data thread (cons cv mutex))
                          (condition-signal cv))
                        (with-exception-handler
                          (lambda (exn)
                            (define obj (make-uncaught exn))
                            (with-mutex mutex 
                              (hashtable-set! %thread-results cv obj)
                              (condition-broadcast cv))
                            (return #f))
                          thunk))
                      (lambda vals
                        (with-mutex mutex
                          (hashtable-set! %thread-results cv vals)
                          (condition-broadcast cv))))))))])

      (with-mutex mutex
        (let loop ()
          (cond
            [(hashtable-ref %thread-join-data t #f) =>
              (lambda (_) thread)]
            [else
              (condition-wait cv mutex)
              (loop)])))))

  (define (join-thread thread)
    "Wait for thread to terminate and return its values. If exception
    was raised in the thread, this function will raise &uncaught-exception
    with the original exception as reason."
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
                (if (uncaught? res)
                  (raise (make-uncaught-exception (uncaught-reason res))))
                (apply values res))]
            [else
              (condition-wait cv mutex)
              (loop)]))))))
