(library (core threading)
  (export
    make-thread-parameter
    with-mutex
    current-thread
    thread?
    make-condition
    thread-condition?
    condition-wait
    condition-signal
    condition-broadcast
    make-mutex
    mutex?
    mutex-reentrant?
    mutex-acquire
    mutex-release
    call-with-new-thread
    join-thread
    thread-interrupt!
    &uncaught-exception
    uncaught-exception?
    uncaught-exception-reason
    &terminated-thread-exception
    terminated-thread-exception?
    terminated-thread-exception-thread)
  (import
    (rename (core primitives)
      (fork-thread %fork-thread)
      (%thread-interrupt! %native-thread-interrupt!)
      (mutex? %native-mutex?)
      (thread-condition? %native-thread-condition?)
      (thread? %native-thread?)
      (current-thread %native-current-thread))
    (core conditions)
    (core records))

  (define-condition-type &uncaught-exception &error
    make-uncaught-exception
    uncaught-exception?
    (reason uncaught-exception-reason))

  (define-condition-type &terminated-thread-exception &error
    make-terminated-thread-exception
    terminated-thread-exception?
    (thread terminated-thread-exception-thread))

  (define-record-type uncaught
    (fields
      reason))

  (define-record-type (<thread> make-thread-record thread?)
    (fields
      (mutable native thread-native thread-native-set!)
      (immutable mutex thread-mutex)
      (immutable condition thread-condition)
      (mutable done? thread-done? thread-done?-set!)
      (mutable result thread-result thread-result-set!)))

  (define %current-thread
    (make-thread-parameter
      (make-thread-record (%native-current-thread) #f #f #f #f)
      thread?))

  (define (current-thread)
    (%current-thread))

  (define (mutex? obj)
    (%native-mutex? obj))

  (define (thread-condition? obj)
    (%native-thread-condition? obj))

  (define (call-with-new-thread thunk)
    (define cv (make-condition))
    (define mutex (make-mutex))
    (define thread (make-thread-record #f mutex cv #f #f))

    (define (publish-result! res)
      (with-mutex mutex
        (thread-result-set! thread res)
        (thread-done?-set! thread #t)
        (condition-broadcast cv)))

    (define (run-thread!)
      (%current-thread thread)
      (call/cc
        (lambda (return)
          (with-exception-handler
            (lambda (exn)
              (publish-result! (make-uncaught exn))
              (return #f))
            (lambda ()
              (call-with-values
                thunk
                (lambda vals
                  (publish-result! vals)
                  (return #f))))))))

    (thread-native-set! thread (%fork-thread run-thread!))
    thread)

  (define (join-thread thread)
    "Wait for thread to terminate and return its values. If exception
    was raised in the thread, this function will raise &uncaught-exception
    with the original exception as reason."
    (unless (thread? thread)
      (assertion-violation 'join-thread "expected a thread" thread))
    (when (eq? thread (current-thread))
      (raise (make-terminated-thread-exception thread)))
    (unless (thread-mutex thread)
      (error 'join-thread "thread is not joinable" thread))
    (let ([cv (thread-condition thread)]
          [mutex (thread-mutex thread)])
      (with-mutex mutex
        (let loop ()
          (cond
            [(thread-done? thread)
              (let ([res (thread-result thread)])
                (if (uncaught? res)
                  (raise (make-uncaught-exception (uncaught-reason res))))
                (apply values res))]
            [else
              (condition-wait cv mutex)
              (loop)]))))))

  (define (thread-interrupt! thread thunk)
    (unless (thread? thread)
      (assertion-violation 'thread-interrupt! "expected a thread" thread))
    (unless (procedure? thunk)
      (assertion-violation 'thread-interrupt! "expected a thunk" thunk))
    (%native-thread-interrupt! (thread-native thread) thunk))
