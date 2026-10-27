(library (core thread)
  (export
    ;; Spawn / join. A thread handle is the only handle type: there is
    ;; no separate thread object at this level.
    go
    thread-join
    thread?
    current-thread
    thread-id
    thread-interrupt!
    ;; Structured concurrency: children spawned by `go` inside the scope
    ;; are joined before the scope returns.
    call-with-thread-scope
    with-thread-scope
    ;; Self operations.
    thread-yield
    thread-sleep
    ;; Mutexes and condition variables. The condition-variable names do
    ;; not collide with the R6RS condition-object constructors.
    make-mutex
    mutex?
    mutex-reentrant?
    mutex-acquire
    mutex-release
    with-mutex
    make-condition-variable
    condition-variable?
    condition-variable-wait
    condition-variable-signal
    condition-variable-broadcast
    &abandoned-mutex-exception
    abandoned-mutex-exception?
    abandoned-mutex-exception-mutex
    &uncaught-exception
    uncaught-exception?
    uncaught-exception-reason
    &terminated-thread-exception
    terminated-thread-exception?
    terminated-thread-exception-thread
    ;; Scheduler control for embedders. Not re-exported by
    ;; (capy concurrency): most programs never need a worker index.
    main-worker
    current-worker
    pinned-worker
    worker-count
    pin
    unpin
    go-pinned
    go/priority
    priority-levels
    thread-priority
    set-thread-priority!)
  (import
    (rename (core primitives)
      (%native-fork-thread %fork-thread)
      (%mutex-acquire-status %native-mutex-acquire-status)
      (mutex-acquire %native-mutex-acquire)
      (mutex? %native-mutex?)
      (thread-condition? %native-condition-variable?))
    (core conditions)
    (core exceptions)
    (core records))

  (define-condition-type &uncaught-exception &error
    make-uncaught-exception
    uncaught-exception?
    (reason uncaught-exception-reason))

  (define-condition-type &terminated-thread-exception &error
    make-terminated-thread-exception
    terminated-thread-exception?
    (thread terminated-thread-exception-thread))

  ;; Raised by `mutex-acquire` once, by the first acquirer after the owner
  ;; died holding the mutex (SRFI-18 abandoned mutex).
  (define-condition-type &abandoned-mutex-exception &error
    make-abandoned-mutex-exception
    abandoned-mutex-exception?
    (mutex abandoned-mutex-exception-mutex))

  (define-record-type uncaught
    (fields
      reason))

  (define-record-type (<thread> make-thread-record thread?)
    (fields
      (mutable native thread-native thread-native-set!)
      (immutable joinable? thread-joinable?)
      (mutable result thread-result thread-result-set!)))

  (define %current-thread
    (make-thread-parameter
      (make-thread-record (%native-current-thread) #f #f)
      thread?))

  (define (current-thread)
    (%current-thread))

  (define (mutex? obj)
    (%native-mutex? obj))

  (define (mutex-acquire mutex . block)
    (acquire-checked mutex (if (null? block) #t (car block))))

  (define (acquire-checked mutex block)
    (let ([status (%native-mutex-acquire-status mutex block)])
      (cond
        [(eqv? status 2) (raise (make-abandoned-mutex-exception mutex))]
        [else (not (eqv? status 0))])))

  ;; Condition variables are the native objects formerly spelled
  ;; make-condition / condition-wait / condition-signal /
  ;; condition-broadcast in (core primitives).
  (define make-condition-variable make-condition)
  (define condition-variable? %native-condition-variable?)
  (define condition-variable-wait condition-wait)
  (define condition-variable-signal condition-signal)
  (define condition-variable-broadcast condition-broadcast)

  ;; --- Structured-concurrency scope -------------------------------------
  ;; A scope records every thread spawned by `go` while it is the
  ;; current scope. Children spawned inside a child thread do not
  ;; register (the parameter is thread-local), so a scope tracks the
  ;; threads its own dynamic extent spawns directly.
  (define-record-type (scope make-scope scope?)
    (fields
      lock
      (mutable children scope-children scope-children-set!)
      (mutable first-error scope-first-error scope-first-error-set!)))

  ;; The sentinel is '(), not #f: make-thread-parameter rejects #f.
  (define %current-scope (make-thread-parameter '()))

  (define (scope-register! g)
    (let ([s (%current-scope)])
      (when (scope? s)
        (with-mutex (scope-lock s)
          (scope-children-set! s (cons g (scope-children s)))))))

  (define (call-with-thread-scope thunk)
    "Run `thunk`. Every thread started with `go` inside it is joined
    before this returns, even if `thunk` or a child raises. The first
    condition raised (by `thunk` first, then by children in join order)
    is re-raised after all children finish. Returns `thunk`'s values."
    (unless (procedure? thunk)
      (assertion-violation 'call-with-thread-scope "expected a thunk" thunk))
    (let ([s (make-scope (make-mutex) '() #f)]
          [old (%current-scope)]
          [thunk-error #f]
          [thunk-values #f])
      (dynamic-wind
        (lambda () (%current-scope s))
        (lambda ()
          (guard (e [#t (set! thunk-error e)])
            (call-with-values thunk
              (lambda vals (set! thunk-values vals)))))
        (lambda () (%current-scope old)))
      ;; Join in waves: joining cannot register new children in this
      ;; thread, but be robust if a child was registered late.
      (let join-all ()
        (let ([children
                (with-mutex (scope-lock s)
                  (let ([cs (scope-children s)])
                    (scope-children-set! s '())
                    cs))])
          (unless (null? children)
            (for-each
              (lambda (g)
                (guard (e [#t
                            (unless (or thunk-error (scope-first-error s))
                              (scope-first-error-set! s e))])
                  (thread-join g)))
              (reverse children))
            (join-all))))
      (cond
        [thunk-error (raise thunk-error)]
        [(scope-first-error s) (raise (scope-first-error s))]
        [else (apply values thunk-values)])))

  (define-syntax with-thread-scope
    (syntax-rules ()
      [(_ body ...)
        (call-with-thread-scope (lambda () body ...))]))

  ;; --- Spawn / join ------------------------------------------------------
  (define (go thunk)
    "Start `thunk` as a new thread and return its handle. Use
    `thread-join` to wait for it and collect its values, or spawn it
    inside `with-thread-scope`. A thread that is never joined
    reports nothing: if it raises, the condition is stored in the
    handle and lost when the handle is dropped."
    (unless (procedure? thunk)
      (assertion-violation 'go "expected a thunk" thunk))
    (define thread (make-thread-record #f #t #f))
    (define (run-thread!)
      (%current-thread thread)
      (call/1cc
        (lambda (return)
          ;; A kill unwinds the thread by invoking `return`, which runs
          ;; the after-thunks between here and the kill point. Killing is
          ;; not exported here; the runtime delivers it through this
          ;; continuation for SRFI-226 and interrupt internals.
          (%native-set-thread-unwind-k! return)
          (with-exception-handler
            (lambda (exn)
              (thread-result-set! thread (make-uncaught exn))
              (return #f))
            (lambda ()
              (call-with-values
                thunk
                (lambda vals
                  (thread-result-set! thread vals)
                  (return #f))))))))
    (thread-native-set! thread (%fork-thread run-thread!))
    (scope-register! thread)
    thread)

  (define (thread-join thread)
    "Wait for `thread` to terminate and return its values. If it
    raised, raise &uncaught-exception with the original condition as
    its reason (see `uncaught-exception-reason`)."
    (unless (thread? thread)
      (assertion-violation 'thread-join "expected a thread" thread))
    (when (eq? thread (current-thread))
      (raise (make-terminated-thread-exception thread)))
    (unless (thread-joinable? thread)
      (error 'thread-join "thread is not joinable" thread))
    ;; Parks until the thread has finished, which is after its
    ;; result is published.
    (%native-thread-join (thread-native thread))
    (when (%native-thread-killed? (thread-native thread))
      (raise (make-terminated-thread-exception thread)))
    (let ([res (thread-result thread)])
      (if (uncaught? res)
        (raise (make-uncaught-exception (uncaught-reason res)))
        (apply values res))))

  (define (thread-id . maybe-g)
    "With no argument, id of the calling thread, or 0 when not in a
    spawned thread. With a thread handle, its id, or 0 before it
    has started."
    (if (null? maybe-g)
      (%thread-id)
      (let ([g (car maybe-g)])
        (unless (thread? g)
          (assertion-violation 'thread-id "expected a thread" g))
        (%native-thread-id (thread-native g)))))

  (define (thread-interrupt! thread thunk)
    "Run `thunk` in `thread` at its next interrupt point."
    (unless (thread? thread)
      (assertion-violation 'thread-interrupt! "expected a thread" thread))
    (unless (procedure? thunk)
      (assertion-violation 'thread-interrupt! "expected a thunk" thunk))
    (%native-thread-interrupt! (thread-native thread) thunk))

  ;; --- Self operations ---------------------------------------------------
  (define (thread-yield)
    "Yield the calling thread so others can run. Does nothing
    outside a thread."
    (%thread-yield))

  (define (thread-sleep seconds)
    "Park the calling thread for `seconds` (fractions allowed)."
    (unless (real? seconds)
      (assertion-violation 'thread-sleep "expected a number of seconds" seconds))
    (%thread-sleep (inexact seconds)))

  ;; --- Scheduler control (embedders only) --------------------------------
  ;; Workers. Threads run on a fixed pool of OS threads numbered from
  ;; 0, and worker 0 is the process's main thread: the one that UI
  ;; toolkits (GLFW, Cocoa, ...) require.
  (define (main-worker)
    "Index of the main thread's worker."
    0)

  (define (current-worker)
    "Index of the worker running the caller, or #f when not in a thread."
    (%thread-worker))

  (define (worker-count)
    "Number of scheduler workers (0 when not in a thread)."
    (%thread-worker-count))

  (define (pinned-worker)
    "The worker the caller is pinned to, or #f."
    (%thread-pinned))

  (define (pin n)
    "Restrict the calling thread to worker N and yield until it is
    running there. It stays on that worker, across blocking and waking,
    until `unpin`. `(pin (main-worker))` moves it onto the main thread.
    Calling from inside a native call is an error."
    (unless (and (integer? n) (exact? n) (>= n 0))
      (assertion-violation 'pin "expected a worker index" n))
    (%thread-pin! n))

  (define (unpin)
    "Let the scheduler run the calling thread on any worker again."
    (%thread-pin! #f))

  (define (go-pinned n thunk)
    "Start `thunk` as a new thread that only ever runs on worker N."
    (unless (procedure? thunk)
      (assertion-violation 'go-pinned "expected a thunk" thunk))
    (go (lambda ()
          (pin n)
          (thunk))))

  ;; Priorities. Level 0 is the most urgent. A thread's level moves
  ;; as it runs: it drops when it uses a whole quantum, rises when it
  ;; parks early, and returns to its base level on a periodic boost.
  (define (priority-levels)
    "Number of priority levels. Level 0 is the most urgent."
    (%thread-priority-levels))

  (define (check-priority who n)
    (unless (and (integer? n) (exact? n) (>= n 0) (< n (priority-levels)))
      (assertion-violation who "expected a priority level" n)))

  (define (thread-priority)
    "The calling thread's base priority, or #f when not in a thread."
    (%thread-priority))

  (define (set-thread-priority! n)
    "Set the calling thread's base priority to N. Does nothing
    outside a thread."
    (check-priority 'set-thread-priority! n)
    (%thread-set-priority! n))

  (define (go/priority n thunk)
    "Start `thunk` as a new thread with base priority N."
    (unless (procedure? thunk)
      (assertion-violation 'go/priority "expected a thunk" thunk))
    (check-priority 'go/priority n)
    (go (lambda ()
          (%thread-set-priority! n)
          (thunk)))))
