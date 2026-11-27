(library (core thread)
  (export
    go
    thread-join
    thread?
    current-thread
    thread-id
    thread-interrupt!
    call-with-thread-scope
    with-thread-scope
    call-with-thread-supervisor
    with-thread-supervisor
    call-with-thread-deadline
    with-thread-deadline
    current-thread-scope
    thread-scope?
    thread-scope-cancel!
    go-in
    go-detached
    thread-cancel!
    &scope-cancelled
    scope-cancelled?
    scope-cancelled-scope
    &deadline-exceeded
    deadline-exceeded?
    deadline-exceeded-seconds
    set-thread-name!
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
    (core records)
    (only (core hashtables) make-eq-hashtable hashtable-set! hashtable-delete! hashtable->alist)
    (only (core lists) remq))

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


  (define make-condition-variable make-condition)
  (define condition-variable? %native-condition-variable?)
  (define condition-variable-wait condition-wait)
  (define condition-variable-signal condition-signal)
  (define condition-variable-broadcast condition-broadcast)


  (define-condition-type &scope-cancelled &error
    make-scope-cancelled
    scope-cancelled?
    (scope scope-cancelled-scope))

  (define-condition-type &deadline-exceeded &scope-cancelled
    make-deadline-exceeded
    deadline-exceeded?
    (seconds deadline-exceeded-seconds))

  ;; `state` goes open -> closing (the body is over, children are being
  ;; joined) -> closed (no children left, nothing more is accepted).
  (define-record-type (scope make-scope scope?)
    (fields
      lock
      owner
      fail-fast?
      children
      (mutable first-error scope-first-error scope-first-error-set!)
      (mutable cancelled scope-cancelled scope-cancelled-set!)
      (mutable state scope-state scope-state-set!)))

  (define %current-scope (make-thread-parameter '()))

  (define (current-thread-scope)
    "The innermost scope the calling thread is running in, or #f."
    (let ([s (%current-scope)])
      (and (scope? s) s)))

  (define (thread-scope? obj)
    (scope? obj))

  (define (scope-child-list s)
    (map car (hashtable->alist (scope-children s))))

  (define (scope-forget! s thread)
    (with-mutex (scope-lock s)
      (hashtable-delete! (scope-children s) thread)))

  (define (scope-register! s thread)
    (with-mutex (scope-lock s)
      (cond
        [(eq? (scope-state s) 'closed) 'closed]
        [else
          (hashtable-set! (scope-children s) thread #t)
          (if (scope-cancelled s) 'cancelled 'ok)])))

  (define (scope-note-error! s e)
    (with-mutex (scope-lock s)
      (unless (scope-first-error s)
        (scope-first-error-set! s e))))

  (define (scope-interrupt-owner! s)
    (thread-interrupt! (scope-owner s)
      (lambda ()
        (when (eq? (scope-state s) 'open)
          (let ([reason (scope-cancelled s)])
            (when reason
              (raise reason)))))))

  (define (scope-begin-cancel! s reason self)
    (with-mutex (scope-lock s)
      (and (not (scope-cancelled s))
        (eq? (scope-state s) 'open)
        (begin
          (scope-cancelled-set! s reason)
          (remq self (scope-child-list s))))))

  ;; A child of `s` raised `exn`, in the child's own thread.
  (define (scope-child-failed! s thread exn)
    (let ([victims
            (with-mutex (scope-lock s)
              (cond
                [(scope-first-error s) #f]
                [else
                  (scope-first-error-set! s (make-uncaught-exception exn))
                  (and (scope-fail-fast? s)
                    (begin
                      (unless (scope-cancelled s)
                        (scope-cancelled-set! s (make-scope-cancelled s)))
                      (remq thread (scope-child-list s))))]))])
      (when victims
        (for-each thread-cancel! victims)
        (scope-interrupt-owner! s))))

  (define (scope-expire! s seconds)
    (let ([victims
            (scope-begin-cancel! s (make-deadline-exceeded s seconds) (current-thread))])
      (when victims
        (for-each thread-cancel! victims)
        (scope-interrupt-owner! s))))

  (define (scope-join-all! s)
    (let loop ()
      (let ([children
              (with-mutex (scope-lock s)
                (let ([cs (scope-child-list s)])
                  (when (null? cs)
                    (scope-state-set! s 'closed))
                  cs))])
        (unless (null? children)
          (for-each
            (lambda (g)
              (guard (e [(terminated-thread-exception? e) #f]
                         [#t (scope-note-error! s e)])
                (thread-join g))
              (scope-forget! s g))
            children)
          (loop)))))

  ;; Leave the body: stop accepting cancellations from the owner side, cancel
  ;; the children when the body did not end normally, and wait for them.
  (define (scope-close! s watchdog abort?)
    (let ([victims
            (with-mutex (scope-lock s)
              (scope-state-set! s 'closing)
              (when (and abort? (not (scope-cancelled s)))
                (scope-cancelled-set! s (make-scope-cancelled s)))
              (if (scope-cancelled s)
                (scope-child-list s)
                '()))])
      (when watchdog
        (thread-cancel! watchdog))
      (for-each thread-cancel! victims)
      (scope-join-all! s)))

  (define (our-cancellation? s e)
    (and (scope-cancelled? e) (eq? (scope-cancelled-scope e) s)))

  (define (run-scope thunk fail-fast? deadline)
    (let ([s (make-scope (make-mutex) (current-thread) fail-fast?
               (make-eq-hashtable) #f #f 'open)]
          [old (%current-scope)]
          [completed #f]
          [body-done #f]
          [thunk-error #f]
          [thunk-values '()]
          [watchdog #f])
      (dynamic-wind
        (lambda () (%current-scope s))
        (lambda ()
          (guard (e [#t (set! thunk-error e)])
            (when deadline
              (set! watchdog
                (go-in s
                  (lambda ()
                    (thread-sleep deadline)
                    (scope-expire! s deadline)))))
            (call-with-values thunk
              (lambda vals
                (set! thunk-values vals)
                (set! body-done #t))))
          (set! completed #t))
        (lambda ()
          (%current-scope old)
          (scope-close! s watchdog
            (or (not completed)
              (and thunk-error (not (our-cancellation? s thunk-error)))))))
      (cond
        [(and thunk-error (not (our-cancellation? s thunk-error)))
          (raise thunk-error)]
        [(scope-first-error s)
          => raise]
        [(deadline-exceeded? (or (scope-cancelled s) #f))
          (raise (scope-cancelled s))]
        [body-done (apply values thunk-values)]
        [else (if #f #f)])))

  (define (check-thunk who thunk)
    (unless (procedure? thunk)
      (assertion-violation who "expected a thunk" thunk)))

  (define (call-with-thread-scope thunk)
    "Run `thunk`. Every thread started with `go` inside it is joined before
    this returns. If the thunk or any of those threads raises, the rest are
    cancelled (they unwind, running their `dynamic-wind` after-thunks), the
    thunk itself is interrupted if it is still running, and the first condition
    raised is re-raised once everything has stopped: the thunk's own, or a
    child's as `&uncaught-exception`. Returns `thunk`'s values."
    (check-thunk 'call-with-thread-scope thunk)
    (run-scope thunk #t #f))

  (define (call-with-thread-supervisor thunk)
    "Like `call-with-thread-scope`, except that a failing child does not
    cancel the others. Every thread runs to the end, and the first condition
    raised is re-raised after all have finished."
    (check-thunk 'call-with-thread-supervisor thunk)
    (run-scope thunk #f #f))

  (define (call-with-thread-deadline seconds thunk)
    "Like `call-with-thread-scope`, with a time limit. When `seconds` have
    passed and the scope is still running, it is cancelled and raises
    `&deadline-exceeded` after its threads have stopped."
    (unless (and (real? seconds) (>= seconds 0))
      (assertion-violation 'call-with-thread-deadline "expected a number of seconds" seconds))
    (check-thunk 'call-with-thread-deadline thunk)
    (run-scope thunk #t seconds))

  (define-syntax with-thread-scope
    (syntax-rules ()
      [(_ body ...)
        (call-with-thread-scope (lambda () body ...))]))

  (define-syntax with-thread-supervisor
    (syntax-rules ()
      [(_ body ...)
        (call-with-thread-supervisor (lambda () body ...))]))

  (define-syntax with-thread-deadline
    (syntax-rules ()
      [(_ seconds body ...)
        (call-with-thread-deadline seconds (lambda () body ...))]))

  (define (thread-scope-cancel! . maybe-scope)
    "Cancel a scope: its threads are killed and its owner is interrupted with
    `&scope-cancelled`, which the scope absorbs when it leaves. The scope
    defaults to the current one. Does nothing when it is already cancelled or
    done."
    (let ([s (if (null? maybe-scope) (%current-scope) (car maybe-scope))])
      (unless (scope? s)
        (assertion-violation 'thread-scope-cancel! "expected a thread scope" s))
      (let ([victims (scope-begin-cancel! s (make-scope-cancelled s) (current-thread))])
        (when victims
          (for-each thread-cancel! victims)
          (scope-interrupt-owner! s)))))

  (define (thread-cancel! thread)
    "Ask `thread` to stop. It unwinds, running its `dynamic-wind` after-thunks,
    and `thread-join` on it then raises &terminated-thread-exception. Returns
    #t when the request was made, #f when the thread has not started, is
    finished, or is already being cancelled."
    (unless (thread? thread)
      (assertion-violation 'thread-cancel! "expected a thread" thread))
    (let ([native (thread-native thread)])
      ;; A published result means the body is over: the runtime may still be
      ;; retiring the thread, but there is nothing left to cancel.
      (and native
        (not (thread-result thread))
        (%native-thread-kill! native))))

  (define (set-thread-name! name)
    "Name the calling thread for thread dumps (see `(capy threads)`). #f
    clears the name."
    (unless (or (not name) (string? name))
      (assertion-violation 'set-thread-name! "expected a string or #f" name))
    (%thread-set-name! name))

  ;; The body of a thread: records its outcome in `thread`, reports a failure
  ;; to its scope, and leaves the scope when it is done.
  (define (make-runner thread thunk scope)
    (lambda ()
      (%current-thread thread)
      (call/1cc
        (lambda (return)
          (%native-set-thread-unwind-k! return)
          (with-exception-handler
            (lambda (exn)
              (thread-result-set! thread (make-uncaught exn))
              (when (scope? scope)
                (scope-child-failed! scope thread exn))
              (return #f))
            (lambda ()
              (call-with-values
                thunk
                (lambda vals
                  (thread-result-set! thread vals)
                  (return #f)))))))
      (when (scope? scope)
        (scope-forget! scope thread))))

  (define (spawn-thread thunk scope)
    (let ([thread (make-thread-record #f #t #f)])
      (cond
        [(scope? scope)
          ;; Interrupts are held back so that the owner cannot be cancelled
          ;; between forking the thread and recording it in the scope.
          (let ([status
                  (%call-without-interrupts
                    (lambda ()
                      (let ([status (scope-register! scope thread)])
                        (unless (eq? status 'closed)
                          (thread-native-set! thread
                            (%fork-thread (make-runner thread thunk scope)))
                          ;; Cancelled before it was forked, so the cancellation
                          ;; could not reach it.
                          (when (scope-cancelled scope)
                            (thread-cancel! thread)))
                        status)))])
            (when (eq? status 'closed)
              (assertion-violation 'go-in "the scope is closed" scope)))]
        [else
          (thread-native-set! thread
            (%fork-thread (make-runner thread thunk #f)))])
      thread))

  (define (go thunk)
    "Start `thunk` as a new thread and return its handle. Use
    `thread-join` to wait for it and collect its values, or spawn it
    inside `with-thread-scope`, which waits for it. A thread that is never
    joined reports nothing: if it raises, the condition is stored in the
    handle and lost when the handle is dropped."
    (unless (procedure? thunk)
      (assertion-violation 'go "expected a thunk" thunk))
    (spawn-thread thunk (%current-scope)))

  (define (go-detached thunk)
    "Like `go`, but the thread never belongs to a scope: no scope waits for it
    or cancels it. For helper threads that a library owns and stops itself,
    such as timers."
    (unless (procedure? thunk)
      (assertion-violation 'go-detached "expected a thunk" thunk))
    (spawn-thread thunk #f))

  (define (go-in scope thunk)
    "Start `thunk` as a new thread that belongs to `scope`, from any thread.
    The scope waits for it, and it is cancelled with the scope. The scope
    must not be done: raises an assertion violation when it is."
    (unless (scope? scope)
      (assertion-violation 'go-in "expected a thread scope" scope))
    (unless (procedure? thunk)
      (assertion-violation 'go-in "expected a thunk" thunk))
    (spawn-thread thunk scope))

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
    (%native-thread-interrupt!
      (thread-native thread)
      (if (eq? thread (current-thread))
        thunk
        ;; The target runs `thunk` with interrupts masked and unmasks when it
        ;; returns. If `thunk` escapes instead, unmask here, or the target
        ;; would never see another interrupt.
        (lambda ()
          (let ([returned #f])
            (dynamic-wind
              (lambda () #f)
              (lambda () (thunk) (set! returned #t))
              (lambda () (unless returned (%interrupts-unmask!)))))))))

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
