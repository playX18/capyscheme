;; Prelude for concurrent programming
(library (capy concurrency)
  (export
    ;; Threads (core thread)
    go
    thread-join
    thread?
    current-thread
    thread-id
    thread-interrupt!
    call-with-thread-scope
    with-thread-scope
    (rename (thread-yield yield))
    (rename (thread-sleep sleep))
    &uncaught-exception
    uncaught-exception?
    uncaught-exception-reason
    ;; Mutexes and condition variables (core thread)
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
    ;; Channels (capy channel, capy select)
    make-channel
    channel-send
    channel-recv
    channel-recv/ok
    channel-recv/timeout
    channel-try-send
    channel-try-recv
    channel-close
    channel-closed?
    channel-length
    channel-capacity
    closed-channel-violation?
    select
    ;; Synchronization (capy sync)
    make-wait-group
    wait-group-add!
    wait-group-done!
    wait-group-wait
    go/wg
    make-once
    once-do
    make-rw-mutex
    rw-read-lock!
    rw-read-unlock!
    rw-write-lock!
    rw-write-unlock!
    with-read-lock
    with-write-lock
    make-concurrent-hash-table
    concurrent-hash-table?
    concurrent-hash-table-get
    concurrent-hash-table-set!
    concurrent-hash-table-delete!
    concurrent-hash-table-update!
    concurrent-hash-table-size
    concurrent-hash-table->alist
    make-atomic
    atomic?
    atomic-ref
    atomic-set!
    atomic-add!
    atomic-cas!
    make-semaphore
    semaphore-acquire!
    semaphore-try-acquire!
    semaphore-release!
    with-semaphore
    make-errgroup
    errgroup-go
    errgroup-wait
    ;; Contexts (capy context)
    background-context
    context?
    with-cancel
    with-timeout
    with-deadline
    with-value
    context-done
    context-err
    context-value
    ;; Timers (capy timer)
    after
    make-timer
    timer?
    timer-channel
    timer-reset!
    timer-stop!
    tick
    ticker?
    ticker-channel
    ticker-stop!
    call-with-timeout
    timeout-expired?
    &timeout-expired
    ;; Pools and pipelines (capy pool)
    make-worker-pool
    pool?
    pool-submit!
    pool-shutdown!
    pool-future?
    pool-future-get
    pool-future-done?
    parallel-map
    fan-out
    fan-out-error
    fan-in
    map-stage
    pipeline)
  (import (core thread)
    (capy channel)
    (capy select)
    (capy sync)
    (capy context)
    (capy timer)
    (capy pool)))
