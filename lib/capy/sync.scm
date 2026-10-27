(library (capy sync)
  (export
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
    errgroup-wait)
  (import (rnrs)
    (core thread))

  ;; Every primitive here is built on a (core thread) mutex and condition.
  ;; Threads park on the condition, so nothing blocks the scheduler.

  ;; WaitGroup, as Go's sync.WaitGroup.
  (define-record-type (wait-group make-wait-group* wait-group?)
    (fields
      lock
      cv
      (mutable count)))

  (define (make-wait-group)
    "A new wait group with a counter of zero."
    (make-wait-group* (make-mutex) (make-condition-variable) 0))

  (define (wait-group-add! wg n)
    "Add `n` (which may be negative) to the counter. Raises an assertion
    violation if the counter goes negative. Waiters wake when it reaches zero."
    (with-mutex (wait-group-lock wg)
      (let ([next (+ (wait-group-count wg) n)])
        (when (< next 0)
          (assertion-violation 'wait-group-add! "negative WaitGroup counter"))
        (wait-group-count-set! wg next)
        (when (= next 0)
          (condition-variable-broadcast (wait-group-cv wg))))))

  (define (wait-group-done! wg)
    "Decrement the counter by one."
    (wait-group-add! wg -1))

  (define (wait-group-wait wg)
    "Block until the counter is zero."
    (with-mutex (wait-group-lock wg)
      (let loop ()
        (unless (= (wait-group-count wg) 0)
          (condition-variable-wait (wait-group-cv wg) (wait-group-lock wg))
          (loop)))))

  (define (go/wg wg thunk)
    "Run `thunk` in a new thread that is counted by `wg`. The count is
    raised before the thread starts and lowered when it finishes, even if
    it raises. Returns the thread handle."
    (wait-group-add! wg 1)
    (go (lambda ()
          (dynamic-wind
            (lambda () #f)
            thunk
            (lambda () (wait-group-done! wg))))))

  ;; Once, as Go's sync.Once. Concurrent callers wait until the first call
  ;; returns. If the thunk raises, the once still counts as done.
  (define-record-type (once make-once* once?)
    (fields
      lock
      cv
      (mutable state)))

  (define (make-once)
    "A new Once that has not run yet."
    (make-once* (make-mutex) (make-condition-variable) 'new))

  (define (once-do o thunk)
    "Call `thunk` the first time only."
    (let ([run? (with-mutex (once-lock o)
                  (let loop ()
                    (case (once-state o)
                      [(done) #f]
                      [(running)
                        (condition-variable-wait (once-cv o) (once-lock o))
                        (loop)]
                      [else
                        (once-state-set! o 'running)
                        #t])))])
      (when run?
        (dynamic-wind
          (lambda () #f)
          thunk
          (lambda ()
            (with-mutex (once-lock o)
              (once-state-set! o 'done)
              (condition-variable-broadcast (once-cv o))))))))

  ;; Read-write lock with writer preference: once a writer is waiting, new
  ;; readers queue behind it, so writers cannot be starved. A thread holding
  ;; a read lock must not take it again while a writer waits (it deadlocks,
  ;; as with Go's RWMutex).
  (define-record-type (rw-mutex make-rw-mutex* rw-mutex?)
    (fields
      lock
      cv
      (mutable readers)
      (mutable writer?)
      (mutable writers-waiting)))

  (define (make-rw-mutex)
    "A new read-write lock, initially unlocked."
    (make-rw-mutex* (make-mutex) (make-condition-variable) 0 #f 0))

  (define (rw-read-lock! rw)
    (with-mutex (rw-mutex-lock rw)
      (let loop ()
        (when (or (rw-mutex-writer? rw) (> (rw-mutex-writers-waiting rw) 0))
          (condition-variable-wait (rw-mutex-cv rw) (rw-mutex-lock rw))
          (loop)))
      (rw-mutex-readers-set! rw (+ 1 (rw-mutex-readers rw)))))

  (define (rw-read-unlock! rw)
    (with-mutex (rw-mutex-lock rw)
      (when (<= (rw-mutex-readers rw) 0)
        (assertion-violation 'rw-read-unlock! "read lock not held"))
      (rw-mutex-readers-set! rw (- (rw-mutex-readers rw) 1))
      (when (= (rw-mutex-readers rw) 0)
        (condition-variable-broadcast (rw-mutex-cv rw)))))

  (define (rw-write-lock! rw)
    (with-mutex (rw-mutex-lock rw)
      (rw-mutex-writers-waiting-set! rw (+ 1 (rw-mutex-writers-waiting rw)))
      (let loop ()
        (when (or (rw-mutex-writer? rw) (> (rw-mutex-readers rw) 0))
          (condition-variable-wait (rw-mutex-cv rw) (rw-mutex-lock rw))
          (loop)))
      (rw-mutex-writers-waiting-set! rw (- (rw-mutex-writers-waiting rw) 1))
      (rw-mutex-writer?-set! rw #t)))

  (define (rw-write-unlock! rw)
    (with-mutex (rw-mutex-lock rw)
      (unless (rw-mutex-writer? rw)
        (assertion-violation 'rw-write-unlock! "write lock not held"))
      (rw-mutex-writer?-set! rw #f)
      (condition-variable-broadcast (rw-mutex-cv rw))))

  (define (with-read-lock rw thunk)
    "Run `thunk` holding a read lock on `rw`."
    (dynamic-wind
      (lambda () (rw-read-lock! rw))
      thunk
      (lambda () (rw-read-unlock! rw))))

  (define (with-write-lock rw thunk)
    "Run `thunk` holding the write lock on `rw`."
    (dynamic-wind
      (lambda () (rw-write-lock! rw))
      thunk
      (lambda () (rw-write-unlock! rw))))

  ;; A hash table safe to share between threads. Keys are split over
  ;; several shards, each with its own mutex, so unrelated keys rarely
  ;; contend. Keys are compared with equal?.
  (define-record-type cht-shard
    (fields
      lock
      table))

  (define-record-type (concurrent-hash-table make-concurrent-hash-table* concurrent-hash-table?)
    (fields
      shards))

  (define (make-concurrent-hash-table . maybe-shards)
    "A new concurrent hash table. The optional argument is the number of
    shards (default 16)."
    (let* ([n (if (null? maybe-shards) 16 (car maybe-shards))]
           [shards (make-vector n #f)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (vector-set! shards i
          (make-cht-shard (make-mutex) (make-hashtable equal-hash equal?))))
      (make-concurrent-hash-table* shards)))

  (define (cht-shard-for t key)
    (let ([shards (concurrent-hash-table-shards t)])
      (vector-ref shards (mod (equal-hash key) (vector-length shards)))))

  (define (concurrent-hash-table-get t key . maybe-default)
    "Value stored under `key`, or the optional default (#f if not given)."
    (let ([s (cht-shard-for t key)])
      (with-mutex (cht-shard-lock s)
        (hashtable-ref (cht-shard-table s) key
          (if (null? maybe-default) #f (car maybe-default))))))

  (define (concurrent-hash-table-set! t key value)
    "Store `value` under `key`."
    (let ([s (cht-shard-for t key)])
      (with-mutex (cht-shard-lock s)
        (hashtable-set! (cht-shard-table s) key value))))

  (define (concurrent-hash-table-delete! t key)
    "Remove `key` if present."
    (let ([s (cht-shard-for t key)])
      (with-mutex (cht-shard-lock s)
        (hashtable-delete! (cht-shard-table s) key))))

  (define (concurrent-hash-table-update! t key proc default)
    "Atomically replace the value under `key` with (proc old), where old is
    the current value or `default` when absent. Returns the new value. `proc`
    runs while the shard is locked, so it must not touch the same table."
    (let ([s (cht-shard-for t key)])
      (with-mutex (cht-shard-lock s)
        (let* ([table (cht-shard-table s)]
               [new (proc (hashtable-ref table key default))])
          (hashtable-set! table key new)
          new))))

  (define (concurrent-hash-table-size t)
    "Number of entries. Not a consistent snapshot if the table is in use."
    (let loop ([i 0] [total 0])
      (let ([shards (concurrent-hash-table-shards t)])
        (if (= i (vector-length shards))
          total
          (let ([s (vector-ref shards i)])
            (loop (+ i 1)
              (+ total
                (with-mutex (cht-shard-lock s)
                  (hashtable-size (cht-shard-table s))))))))))

  (define (concurrent-hash-table->alist t)
    "All entries as an association list, shard by shard."
    (let ([shards (concurrent-hash-table-shards t)])
      (let loop ([i 0] [acc '()])
        (if (= i (vector-length shards))
          acc
          (let* ([s (vector-ref shards i)]
                 [entries
                   (with-mutex (cht-shard-lock s)
                     (let-values ([(keys vals) (hashtable-entries (cht-shard-table s))])
                       (let collect ([j 0] [out '()])
                         (if (= j (vector-length keys))
                           out
                           (collect (+ j 1)
                             (cons (cons (vector-ref keys j) (vector-ref vals j))
                               out))))))])
            (loop (+ i 1) (append entries acc)))))))

  ;; Atomic cell. Without an atomics primitive in the runtime, every
  ;; operation takes the cell's mutex.
  (define-record-type (atomic make-atomic* atomic?)
    (fields
      lock
      (mutable value)))

  (define (make-atomic value)
    "A new atomic cell holding `value`."
    (make-atomic* (make-mutex) value))

  (define (atomic-ref a)
    "Current value of the atomic cell `a`."
    (with-mutex (atomic-lock a)
      (atomic-value a)))

  (define (atomic-set! a value)
    "Store `value` in `a`."
    (with-mutex (atomic-lock a)
      (atomic-value-set! a value)))

  (define (atomic-add! a n)
    "Add `n` to the numeric cell `a` and return the new value."
    (with-mutex (atomic-lock a)
      (let ([next (+ (atomic-value a) n)])
        (atomic-value-set! a next)
        next)))

  (define (atomic-cas! a expected new)
    "Store `new` in `a` if its current value is eqv? to `expected`. Returns
    #t if the store happened."
    (with-mutex (atomic-lock a)
      (and (eqv? (atomic-value a) expected)
        (begin
          (atomic-value-set! a new)
          #t))))

  ;; Counting semaphore.
  (define-record-type (semaphore make-semaphore* semaphore?)
    (fields
      lock
      cv
      (mutable count)))

  (define (make-semaphore n)
    "A semaphore with `n` permits available."
    (unless (and (integer? n) (>= n 0))
      (assertion-violation 'make-semaphore "expected a non-negative integer" n))
    (make-semaphore* (make-mutex) (make-condition-variable) n))

  (define (semaphore-acquire! s)
    "Take one permit, blocking until one is available."
    (with-mutex (semaphore-lock s)
      (let loop ()
        (when (= (semaphore-count s) 0)
          (condition-variable-wait (semaphore-cv s) (semaphore-lock s))
          (loop)))
      (semaphore-count-set! s (- (semaphore-count s) 1))))

  (define (semaphore-try-acquire! s)
    "Take one permit if one is available. Returns #t if it was taken."
    (with-mutex (semaphore-lock s)
      (and (> (semaphore-count s) 0)
        (begin
          (semaphore-count-set! s (- (semaphore-count s) 1))
          #t))))

  (define (semaphore-release! s)
    "Return one permit."
    (with-mutex (semaphore-lock s)
      (semaphore-count-set! s (+ (semaphore-count s) 1))
      (condition-variable-signal (semaphore-cv s))))

  (define (with-semaphore s thunk)
    "Run `thunk` while holding one permit of `s`."
    (dynamic-wind
      (lambda () (semaphore-acquire! s))
      thunk
      (lambda () (semaphore-release! s))))

  ;; Errgroup, as golang.org/x/sync/errgroup without a context. Threads
  ;; started with errgroup-go are waited for by errgroup-wait, which returns
  ;; the first condition raised by one of them (or #f).
  (define-record-type (errgroup make-errgroup* errgroup?)
    (fields
      wg
      lock
      (mutable error)))

  (define (make-errgroup)
    "A new error group with no threads."
    (make-errgroup* (make-wait-group) (make-mutex) #f))

  (define (errgroup-go g thunk)
    "Run `thunk` in a new thread as part of `g`. If it raises, the first
    raised condition is kept and reported by errgroup-wait. Returns the
    thread handle."
    (wait-group-add! (errgroup-wg g) 1)
    (go (lambda ()
          (guard (e [#t
                      (with-mutex (errgroup-lock g)
                        (unless (errgroup-error g)
                          (errgroup-error-set! g e)))])
            (thunk))
          (wait-group-done! (errgroup-wg g)))))

  (define (errgroup-wait g)
    "Wait for all threads started with errgroup-go. Returns the first
    raised condition, or #f if none raised."
    (wait-group-wait (errgroup-wg g))
    (with-mutex (errgroup-lock g)
      (errgroup-error g))))
