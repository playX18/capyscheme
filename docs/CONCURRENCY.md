# Concurrency

Capy has a thread system similar to Go's. It maps M virtual threads to N OS
threads. Preemption is automatic. It happens every 10ms by default, and on I/O and other blocking calls.

Most concurrent Go code ports to Capy Scheme easily. See the cheat sheet at the end.


## Threads

```scheme
(import (rnrs) (capy concurrency))

(define g (go (lambda () (display "in a thread\n") 42)))
(thread-join g)             ; wait for it, returns its values (42)
(sleep 0.1)                 ; seconds, fractions allowed
(yield)                     ; let other threads run
```

`go` takes a thunk and returns a thread handle. The main program does not
wait for threads when it exits, so join them. `thread-join` waits and
returns the thunk's values.

If the thread raised, `thread-join` raises `&uncaught-exception`.
`uncaught-exception-reason` gives the original condition. A thread that
is never joined raises nothing, because its outcome is stored in the handle and
lost with it. Join every `go`, use an error group (below), or
spawn inside a scope that joins for you:

```scheme
(import (rnrs) (capy concurrency))

(with-thread-scope
  (go (lambda () (display "worker one\n")))
  (go (lambda () (display "worker two\n"))))
;; both workers have finished here
```

## Structured concurrency

`with-thread-scope` runs its body, then waits for every thread started with
`go` inside it. It does not return until they are all done, whichever way the
body ends: normally, by an exception, or by a continuation that escapes. A scope
that finishes returns the body's values.

A scope is **fail-fast**. When a child raises, the scope:

1. cancels the other children, which unwind and run their `dynamic-wind`
   thunks,
2. interrupts the body, if it is still running, so that a body blocked on
   something a dead child would have provided wakes up, and
3. raises the first child's failure as `&uncaught-exception` once everything
   has stopped.

```scheme
(guard (e [(uncaught-exception? e)
            (display "failed: ")
            (display (condition-message (uncaught-exception-reason e)))
            (newline)])
  (with-thread-scope
    (go (lambda ()
          (dynamic-wind
            (lambda () #f)
            (lambda () (sleep 30))
            (lambda () (display "sibling cancelled\n")))))
    (go (lambda () (sleep 0.05) (error 'job "boom")))
    (sleep 30)))                    ; interrupted as soon as the second child fails
```

If the body itself raises, its children are cancelled in the same way. Scopes
nest. Cancelling a scope cancels the scopes its children opened, so a
cancellation reaches every descendant.

| Form | What it does |
|------|--------------|
| `(with-thread-scope body ...)` | Fail-fast scope, as above. |
| `(with-thread-supervisor body ...)` | Waits for every child, whatever any of them does. A child's failure does not cancel its siblings. The first failure is raised at the end. |
| `(with-thread-deadline secs body ...)` | A fail-fast scope that is cancelled after `secs` seconds. Raises `&deadline-exceeded` (see `deadline-exceeded?` and `deadline-exceeded-seconds`). |
| `(current-thread-scope)` | The innermost scope the calling thread runs in, or `#f`. |
| `(thread-scope-cancel!)` | Cancel the current scope: its children are stopped and the body ends. Takes an optional scope. |
| `(go-in scope thunk)` | Start a thread that belongs to `scope`, from any thread. |
| `(go-detached thunk)` | Start a thread that belongs to no scope. For helpers that a library owns and stops itself. |

Each of these also has a procedure form taking a thunk: `call-with-thread-scope`,
`call-with-thread-supervisor` and `call-with-thread-deadline`.

```scheme
(guard (e [(deadline-exceeded? e) 'too-slow])
  (with-thread-deadline 0.5
    (go (lambda () (sleep 30)))     ; cancelled when the deadline passes
    (sleep 30)))                    ; interrupted too
```


## Cancelling a thread

`(thread-cancel! t)` asks a thread to stop. It does not wait: join the thread to
wait. The thread unwinds from wherever it is, including from `sleep`, a mutex
or condition wait, a join, I/O, and a busy loop, and runs the after-thunks of
its `dynamic-wind`s on the way out. `thread-join` on a cancelled thread raises
`&terminated-thread-exception`.

```scheme
(define t (go (lambda ()
                (dynamic-wind
                  (lambda () #f)
                  (lambda () (sleep 30))
                  (lambda () (display "cleaned up\n"))))))
(sleep 0.1)
(thread-cancel! t)                  ; => #t
(guard (e [(terminated-thread-exception? e) 'cancelled])
  (thread-join t))                  ; prints "cleaned up", then => cancelled
```

`thread-cancel!` returns `#f` when there is nothing to cancel: the thread has
not started, has finished, or is already being cancelled. A mutex held by a
cancelled thread is released, and the next `mutex-acquire` of it raises
`&abandoned-mutex-exception`.

`thread-interrupt!` runs a thunk in another thread at its next blocking call or
yield point, without stopping it. If the thunk raises, the exception is raised
inside the target, where a `guard` there can catch it. This is how a scope wakes
its body.


## Looking at running threads

`(import (capy threads))` shows what every thread is doing and why it is stuck.

```scheme
(import (rnrs) (capy concurrency) (capy threads))

(define a (make-mutex))
(define b (make-mutex))
(go (lambda () (set-thread-name! "alice") (mutex-acquire a) (sleep 0.1) (mutex-acquire b)))
(go (lambda () (set-thread-name! "bob")   (mutex-acquire b) (sleep 0.1) (mutex-acquire a)))
(sleep 0.5)
(dump-threads)
```

prints something like:

```
3 threads, 2 blocked
  #1    running    [worker 0, level 0, age 512ms]
  #2    blocked   alice on mutex 0x6000..., held by #3 (bob), for 400ms  [worker 2, level 0, parent #1, age 511ms]
  #3    blocked   bob on mutex 0x6000..., held by #2 (alice), for 400ms  [worker 5, level 0, parent #1, age 511ms]
deadlock among 2 threads: #2 (alice) -> #3 (bob) -> #2 (alice)
```

| Procedure | Result |
|-----------|--------|
| `(thread-list)` | A list of `thread-info` records, one per live thread, ordered by id. |
| `(thread-info-id i)`, `-parent`, `-name`, `-state`, `-level`, `-base-level`, `-worker`, `-pinned`, `-age`, `-killed?` | Fields of a `thread-info`. `state` is `runnable`, `running`, `blocked` or `done`. |
| `(thread-info-wait i)` | What a blocked thread waits for: `mutex`, `condition`, `join`, `sleep`, `io`, `park`, or `#f`. |
| `(thread-info-wait-on i)` | The id of the thread that holds the mutex or is being joined, the fd for `io`, otherwise `#f`. |
| `(thread-info-waiting i)` | Seconds the current wait has lasted. |
| `(find-deadlocks)` | A list of cycles, each a list of thread ids: threads that wait for each other through mutexes and joins. Condition waits never form a cycle, since any thread may signal one. |
| `(dump-threads [port])` | The text report above. |
| `(print-thread-tree [port])` | Threads indented under the thread that started them. |
| `(thread-backtrace thread [timeout])` | The thread's call stack, innermost frame first, or `#f` if it does not answer within `timeout` seconds. It interrupts the thread to read its own stack, so a blocked thread wakes briefly and goes back to waiting. |
| `(set-thread-name! name)` | Name the calling thread for dumps. |
| `(call-with-thread-trace path [interval] thunk)` | Run `thunk` while recording, then write a trace to `path`. |
| `(start-thread-trace [interval])`, `(stop-thread-trace! tr)`, `(write-thread-trace tr port-or-path)` | The same in steps. |

### Traces for Perfetto

A trace samples every thread's state every `interval` seconds (5ms by default)
and writes each stretch of one state as a span, in Chrome Trace Event JSON.
Open the file at <https://ui.perfetto.dev> or in `chrome://tracing`: each thread
is a row, and spans read `running`, `runnable`, `blocked on mutex` and so on.
A span is only as sharp as the interval, so a state shorter than that can be
missed.

```scheme
(call-with-thread-trace "trace.json"
  (lambda () (run-my-program)))
```


## Channels

A channel carries values between threads. `(make-channel n)` makes a
channel with room for `n` values. With `0`, a send waits until a receiver takes
the value.

| Procedure | Behavior |
|-----------|----------|
| `(channel-send ch v)` | Block until delivered. Raises a `closed-channel-violation` if `ch` is closed. |
| `(channel-recv ch)` | Block for a value. Returns `#f` once closed and drained, which is ambiguous when `#f` was sent. |
| `(channel-recv/ok ch)` | Two values: the value and `#t`, or `#f` and `#f` when closed and drained. |
| `(channel-try-send ch v)` | Returns `'sent`, `'would-block`, or `'closed`. Never blocks. |
| `(channel-try-recv ch)` | Returns two values: `'value`, `'empty` or `'closed`, and the value. Never blocks. |
| `(channel-recv/timeout ch secs)` | Three values: value, `#t`, `#f` on success; `#f`, `#f`, `#f` when closed and drained; `#f`, `#f`, `#t` on timeout. The third value tells timeout from close. |
| `(channel-close ch)` | Close. Pending values can still be received. |
| `(channel-closed? ch)`, `(channel-length ch)`, `(channel-capacity ch)` | Inspection. |

Closing tells a receiver that no more values are coming. Only the
sender should close:

```scheme
(import (rnrs) (capy concurrency))

(define jobs (make-channel 4))

(go (lambda ()
      (for-each (lambda (n) (channel-send jobs n)) '(1 2 3))
      (channel-close jobs)))

(let loop ([total 0])
  (let-values ([(n ok?) (channel-recv/ok jobs)])
    (if ok?
      (loop (+ total n))
      (begin (display total) (newline)))))
```

Output: `6`.

## Select

`select` waits on several channels and runs the body of whichever is ready first.
When several are ready, one is chosen at random.

```text
(select
  ((recv ch) => (lambda (v ok) ...))   ; receive; ok is #f when ch is closed
  ((recv ch) body ...)                 ; receive and ignore the value
  ((send ch v) body ...)               ; send v
  ((timeout secs) body ...)            ; nothing else ready within secs
  (default body ...))                  ; nothing ready right now; does not block
```

With no `default` and no ready case, the thread blocks until one is ready.
Channel and value expressions are evaluated once, when the `select` is entered.

Example:

```scheme
(import (rnrs) (capy concurrency))

(define ch (make-channel 1))
(go (lambda () (sleep 0.3) (channel-send ch 'late)))

(select
  ((recv ch) (display "early\n"))
  ((timeout 0.05) (display "timed out\n")))

(select
  ((recv ch) => (lambda (v ok) (display v) (newline)))
  ((timeout 2) (display "gave up\n")))

(select
  ((recv ch) (display "unexpected\n"))
  (default (display "nothing ready\n")))
```

Output:

```
timed out
late
nothing ready
```

## Wait groups, once, locks and atomics

`(capy sync)` provides Go-like synchronization with almost the same API:

| Go | Capy |
|----|------|
| `var wg sync.WaitGroup` | `(make-wait-group)` |
| `wg.Add(n)` | `(wait-group-add! wg n)` |
| `wg.Done()` | `(wait-group-done! wg)` |
| `wg.Wait()` | `(wait-group-wait wg)` |
| `wg.Add(1); go func() { defer wg.Done(); ... }()` | `(go/wg wg (lambda () ...))` |
| `sync.Once` / `once.Do(f)` | `(make-once)` / `(once-do o thunk)` |
| `sync.RWMutex` | `(make-rw-mutex)`, `rw-read-lock!`, `rw-read-unlock!`, `rw-write-lock!`, `rw-write-unlock!`, `with-read-lock`, `with-write-lock` |
| `sync.Map` | `(make-concurrent-hash-table)`, `concurrent-hash-table-get`, `-set!`, `-delete!`, `-update!`, `-size`, `->alist` |
| `atomic.Int64` | `(make-atomic v)`, `atomic-ref`, `atomic-set!`, `atomic-add!`, `atomic-cas!` |
| `make(chan struct{}, n)` as a semaphore | `(make-semaphore n)`, `semaphore-acquire!`, `semaphore-release!`, `with-semaphore` |
| `errgroup.Group` | `(make-errgroup)`, `errgroup-go`, `errgroup-wait` |

Example:

```scheme
(import (rnrs) (capy concurrency))

(define total (make-atomic 0))
(define wg (make-wait-group))

(do ([i 1 (+ i 1)]) ((> i 100))
  (let ([n i])
    (go/wg wg (lambda () (atomic-add! total n)))))

(wait-group-wait wg)
(display (atomic-ref total)) (newline)
```

Output: `5050`.

`errgroup-go` runs a thunk in a thread that belongs to the group.
`errgroup-wait` waits for all of them and returns the first condition any of
them raised, or `#f`:

```scheme
(import (rnrs) (capy concurrency))

(define g (make-errgroup))
(errgroup-go g (lambda () (display "ok task\n")))
(errgroup-go g (lambda () (error 'task "failed")))
(let ([err (errgroup-wait g)])
  (display (if err "one task failed\n" "all tasks succeeded\n")))
```

## Context

A context carries a cancellation signal, an optional deadline and key/value
pairs down a call tree. Create a root with `background-context`, then derive
from it. The derived constructors return two values: the new context and a
procedure that cancels it.

| Procedure | Result |
|-----------|--------|
| `(with-cancel parent)` | `(values ctx cancel)`. `(cancel)` cancels with `'canceled`. |
| `(with-timeout parent secs)` | `(values ctx cancel)`, canceled with `'deadline-exceeded` after `secs`. |
| `(with-deadline parent t)` | Like `with-timeout`, at an absolute time in seconds (`current-second`). |
| `(with-value parent key v)` | A context carrying `v` under `key`, compared with `equal?`. Returns one context. |
| `(context-done ctx)` | A channel that is closed when the context is cancelled. Use it in `select`. |
| `(context-err ctx)` | `#f` while live, else `'canceled` or `'deadline-exceeded`. |
| `(context-value ctx key)` | The value bound to `key` by the nearest `with-value`, or `#f`. |

Example:

```scheme
(import (rnrs) (capy concurrency))

(define (worker ctx out)
  (let loop ([n 0])
    (select
      ((recv (context-done ctx))
        (channel-send out n))
      ((timeout 0.01)
        (loop (+ n 1))))))

(let-values ([(ctx cancel) (with-timeout background-context 0.1)])
  (let ([out (make-channel 1)])
    (go (lambda () (worker ctx out)))
    (let ([n (channel-recv out)])
      (cancel)
      (display (if (> n 0) "worker ran\n" "worker did not run\n"))
      (display (context-err ctx)) (newline))))
```

Output:

```
worker ran
deadline-exceeded
```

## Timers and tickers

```text
(after secs)                 ; channel that receives the current time once
(make-timer secs)            ; a timer record; (timer-channel t) is its channel
(timer-reset! t secs)        ; restart it; #t if it was still pending
(timer-stop! t)              ; stop it; #t if it was still pending
(tick secs)                  ; ticker: sends the time every secs seconds
(ticker-channel k)
(ticker-stop! k)
```

Example:

```scheme
(import (rnrs) (capy concurrency))

(define k (tick 0.1))
(define stop (after 0.35))
(define beats 0)

(let loop ()
  (select
    ((recv (ticker-channel k)) (set! beats (+ beats 1)) (loop))
    ((recv stop) (ticker-stop! k))))

(display (if (>= beats 2) "ticked\n" "too few ticks\n"))
```


## Pools and pipelines

`(capy pool)` runs jobs on a fixed number of workers.

```scheme
(import (rnrs) (capy concurrency))

(define pool (make-worker-pool 4))
(define futures
  (map (lambda (n) (pool-submit! pool (lambda () (* n n)))) '(1 2 3 4)))
(display (map pool-future-get futures)) (newline)
(pool-shutdown! pool)
```

Output: `(1 4 9 16)`.

- `pool-submit!` blocks while every worker is busy, then returns a future.
- `pool-future-get` waits for a future.
- `pool-future-done?` checks whether a future has completed.
- `pool-shutdown!` shuts down the pool and stops it accepting new futures.

`parallel-map` processes a list in parallel:

```scheme
(import (rnrs) (capy concurrency))

(display (parallel-map (lambda (n) (* n n)) '(1 2 3 4) 2)) (newline)
```


## Go to Capy cheat sheet

| Go | Capy |
|----|------|
| `go f(x)` | `(go (lambda () (f x)))` |
| `runtime.Gosched()` | `(yield)` |
| `time.Sleep(d)` | `(sleep secs)` |
| `ch := make(chan T)` | `(make-channel 0)` |
| `ch := make(chan T, n)` | `(make-channel n)` |
| `ch <- v` | `(channel-send ch v)` |
| `v := <-ch` | `(channel-recv ch)` |
| `v, ok := <-ch` | `(let-values ([(v ok) (channel-recv/ok ch)]) ...)` |
| `close(ch)` | `(channel-close ch)` |
| `len(ch)`, `cap(ch)` | `(channel-length ch)`, `(channel-capacity ch)` |
| `select { case v := <-ch: ... case <-time.After(d): ... default: ... }` | `(select ((recv ch) => (lambda (v ok) ...)) ((timeout d) ...) (default ...))` |
| `time.After(d)` | `(after d)` |
| `time.NewTimer(d)`, `t.Stop()`, `t.Reset(d)` | `(make-timer d)`, `(timer-stop! t)`, `(timer-reset! t d)` |
| `time.NewTicker(d)`, `t.Stop()` | `(tick d)`, `(ticker-stop! k)` |
| `ctx, cancel := context.WithCancel(parent)` | `(let-values ([(ctx cancel) (with-cancel parent)]) ...)` |
| `context.WithTimeout(parent, d)` | `(let-values ([(ctx cancel) (with-timeout parent secs)]) ...)` |
| `context.WithDeadline(parent, t)` | `(with-deadline parent t)` |
| `context.WithValue(parent, k, v)` | `(with-value parent k v)` |
| `<-ctx.Done()` | `(channel-recv (context-done ctx))` in a `select` |
| `ctx.Err()` | `(context-err ctx)` |
| `ctx.Value(k)` | `(context-value ctx k)` |
| `var wg sync.WaitGroup` | `(make-wait-group)` |
| `wg.Add`, `wg.Done`, `wg.Wait` | `wait-group-add!`, `wait-group-done!`, `wait-group-wait` |
| `sync.Mutex` | `(make-mutex)`, `(with-mutex m thunk)` |
| `sync.Once` | `(make-once)`, `(once-do o thunk)` |
| `sync.RWMutex` | `(make-rw-mutex)`, `(with-read-lock rw thunk)`, `(with-write-lock rw thunk)` |
| `sync.Map` | `(make-concurrent-hash-table)` and the `concurrent-hash-table-*` procedures |
| `atomic.Int64` | `(make-atomic 0)`, `atomic-add!`, `atomic-ref`, `atomic-cas!` |
| `errgroup.Group` | `(make-errgroup)`, `(errgroup-go g thunk)`, `(errgroup-wait g)` |
| worker pool with a job channel | `(make-worker-pool n)`, `(pool-submit! p thunk)`, `(pool-future-get f)` |
| `context.WithTimeout` around a group of goroutines | `(with-thread-deadline secs body ...)` |
| `errgroup.WithContext` (cancel the rest on first error) | `(with-thread-scope body ...)` |
| a goroutine dump (`SIGQUIT`) | `(dump-threads)` |
| `errors.Is`, panics recovered in a goroutine | `(guard (e [pred ...]) ...)` around the thunk |

