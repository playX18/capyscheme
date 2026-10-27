# Concurrency

Capy implements a concurrent thread system similar to that of Go. Capy maps M virtual threads to N underlying OS 
threads. Preemption is automatic and happens every 10ms by default, or on I/O and any other blocking calls. 

Generally speaking, most of concurrent Go code can be ported to Capy Scheme easily, see cheat sheet down below.


## Threads

```scheme
(import (rnrs) (capy concurrency))

(define g (go (lambda () (display "in a thread\n") 42)))
(thread-join g)             ; wait for it, returns its values (42)
(sleep 0.1)                 ; seconds, fractions allowed
(yield)                     ; let other threads run
```

`go` takes a thunk and returns a thread handle. The main program does not
wait for threads when it exits, so join them: `thread-join` waits and
returns the thunk's values.

If the thread raised, `thread-join` raises `&uncaught-exception`.
`uncaught-exception-reason` gives the original condition. A thread that
is never joined reports nothing -- its outcome is stored in the handle and
lost with it -- so either join every `go`, use an error group (below), or
spawn inside a scope that joins for you:

```scheme
(import (rnrs) (capy concurrency))

(with-thread-scope
  (go (lambda () (display "worker one\n")))
  (go (lambda () (display "worker two\n"))))
;; both workers have finished here. The first failure either of them
;; raised would be re-raised here as &uncaught-exception
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

Closing is the way to tell a receiver that no more values are coming. Only the
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

`(capy sync)` provide Go-like synchronization functionality with almost exactly same API:

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

Here is a small example:

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
| `(with-timeout parent secs)` | `(values ctx cancel)`, cancelled with `'deadline-exceeded` after `secs`. |
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
- `pool-future-get` can be used to wait for future.
- `pool-future-done?` checks if future is completed.
- `pool-shutdown!` shutdowns the pool and stops accepting new futures.

You can also simply call `parallel-map` to automatically schedule list to be processed in parallel:

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
| `errors.Is`, panics recovered in a goroutine | `(guard (e [pred ...]) ...)` around the thunk |

