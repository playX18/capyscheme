(library (capy timer)
  (export
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
    &timeout-expired)
  (import (rnrs)
    (core thread)
    (capy channel)
    (only (core primitives) current-second))

  ;; Timers and tickers send the current time, in seconds as a flonum, on their
  ;; channel, which has room for one value. A value that nobody picks up is
  ;; dropped, so a slow reader never stalls a ticker.
  ;;
  ;; A timer's thread sleeps for the full duration. Stopping or resetting
  ;; a timer bumps its generation, so a sleeper that is woken late does
  ;; nothing, and cancels the thread so that it does not linger. Timer threads
  ;; are detached: a scope neither waits for them nor cancels them.

  (define-condition-type &timeout-expired &violation
    make-timeout-expired
    timeout-expired?)

  (define (check-seconds who secs)
    (unless (and (real? secs) (>= secs 0))
      (assertion-violation who "expected a non-negative number of seconds" secs)))

  (define-record-type (timer make-timer* timer?)
    (fields
      lock
      chan
      (mutable active?)
      (mutable generation)
      (mutable thread)))

  (define (timer-channel t)
    "The channel on which the timer fires."
    (timer-chan t))

  (define (start-timer! t secs)
    (let ([gen (+ 1 (timer-generation t))])
      (timer-generation-set! t gen)
      (timer-active?-set! t #t)
      (timer-thread-set! t
        (go-detached
          (lambda ()
            (thread-sleep secs)
            (with-mutex (timer-lock t)
              (when (and (timer-active? t) (= gen (timer-generation t)))
                (timer-active?-set! t #f)
                (channel-try-send (timer-chan t) (current-second)))))))))

  ;; The timer's thread, to cancel once the lock is released. Call it with the
  ;; lock held, before starting or abandoning the timer.
  (define (take-thread! t)
    (let ([th (timer-thread t)])
      (timer-thread-set! t #f)
      th))

  (define (cancel-thread! th)
    (when th (thread-cancel! th)))

  (define (make-timer secs)
    "A timer that fires once, on its channel, after `secs` seconds."
    (check-seconds 'make-timer secs)
    (let ([t (make-timer* (make-mutex) (make-channel 1) #f 0 #f)])
      (with-mutex (timer-lock t)
        (start-timer! t secs))
      t))

  (define (timer-reset! t secs)
    "Restart the timer to fire after `secs` seconds from now. Returns #t if
    the timer was still pending."
    (check-seconds 'timer-reset! secs)
    (let-values ([(was-active? old)
                   (with-mutex (timer-lock t)
                     (let ([was-active? (timer-active? t)]
                           [old (take-thread! t)])
                       (start-timer! t secs)
                       (values was-active? old)))])
      (cancel-thread! old)
      was-active?))

  (define (timer-stop! t)
    "Stop the timer so it does not fire. Returns #t if it was pending."
    (let-values ([(was-active? old)
                   (with-mutex (timer-lock t)
                     (let ([was-active? (timer-active? t)])
                       (timer-active?-set! t #f)
                       (timer-generation-set! t (+ 1 (timer-generation t)))
                       (values was-active? (take-thread! t))))])
      (cancel-thread! old)
      was-active?))

  (define (after secs)
    "A channel that receives the current time once, after `secs` seconds."
    (timer-channel (make-timer secs)))

  (define-record-type (ticker make-ticker* ticker?)
    (fields
      chan
      (mutable stopped?)
      (mutable thread)))

  (define (ticker-channel k)
    "The channel on which the ticker sends the time every period."
    (ticker-chan k))

  (define (tick secs)
    "A ticker that sends the current time on its channel every `secs`
    seconds until ticker-stop! is called. Ticks are dropped while nobody is
    receiving."
    (check-seconds 'tick secs)
    (let ([k (make-ticker* (make-channel 1) #f #f)])
      (ticker-thread-set! k
        (go-detached
          (lambda ()
            (let loop ()
              (thread-sleep secs)
              (unless (ticker-stopped? k)
                (channel-try-send (ticker-chan k) (current-second))
                (loop))))))
      k))

  (define (ticker-stop! k)
    "Stop the ticker and its thread."
    (ticker-stopped?-set! k #t)
    (let ([th (ticker-thread k)])
      (when (and th (not (eq? th (current-thread))))
        (thread-cancel! th))))

  (define (call-with-timeout secs thunk)
    "Call `thunk` in a new thread and return its values if it finishes
    within `secs` seconds, otherwise raises a &timeout-expired condition.
    The thread is not killed, so it runs to completion and its
    result is discarded. For cooperative cancellation use (capy context).
    Dynamic state (parameters) of the caller is not seen by `thunk`."
    (check-seconds 'call-with-timeout secs)
    (let ([result (make-channel 1)]
          [timer (make-timer secs)])
      (go (lambda ()
            (guard (e [#t (channel-send result (cons 'error e))])
              (call-with-values thunk
                (lambda vals
                  (channel-send result (cons 'ok vals)))))))
      (let-values ([(index value ok?)
                    (channel-select
                      (vector
                        (vector 'recv result #f)
                        (vector 'recv (timer-channel timer) #f))
                      #f)])
        (timer-stop! timer)
        (cond
          [(= index 0)
            (case (car value)
              [(ok) (apply values (cdr value))]
              [else (raise (cdr value))])]
          [else
            (raise (make-timeout-expired))]))))
  )
