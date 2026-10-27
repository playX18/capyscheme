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
  ;; a timer bumps its generation so the sleeper does nothing when it wakes.

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
      (mutable generation)))

  (define (timer-channel t)
    "The channel on which the timer fires."
    (timer-chan t))

  (define (start-timer! t secs)
    (let ([gen (+ 1 (timer-generation t))])
      (timer-generation-set! t gen)
      (timer-active?-set! t #t)
      (go (lambda ()
            (thread-sleep secs)
            (with-mutex (timer-lock t)
              (when (and (timer-active? t) (= gen (timer-generation t)))
                (timer-active?-set! t #f)
                (channel-try-send (timer-chan t) (current-second))))))))

  (define (make-timer secs)
    "A timer that fires once, on its channel, after `secs` seconds."
    (check-seconds 'make-timer secs)
    (let ([t (make-timer* (make-mutex) (make-channel 1) #f 0)])
      (with-mutex (timer-lock t)
        (start-timer! t secs))
      t))

  (define (timer-reset! t secs)
    "Restart the timer to fire after `secs` seconds from now. Returns #t if
    the timer was still pending."
    (check-seconds 'timer-reset! secs)
    (with-mutex (timer-lock t)
      (let ([was-active? (timer-active? t)])
        (start-timer! t secs)
        was-active?)))

  (define (timer-stop! t)
    "Stop the timer so it does not fire. Returns #t if it was pending."
    (with-mutex (timer-lock t)
      (let ([was-active? (timer-active? t)])
        (timer-active?-set! t #f)
        (timer-generation-set! t (+ 1 (timer-generation t)))
        was-active?)))

  (define (after secs)
    "A channel that receives the current time once, after `secs` seconds."
    (timer-channel (make-timer secs)))

  (define-record-type (ticker make-ticker* ticker?)
    (fields
      chan
      (mutable stopped?)))

  (define (ticker-channel k)
    "The channel on which the ticker sends the time every period."
    (ticker-chan k))

  (define (tick secs)
    "A ticker that sends the current time on its channel every `secs`
    seconds until ticker-stop! is called. Ticks are dropped while nobody is
    receiving."
    (check-seconds 'tick secs)
    (let ([k (make-ticker* (make-channel 1) #f)])
      (go (lambda ()
            (let loop ()
              (thread-sleep secs)
              (unless (ticker-stopped? k)
                (channel-try-send (ticker-chan k) (current-second))
                (loop)))))
      k))

  (define (ticker-stop! k)
    "Stop the ticker. Its thread exits after its current sleep."
    (ticker-stopped?-set! k #t))

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
