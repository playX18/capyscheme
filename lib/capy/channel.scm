(library (capy channel)
  (export
    closed-channel-violation?
    channel-close
    channel-send
    channel-recv
    channel-recv/ok
    channel-try-send
    channel-try-recv
    channel-closed?
    channel-length
    channel-capacity
    channel-select
    make-channel)
  (import (rnrs)
    (capy deque)
    (core thread))

  (define-condition-type &closed-channel &violation
    make-closed-channel-violation
    closed-channel-violation?)

  (define (closed-channel-violation who msg)
    (let ((c (condition
              (make-closed-channel-violation)
              (make-message-condition msg))))
      (raise
        (if who
          (condition c (make-who-condition who))
          c))))

  (define-record-type sel
    (fields
      lock
      cv
      (mutable fired?)
      (mutable index)
      (mutable value)
      (mutable ok?)))

  (define (make-sel*)
    (make-sel (make-mutex) (make-condition-variable) #f #f #f #f))

  (define (sel-fire! s index value ok?)
    (with-mutex (sel-lock s)
      (and (not (sel-fired? s))
        (begin
          (sel-fired?-set! s #t)
          (sel-index-set! s index)
          (sel-value-set! s value)
          (sel-ok?-set! s ok?)
          (condition-variable-broadcast (sel-cv s))
          #t))))

  (define (sel-wait s)
    (with-mutex (sel-lock s)
      (let loop ()
        (unless (sel-fired? s)
          (condition-variable-wait (sel-cv s) (sel-lock s))
          (loop)))))

  (define-record-type waiter
    (fields
      sel
      index
      value
      queue
      (mutable prev)
      (mutable next)
      (mutable queued?)))

  (define-record-type wait-queue
    (fields
      (mutable head)
      (mutable tail)))

  (define (make-wait-queue*)
    (make-wait-queue #f #f))

  (define (queue-push! q w)
    (let ([tail (wait-queue-tail q)])
      (waiter-prev-set! w tail)
      (waiter-next-set! w #f)
      (if tail
        (waiter-next-set! tail w)
        (wait-queue-head-set! q w))
      (wait-queue-tail-set! q w)
      (waiter-queued?-set! w #t)))

  ;; Remove `w` from its queue if it is still linked. Callers hold the
  ;; channel lock that owns the queue.
  (define (queue-unlink! w)
    (when (waiter-queued? w)
      (let ([q (waiter-queue w)]
            [p (waiter-prev w)]
            [n (waiter-next w)])
        (if p
          (waiter-next-set! p n)
          (wait-queue-head-set! q n))
        (if n
          (waiter-prev-set! n p)
          (wait-queue-tail-set! q p))
        (waiter-queued?-set! w #f)
        (waiter-prev-set! w #f)
        (waiter-next-set! w #f))))

  ;; Pop waiters off the front of `q` until one can be claimed with
  ;; (value, ok?). Returns the claimed waiter, or #f if none is left.
  ;; A waiter whose sel was already completed elsewhere is dropped.
  (define (claim-first! q value ok?)
    (let loop ()
      (let ([w (wait-queue-head q)])
        (and w
          (begin
            (queue-unlink! w)
            (if (sel-fire! (waiter-sel w) (waiter-index w) value ok?)
              w
              (loop)))))))

  (define-record-type channel
    (fields
      id
      lock
      (mutable %closed?)
      buf
      senders
      receivers
      cap)
    (protocol
      (lambda (new)
        (lambda (cap)
          (new
            (next-channel-id!)
            (make-mutex)
            #f
            (make-deque cap)
            (make-wait-queue*)
            (make-wait-queue*)
            cap)))))

  (define id-lock (make-mutex))
  (define last-id 0)

  (define (next-channel-id!)
    (with-mutex id-lock
      (set! last-id (+ last-id 1))
      last-id))

  ;; The operations below require the channel's lock to be held.

  ;; Returns 'sent, 'would-block or 'closed.
  (define (try-send-locked ch v)
    (cond
      [(channel-%closed? ch) 'closed]
      [(claim-first! (channel-receivers ch) v #t) 'sent]
      [(< (deque-length (channel-buf ch)) (channel-cap ch))
        (deque-push-back! (channel-buf ch) v)
        'sent]
      [else 'would-block]))

  ;; Returns (values 'value v), (values 'empty #f) or (values 'closed #f).
  (define (try-recv-locked ch)
    (let ([buf (channel-buf ch)])
      (cond
        [(> (deque-length buf) 0)
          (let ([v (deque-pop-front! buf)])
            ;; A blocked sender can now move its value into the freed slot.
            (let ([w (claim-first! (channel-senders ch) #f #t)])
              (when w
                (deque-push-back! buf (waiter-value w))))
            (values 'value v))]
        [(claim-first! (channel-senders ch) #f #t)
          => (lambda (w) (values 'value (waiter-value w)))]
        [(channel-%closed? ch) (values 'closed #f)]
        [else (values 'empty #f)])))

  (define (lock-all! chans)
    (for-each
      (lambda (ch)
        (guard (c [(abandoned-mutex-exception? c) #t])
          (mutex-acquire (channel-lock ch))))
      chans))

  (define (unlock-all! chans)
    (for-each (lambda (ch) (mutex-release (channel-lock ch))) (reverse chans)))

  (define (with-locks chans thunk)
    (dynamic-wind
      (lambda () (lock-all! chans))
      thunk
      (lambda () (unlock-all! chans))))

  (define (case-channels cases)
    (let loop ([i 0] [acc '()])
      (if (= i (vector-length cases))
        (let sort-unique ([chs (sort-by-id acc)])
          (cond
            [(or (null? chs) (null? (cdr chs))) chs]
            [(eq? (car chs) (cadr chs)) (sort-unique (cdr chs))]
            [else (cons (car chs) (sort-unique (cdr chs)))]))
        (loop (+ i 1) (cons (vector-ref (vector-ref cases i) 1) acc)))))

  (define (sort-by-id chs)
    (sort-list-by-id (lambda (a b) (< (channel-id a) (channel-id b))) chs))

  (define (sort-list-by-id less lst)
    (cond
      [(or (null? lst) (null? (cdr lst))) lst]
      [else
        (let split ([xs lst] [a '()] [b '()])
          (if (null? xs)
            (merge-sorted (sort-list-by-id less a) (sort-list-by-id less b) less)
            (split (cdr xs) (cons (car xs) b) a)))]))

  (define (merge-sorted a b less)
    (cond
      [(null? a) b]
      [(null? b) a]
      [(less (car b) (car a)) (cons (car b) (merge-sorted a (cdr b) less))]
      [else (cons (car a) (merge-sorted (cdr a) b less))]))

  (define rng-state 2463534242)

  (define (random-below n)
    (set! rng-state
      (bitwise-and
        (+ (* rng-state 1103515245) 12345)
        #x7FFFFFFF))
    (mod (bitwise-arithmetic-shift-right rng-state 8) n))

  (define (shuffled-indices n)
    (let ([v (make-vector n 0)])
      (do ([i 0 (+ i 1)]) ((= i n)) (vector-set! v i i))
      (do ([i (- n 1) (- i 1)]) ((<= i 0) (vector->list v))
        (let* ([j (random-below (+ i 1))]
               [t (vector-ref v i)])
          (vector-set! v i (vector-ref v j))
          (vector-set! v j t)))))

  ;; Try each case once, under the locks. Returns (list index value ok?) for a
  ;; completed case, or #f when none is ready.
  (define (poll-cases cases)
    (let loop ([order (shuffled-indices (vector-length cases))])
      (if (null? order)
        #f
        (let* ([i (car order)]
               [c (vector-ref cases i)]
               [kind (vector-ref c 0)]
               [ch (vector-ref c 1)])
          (if (eq? kind (quote send))
            (case (try-send-locked ch (vector-ref c 2))
              [(sent) (list i #f #t)]
              [(closed) (closed-channel-violation 'select "send on closed channel")]
              [else (loop (cdr order))])
            (call-with-values
              (lambda () (try-recv-locked ch))
              (lambda (status v)
                (case status
                  [(value) (list i v #t)]
                  [(closed) (list i #f #f)]
                  [else (loop (cdr order))]))))))))

  ;; Queue one waiter per case. Returns the list of waiters.
  (define (enqueue-cases! cases s)
    (let loop ([i 0] [acc '()])
      (if (= i (vector-length cases))
        acc
        (let* ([c (vector-ref cases i)]
               [send? (eq? (vector-ref c 0) (quote send))]
               [ch (vector-ref c 1)]
               [q (if send? (channel-senders ch) (channel-receivers ch))]
               [w (make-waiter s i (and send? (vector-ref c 2)) q #f #f #f)])
          (queue-push! q w)
          (loop (+ i 1) (cons w acc))))))

  ;; Run the select over `cases` (a vector of #(kind channel value), kind being
  ;; 'send or 'recv). Returns (values index value ok?): the index of the case
  ;; that completed, the received value (for receives) and whether it
  ;; succeeded (a closed channel makes a receive return #f #f, and a send
  ;; on a channel that was closed while it waited raises). With `default?`
  ;; and no ready case, returns index -1.
  (define (channel-select cases default?)
    (let* ([chans (case-channels cases)]
           ;; Poll and enqueue under one lock session, so a counterpart that
           ;; arrives after the poll cannot miss us.
           [r (with-locks chans
                (lambda ()
                  (or (poll-cases cases)
                    (and (not default?)
                      (let* ([s (make-sel*)])
                        (cons s (enqueue-cases! cases s)))))))])
      (cond
        [(not r) (values -1 #f #f)]
        [(sel? (car r))
          (let ([s (car r)] [waiters (cdr r)])
            (dynamic-wind
              (lambda () #f)
              (lambda () (sel-wait s))
              (lambda ()
                (with-locks chans
                  (lambda () (for-each queue-unlink! waiters)))))
            (finish-case cases (sel-index s) (sel-value s) (sel-ok? s)))]
        [else (finish-case cases (car r) (cadr r) (caddr r))])))

  (define (finish-case cases i v ok?)
    (when (and (not ok?) (eq? (vector-ref (vector-ref cases i) 0) 'send))
      (closed-channel-violation 'select "send on closed channel"))
    (values i v ok?))

  (define (channel-close chan)
    "Close `chan`. Blocked receivers wake up with eof and blocked senders
    raise a closed-channel violation. Closing an already closed channel does
    nothing. Buffered values can still be received."
    (with-mutex (channel-lock chan)
      (unless (channel-%closed? chan)
        (channel-%closed?-set! chan #t)
        (let loop () (when (claim-first! (channel-receivers chan) #f #f) (loop)))
        (let loop () (when (claim-first! (channel-senders chan) #f #f) (loop))))))

  (define (channel-send chan v)
    "Send a value `v` to the channel `chan`.
    - Blocks until it can be delivered (buffer space or rendezvous).
    - Raises channel-closed-violation if the channel is closed."
    ;; Single-channel fast path of channel-select: no case vectors, and the
    ;; waiter is queued in the same lock session as the attempt.
    (let ([parked
            (with-mutex (channel-lock chan)
              (case (try-send-locked chan v)
                [(sent) #f]
                [(closed) (closed-channel-violation 'send "send on closed channel")]
                [else
                  (let* ([s (make-sel*)]
                         [w (make-waiter s 0 v (channel-senders chan) #f #f #f)])
                    (queue-push! (channel-senders chan) w)
                    (cons s w))]))])
      (when parked
        (park! chan (car parked) (cdr parked))
        (unless (sel-ok? (car parked))
          (closed-channel-violation 'send "send on closed channel")))
      (if #f #f)))

  (define (channel-recv/ok chan)
    "Receive from `chan`. Returns two values: the received value and #t, or
    #f and #f when the channel is closed and drained. Blocks while the channel
    is open and empty."
    (let ([r (with-mutex (channel-lock chan)
               (call-with-values
                 (lambda () (try-recv-locked chan))
                 (lambda (status v)
                   (case status
                     [(value) (list 'got v #t)]
                     [(closed) (list 'got #f #f)]
                     [else
                       (let* ([s (make-sel*)]
                              [w (make-waiter s 0 #f (channel-receivers chan) #f #f #f)])
                         (queue-push! (channel-receivers chan) w)
                         (list 'parked s w))]))))])
      (if (eq? (car r) 'got)
        (values (cadr r) (caddr r))
        (let ([s (cadr r)])
          (park! chan s (caddr r))
          (values (sel-value s) (sel-ok? s))))))

  ;; Wait on `s` for a waiter `w` queued on `chan`
  (define (park! chan s w)
    (let ([done? #f])
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (sel-wait s)
          (set! done? #t))
        (lambda ()
          (unless done?
            (with-mutex (channel-lock chan)
              (queue-unlink! w)))))))

  (define (channel-recv chan)
    "Receive a value from `chan`, blocking while it is open and empty.
    Returns #f once the channel is closed and drained, which cannot be told
    apart from a received #f; use `channel-recv/ok` to distinguish them."
    (let-values ([(v ok?) (channel-recv/ok chan)])
      v))

  (define (channel-try-send chan v)
    "Send `v` without blocking. Returns 'sent, 'would-block when the send
    would have to wait, or 'closed when the channel is closed. Does not raise."
    (with-mutex (channel-lock chan)
      (try-send-locked chan v)))

  (define (channel-try-recv chan)
    "Receive without blocking. Returns two values: a status and the value.
    The status is 'value (the value was received), 'empty (nothing is
    available yet) or 'closed (closed and drained; the value is #f)."
    (with-mutex (channel-lock chan)
      (try-recv-locked chan)))

  (define (channel-closed? chan)
    "Returns #t if the channel is closed, #f otherwise.

    Note: a closed channel may still have buffered values to receive."
    (with-mutex (channel-lock chan)
      (channel-%closed? chan)))

  (define (channel-length chan)
    "Number of values currently buffered in `chan`."
    (with-mutex (channel-lock chan)
      (deque-length (channel-buf chan))))

  (define (channel-capacity chan)
    "Buffer capacity of `chan`; zero for an unbuffered channel."
    (channel-cap chan)))
