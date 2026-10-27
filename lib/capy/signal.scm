(library (capy signal)
  (export
    signal-notify
    signal-stop
    signal-ignore
    signal-reset
    signal-number
    signal-name)
  (import (rnrs)
    (core thread)
    (capy channel)
    (rename (capy)
      (signal/fd raw-signal-fd)
      (signal/install! raw-install!)
      (signal/uninstall! raw-uninstall!)
      (signal/number raw-signal-number)))

  ;; Signals reach Scheme through one self-pipe: the C handler writes the
  ;; signal number as a byte, and a single dispatcher thread reads the pipe
  ;; and forwards the signal's name to every channel subscribed to it. The
  ;; handler does nothing else, so it stays async-signal-safe, and waiting on
  ;; the pipe parks only the dispatcher.
  ;;
  ;; Delivery never blocks: a full channel drops the signal, as in Go's
  ;; os/signal, so give a channel room for the signals you can't miss.

  ;; Order matches the table in signal/number.
  (define names '(hup int quit term winch usr1 usr2 alarm chld pipe cont))

  (define lock (make-mutex))
  ;; ((signo . (channel ...)) ...)
  (define subscriptions '())
  ;; Signals whose default action is suppressed by signal-ignore.
  (define ignored '())
  (define dispatcher-started? #f)

  (define (index-of name)
    (let loop ([names names] [i 0])
      (cond
        [(null? names) #f]
        [(eq? (car names) name) i]
        [else (loop (cdr names) (+ i 1))])))

  (define (signal-number sig)
    "The platform's number for `sig`, a symbol such as 'winch or an integer."
    (cond
      [(and (symbol? sig) (index-of sig)) => raw-signal-number]
      [(and (integer? sig) (exact? sig) (> sig 0)) sig]
      [else (assertion-violation 'signal-number "unknown signal" sig)]))

  (define (signal-name signo)
    "The symbol for `signo`, or the number itself if it has no name."
    (let loop ([i 0] [ns names])
      (cond
        [(null? ns) signo]
        [(eqv? (raw-signal-number i) signo) (car ns)]
        [else (loop (+ i 1) (cdr ns))])))

  (define (channels-for signo)
    (cond [(assv signo subscriptions) => cdr] [else '()]))

  (define (install! signo)
    (unless (raw-install! signo)
      (assertion-violation 'signal-notify "signal cannot be caught" signo)))

  (define (dispatch! signo)
    (let ([name (signal-name signo)])
      (for-each (lambda (ch) (channel-try-send ch name))
        (with-mutex lock (channels-for signo)))))

  (define (run-dispatcher port)
    (let loop ()
      (let ([bytes (get-bytevector-some port)])
        (unless (eof-object? bytes)
          (let scan ([i 0])
            (when (< i (bytevector-length bytes))
              (dispatch! (bytevector-u8-ref bytes i))
              (scan (+ i 1))))
          (loop)))))

  (define (ensure-dispatcher!)
    (unless dispatcher-started?
      (set! dispatcher-started? #t)
      (let ([port (open-binary-fd-input-port "signals" (raw-signal-fd) 'none)])
        (go (lambda () (run-dispatcher port))))))

  (define (signal-notify ch . sigs)
    "Send each of `sigs` (names or numbers) to `ch` when it is delivered."
    (let ([signos (map signal-number sigs)])
      (with-mutex lock
        (for-each
          (lambda (signo)
            (unless (memq ch (channels-for signo))
              (when (null? (channels-for signo))
                (install! signo))
              (let ([entry (assv signo subscriptions)])
                (if entry
                  (set-cdr! entry (cons ch (cdr entry)))
                  (set! subscriptions (cons (list signo ch) subscriptions))))))
          signos)
        (ensure-dispatcher!))
      ch))

  (define (signal-stop ch)
    "Stop sending signals to `ch`. A signal nobody listens for gets its
    previous action back."
    (with-mutex lock
      (set! subscriptions
        (filter
          (lambda (entry)
            (let ([rest (remq ch (cdr entry))])
              (cond
                [(null? rest) (unless (memv (car entry) ignored) (raw-uninstall! (car entry))) #f]
                [else (set-cdr! entry rest) #t])))
          subscriptions))))

  (define (signal-ignore sig)
    "Swallow `sig`: it is caught but nothing happens."
    (let ([signo (signal-number sig)])
      (with-mutex lock
        (unless (memv signo ignored)
          (when (null? (channels-for signo)) (install! signo))
          (set! ignored (cons signo ignored))
          (ensure-dispatcher!)))))

  (define (signal-reset sig)
    "Undo signal-ignore, restoring the default action if nobody subscribes."
    (let ([signo (signal-number sig)])
      (with-mutex lock
        (when (memv signo ignored)
          (set! ignored (remv signo ignored))
          (when (null? (channels-for signo)) (raw-uninstall! signo)))))))
