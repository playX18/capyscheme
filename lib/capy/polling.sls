;; Polling API

(library (capy polling)
  (export
    make-poller
    poller-add!
    poller-add/mode!
    poller-modify!
    poller-modify/mode!
    poller-delete!
    poller-wait
    poller-notify
    make-event
    poll-event?
    event-readable?
    event-writable?
    event-hup?
    event-priority?
    event-error?
    event-key
    event-flags
    event-mode
    event-bits)
  (import
    (rename (capy)
      (poller-add! raw:poller-add!)
      (poller-add/mode! raw:poller-add/mode!)
      (poller-modify! raw:poller-modify!)
      (poller-modify/mode! raw:poller-modify/mode!)
      (poller-delete! raw:poller-delete!)
      (poller-wait raw:poller-wait))
    (scheme base)
    (rnrs enums))

  (define EREADABLE #x01)
  (define EWRITABLE #x02)
  (define EHUP #x04)
  (define EPRIORITY #x08)
  (define EERROR #x10)

  (define EONESHOT #x01)
  (define ELEVEL #x02)
  (define EEDGE #x04)
  (define EEDGEONESHOT #x08)

  (define-record-type <poll-event>
    (%make-event key bits)
    poll-event?
    (key event-key)
    (bits event-bits))

  (define (event-readable? event)
    (not (zero? (bitwise-and (event-bits event) EREADABLE))))
  (define (event-writable? event)
    (not (zero? (bitwise-and (event-bits event) EWRITABLE))))
  (define (event-hup? event)
    (not (zero? (bitwise-and (event-bits event) EHUP))))
  (define (event-priority? event)
    (not (zero? (bitwise-and (event-bits event) EPRIORITY))))
  (define (event-error? event)
    (not (zero? (bitwise-and (event-bits event) EERROR))))

  (define-enumeration event-flag
    (readable writable hup priority error)
    event-flags)

  (define-enumeration poll-mode
    (oneshot level edge edge+oneshot)
    poll-modes)

  (define (poll-mode->bits mode)
    (case mode
      ((oneshot) EONESHOT)
      ((level) ELEVEL)
      ((edge) EEDGE)
      ((edge+oneshot) EEDGEONESHOT)
      (else (error "Invalid poll mode" mode))))

  (define (make-event key flags)
    (define list (enum-set->list flags))
    (define flag-bits
      (+ (if (memv 'readable list) EREADABLE 0)
        (if (memv 'writable list) EWRITABLE 0)
        (if (memv 'hup list) EHUP 0)
        (if (memv 'priority list) EPRIORITY 0)
        (if (memv 'error list) EERROR 0)))

    (%make-event key flag-bits))

  (define (poller-add! poller input-source event)
    (define fileno (if (port? input-source)
                    (port-fileno input-source)
                    (if (fixnum? input-source)
                      input-source
                      (error "Invalid input source" input-source))))
    (raw:poller-add! poller (event-key event) fileno (event-bits event)))

  (define (poller-add/mode! poller input-source event mode)
    (define fileno (if (port? input-source)
                    (port-fileno input-source)
                    (if (fixnum? input-source)
                      input-source
                      (error "Invalid input source" input-source))))
    (raw:poller-add/mode! poller (event-key event) fileno (event-bits event) (poll-mode->bits mode)))

  (define (poller-modify! poller input-source event)
    (define fileno (if (port? input-source)
                    (port-fileno input-source)
                    (if (fixnum? input-source)
                      input-source
                      (error "Invalid input source" input-source))))
    (raw:poller-modify! poller fileno (event-bits event)))

  (define (poller-modify/mode! poller input-source event mode)
    (define fileno (if (port? input-source)
                    (port-fileno input-source)
                    (if (fixnum? input-source)
                      input-source
                      (error "Invalid input source" input-source))))
    (raw:poller-modify/mode! poller fileno (event-bits event) (poll-mode->bits mode)))

  (define (poller-delete! poller input-source)
    (define fileno (if (port? input-source)
                    (port-fileno input-source)
                    (if (fixnum? input-source)
                      input-source
                      (error "Invalid input source" input-source))))
    (raw:poller-delete! poller fileno))

  (define (poller-wait poller . timeout?)
    (define timeout (if (null? timeout?) #f (car timeout?)))
    (define events (cond
                    [timeout (raw:poller-wait poller timeout)]
                    [else (raw:poller-wait poller)]))
    (map
      (lambda (ev)
        (%make-event (car ev) (cdr ev)))
      events)))
