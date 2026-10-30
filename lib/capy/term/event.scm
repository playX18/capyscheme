(library (capy term event)
  (export focus-gained-event focus-lost-event key-event mouse-event paste-event resize-event
    focus-gained-event?
    focus-lost-event?
    key-event?
    mouse-event?
    paste-event?
    resize-event?
    key-event-code
    key-event-modifiers
    key-event-kind
    key-event-state
    mouse-event-kind
    mouse-event-column
    mouse-event-row
    mouse-event-modifiers
    resize-event-columns
    resize-event-rows
    key-press-event?
    key-release-event?
    key-repeat-event?
    event-key
    event-mouse
    event-paste
    event-resize
    function-key?
    key-char
    media-key?
    modifier-key?
    enable-mouse-capture
    disable-mouse-capture
    enable-focus-change
    disable-focus-change
    enable-bracketed-paste
    disable-bracketed-paste
    push-keyboard-enhancement-flags
    pop-keyboard-enhancement-flags
    make-terminal
    terminal?
    terminal-events
    terminal-close!
    poll-event
    read-event
    try-read-event)
  (import (rnrs)
    (core thread)
    (capy channel)
    (capy select)
    (capy signal)
    (capy term private ansi)
    (capy term command)
    (capy term event-parser)
    (capy term terminal)
    (capy term tty))

  (define (focus-gained-event)
    '(focus-gained))

  (define (focus-lost-event)
    '(focus-lost))

  (define (key-event code modifiers kind state)
    (list 'key code modifiers kind state))

  (define (mouse-event kind column row modifiers)
    (list 'mouse kind column row modifiers))

  (define (paste-event text)
    (list 'paste text))

  (define (resize-event columns rows)
    (list 'resize columns rows))

  (define (tagged-event? event tag)
    (and (pair? event) (eq? (car event) tag)))

  (define (focus-gained-event? event) (equal? event '(focus-gained)))
  (define (focus-lost-event? event) (equal? event '(focus-lost)))
  (define (key-event? event) (tagged-event? event 'key))
  (define (mouse-event? event) (tagged-event? event 'mouse))
  (define (paste-event? event) (tagged-event? event 'paste))
  (define (resize-event? event) (tagged-event? event 'resize))

  (define (require-event who pred event)
    (unless (pred event)
      (assertion-violation who "unexpected terminal event" event)))

  (define (key-event-code event)
    (require-event 'key-event-code key-event? event)
    (cadr event))

  (define (key-event-modifiers event)
    (require-event 'key-event-modifiers key-event? event)
    (caddr event))

  (define (key-event-kind event)
    (require-event 'key-event-kind key-event? event)
    (cadddr event))

  (define (key-event-state event)
    (require-event 'key-event-state key-event? event)
    (car (cddddr event)))

  (define (mouse-event-kind event)
    (require-event 'mouse-event-kind mouse-event? event)
    (cadr event))

  (define (mouse-event-column event)
    (require-event 'mouse-event-column mouse-event? event)
    (caddr event))

  (define (mouse-event-row event)
    (require-event 'mouse-event-row mouse-event? event)
    (cadddr event))

  (define (mouse-event-modifiers event)
    (require-event 'mouse-event-modifiers mouse-event? event)
    (car (cddddr event)))

  (define (resize-event-columns event)
    (require-event 'resize-event-columns resize-event? event)
    (cadr event))

  (define (resize-event-rows event)
    (require-event 'resize-event-rows resize-event? event)
    (caddr event))

  (define (key-press-event? event)
    (and (key-event? event) (eq? (key-event-kind event) 'press)))

  (define (key-release-event? event)
    (and (key-event? event) (eq? (key-event-kind event) 'release)))

  (define (key-repeat-event? event)
    (and (key-event? event) (eq? (key-event-kind event) 'repeat)))

  (define (event-key event)
    (and (key-event? event) event))

  (define (event-mouse event)
    (and (mouse-event? event) event))

  (define (event-paste event)
    (and (paste-event? event) (cadr event)))

  (define (event-resize event)
    (and (resize-event? event) (list (cadr event) (caddr event))))

  (define (function-key? code)
    (and (pair? code) (eq? (car code) 'function)))

  (define (key-char code)
    (and (pair? code) (eq? (car code) 'char) (cadr code)))

  (define (media-key? code)
    (and (pair? code) (eq? (car code) 'media)))

  (define (modifier-key? code)
    (and (pair? code) (eq? (car code) 'modifier)))

  (define (command name body)
    (make-command name (lambda () body)))

  (define (enable-mouse-capture)
    (command 'enable-mouse-capture
      (string-append (csi "?1000h") (csi "?1002h") (csi "?1015h") (csi "?1006h"))))

  (define (disable-mouse-capture)
    (command 'disable-mouse-capture
      (string-append (csi "?1006l") (csi "?1015l") (csi "?1002l") (csi "?1000l"))))

  (define (enable-focus-change)
    (command 'enable-focus-change (csi "?1004h")))

  (define (disable-focus-change)
    (command 'disable-focus-change (csi "?1004l")))

  (define (enable-bracketed-paste)
    (command 'enable-bracketed-paste (csi "?2004h")))

  (define (disable-bracketed-paste)
    (command 'disable-bracketed-paste (csi "?2004l")))

  (define (enhancement-flag-bit flag)
    (case flag
      [(disambiguate-escape-codes) 1]
      [(report-event-types) 2]
      [(report-alternate-keys) 4]
      [(report-all-keys-as-escape-codes) 8]
      [(report-associated-text) 16]
      [else (assertion-violation 'push-keyboard-enhancement-flags
             "unknown keyboard enhancement flag"
             flag)]))

  (define (push-keyboard-enhancement-flags flags)
    (command 'push-keyboard-enhancement-flags
      (csi (string-append ">"
            (number->string (apply + (map enhancement-flag-bit flags)))
            "u"))))

  (define (pop-keyboard-enhancement-flags)
    (command 'pop-keyboard-enhancement-flags (csi "<u")))

;; A terminal turns raw input and window resizes into one stream of events
  ;; on a channel. Two threads feed it:
  ;;
  ;;   reader   blocks on the input port and forwards each chunk of bytes;
  ;;   decoder  buffers chunks, decodes them with `parse-event`, and sends
  ;;            events (a lone Escape is told from the start of a sequence
  ;;            by waiting `escape-timeout` seconds for more bytes);
  ;;   resizer  waits for SIGWINCH on a channel from (capy signal) and sends a
  ;;            `resize` event carrying the new size.
  ;;
  ;; Reading from the port parks only the thread that does it. Closing a
  ;; terminal stops delivery at once; the reader notices at its next read.
  (define-record-type (terminal make-terminal* terminal?)
    (fields events winch (mutable peeked) (mutable closed?)))

  (define event-capacity 64)
  (define escape-timeout 0.05)

  ;; End of input reads as Ctrl-D, which is what a line editor expects.
  (define eof-bytes (u8-list->bytevector '(4)))

  (define (bytevector-append a b)
    (let* ([a-len (bytevector-length a)]
           [b-len (bytevector-length b)]
           [out (make-bytevector (+ a-len b-len))])
      (bytevector-copy! a 0 out 0 a-len)
      (bytevector-copy! b 0 out a-len b-len)
      out))

  (define (bytevector-drop bytes count)
    (let* ([rest (- (bytevector-length bytes) count)]
           [out (make-bytevector rest)])
      (bytevector-copy! bytes count out 0 rest)
      out))

  ;; Sends EVENT unless the terminal is closed. Returns #f once it is.
  (define (emit! t event)
    (and (not (terminal-closed? t))
      (guard (c [(closed-channel-violation? c) #f])
        (channel-send (terminal-events t) event)
        #t)))

  (define (read-chunks! port chunks)
    (let loop ()
      (let ([chunk (get-bytevector-some port)])
        (cond
          [(eof-object? chunk)
            (channel-send chunks eof-bytes)
            (channel-close chunks)]
          [else
            (channel-send chunks chunk)
            (loop)]))))

  (define (decode! t chunks)
    (let loop ([buffer (make-bytevector 0)])
      (define (more)
        (let-values ([(chunk ok?) (channel-recv/ok chunks)])
          (and ok? (loop (bytevector-append buffer chunk)))))
      (define (lone-escape?)
        (and (= (bytevector-length buffer) 1) (= (bytevector-u8-ref buffer 0) #x1b)))
      (cond
        [(terminal-closed? t) #f]
        [(= (bytevector-length buffer) 0) (more)]
        [else
          (let-values ([(status event consumed) (parse-event buffer #t (raw-mode-enabled?))])
            (case status
              [(complete)
                (and (emit! t event) (loop (bytevector-drop buffer consumed)))]
              [(invalid) (loop (bytevector-drop buffer consumed))]
              [else
                (if (lone-escape?)
                  (select
                    [(recv chunks) =>
                      (lambda (chunk ok?)
                        (and ok? (loop (bytevector-append buffer chunk))))]
                    [(timeout escape-timeout)
                      (let-values ([(status event consumed)
                                    (parse-event buffer #f (raw-mode-enabled?))])
                        (if (eq? status 'complete)
                          (and (emit! t event)
                            (loop (bytevector-drop buffer consumed)))
                          ;; Nothing decodable after the timeout: discard the
                          ;; buffer so the loop makes progress.
                          (loop (make-bytevector 0))))])
                  (more))]))])))

  (define (watch-resizes! t)
    (let loop ()
      (let-values ([(signal ok?) (channel-recv/ok (terminal-winch t))])
        (when ok?
          ;; Without a controlling terminal there is no size to report.
          (let ([size (guard (c [#t #f])
                       (let-values ([(columns rows) (terminal-size)])
                         (resize-event columns rows)))])
            (when (or (not size) (emit! t size))
              (loop)))))))

  ;; (make-terminal [port]) starts delivering events from PORT, a binary input
  ;; port that defaults to the controlling terminal.
  (define (make-terminal . args)
    (let* ([port (if (null? args) (open-tty-input-port) (car args))]
           [chunks (make-channel 16)]
           [t (make-terminal* (make-channel event-capacity)
               (signal-notify (make-channel 1) 'winch)
               #f
               #f)])
      (go (lambda () (read-chunks! port chunks)))
      (go (lambda () (decode! t chunks)))
      (go (lambda () (watch-resizes! t)))
      t))

  (define (terminal-close! t)
    (unless (terminal-closed? t)
      (terminal-closed?-set! t #t)
      (signal-stop (terminal-winch t))
      (channel-close (terminal-winch t))
      (channel-close (terminal-events t))))

  (define default-terminal #f)
  (define default-lock (make-mutex))

  (define (terminal-for source)
    (cond
      [(terminal? source) source]
      [(not source)
        (with-mutex default-lock
          (unless (and default-terminal (not (terminal-closed? default-terminal)))
            (set! default-terminal (make-terminal)))
          default-terminal)]
      [else (assertion-violation 'read-event "expected a terminal" source)]))

  (define (optional args index default)
    (if (> (length args) index) (list-ref args index) default))

  ;; (read-event [terminal]) waits for the next event; #f once the terminal
  ;; is closed.
  (define (read-event . args)
    (let ([t (terminal-for (optional args 0 #f))])
      (cond
        [(terminal-peeked t) => (lambda (e) (terminal-peeked-set! t #f) e)]
        [else
          (let-values ([(event ok?) (channel-recv/ok (terminal-events t))])
            (and ok? event))])))

  ;; (try-read-event [terminal]) returns the next event or #f without waiting.
  (define (try-read-event . args)
    (let ([t (terminal-for (optional args 0 #f))])
      (cond
        [(terminal-peeked t) => (lambda (e) (terminal-peeked-set! t #f) e)]
        [else
          (let-values ([(status event) (channel-try-recv (terminal-events t))])
            (and (eq? status 'value) event))])))

  ;; (poll-event [timeout-ms [terminal]]) is #t when an event can be read
  ;; within TIMEOUT-MS, without consuming it.
  (define (poll-event . args)
    (let ([t (terminal-for (optional args 1 #f))]
          [timeout-ms (optional args 0 0)])
      (or (and (terminal-peeked t) #t)
        (let ([event
               (if (eqv? timeout-ms 0)
                 (try-read-event t)
                 (select
                   [(recv (terminal-events t)) => (lambda (e ok?) (and ok? e))]
                   [(timeout (/ timeout-ms 1000.0)) #f]))])
          (and event (begin (terminal-peeked-set! t event) #t)))))))
