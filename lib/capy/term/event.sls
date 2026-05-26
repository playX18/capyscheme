(library (capy term event)
  (export focus-gained-event focus-lost-event key-event mouse-event paste-event resize-event
          focus-gained-event? focus-lost-event? key-event? mouse-event? paste-event? resize-event?
          key-event-code key-event-modifiers key-event-kind key-event-state
          mouse-event-kind mouse-event-column mouse-event-row mouse-event-modifiers
          resize-event-columns resize-event-rows
          key-press-event? key-release-event? key-repeat-event?
          event-key event-mouse event-paste event-resize
          function-key? key-char media-key? modifier-key?
          enable-mouse-capture disable-mouse-capture
          enable-focus-change disable-focus-change
          enable-bracketed-paste disable-bracketed-paste
          push-keyboard-enhancement-flags pop-keyboard-enhancement-flags
          poll-event read-event try-read-event)
  (import (rnrs)
          (rename (capy)
            (term/open-tty raw-open-tty)
            (term/read-fd raw-read-fd)
            (term/nonblocking! raw-nonblocking!)
            (term/raw-mode-enabled? raw-mode-enabled?)
            (term/sigwinch-version raw-sigwinch-version)
            (term/install-sigwinch-handler! raw-install-sigwinch-handler!)
            (term/terminal-size-list raw-terminal-size-list)
            (syscall:pollinput raw-pollinput))
          (capy term private ansi)
          (capy term command)
          (capy term event-parser))

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
              "unknown keyboard enhancement flag" flag)]))

  (define (push-keyboard-enhancement-flags flags)
    (command 'push-keyboard-enhancement-flags
             (csi (string-append ">"
                                 (number->string (apply + (map enhancement-flag-bit flags)))
                                 "u"))))

  (define (pop-keyboard-enhancement-flags)
    (command 'pop-keyboard-enhancement-flags (csi "<u")))

  (define event-buffers '())
  (define default-event-fd #f)
  (define resize-handler-installed? #f)
  (define last-sigwinch-version #f)
  (define pending-resize-event #f)

  (define (ensure-resize-handler!)
    (unless resize-handler-installed?
      (raw-install-sigwinch-handler!)
      (set! last-sigwinch-version (raw-sigwinch-version))
      (set! resize-handler-installed? #t)))

  (define (read-terminal-resize-event)
    (let ([size (raw-terminal-size-list)])
      (resize-event (car size) (cadr size))))

  (define (update-resize-event!)
    (ensure-resize-handler!)
    (let ([version (raw-sigwinch-version)])
      (when (and last-sigwinch-version
                 (not (= version last-sigwinch-version))
                 (not pending-resize-event))
        (set! last-sigwinch-version version)
        (set! pending-resize-event (read-terminal-resize-event)))
      pending-resize-event))

  (define (take-resize-event!)
    (let ([event (update-resize-event!)])
      (when event
        (set! pending-resize-event #f))
      event))

  (define (source->fd source)
    (cond
      [(not source)
       (unless default-event-fd
         (set! default-event-fd (raw-open-tty #t #f))
         (raw-nonblocking! default-event-fd #t))
       default-event-fd]
      [(integer? source) source]
      [(port? source) (port-fileno source)]
      [else (assertion-violation 'source->fd "expected fd or port" source)]))

  (define (fd-buffer fd)
    (let ([cell (assv fd event-buffers)])
      (if cell (cdr cell) (make-bytevector 0))))

  (define (set-fd-buffer! fd bytes)
    (let ([cell (assv fd event-buffers)])
      (if cell
          (set-cdr! cell bytes)
          (set! event-buffers (cons (cons fd bytes) event-buffers)))))

  (define (bytevector-append a b)
    (let* ([a-len (bytevector-length a)]
           [b-len (bytevector-length b)]
           [out (make-bytevector (+ a-len b-len))])
      (bytevector-copy! a 0 out 0 a-len)
      (bytevector-copy! b 0 out a-len b-len)
      out))

  (define (bytevector-drop bytes count)
    (let* ([n (- (bytevector-length bytes) count)]
           [out (make-bytevector n)])
      (bytevector-copy! bytes count out 0 n)
      out))

  (define (fd-ready? fd timeout-ms)
    (let loop ([remaining timeout-ms])
      (cond
        [(update-resize-event!) #t]
        [(= (raw-pollinput fd) 1) #t]
        [(or (not remaining) (= remaining 0)) #f]
        [else (loop (- remaining 1))])))

  (define (read-available! fd)
    (let ([buf (make-bytevector 4096)])
      (let ([n (raw-read-fd fd buf 0 4096)])
        (if (> n 0)
            (let ([chunk (make-bytevector n)])
              (bytevector-copy! buf 0 chunk 0 n)
              (set-fd-buffer! fd (bytevector-append (fd-buffer fd) chunk))
              #t)
            #f))))

  (define (parse-buffer fd input-available?)
    (call-with-values
      (lambda () (parse-event (fd-buffer fd) input-available? (raw-mode-enabled?)))
      (lambda (status event consumed)
        (cond
          [(eq? status 'complete)
           (set-fd-buffer! fd (bytevector-drop (fd-buffer fd) consumed))
           event]
          [(eq? status 'invalid)
           (set-fd-buffer! fd (bytevector-drop (fd-buffer fd) consumed))
           #f]
          [else #f]))))

  (define (poll-event . args)
    (let* ([timeout-ms (if (null? args) 0 (car args))]
           [source (if (or (null? args) (null? (cdr args))) #f (cadr args))]
           [fd (source->fd source)])
      (or (and (update-resize-event!) #t)
          (and (> (bytevector-length (fd-buffer fd)) 0)
               (parse-buffer fd (fd-ready? fd 0))
               #t)
          (and (fd-ready? fd timeout-ms)
               (begin
                 (read-available! fd)
                 #t)))))

  (define (read-event . args)
    (let* ([source (if (null? args) #f (car args))]
           [fd (source->fd source)])
      (let loop ()
        (let ([event (or (take-resize-event!)
                         (and (> (bytevector-length (fd-buffer fd)) 0)
                              (parse-buffer fd (fd-ready? fd 0))))])
          (if event
              event
              (begin
                (unless (read-available! fd)
                  (fd-ready? fd #f)
                  (read-available! fd))
                (loop)))))))

  (define (try-read-event . args)
    (let* ([source (if (null? args) #f (car args))]
           [fd (source->fd source)])
      (or (take-resize-event!)
          (and (> (bytevector-length (fd-buffer fd)) 0)
               (parse-buffer fd (fd-ready? fd 0)))
          (and (fd-ready? fd 0)
               (begin
                 (read-available! fd)
                 (parse-buffer fd (fd-ready? fd 0))))))))
