(library (capy pkg ui)
  (export make-ui
    make-quiet-ui
    ui-status!
    ui-note!
    ui-warning!
    ui-error-text
    ui-painter
    ui-port
    ui-message-format
    ui-message-format-set!
    ui-progress!
    ui-progress-done!
    driver-error?
    driver-error-message
    fail
    fail-reported)
  (import (rnrs)
    (only (capy) getenv)
    (capy term paint)
    (only (capy term tty) tty?)
    (only (capy term terminal) terminal-size))

  (define-record-type (ui %make-ui ui?)
    (fields painter port quiet? (mutable message-format) live? (mutable progress)))

  (define-record-type (driver-error make-driver-error driver-error?)
    (fields message))

  (define (fail message . irritants)
    (raise (make-driver-error
            (apply string-append message
              (map (lambda (x) (string-append " " (call-with-string-output-port (lambda (p) (display x p)))))
                irritants)))))

  (define (fail-reported)
    (raise (make-driver-error #f)))

  (define (live-terminal? port)
    (and (not (equal? (getenv "TERM") "dumb"))
      (guard (e [#t #f]) (tty? port))))

  (define (make-ui mode)
    (let ([port (current-error-port)])
      (%make-ui (make-painter mode port) port #f 'human (live-terminal? port) #f)))

  (define (make-quiet-ui mode)
    (let ([port (current-error-port)])
      (%make-ui (make-painter mode port) port #t 'human (live-terminal? port) #f)))

  (define (pad-left s n)
    (if (< (string-length s) n)
      (string-append (make-string (- n (string-length s)) #\space) s)
      s))

  (define (ui-status! ui verb message)
    (unless (and (ui-quiet? ui) (member verb '("Finished" "Fresh")))
      (ui-status-line! ui verb message)))

  (define (ui-status-line! ui verb message)
    (above-progress ui
      (lambda (port)
        (put-string port (paint (ui-painter ui) '(bold green) (pad-left verb 12)))
        (put-string port " ")
        (put-string port message)
        (newline port))))

  (define (ui-note! ui message)
    (above-progress ui
      (lambda (port)
        (put-string port (make-string 13 #\space))
        (put-string port message)
        (newline port))))

  (define (ui-warning! ui message)
    (above-progress ui
      (lambda (port)
        (put-string port (paint (ui-painter ui) '(bold yellow) "warning"))
        (put-string port ": ")
        (put-string port message)
        (newline port))))

  (define (ui-error-text ui message)
    (string-append (paint (ui-painter ui) '(bold red) "error") ": " message "\n"))

  (define (ui-progress! ui done total names)
    (when (ui-live? ui)
      (ui-progress-set! ui (list done total names))
      (let ([port (ui-port ui)])
        (clear-line port)
        (draw-progress ui port)
        (flush-output-port port))))

  (define (ui-progress-done! ui)
    (when (ui-progress ui)
      (ui-progress-set! ui #f)
      (let ([port (ui-port ui)])
        (clear-line port)
        (flush-output-port port))))

  (define (above-progress ui write)
    (let ([port (ui-port ui)])
      (when (ui-progress ui) (clear-line port))
      (write port)
      (when (ui-progress ui) (draw-progress ui port))
      (flush-output-port port)))

  (define (clear-line port)
    (put-string port "\r\x1b;[2K"))

  (define bar-width 25)

  (define (terminal-columns)
    (or (let ([columns (getenv "COLUMNS")])
          (and columns (string->number columns)))
      (guard (e [#t #f])
        (let-values ([(columns rows) (terminal-size)])
          (and (integer? columns) (> columns 20) columns)))
      80))

  (define (draw-progress ui port)
    (let* ([progress (ui-progress ui)]
           [done (car progress)]
           [total (max 1 (cadr progress))]
           [names (caddr progress)]
           [filled (div (* bar-width (min done total)) total)]
           [bar (string-append "["
                  (make-string filled #\=)
                  (if (< filled bar-width) ">" "")
                  (make-string (max 0 (- bar-width filled 1)) #\space)
                  "]")]
           [count (string-append (number->string done) "/" (number->string total))]
           [head (string-append " " bar " " count)]
           [room (- (terminal-columns) 12 (string-length head) 1)]
           [tail (names-text names (- room 2))])
      (put-string port (paint (ui-painter ui) '(bold cyan) (pad-left "Building" 12)))
      (put-string port head)
      (unless (string=? tail "")
        (put-string port ": ")
        (put-string port tail))))

  (define (names-text names room)
    (let loop ([names names] [out ""])
      (cond
        [(null? names) out]
        [else
          (let ([next (if (string=? out "") (car names) (string-append out ", " (car names)))])
            (cond
              [(<= (string-length next) room) (loop (cdr names) next)]
              [(string=? out "") ""]
              [(<= (+ (string-length out) 3) room) (string-append out ", ...")]
              [else out]))]))))
