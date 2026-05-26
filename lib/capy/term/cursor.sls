(library (capy term cursor)
  (export move-to move-to-next-line move-to-previous-line move-to-column move-to-row
          move-up move-right move-down move-left
          save-position restore-position hide-cursor show-cursor
          enable-blinking disable-blinking set-cursor-style
          cursor-move-to cursor-position)
  (import (rnrs) (capy term private ansi) (capy term command) (capy term event))

  (define (command name body)
    (make-command name (lambda () body)))

  (define (count-string who n)
    (uint16-string who n))

  (define (position-string who n)
    (uint16-string who (+ n 1)))

  (define (move-to column row)
    (command 'move-to
             (csi (string-append (position-string 'move-to row)
                                 ";"
                                 (position-string 'move-to column)
                                 "H"))))

  (define cursor-move-to move-to)

  (define (move-to-next-line n)
    (command 'move-to-next-line (csi (string-append (count-string 'move-to-next-line n) "E"))))

  (define (move-to-previous-line n)
    (command 'move-to-previous-line (csi (string-append (count-string 'move-to-previous-line n) "F"))))

  (define (move-to-column n)
    (command 'move-to-column (csi (string-append (position-string 'move-to-column n) "G"))))

  (define (move-to-row n)
    (command 'move-to-row (csi (string-append (position-string 'move-to-row n) "d"))))

  (define (move-up n)
    (command 'move-up (csi (string-append (count-string 'move-up n) "A"))))

  (define (move-right n)
    (command 'move-right (csi (string-append (count-string 'move-right n) "C"))))

  (define (move-down n)
    (command 'move-down (csi (string-append (count-string 'move-down n) "B"))))

  (define (move-left n)
    (command 'move-left (csi (string-append (count-string 'move-left n) "D"))))

  (define (save-position)
    (command 'save-position (string-append esc "7")))

  (define (restore-position)
    (command 'restore-position (string-append esc "8")))

  (define (hide-cursor)
    (command 'hide-cursor (csi "?25l")))

  (define (show-cursor)
    (command 'show-cursor (csi "?25h")))

  (define (enable-blinking)
    (command 'enable-blinking (csi "?12h")))

  (define (disable-blinking)
    (command 'disable-blinking (csi "?12l")))

  (define (cursor-style-code style)
    (case style
      [(default-user-shape) 0]
      [(blinking-block) 1]
      [(steady-block) 2]
      [(blinking-underline) 3]
      [(steady-underline) 4]
      [(blinking-bar) 5]
      [(steady-bar) 6]
      [else (assertion-violation 'set-cursor-style "unknown cursor style" style)]))

  (define (set-cursor-style style)
    (command 'set-cursor-style
             (csi (string-append (number->string (cursor-style-code style)) " q"))))

  (define (poll-source timeout source)
    (if source
        (poll-event timeout source)
        (poll-event timeout)))

  (define (read-source source)
    (if source
        (read-event source)
        (read-event)))

  (define (cursor-position-event? event)
    (and (pair? event) (eq? (car event) 'cursor-position)))

  (define (cursor-position . args)
    (let* ([source (if (null? args) #f (car args))]
           [port (if (or (null? args) (null? (cdr args)))
                     (current-output-port)
                     (cadr args))]
           [timeout (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                        2000
                        (caddr args))])
      (display (csi "6n") port)
      (flush-output-port port)
      (let loop ([remaining timeout])
        (if (poll-source remaining source)
            (let ([event (read-source source)])
              (if (cursor-position-event? event)
                  (values (cadr event) (caddr event))
                  (loop 0)))
            (assertion-violation 'cursor-position
              "timed out waiting for cursor position"))))))
