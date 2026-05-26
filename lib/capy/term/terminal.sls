(library (capy term terminal)
  (export disable-line-wrap enable-line-wrap
    enter-alternate-screen
    leave-alternate-screen
    scroll-up
    scroll-down
    clear
    terminal-clear
    set-size
    set-title
    begin-synchronized-update
    end-synchronized-update
    enable-raw-mode!
    disable-raw-mode!
    raw-mode-enabled?
    terminal-size
    terminal-window-size
    window-size?
    window-size-columns
    window-size-rows
    window-size-width
    window-size-height)
  (import (rnrs)
    (rename (capy)
      (term/enable-raw-mode! raw-enable-raw-mode!)
      (term/disable-raw-mode! raw-disable-raw-mode!)
      (term/raw-mode-enabled? raw-raw-mode-enabled?)
      (term/terminal-size-list raw-terminal-size-list))
    (capy term private ansi)
    (capy term command))

  (define-record-type (window-size make-window-size window-size?)
    (fields
      (immutable columns window-size-columns)
      (immutable rows window-size-rows)
      (immutable width window-size-width)
      (immutable height window-size-height)))

  (define (command name body)
    (make-command name (lambda () body)))

  (define (count-string who n)
    (uint16-string who n))

  (define (disable-line-wrap)
    (command 'disable-line-wrap (csi "?7l")))

  (define (enable-line-wrap)
    (command 'enable-line-wrap (csi "?7h")))

  (define (enter-alternate-screen)
    (command 'enter-alternate-screen (csi "?1049h")))

  (define (leave-alternate-screen)
    (command 'leave-alternate-screen (csi "?1049l")))

  (define (scroll-up n)
    (command 'scroll-up (csi (string-append (count-string 'scroll-up n) "S"))))

  (define (scroll-down n)
    (command 'scroll-down (csi (string-append (count-string 'scroll-down n) "T"))))

  (define (clear-kind->body kind)
    (case kind
      [(all) "2J"]
      [(purge) "3J"]
      [(from-cursor-down) "J"]
      [(from-cursor-up) "1J"]
      [(current-line) "2K"]
      [(until-new-line) "K"]
      [else (assertion-violation 'clear "unknown clear type" kind)]))

  (define (clear kind)
    (command 'clear (csi (clear-kind->body kind))))

  (define terminal-clear clear)

  (define (set-size columns rows)
    (command 'set-size
      (csi (string-append "8;"
            (count-string 'set-size rows)
            ";"
            (count-string 'set-size columns)
            "t"))))

  (define (set-title title)
    (unless (string? title)
      (assertion-violation 'set-title "expected string title" title))
    (command 'set-title (osc (string-append "0;" (osc-escape title)))))

  (define (enable-raw-mode!)
    (raw-enable-raw-mode!))

  (define (disable-raw-mode!)
    (raw-disable-raw-mode!))

  (define (raw-mode-enabled?)
    (raw-raw-mode-enabled?))

  (define (terminal-size)
    (let ([size (raw-terminal-size-list)])
      (values (car size) (cadr size))))

  (define (terminal-window-size)
    (let ([size (raw-terminal-size-list)])
      (make-window-size (car size) (cadr size) (caddr size) (cadddr size)))))
