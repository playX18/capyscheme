(library (capy term tty)
  (export tty? ansi-supported? open-tty-input-port open-tty-input-fd open-tty-output-fd open-tty-fd)
  (import (rnrs)
    (only (capy) getenv open-binary-fd-input-port)
    (rename (capy)
      (term/isatty? raw-isatty?)
      (term/nonblocking! raw-nonblocking!)
      (term/open-tty raw-open-tty)))

  (define (source->fd who source)
    (cond
      [(integer? source) source]
      [(port? source) (port-fileno source)]
      [else (assertion-violation who "expected fd or port" source)]))

  (define (tty? source)
    (let ([fd (source->fd 'tty? source)])
      (and fd (raw-isatty? fd))))

  (define (ansi-supported?)
    (not (equal? (getenv "TERM") "dumb")))

  ;; A binary input port on the controlling terminal. The descriptor is
  ;; non-blocking, so a read with nothing to read waits without holding up
  ;; the collector or other threads.
  (define (open-tty-input-port)
    (let ([fd (raw-open-tty #t #f)])
      (raw-nonblocking! fd #t)
      (open-binary-fd-input-port "/dev/tty" fd 'none)))

  (define (open-tty-input-fd)
    (raw-open-tty #t #f))

  (define (open-tty-output-fd)
    (raw-open-tty #f #t))

  (define (open-tty-fd)
    (raw-open-tty #t #t)))
