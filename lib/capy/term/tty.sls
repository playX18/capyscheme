(library (capy term tty)
  (export tty? ansi-supported? open-tty-input-fd open-tty-output-fd open-tty-fd)
  (import (rnrs)
          (only (capy) getenv)
          (rename (capy)
            (term/isatty? raw-isatty?)
            (term/open-tty raw-open-tty)))

  (define (source->fd who source)
    (cond
      [(integer? source) source]
      [(port? source) (port-fileno source)]
      [else (assertion-violation who "expected fd or port" source)]))

  (define (tty? source)
    (raw-isatty? (source->fd 'tty? source)))

  (define (ansi-supported?)
    (not (equal? (getenv "TERM") "dumb")))

  (define (open-tty-input-fd)
    (raw-open-tty #t #f))

  (define (open-tty-output-fd)
    (raw-open-tty #f #t))

  (define (open-tty-fd)
    (raw-open-tty #t #t)))
