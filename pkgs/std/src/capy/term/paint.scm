;;; Styled text for command line tools, in the way of cargo: a green bold "Compiling", a red
;;; "error".
;;;
;;;   (let ([painter (make-painter (parse-color-mode "auto") (current-error-port))])
;;;     (put-string (current-error-port) (paint painter '(bold red) "error")))
;;;
;;; A painter decides once whether colors are used: always (mode `always`), never (`never`), or
;;; when the port is a terminal, `NO_COLOR` is not set (to anything but the empty string) and
;;; `TERM` is not `dumb` (mode `auto`). Without colors `paint` returns the text as it is.

(library (capy term paint)
  (export parse-color-mode
    make-painter
    painter?
    painter-enabled?
    paint)
  (import (rnrs)
    (only (capy) getenv)
    (only (capy term tty) tty?))

  (define-record-type (painter %make-painter painter?)
    (fields enabled?))

  ;; The mode named by the string S, "auto", "always" or "never", as a symbol; an error for
  ;; anything else.
  (define (parse-color-mode s)
    (cond
      [(member s '("auto" "always" "never")) (string->symbol s)]
      [else (error 'parse-color-mode "expected auto, always or never" s)]))

  (define (environment-set? name)
    (let ([value (getenv name)])
      (and value (not (string=? value "")))))

  ;; A painter for output to PORT. MODE is `auto`, `always` or `never`.
  (define (make-painter mode port)
    (%make-painter
      (case mode
        [(always) #t]
        [(never) #f]
        [(auto)
          (and (not (environment-set? "NO_COLOR"))
            (not (equal? (getenv "TERM") "dumb"))
            (guard (e [#t #f]) (tty? port)))]
        [else (error 'make-painter "expected auto, always or never" mode)])))

  (define (style-code style)
    (case style
      [(bold) 1]
      [(dim) 2]
      [(italic) 3]
      [(underline) 4]
      [(black) 30]
      [(red) 31]
      [(green) 32]
      [(yellow) 33]
      [(blue) 34]
      [(magenta) 35]
      [(cyan) 36]
      [(white) 37]
      [else (error 'paint "unknown style" style)]))

  ;; The string TEXT in the styles STYLES, a list of `bold`, `dim`, `italic`, `underline`,
  ;; `black`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan` and `white`.
  (define (paint painter styles text)
    (if (and (painter-enabled? painter) (pair? styles))
      (string-append "\x1b;["
        (let loop ([codes (map style-code styles)] [out ""])
          (if (null? codes)
            out
            (loop (cdr codes)
              (string-append out (if (string=? out "") "" ";") (number->string (car codes))))))
        "m" text "\x1b;[0m")
      text)))
