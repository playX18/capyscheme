(library (capy term private ansi)
  (export esc csi osc sgr uint16-string join-params osc-escape)
  (import (rnrs))

  (define esc "\x1b;")
  (define esc-char (integer->char #x1b))
  (define bel-char (integer->char #x07))

  (define (csi body)
    (string-append esc "[" body))

  (define (osc body)
    (string-append esc "]" body "\x07;"))

  (define (param->string param)
    (cond
      [(number? param) (number->string param)]
      [(string? param) param]
      [else (assertion-violation 'join-params "expected numeric or string parameter" param)]))

  (define (sgr params)
    (csi (string-append (join-params params) "m")))

  (define (uint16-string who n)
    (unless (and (integer? n) (<= 0 n 65535))
      (assertion-violation who "expected integer in u16 range" n))
    (number->string n))

  (define (join-params params)
    (let loop ([xs params] [out ""])
      (cond
        [(null? xs) out]
        [(string=? out "") (loop (cdr xs) (param->string (car xs)))]
        [else (loop (cdr xs)
                    (string-append out ";" (param->string (car xs))))])))

  (define (osc-escape s)
    (list->string
      (filter (lambda (ch)
                (not (or (char=? ch bel-char) (char=? ch esc-char))))
              (string->list s)))))
