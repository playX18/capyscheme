(library (capy term clipboard)
  (export clipboard-selection copy-to-clipboard)
  (import (rnrs) (capy term command) (capy term private ansi))

  (define base64-alphabet
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

  (define (base64-char n)
    (string-ref base64-alphabet n))

  (define (base64-encode bytes)
    (let ([n (bytevector-length bytes)])
      (let loop ([i 0] [out '()])
        (if (>= i n)
            (list->string (reverse out))
            (let* ([b0 (bytevector-u8-ref bytes i)]
                   [have-b1 (< (+ i 1) n)]
                   [have-b2 (< (+ i 2) n)]
                   [b1 (if have-b1 (bytevector-u8-ref bytes (+ i 1)) 0)]
                   [b2 (if have-b2 (bytevector-u8-ref bytes (+ i 2)) 0)]
                   [c0 (base64-char (div b0 4))]
                   [c1 (base64-char (+ (* (mod b0 4) 16) (div b1 16)))]
                   [c2 (if have-b1
                           (base64-char (+ (* (mod b1 16) 4) (div b2 64)))
                           #\=)]
                   [c3 (if have-b2 (base64-char (mod b2 64)) #\=)])
              (loop (+ i 3) (cons c3 (cons c2 (cons c1 (cons c0 out))))))))))

  (define (clipboard-selection selection)
    (cond
      [(eq? selection 'clipboard) "c"]
      [(eq? selection 'primary) "p"]
      [(eq? selection 'secondary) "s"]
      [(string? selection) selection]
      [(char? selection) (string selection)]
      [else (assertion-violation 'clipboard-selection
              "expected clipboard, primary, secondary, string, or char"
              selection)]))

  (define (copy-to-clipboard selection text)
    (let ([destination (clipboard-selection selection)]
          [encoded (base64-encode (string->utf8 text))])
      (make-command 'copy-to-clipboard
                    (lambda ()
                      (osc (string-append "52;" destination ";" encoded)))))))
