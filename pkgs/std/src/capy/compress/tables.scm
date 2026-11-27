;;; The constant tables of DEFLATE (RFC 1951, section 3.2.5) and the condition that both
;;; directions raise for bad input.

(library (capy compress tables)
  (export length-base length-extra dist-base dist-extra clen-order
    reverse-bits
    &compress-error make-compress-error compress-error? compress-error)
  (import (rnrs))

  ;; Length symbols 257..285, by symbol - 257.
  (define length-base
    '#(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258))
  (define length-extra
    '#(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))

  ;; Distance symbols 0..29.
  (define dist-base
    '#(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
       1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
  (define dist-extra
    '#(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))

  ;; The order in which the code-length code's lengths are sent.
  (define clen-order '#(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

  ;; The low N bits of X in reverse order. Huffman codes are defined most significant bit first
  ;; but are packed into bytes starting from the least significant bit.
  (define (reverse-bits x n)
    (let loop ([x x] [n n] [r 0])
      (if (fx=? n 0)
        r
        (loop (fxarithmetic-shift-right x 1) (fx- n 1)
              (fxior (fxarithmetic-shift-left r 1) (fxand x 1))))))

  ;; Raised for malformed compressed data: a bad header, an invalid code, a checksum mismatch or
  ;; data that ends too early.
  (define-condition-type &compress-error &error
    make-compress-error
    compress-error?)

  (define (compress-error who message . irritants)
    (raise
      (condition
        (make-compress-error)
        (make-who-condition who)
        (make-message-condition message)
        (make-irritants-condition irritants)))))
