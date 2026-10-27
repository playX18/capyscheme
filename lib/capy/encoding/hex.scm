;; Hexadecimal encoding of bytevectors (lowercase output, either case accepted).

(library (capy encoding hex)
  (export bytevector->hex-string hex-string->bytevector)
  (import (rnrs)
    (rnrs mutable-strings))

  (define digits "0123456789abcdef")

  (define (bytevector->hex-string bv)
    "Return the lowercase hex encoding of `bv` (two characters per byte)."
    (let* ([n (bytevector-length bv)]
           [out (make-string (fx* 2 n) #\0)])
      (do ([i 0 (fx+ i 1)])
          ((fx=? i n) out)
        (let ([b (bytevector-u8-ref bv i)])
          (string-set! out (fx* 2 i) (string-ref digits (fxarithmetic-shift-right b 4)))
          (string-set! out (fx+ 1 (fx* 2 i)) (string-ref digits (fxand b 15)))))))

  (define (hex-value c)
    (cond
      [(and (char>=? c #\0) (char<=? c #\9)) (fx- (char->integer c) 48)]
      [(and (char>=? c #\a) (char<=? c #\f)) (fx- (char->integer c) 87)]
      [(and (char>=? c #\A) (char<=? c #\F)) (fx- (char->integer c) 55)]
      [else #f]))

  (define (hex-string->bytevector str)
    "Decode a hex string into a bytevector. Raises an assertion violation on odd length or non-hex characters."
    (let ([len (string-length str)])
      (unless (fx=? 0 (fxand len 1))
        (assertion-violation 'hex-string->bytevector "odd number of hex digits" str))
      (let* ([n (fxarithmetic-shift-right len 1)]
             [out (make-bytevector n 0)])
        (do ([i 0 (fx+ i 1)])
            ((fx=? i n) out)
          (let ([hi (hex-value (string-ref str (fx* 2 i)))]
                [lo (hex-value (string-ref str (fx+ 1 (fx* 2 i))))])
            (unless (and hi lo)
              (assertion-violation 'hex-string->bytevector "invalid hex digit" str))
            (bytevector-u8-set! out i (fxior (fxarithmetic-shift-left hi 4) lo)))))))
  )
