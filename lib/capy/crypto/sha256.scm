;; SHA-256 (FIPS 180-4, section 6.2).
;;
;; One-shot: (sha256 bv) -> 32-byte bytevector.
;; Streaming: (make-sha256-state), (sha256-update! st bv start end), (sha256-finish st).
;; sha256-finish consumes the state.

(library (capy crypto sha256)
  (export sha256
    make-sha256-state
    sha256-update!
    sha256-finish
    sha256-block-size
    sha256-digest-size)
  (import (rnrs)
    (capy crypto u32))

  (define sha256-block-size 64)
  (define sha256-digest-size 32)

  (define-record-type sha256-state
    (fields
      h   ; vector of 8 signed 32-bit words
      w   ; message schedule, 64 words
      buf ; pending bytes, up to one block
      (mutable buflen)
      (mutable total))
    (protocol
      (lambda (new)
        (lambda ()
          (new
            (vector (u32->signed #x6a09e667) (u32->signed #xbb67ae85) (u32->signed #x3c6ef372) (u32->signed #xa54ff53a) (u32->signed #x510e527f) (u32->signed #x9b05688c) (u32->signed #x1f83d9ab) (u32->signed #x5be0cd19))
            (make-vector 64 0)
            (make-bytevector 64 0)
            0
            0)))))

  (define K
    (vector
      1116352408 1899447441 -1245643825 -373957723 961987163 1508970993 -1841331548 -1424204075
      -670586216 310598401 607225278 1426881987 1925078388 -2132889090 -1680079193 -1046744716
      -459576895 -272742522 264347078 604807628 770255983 1249150122 1555081692 1996064986
      -1740746414 -1473132947 -1341970488 -1084653625 -958395405 -710438585 113926993 338241895
      666307205 773529912 1294757372 1396182291 1695183700 1986661051 -2117940946 -1838011259
      -1564481375 -1474664885 -1035236496 -949202525 -778901479 -694614492 -200395387 275423344
      430227734 506948616 659060556 883997877 958139571 1322822218 1537002063 1747873779
      1955562222 2024104815 -2067236844 -1933114872 -1866530822 -1538233109 -1090935817 -965641998))

  (define (compress! st bv off)
    (let ([w (sha256-state-w st)]
          [h (sha256-state-h st)])
      (do ([t 0 (fx+ t 1)])
          ((fx=? t 16))
        (vector-set! w t (bytevector-s32-ref bv (fx+ off (fx* 4 t)) (endianness big))))
      (do ([t 16 (fx+ t 1)])
          ((fx=? t 64))
        (let* ([x (vector-ref w (fx- t 15))]
               [y (vector-ref w (fx- t 2))]
               [s0 (fxxor (u32-rotr x 7) (fxxor (u32-rotr x 18) (u32-shr x 3)))]
               [s1 (fxxor (u32-rotr y 17) (fxxor (u32-rotr y 19) (u32-shr y 10)))])
          (vector-set! w t (u32+ (vector-ref w (fx- t 16)) s0 (vector-ref w (fx- t 7)) s1))))
      (let loop ([t 0]
                 [a (vector-ref h 0)]
                 [b (vector-ref h 1)]
                 [c (vector-ref h 2)]
                 [d (vector-ref h 3)]
                 [e (vector-ref h 4)]
                 [f (vector-ref h 5)]
                 [g (vector-ref h 6)]
                 [hh (vector-ref h 7)])
        (if (fx=? t 64)
          (begin
            (vector-set! h 0 (u32+ (vector-ref h 0) a))
            (vector-set! h 1 (u32+ (vector-ref h 1) b))
            (vector-set! h 2 (u32+ (vector-ref h 2) c))
            (vector-set! h 3 (u32+ (vector-ref h 3) d))
            (vector-set! h 4 (u32+ (vector-ref h 4) e))
            (vector-set! h 5 (u32+ (vector-ref h 5) f))
            (vector-set! h 6 (u32+ (vector-ref h 6) g))
            (vector-set! h 7 (u32+ (vector-ref h 7) hh)))
          (let* ([s1 (fxxor (u32-rotr e 6) (fxxor (u32-rotr e 11) (u32-rotr e 25)))]
                 [ch (fxxor (fxand e f) (fxand (fxnot e) g))]
                 [t1 (u32+ hh s1 ch (vector-ref K t) (vector-ref w t))]
                 [s0 (fxxor (u32-rotr a 2) (fxxor (u32-rotr a 13) (u32-rotr a 22)))]
                 [maj (fxxor (fxand a b) (fxxor (fxand a c) (fxand b c)))]
                 [t2 (u32+ s0 maj)])
            (loop (fx+ t 1) (u32+ t1 t2) a b c (u32+ d t1) e f g))))))

  (define (zero-fill! bv start end)
    (do ([i start (fx+ i 1)])
        ((fx>=? i end))
      (bytevector-u8-set! bv i 0)))

  (define (sha256-update! st bv start end)
    (let ([buf (sha256-state-buf st)])
      (sha256-state-total-set! st (+ (sha256-state-total st) (- end start)))
      (let loop ([pos start])
        (let ([buflen (sha256-state-buflen st)]
              [remaining (fx- end pos)])
          (cond
            [(fx=? remaining 0) (if #f #f)]
            [(and (fx=? buflen 0) (fx>=? remaining 64))
              (compress! st bv pos)
              (loop (fx+ pos 64))]
            [else
              (let ([n (fxmin remaining (fx- 64 buflen))])
                (bytevector-copy! bv pos buf buflen n)
                (sha256-state-buflen-set! st (fx+ buflen n))
                (when (fx=? (fx+ buflen n) 64)
                  (compress! st buf 0)
                  (sha256-state-buflen-set! st 0))
                (loop (fx+ pos n)))])))))

  (define (sha256-finish st)
    (let* ([buf (sha256-state-buf st)]
           [bits (* 8 (sha256-state-total st))]
           [buflen (sha256-state-buflen st)])
      (bytevector-u8-set! buf buflen #x80)
      (cond
        [(fx>? buflen 55)
          (zero-fill! buf (fx+ buflen 1) 64)
          (compress! st buf 0)
          (zero-fill! buf 0 56)]
        [else (zero-fill! buf (fx+ buflen 1) 56)])
      (do ([i 0 (fx+ i 1)])
          ((fx=? i 8))
        (bytevector-u8-set! buf (fx+ 56 i)
          (bitwise-and (bitwise-arithmetic-shift-right bits (* 8 (- 7 i))) #xff)))
      (compress! st buf 0)
      (let ([out (make-bytevector 32 0)]
            [h (sha256-state-h st)])
        (do ([i 0 (fx+ i 1)])
            ((fx=? i 8) out)
          (bytevector-s32-set! out (fx* 4 i) (vector-ref h i) (endianness big))))))

  (define (sha256 bv)
    "Return the SHA-256 digest of bytevector `bv` as a 32-byte bytevector."
    (let ([st (make-sha256-state)])
      (sha256-update! st bv 0 (bytevector-length bv))
      (sha256-finish st))))
