;; SHA-1 (FIPS 180-4, section 6.1).
;;
;; One-shot: (sha1 bv) -> 20-byte bytevector.
;; Streaming: (make-sha1-state), (sha1-update! st bv start end), (sha1-finish st).
;; sha1-finish consumes the state.

(library (capy crypto sha1)
  (export sha1
    make-sha1-state
    sha1-update!
    sha1-finish
    sha1-block-size
    sha1-digest-size)
  (import (rnrs)
    (capy crypto u32))

  (define sha1-block-size 64)
  (define sha1-digest-size 20)

  (define-record-type sha1-state
    (fields
      h   ; vector of 5 signed 32-bit words
      w   ; message schedule, 80 words
      buf ; pending bytes, up to one block
      (mutable buflen)
      (mutable total))
    (protocol
      (lambda (new)
        (lambda ()
          (new
            (vector (u32->signed #x67452301)
              (u32->signed #xEFCDAB89)
              (u32->signed #x98BADCFE)
              (u32->signed #x10325476)
              (u32->signed #xC3D2E1F0))
            (make-vector 80 0)
            (make-bytevector 64 0)
            0
            0)))))

  (define K0 (u32->signed #x5A827999))
  (define K1 (u32->signed #x6ED9EBA1))
  (define K2 (u32->signed #x8F1BBCDC))
  (define K3 (u32->signed #xCA62C1D6))

  (define (compress! st bv off)
    (let ([w (sha1-state-w st)]
          [h (sha1-state-h st)])
      (do ([t 0 (fx+ t 1)])
          ((fx=? t 16))
        (vector-set! w t (bytevector-s32-ref bv (fx+ off (fx* 4 t)) (endianness big))))
      (do ([t 16 (fx+ t 1)])
          ((fx=? t 80))
        (vector-set! w t
          (u32-rotl
            (fxxor (vector-ref w (fx- t 3))
              (fxxor (vector-ref w (fx- t 8))
                (fxxor (vector-ref w (fx- t 14)) (vector-ref w (fx- t 16)))))
            1)))
      (let loop ([t 0]
                 [a (vector-ref h 0)]
                 [b (vector-ref h 1)]
                 [c (vector-ref h 2)]
                 [d (vector-ref h 3)]
                 [e (vector-ref h 4)])
        (if (fx=? t 80)
          (begin
            (vector-set! h 0 (u32+ (vector-ref h 0) a))
            (vector-set! h 1 (u32+ (vector-ref h 1) b))
            (vector-set! h 2 (u32+ (vector-ref h 2) c))
            (vector-set! h 3 (u32+ (vector-ref h 3) d))
            (vector-set! h 4 (u32+ (vector-ref h 4) e)))
          (let* ([f (cond
                      [(fx<? t 20) (fxior (fxand b c) (fxand (fxnot b) d))]
                      [(fx<? t 40) (fxxor b (fxxor c d))]
                      [(fx<? t 60) (fxior (fxand b c) (fxior (fxand b d) (fxand c d)))]
                      [else (fxxor b (fxxor c d))])]
                 [k (cond
                      [(fx<? t 20) K0]
                      [(fx<? t 40) K1]
                      [(fx<? t 60) K2]
                      [else K3])]
                 [temp (u32+ (u32-rotl a 5) f e k (vector-ref w t))])
            (loop (fx+ t 1) temp a (u32-rotl b 30) c d))))))

  (define (sha1-update! st bv start end)
    (let ([buf (sha1-state-buf st)])
      (sha1-state-total-set! st (+ (sha1-state-total st) (- end start)))
      (let loop ([pos start])
        (let ([buflen (sha1-state-buflen st)]
              [remaining (fx- end pos)])
          (cond
            [(fx=? remaining 0) (if #f #f)]
            [(and (fx=? buflen 0) (fx>=? remaining 64))
              (compress! st bv pos)
              (loop (fx+ pos 64))]
            [else
              (let ([n (fxmin remaining (fx- 64 buflen))])
                (bytevector-copy! bv pos buf buflen n)
                (sha1-state-buflen-set! st (fx+ buflen n))
                (when (fx=? (fx+ buflen n) 64)
                  (compress! st buf 0)
                  (sha1-state-buflen-set! st 0))
                (loop (fx+ pos n)))])))))

  (define (zero-fill! bv start end)
    (do ([i start (fx+ i 1)])
        ((fx>=? i end))
      (bytevector-u8-set! bv i 0)))

  (define (sha1-finish st)
    (let* ([buf (sha1-state-buf st)]
           [bits (* 8 (sha1-state-total st))]
           [buflen (sha1-state-buflen st)])
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
      (let ([out (make-bytevector 20 0)]
            [h (sha1-state-h st)])
        (do ([i 0 (fx+ i 1)])
            ((fx=? i 5) out)
          (bytevector-s32-set! out (fx* 4 i) (vector-ref h i) (endianness big))))))

  (define (sha1 bv)
    "Return the SHA-1 digest of bytevector `bv` as a 20-byte bytevector."
    (let ([st (make-sha1-state)])
      (sha1-update! st bv 0 (bytevector-length bv))
      (sha1-finish st))))
