;;; CRC-32 (ISO 3309 / ITU-T V.42, as used by gzip and PNG) and Adler-32 (RFC 1950).
;;;
;;;   (crc32 (string->utf8 "123456789"))           ; => #xCBF43926
;;;   (adler32 (string->utf8 "Wikipedia"))         ; => #x11E60398
;;;   (crc32-update (crc32 a) b)                   ; = (crc32 (bytevector-append a b))
;;;
;;; Every procedure takes an optional START and END into the bytevector.
(library (capy compress checksum)
  (export crc32 crc32-update adler32 adler32-update)
  (import (rnrs)
    (only (capy) fxsrl))

  (define (u32->signed k)
    (if (>= k #x80000000) (- k #x100000000) k))

  (define (signed->u32 k)
    (if (< k 0) (+ k #x100000000) k))

  ;; 4 x 256 entries: entry k*256+n is the CRC of byte n followed by k zero bytes.
  (define crc-tables
    (let ([t (make-vector 1024 0)]
          [base (make-vector 256 0)])
      (do ([n 0 (+ n 1)]) ((= n 256))
        (let loop ([c n] [k 0])
          (if (= k 8)
            (vector-set! base n c)
            (loop (if (odd? c)
                    (bitwise-xor #xEDB88320 (bitwise-arithmetic-shift-right c 1))
                    (bitwise-arithmetic-shift-right c 1))
                  (+ k 1)))))
      (do ([n 0 (+ n 1)]) ((= n 256))
        (let loop ([k 0] [c (vector-ref base n)])
          (when (< k 4)
            (vector-set! t (+ (* k 256) n) (u32->signed c))
            (loop (+ k 1)
                  (bitwise-xor (bitwise-arithmetic-shift-right c 8)
                               (vector-ref base (bitwise-and c 255)))))))
      t))

  (define (check-range who bv start end)
    (unless (bytevector? bv)
      (assertion-violation who "expected a bytevector" bv))
    (unless (and (fixnum? start) (fixnum? end)
                 (fx<=? 0 start) (fx<=? start end) (fx<=? end (bytevector-length bv)))
      (assertion-violation who "invalid range" start end (bytevector-length bv))))

  ;; The CRC register C (inverted, signed) after bv[start, end).
  (define (crc-run c bv start end)
    (let ([t crc-tables])
      (let fast ([c c] [i start])
        (if (fx<=? (fx+ i 4) end)
          (let ([c (fxxor c (bytevector-s32-ref bv i (endianness little)))])
            (fast (fxxor (fxxor (vector-ref t (fx+ 768 (fxand c 255)))
                                (vector-ref t (fx+ 512 (fxand (fxsrl c 8) 255))))
                         (fxxor (vector-ref t (fx+ 256 (fxand (fxsrl c 16) 255)))
                                (vector-ref t (fxsrl c 24))))
                  (fx+ i 4)))
          (let tail ([c c] [i i])
            (if (fx=? i end)
              c
              (tail (fxxor (vector-ref t (fxand (fxxor c (bytevector-u8-ref bv i)) 255))
                           (fxsrl c 8))
                    (fx+ i 1))))))))

  (define (crc32-update crc bv . range)
    (let-values ([(start end) (range-of bv range)])
      (check-range 'crc32-update bv start end)
      (unless (and (integer? crc) (exact? crc) (<= 0 crc #xFFFFFFFF))
        (assertion-violation 'crc32-update "expected a 32-bit CRC" crc))
      (let ([c (fxnot (u32->signed crc))])
        (signed->u32 (fxnot (crc-run c bv start end))))))

  ;; The CRC-32 of bv[start, end).
  (define (crc32 bv . range)
    (let-values ([(start end) (range-of bv range)])
      (check-range 'crc32 bv start end)
      (signed->u32 (fxnot (crc-run -1 bv start end)))))

  (define (range-of bv range)
    (cond
      [(null? range) (values 0 (if (bytevector? bv) (bytevector-length bv) 0))]
      [(null? (cdr range)) (values (car range) (if (bytevector? bv) (bytevector-length bv) 0))]
      [else (values (car range) (cadr range))]))

  ;; Adler-32: s1 and s2 modulo 65521. 3854 bytes is the most that can be summed before s2 can
  ;; pass 2^31 - 1, starting from s1, s2 < 65521: 255 n (n + 1) / 2 + 65520 (n + 1) < 2^31.
  (define-syntax nmax (identifier-syntax 3854))

  (define (adler-run a b bv start end)
    (let outer ([a a] [b b] [i start])
      (if (fx=? i end)
        (values a b)
        (let ([stop (fxmin end (fx+ i nmax))])
          (let inner ([a a] [b b] [i i])
            (if (fx<? i stop)
              (let ([a (fx+ a (bytevector-u8-ref bv i))])
                (inner a (fx+ b a) (fx+ i 1)))
              (outer (fxmod a 65521) (fxmod b 65521) i)))))))

  (define (adler32-update adler bv . range)
    (let-values ([(start end) (range-of bv range)])
      (check-range 'adler32-update bv start end)
      (unless (and (integer? adler) (exact? adler) (<= 0 adler #xFFFFFFFF))
        (assertion-violation 'adler32-update "expected a 32-bit Adler-32 value" adler))
      (let-values ([(a b) (adler-run (bitwise-and adler #xFFFF)
                                     (bitwise-arithmetic-shift-right adler 16)
                                     bv start end)])
        (+ (* b 65536) a))))

  ;; The Adler-32 of bv[start, end).
  (define (adler32 bv . range)
    (let-values ([(start end) (range-of bv range)])
      (check-range 'adler32 bv start end)
      (let-values ([(a b) (adler-run 1 0 bv start end)])
        (+ (* b 65536) a)))))
