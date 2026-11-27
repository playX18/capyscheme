(define-library (srfi 27)
  (export random-integer
    random-real
    default-random-source
    make-random-source
    random-source?
    random-source-state-ref
    random-source-state-set!
    random-source-randomize!
    random-source-pseudo-randomize!
    random-source-make-integers
    random-source-make-reals)

;; The generator is xoshiro256**, with its four 64-bit state words in a
;; 32-byte bytevector in native byte order. The words are bignums half the
;; time as Scheme integers, so the step uses the u64 procedures of (capy):
;; the optimizing JIT keeps their operands in raw 64-bit registers, and a
;; draw allocates only the result it returns.
  (import (rnrs)
    (scheme base)
    (only (capy) microsecond tuple-ref tuple tuple?
      u64+/wraparound u64*/wraparound u64sll/wraparound u64srl u64rotl
      u64->flonum))

  (begin
    (define two^64 18446744073709551616)
    (define mask64 #xffffffffffffffff)

    (define (state-bytes rs) (tuple-ref rs 1))

    ;; The native accessors are primitives the compiler opens up, with no
    ;; endianness operand to look up.
    (define-syntax word
      (syntax-rules () [(_ bv i) (bytevector-u64-native-ref bv i)]))
    (define-syntax word-set!
      (syntax-rules () [(_ bv i w) (bytevector-u64-native-set! bv i w)]))

    (define (next! bv)
      "Advance the xoshiro256** state in bv and return its next 64-bit output."
      (let* ([s0 (word bv 0)]
             [s1 (word bv 8)]
             [s2 (word bv 16)]
             [s3 (word bv 24)]
             [result (u64*/wraparound (u64rotl (u64*/wraparound s1 5) 7) 9)]
             [t (u64sll/wraparound s1 17)]
             [s2 (bitwise-xor s2 s0)]
             [s3 (bitwise-xor s3 s1)]
             [s1 (bitwise-xor s1 s2)]
             [s0 (bitwise-xor s0 s3)]
             [s2 (bitwise-xor s2 t)]
             [s3 (u64rotl s3 45)])
        (word-set! bv 0 s0)
        (word-set! bv 8 s1)
        (word-set! bv 16 s2)
        (word-set! bv 24 s3)
        result))

    (define (seed! bv seed)
      "Expand the 64-bit seed into the state in bv with splitmix64."
      (let loop ([i 0] [x seed])
        (when (< i 32)
          (let* ([x (u64+/wraparound x #x9e3779b97f4a7c15)]
                 [z (u64*/wraparound (bitwise-xor x (u64srl x 30)) #xbf58476d1ce4e5b9)]
                 [z (u64*/wraparound (bitwise-xor z (u64srl z 27)) #x94d049bb133111eb)])
            (word-set! bv i (bitwise-xor z (u64srl z 31)))
            (loop (+ i 8) x)))))

    (define (below-fixnum bv n)
      "A uniform integer in [0, n) for a fixnum n > 0, by Lemire's multiply and reject on 31 bits of output: the high part of x * n is uniform once the low part clears (2^31 - n) mod n."
      (let* ([m (u64*/wraparound (u64srl (next! bv) 33) n)]
             [low (bitwise-and m #x7fffffff)])
        (if (and (< low n) (< low (mod (+ (- 2147483647 n) 1) n)))
          (below-fixnum bv n)
          (u64srl m 31))))

    (define (below-word bv n)
      "A uniform integer in [0, n) for 2^31 <= n < 2^64: the top bitwise-length(n) bits of an output, rejected until below n."
      (let ([shift (- 64 (bitwise-length n))])
        (let loop ()
          (let ([x (u64srl (next! bv) shift)])
            (if (< x n) x (loop))))))

    (define (make-random-source . seed?)
      "Return a new xoshiro256** random source, seeded from (microsecond) or from the optional integer seed."
      (let ([bv (make-bytevector 32 0)]
            [seed (if (null? seed?) (microsecond) (car seed?))])
        (seed! bv (bitwise-and seed mask64))
        (tuple 'type:xoshiro256 bv)))

    (define (random-source? x)
      (and (tuple? x)
        (eq? (tuple-ref x 0) 'type:xoshiro256)))

    (define (check-source who rs)
      (unless (random-source? rs)
        (error (string-append (symbol->string who) ": not a random source") rs)))

    (define (random-source-state-ref rs)
      "Return the state of the random source as one 256-bit integer: the four 64-bit state words, the first in the low bits."
      (check-source 'random-source-state-ref rs)
      (let ([bv (state-bytes rs)])
        (let loop ([i 3] [acc 0])
          (if (< i 0)
            acc
            (loop (- i 1)
              (bitwise-ior (bitwise-arithmetic-shift-left acc 64)
                (word bv (* 8 i))))))))

    (define (random-source-state-set! rs state)
      (check-source 'random-source-state-set! rs)
      (let ([bv (state-bytes rs)])
        (do ([i 0 (+ i 1)])
            ((= i 4))
          (word-set! bv (* 8 i)
            (bitwise-and (bitwise-arithmetic-shift-right state (* 64 i)) mask64)))))

    (define (random-source-randomize! rs)
      (check-source 'random-source-randomize! rs)
      (seed! (state-bytes rs) (bitwise-and (microsecond) mask64)))

    (define (random-source-pseudo-randomize! rs . ij)
      "Reseed the source deterministically: from the indices i and j when given, else by advancing the state."
      (check-source 'random-source-pseudo-randomize! rs)
      (if (null? ij)
        (random-source-state-set! rs (+ (random-source-state-ref rs) #x9e3779b97f4a7c15))
        (let ([i (car ij)] [j (if (null? (cdr ij)) 0 (cadr ij))])
          (seed! (state-bytes rs)
            (bitwise-and (bitwise-xor (* i #x9e3779b97f4a7c15) j) mask64)))))

    (define (make-integer-rng rs)
      (let ([bv (state-bytes rs)])
        (lambda (n)
          (unless (or (and (fixnum? n) (fx>? n 0)) (and (exact-integer? n) (positive? n)))
            (error "random-integer: expected a positive exact integer" n))
          (cond
            [(fixnum? n) (below-fixnum bv n)]
            [(< n two^64) (below-word bv n)]
            [(= n two^64) (next! bv)]
            [else
              ;; Draw enough 64-bit words to cover n, and reject the top
              ;; partial multiple of n so the result stays uniform.
              (let* ([count (div (+ (bitwise-length n) 63) 64)]
                     [range (expt 2 (* count 64))]
                     [quo (div range n)]
                     [limit (* n quo)])
                (define (draw)
                  (let loop ([acc 0] [i 0])
                    (if (= i count)
                      acc
                      (loop (bitwise-ior (bitwise-arithmetic-shift-left acc 64)
                              (next! bv))
                        (+ i 1)))))
                (let loop ([x (draw)])
                  (if (>= x limit)
                    (loop (draw))
                    (div x quo))))]))))

    (define (make-real-rng rs)
      (let ([bv (state-bytes rs)])
        ;; The top 53 bits, centered in their 2^-53 cell, which keeps both
        ;; ends of (0, 1) out.
        (lambda ()
          (fl* (fl+ (u64->flonum (u64srl (next! bv) 11)) 0.5) 1.1102230246251565e-16))))

    (define (random-source-make-integers s)
      (check-source 'random-source-make-integers s)
      (make-integer-rng s))
    (define (random-source-make-reals s . unit)
      (check-source 'random-source-make-reals s)
      (make-real-rng s))
    (define default-random-source (make-random-source))
    (define random-integer (random-source-make-integers default-random-source))
    (define random-real (random-source-make-reals default-random-source))))
