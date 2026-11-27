(import (rnrs)
  (srfi 64)
  (srfi 27))

(test-begin "srfi 27")

(define (all? pred n)
  (let loop ([i 0])
    (or (= i n) (and (pred) (loop (+ i 1))))))

(test-group "random-integer"
  (test-assert "small bound stays in range"
    (all? (lambda () (let ([x (random-integer 10)]) (and (fixnum? x) (<= 0 x 9)))) 10000))
  (test-equal "bound 1 is always 0" 0 (random-integer 1))
  (test-assert "every value of a small range appears"
    (let ([seen (make-vector 8 #f)])
      (do ([i 0 (+ i 1)]) ((= i 2000)) (vector-set! seen (random-integer 8) #t))
      (for-all (lambda (x) x) (vector->list seen))))
  (test-assert "64-bit bound"
    (all? (lambda () (let ([x (random-integer (expt 2 63))]) (and (>= x 0) (< x (expt 2 63))))) 1000))
  (test-assert "2^64 bound"
    (all? (lambda () (let ([x (random-integer (expt 2 64))]) (and (>= x 0) (< x (expt 2 64))))) 1000))
  (test-assert "bignum bound"
    (all? (lambda () (let ([x (random-integer (expt 10 40))]) (and (>= x 0) (< x (expt 10 40))))) 1000)))

(test-group "random-real"
  (test-assert "in the open unit interval"
    (all? (lambda () (let ([x (random-real)]) (and (flonum? x) (> x 0.0) (< x 1.0)))) 10000)))

(test-group "sources"
  (define (draws s n)
    (let ([next (random-source-make-integers s)])
      (let loop ([i 0] [acc '()])
        (if (= i n) (reverse acc) (loop (+ i 1) (cons (next 1000000) acc))))))
  (test-assert "random-source?" (random-source? (make-random-source)))
  (test-assert "not a source" (not (random-source? 42)))
  (test-equal "the same seed gives the same sequence"
    (draws (make-random-source 12345) 20)
    (draws (make-random-source 12345) 20))
  (test-assert "different seeds differ"
    (not (equal? (draws (make-random-source 1) 20) (draws (make-random-source 2) 20))))
  (test-equal "state round-trips"
    (let ([a (make-random-source 7)])
      (draws a 5)
      (let ([state (random-source-state-ref a)]
            [b (make-random-source 99)])
        (random-source-state-set! b state)
        (list (draws a 10) (= state (random-source-state-ref b)))))
    (let ([a (make-random-source 7)])
      (draws a 5)
      (let ([b (make-random-source 99)])
        (random-source-state-set! b (random-source-state-ref a))
        (list (draws b 10) #t))))
  (test-assert "state is a nonnegative 256-bit integer"
    (let ([state (random-source-state-ref (make-random-source 3))])
      (and (>= state 0) (< state (expt 2 256)))))
  (test-equal "pseudo-randomize! with indices is deterministic"
    (let ([s (make-random-source)]) (random-source-pseudo-randomize! s 1 2) (draws s 5))
    (let ([s (make-random-source)]) (random-source-pseudo-randomize! s 1 2) (draws s 5))))

;; A slow reference xoshiro256** on exact integers, to check the u64 lane of
;; the optimizing JIT against.
(define m64 (- (expt 2 64) 1))
(define (rotl x k) (bitwise-and m64 (bitwise-ior (bitwise-arithmetic-shift-left x k)
                                                  (bitwise-arithmetic-shift-right x (- 64 k)))))
(define (reference-next! s)
  (let* ([s0 (vector-ref s 0)] [s1 (vector-ref s 1)] [s2 (vector-ref s 2)] [s3 (vector-ref s 3)]
         [result (bitwise-and m64 (* 9 (rotl (bitwise-and m64 (* s1 5)) 7)))]
         [t (bitwise-and m64 (bitwise-arithmetic-shift-left s1 17))]
         [s2 (bitwise-xor s2 s0)]
         [s3 (bitwise-xor s3 s1)]
         [s1 (bitwise-xor s1 s2)]
         [s0 (bitwise-xor s0 s3)]
         [s2 (bitwise-xor s2 t)]
         [s3 (rotl s3 45)])
    (vector-set! s 0 s0) (vector-set! s 1 s1) (vector-set! s 2 s2) (vector-set! s 3 s3)
    result))
(define (state-of words)
  (let loop ([ws (reverse words)] [acc 0])
    (if (null? ws) acc (loop (cdr ws) (bitwise-ior (bitwise-arithmetic-shift-left acc 64) (car ws))))))

(test-group "xoshiro256**"
  (test-equal "reference outputs from {1, 2, 3, 4}"
    '(11520 0 1509978240 1215971899390074240)
    (let ([s (make-random-source)])
      (random-source-state-set! s (state-of '(1 2 3 4)))
      (let ([next (random-source-make-integers s)])
        (map (lambda (i) (next (expt 2 64))) '(0 1 2 3)))))
  (test-equal "splitmix64 expands seed 0"
    (state-of '(#xe220a8397b1dcdaf #x6e789e6aa1b965f4 #x06c45d188009454f #xf88bb8a8724c81ec))
    (random-source-state-ref (make-random-source 0)))
  (test-equal "hot draws match the reference"
    0
    (let* ([s (make-random-source 99)]
           [next (random-source-make-integers s)]
           [words (random-source-state-ref s)]
           [ref (vector (bitwise-and words m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 64) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 128) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 192) m64))])
      (let loop ([i 0] [bad 0])
        (if (= i 50000)
          bad
          (loop (+ i 1) (if (= (next (expt 2 64)) (reference-next! ref)) bad (+ bad 1)))))))
  (test-equal "hot reals match the reference"
    0
    (let* ([s (make-random-source 7)]
           [real (random-source-make-reals s)]
           [words (random-source-state-ref s)]
           [ref (vector (bitwise-and words m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 64) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 128) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 192) m64))])
      (let loop ([i 0] [bad 0])
        (if (= i 50000)
          bad
          (let ([want (* (+ (inexact (bitwise-arithmetic-shift-right (reference-next! ref) 11)) 0.5)
                         (expt 2.0 -53))])
            (loop (+ i 1) (if (= (real) want) bad (+ bad 1))))))))
  (test-equal "hot small integers match the reference"
    0
    (let* ([s (make-random-source 3)]
           [next (random-source-make-integers s)]
           [words (random-source-state-ref s)]
           [ref (vector (bitwise-and words m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 64) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 128) m64)
                        (bitwise-and (bitwise-arithmetic-shift-right words 192) m64))]
           [n 1000003])
      (define (want)
        (let* ([m (* (bitwise-arithmetic-shift-right (reference-next! ref) 33) n)]
               [low (bitwise-and m #x7fffffff)])
          (if (and (< low n) (< low (mod (- (expt 2 31) n) n)))
            (want)
            (bitwise-arithmetic-shift-right m 31))))
      (let loop ([i 0] [bad 0])
        (if (= i 50000)
          bad
          (loop (+ i 1) (if (= (next n) (want)) bad (+ bad 1))))))))

(test-end "srfi 27")
