(define-library (srfi srfi-27)
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
  
  (import (rnrs)
          (core arithmetic)
          (scheme base)
          (only (capy) microsecond tuple-ref tuple-set! tuple tuple?))

  (begin 
    (define (fx64/wrapping+ a b)
      (let ((sum (+ a b)))
        (bitwise-and sum #xffffffffffffffff)))

    (define (%split-mix-next state)
      (let ([state (fx64/wrapping+ state #x9e3779b97f4a7c15)])
        (let* ([z (bitwise-xor state (bitwise-arithmetic-shift-right state 30))]
              [z (* z #xbf58476d1ce4e5b9)]
              [z (bitwise-xor z (bitwise-arithmetic-shift-right z 27))]
              [z (* z #x94d049bb133111eb)]
              [result (bitwise-xor z (bitwise-arithmetic-shift-right z 31))])
          (values result state))))

    (define-record-type <split-mix> 
      (make-split-mix state)
      split-mix?
      (state split-mix-state split-mix-state-set!))

    (define (split-mix-next state)
      (call-with-values
          (lambda () (%split-mix-next (split-mix-state state)))
        (lambda (result new-state)
          (split-mix-state-set! state new-state)
          result)))


    (define (make-xoshiro256 smix)
      (define s0 (split-mix-next smix))
      (define s1 (split-mix-next smix))
      (define s2 (split-mix-next smix))
      (define s3 (split-mix-next smix))

      (tuple 'type:xoshiro256 s0 s0 s1 s2 s3))

    (define (rol64 x k)
      (bitwise-and (bitwise-ior (bitwise-arithmetic-shift-left x k)
                                (bitwise-arithmetic-shift-right x (- 64 k)))
                    #xffffffffffffffff))

    (define (xoshiro256-next state)
      (let* ((s0 (tuple-ref state 1))
            (s1 (tuple-ref state 2))
            (s2 (tuple-ref state 3))
            (s3 (tuple-ref state 4))
            (result (rol64 (fx64/wrapping+ s0 s3) 23)))
        (let* ((t (bitwise-arithmetic-shift-left s1 17))
              (s2 (bitwise-xor s2 s0))
              (s3 (bitwise-xor s3 s1))
              (s1 (bitwise-xor s1 s2))
              (s0 (bitwise-xor s0 s3))
              (s2 (bitwise-xor s2 t))
              (s3 (rol64 s3 45)))
          (tuple-set! state 1 s0)
          (tuple-set! state 2 s1)
          (tuple-set! state 3 s2)
          (tuple-set! state 4 s3)
          result)))

    (define (make-random-source)
      (make-xoshiro256 (make-split-mix (microsecond))))
    
    (define (random-source? x)
      (and (tuple? x)
           (eq? (tuple-ref x 0) 'type:xoshiro256)))
      
    (define (random-source-state-ref rs)
      (unless (random-source? rs)
        (error "random-source-state-ref: not a random source" rs))
      (bitwise-ior (tuple-ref rs 1) (bitwise-arithmetic-shift-left (tuple-ref rs 2) 64)
                   (bitwise-arithmetic-shift-left (tuple-ref rs 3) 128)
                   (bitwise-arithmetic-shift-left (tuple-ref rs 4) 192)))
    
    (define (random-source-state-set! rs state)
      (unless (random-source? rs)
        (error "random-source-state-set!: not a random source" rs))
      (define s0 (bitwise-and state #xffffffffffffffff))
      (define s1 (bitwise-and (bitwise-arithmetic-shift-right state 64) #xffffffffffffffff))
      (define s2 (bitwise-and (bitwise-arithmetic-shift-right state 128) #xffffffffffffffff))
      (define s3 (bitwise-and (bitwise-arithmetic-shift-right state 192) #xffffffffffffffff))
      (tuple-set! rs 1 s0)
      (tuple-set! rs 2 s1)    
      (tuple-set! rs 3 s2)
      (tuple-set! rs 4 s3))

    (define (random-source-randomize! rs)
      (unless (random-source? rs)
        (error "random-source-randomize!: not a random source" rs))
      (define sm (make-split-mix (microsecond)))
      (define new-xoshiro (make-xoshiro256 sm))
      (tuple-set! rs 1 (tuple-ref new-xoshiro 1))
      (tuple-set! rs 2 (tuple-ref new-xoshiro 2))
      (tuple-set! rs 3 (tuple-ref new-xoshiro 3))
      (tuple-set! rs 4 (tuple-ref new-xoshiro 4)))

  (define (random-source-pseudo-randomize! rs)
    (unless (random-source? rs)
      (error "random-source-pseudo-randomize!: not a random source" rs))
    (define state (random-source-state-ref rs))
    (random-source-state-set! rs (+ state #x9e3779b97f4a7c15)))
  
  (define (make-integer-rng state)
    (lambda (n)
      (if (<= n 18446744073709551616)
        (let () 
          (define limit (- 18446744073709551616 (modulo (- 18446744073709551616) n)))
          (let loop ()
            (define temp (xoshiro256-next state))
            (if (>= temp limit)
              (loop)
              (modulo temp n)))
        )
        (let () 
          (define rn-count (div (+ (bitwise-length n) 63) 64))
          (define rn-range (- (expt 2 (* rn-count 64)) 1))
          (define quo (div rn-range n))
          (define limit (* n quo))
          (define large-rng
            (lambda () 
              (let loop ([acc 0] [i 0])
                (if (>= i rn-count)
                  acc
                  (loop (fx64/wrapping+ (bitwise-ior (bitwise-arithmetic-shift-left acc 64)
                                                     (xoshiro256-next state))
                                        0)
                        (+ i 1))))))
          (let loop ([temp (large-rng)])
            (if (>= temp limit)
              (loop (large-rng))
              (div temp quo)))))))
  
  (define (make-real-rng state)
    (lambda ()
      (let* ((rnd-int (xoshiro256-next state))
             (rnd-real (/ rnd-int 18446744073709551616.0)))
        rnd-real)))
  (define (random-source-make-integers s) (make-integer-rng s))
  (define (random-source-make-reals s . unit) (make-real-rng s))
  (define default-random-source (make-random-source))
  (define random-integer (random-source-make-integers default-random-source))
  (define random-real (random-source-make-reals default-random-source))))