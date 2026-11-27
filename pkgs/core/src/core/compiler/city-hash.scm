;; The hash a symbol carries in the runtime: `Symbol.hash' of
;; runtime/objectmodel/symbols.rs, which is simplehash's CityHasher64 over the
;; symbol's characters, each written as a native-endian (little endian) u32.
;; Compiled `switch-symbol' tables hold these hashes, so the Scheme compiler
;; has to compute exactly the same u64.  (`symbol-hash' is a different
;; function: a Murmur mix of this one.)

(library (core compiler city-hash)
  (export symbol-raw-hash city-hash-64)
  (import (core) (capy))

  (define mask64 #xFFFFFFFFFFFFFFFF)
  (define k0 #xc3a5c85c97cb3127)
  (define k1 #xb492b66fbe98f273)
  (define k2 #x9ae16a3b2f90404f)

  (define (w+ . xs) (bitwise-and (apply + xs) mask64))
  (define (w* a b) (bitwise-and (* a b) mask64))
  (define (rotate v s)
    (bitwise-and (bitwise-ior (bitwise-arithmetic-shift-right v s)
                   (bitwise-arithmetic-shift-left v (- 64 s)))
      mask64))
  (define (shift-mix v) (bitwise-xor v (bitwise-arithmetic-shift-right v 47)))

  (define (fetch64 bv i) (bytevector-u64-ref bv i 'little))
  (define (fetch32 bv i) (bytevector-u32-ref bv i 'little))

  (define (hash-len-16-mul u v mul)
    (let* ([a (w* (bitwise-xor u v) mul)]
           [a (bitwise-xor a (bitwise-arithmetic-shift-right a 47))]
           [b (w* (bitwise-xor v a) mul)]
           [b (bitwise-xor b (bitwise-arithmetic-shift-right b 47))])
      (w* b mul)))

  (define (hash-small bv)
    (let ([len (bytevector-length bv)])
      (cond
        [(>= len 8)
         (let* ([mul (w+ k2 (* len 2))]
                [a (w+ (fetch64 bv 0) k2)]
                [b (fetch64 bv (- len 8))]
                [c (w+ (w* (rotate b 37) mul) a)]
                [d (w* (w+ (rotate a 25) b) mul)])
           (hash-len-16-mul c d mul))]
        [(>= len 4)
         (let* ([mul (w+ k2 (* len 2))]
                [a (fetch32 bv 0)]
                [b (fetch32 bv (- len 4))])
           (hash-len-16-mul (w+ len (bitwise-arithmetic-shift-left a 3)) b mul))]
        [(> len 0)
         (let* ([a (bytevector-u8-ref bv 0)]
                [b (bytevector-u8-ref bv (bitwise-arithmetic-shift-right len 1))]
                [c (bytevector-u8-ref bv (- len 1))]
                [y (w+ a (bitwise-arithmetic-shift-left b 8))]
                [z (w+ len (bitwise-arithmetic-shift-left c 2))])
           (w* (shift-mix (bitwise-xor (w* y k2) (w* z k0))) k2))]
        [else k2])))

  (define (hash-medium bv)
    (let* ([len (bytevector-length bv)]
           [mul (w+ k2 (* len 2))]
           [a (w* (fetch64 bv 0) k1)]
           [b (fetch64 bv 8)]
           [c (w* (fetch64 bv (- len 8)) mul)]
           [d (w* (fetch64 bv (- len 16)) k2)]
           [s1 (w+ (rotate (w+ a b) 43) (rotate c 30) d)]
           [s2 (w+ a (rotate (w+ b k2) 18) c)])
      (hash-len-16-mul s1 s2 mul)))

  ;; => (values first second)
  (define (weak-hash-32 bv start len)
    (let* ([a (fetch64 bv start)]
           [b (fetch64 bv (+ start 8))]
           [c (fetch64 bv (+ start 16))]
           [d (fetch64 bv (+ start 24))]
           [a (w+ a (fetch64 bv start))]
           [b (rotate (w+ b a d) 21)]
           [c (w+ c a)]
           [a (w+ a (w+ (rotate a 44) b))]
           [vf (w+ a d)]
           [vs (w+ c (rotate b 10))])
      ;; the Rust version only takes this branch when the slice it is given
      ;; has at least 40 bytes, i.e. for the whole input and not for the tail
      (if (and (> len 48) (>= (- (bytevector-length bv) start) 40))
        (let* ([vf (w+ vf (w+ (w* (shift-mix (w* (fetch64 bv (+ start 32)) k2)) k0) a))]
               [vs (w+ vs (w* (shift-mix (w+ c (fetch64 bv (+ start 8 32)))) k2))])
          (values (shift-mix vf) (shift-mix vs)))
        (values vf vs))))

  (define (hash-large bv)
    (let* ([len (bytevector-length bv)]
           [x (w* (fetch64 bv 0) k2)]
           [y (fetch64 bv 8)]
           [z (w* (fetch64 bv (- len 8)) k2)])
      (let-values ([(v0 v1) (weak-hash-32 bv 0 len)]
                   [(w0 w1) (weak-hash-32 bv (- len 32) len)])
        ;; the tail slice is exactly 32 bytes long: no second iteration
        (let* ([x (w+ (w* x k2) (fetch64 bv 16))]
               [y (w+ y (w* (rotate x 48) k2) (fetch64 bv 24))]
               [z (w+ (w* z k2) (fetch64 bv (- len 16)))]
               [v0 (w+ (w* v0 k2) w1)]
               [v1 (w+ (w* v1 k2) w0)]
               [a (w+ (w* (w+ y z) k2) v0 w0)]
               [b (w+ (w* (w+ v1 w1) k2) x y)])
          (hash-len-16-mul a b k2)))))

  (define (city-hash-64 bv)
    (let ([len (bytevector-length bv)])
      (cond [(<= len 16) (hash-small bv)]
            [(<= len 32) (hash-medium bv)]
            [else (hash-large bv)])))

  (define (symbol-raw-hash sym)
    (let* ([s (symbol->string sym)]
           [n (string-length s)]
           [bv (make-bytevector (* 4 n) 0)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (bytevector-u32-set! bv (* 4 i) (char->integer (string-ref s i)) 'little))
      (if (= n 0) 0 (city-hash-64 bv))))
)
