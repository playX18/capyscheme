(import (rnrs) (srfi 64)
  (only (capy) u64+/wraparound u64-/wraparound u64*/wraparound u64sll/wraparound
    u64srl u64rotl u64rotr u64->flonum))
;; The u64 lane of the optimizing tier, checked against exact-integer
;; arithmetic. Each check runs 20000 times, so the procedures reach Tier 2
;; and their inline lowerings (and deopts) are what is tested. The first
;; mismatch of every check is kept.
(test-begin "opt u64 lane")
(define failures '())
(define (check name got want)
  (unless (or (equal? got want) (assq name failures))
    (set! failures (cons (list name got want) failures))))
(define-syntax raises?
  (syntax-rules () [(_ e) (guard (c [#t #t]) e #f)]))

(define m64 (- (expt 2 64) 1))
(define (wrap x) (bitwise-and x m64))
(define (rotl x k) (wrap (bitwise-ior (bitwise-arithmetic-shift-left x k)
                                      (bitwise-arithmetic-shift-right x (- 64 k)))))

;; Words on both sides of 2^63, 2^32 and 2^31, so the fixnum, one-limb and
;; top-bit-set cases of boxing and unboxing all come up.
(define words
  (vector 0 1 5 #x7fffffff #x80000000 #xffffffff #x100000000
    #x7fffffffffffffff #x8000000000000000 #x8000000000000001
    #xfffffffffffffffe m64 #x9e3779b97f4a7c15 #x0123456789abcdef))

(define bv (make-bytevector 16 0))

(define (run i)
  (let ([a (vector-ref words (mod i (vector-length words)))]
        [b (vector-ref words (mod (* 7 i) (vector-length words)))])
    (check 'add (u64+/wraparound a b) (wrap (+ a b)))
    (check 'sub (u64-/wraparound a b) (wrap (- a b)))
    (check 'mul (u64*/wraparound a b) (wrap (* a b)))
    (check 'sll (u64sll/wraparound a 17) (wrap (bitwise-arithmetic-shift-left a 17)))
    (check 'srl (u64srl a 11) (bitwise-arithmetic-shift-right a 11))
    (check 'rotl (u64rotl a 45) (rotl a 45))
    (check 'rotr (u64rotr a 7) (rotl a 57))
    (check 'chain (u64*/wraparound (u64rotl (u64*/wraparound a 5) 7) 9)
      (wrap (* 9 (rotl (wrap (* a 5)) 7))))
    (check 'xor-lane (bitwise-xor (u64+/wraparound a 0) b) (bitwise-xor a b))
    (check 'and-lane (bitwise-and (u64srl a 1) b) (bitwise-and (bitwise-arithmetic-shift-right a 1) b))
    (check 'ior-lane (bitwise-ior (u64sll/wraparound a 1) 1) (bitwise-ior (wrap (* a 2)) 1))
    (check 'ash-lane (bitwise-arithmetic-shift-right (u64+/wraparound a 0) 3)
      (bitwise-arithmetic-shift-right a 3))
    (check 'flonum (u64->flonum a) (inexact a))
    (check 'flonum-xor (u64->flonum (bitwise-xor (u64+/wraparound a 0) b)) (inexact (bitwise-xor a b)))
    (bytevector-u64-set! bv 0 a (endianness little))
    (bytevector-u64-set! bv 8 b (endianness big))
    (check 'le-load (bytevector-u64-ref bv 0 (endianness little)) a)
    (check 'be-load (bytevector-u64-ref bv 8 (endianness big)) b)
    (check 'be-bytes (bytevector-u8-ref bv 15) (bitwise-and b #xff))
    (check 'native (bytevector-u64-native-ref bv 0) (bytevector-u64-ref bv 0 (native-endianness)))
    (bytevector-u64-native-set! bv 0 b)
    (check 'native-set (bytevector-u64-native-ref bv 0) b)
    ;; Wrong operands deopt and raise like the generic procedures.
    (check 'neg-raises (raises? (u64+/wraparound -1 a)) #t)
    (check 'big-raises (raises? (u64*/wraparound (expt 2 64) a)) #t)
    (check 'flo-raises (raises? (u64srl 1.0 3)) #t)
    (check 'set-range (raises? (bytevector-u64-set! bv 9 a (endianness little))) #t)
    (check 'set-neg (raises? (bytevector-u64-set! bv 0 -1 (endianness little))) #t)))

(do ([i 0 (+ i 1)]) ((= i 20000)) (run i))

;; Words that reach a xor chain through a join (the `if`), where the builder
;; cannot see them and picks the signed lane; simplification moves the chain
;; to the u64 lane. A word at or above 2^63 must come out right either way.
(define (joined a b flip)
  (let* ([x (if flip (u64+/wraparound a 0) (u64+/wraparound b 0))]
         [y (if flip (u64+/wraparound b 0) (u64+/wraparound a 0))]
         [p (bitwise-xor x y)]
         [q (bitwise-xor p x)])
    (bitwise-xor q (u64sll/wraparound y 1))))
(do ([i 0 (+ i 1)]) ((= i 20000))
  (let* ([a (vector-ref words (mod i (vector-length words)))]
         [b (vector-ref words (mod (* 5 i) (vector-length words)))]
         [flip (odd? i)]
         [x (if flip a b)]
         [y (if flip b a)])
    (check 'joined (joined a b flip)
      (bitwise-xor (bitwise-xor (bitwise-xor x y) x) (wrap (* y 2))))))

;; Enough doubles live across a call that some spill, so float arithmetic
;; runs with both operands and the result in stack slots.
(define (opaque x) x)
(define (spilled x)
  (let* ([a (+ x 0.5)] [b (* a 1.25)] [c (- b 0.75)] [d (+ c a)] [e (* d b)]
         [f (+ e 0.125)] [g (- f c)] [h (* g 0.5)] [k (+ h d)] [l (* k 1.5)]
         [m (+ l e)] [n (- m f)] [o (* n g)] [p (+ o h)] [q (* p k)]
         [r (+ q l)] [s (opaque (- r m))])
    (+ a b c d e f g h k l m n o p q r s)))
(define want (map spilled '(1.0 2.0 3.0)))
(do ([i 0 (+ i 1)]) ((= i 20000))
  (check 'spilled-float (map spilled '(1.0 2.0 3.0)) want))

(test-equal "no mismatches" '() failures)
(test-end "opt u64 lane")
