;; 32-bit word arithmetic on fixnums.
;;
;; Fixnums on this platform are 32-bit signed (range [-2^31, 2^31)), so an
;; unsigned 32-bit word does not fit. A word is therefore held as its signed
;; two's-complement value, and every operation below keeps intermediate
;; values inside the fixnum range by working on 16-bit halves.
;;
;; The rotate and shift macros take the rotation count as a literal and fold
;; their masks at expansion time, so each use costs only the runtime ops.

(library (capy crypto u32)
  (export u32+ u32-rotl u32-rotr u32-shr u32->signed)
  (import (rnrs)
    (for (rnrs syntax-case) expand))

  ;; Unsigned 32-bit constant -> stored signed word.
  (define (u32->signed k)
    (if (>= k #x80000000) (- k #x100000000) k))

  ;; Build a word from a high part (any integer in fixnum range, taken mod
  ;; 2^16) and a low part in [0, 2^16).
  (define-syntax %u32-pack
    (syntax-rules ()
      [(_ hi lo)
        (let ([h (fx- (fxand (fx+ hi #x8000) #xffff) #x8000)])
          (fxior (fxarithmetic-shift-left h 16) (fxand lo #xffff)))]))

  ;; Sum of any number (at least two) of words, modulo 2^32.
  (define-syntax u32+
    (lambda (stx)
      (syntax-case stx ()
        [(k e ...)
          (let* ([n (length (syntax->datum #'(e ...)))]
                 [names (let loop ([i 0] [acc '()])
                          (if (= i n)
                            (reverse acc)
                            (loop (+ i 1)
                              (cons (string->symbol (string-append "t" (number->string i))) acc))))]
                 [fold (lambda (op xs)
                         (let loop ([acc (car xs)] [rest (cdr xs)])
                           (if (null? rest)
                             acc
                             (loop (list op acc (car rest)) (cdr rest)))))]
                 [lo-expr (fold 'fx+ (map (lambda (t) (list 'fxand t #xffff)) names))]
                 [hi-expr (fold 'fx+ (map (lambda (t) (list 'fxarithmetic-shift-right t 16)) names))])
            (with-syntax ([(t ...) (map (lambda (name) (datum->syntax #'k name)) names)]
                          [lo-sum (datum->syntax #'k lo-expr)]
                          [hi-sum (datum->syntax #'k hi-expr)])
              #'(let ([t e] ...)
                  (let* ([lo lo-sum]
                         [hi hi-sum])
                    (%u32-pack (fx+ hi (fxarithmetic-shift-right lo 16)) lo)))))])))

  ;; Logical right shift, 0 < n < 32.
  (define-syntax u32-shr
    (lambda (stx)
      (syntax-case stx ()
        [(k x n)
          (let* ([kv (syntax->datum #'n)]
                 [mask (- (expt 2 (- 32 kv)) 1)])
            (with-syntax ([m (datum->syntax #'k mask)]
                          [nn (datum->syntax #'k kv)])
              #'(fxand (fxarithmetic-shift-right x nn) m)))])))

  ;; Rotate right, 0 < n < 32.
  (define-syntax u32-rotr
    (lambda (stx)
      (syntax-case stx ()
        [(k x n)
          (let* ([kv (syntax->datum #'n)]
                 [rmask (- (expt 2 (- 32 kv)) 1)]
                 [lmask (- (expt 2 kv) 1)]
                 [lsh (- 31 kv)])
            (with-syntax ([rm (datum->syntax #'k rmask)]
                          [lm (datum->syntax #'k lmask)]
                          [ls (datum->syntax #'k lsh)]
                          [nn (datum->syntax #'k kv)])
              #'(let* ([v x]
                       [right (fxand (fxarithmetic-shift-right v nn) rm)]
                       [z (fxarithmetic-shift-left (fxand v lm) ls)]
                       [zz (fxarithmetic-shift-left (fxand z #x3fffffff) 1)])
                  (fxior (if (fx=? 0 (fxand z #x40000000))
                           zz
                           (fx- (fx- zz #x40000000) #x40000000))
                    right))))])))

  ;; Rotate left, 0 < n < 32.
  (define-syntax u32-rotl
    (lambda (stx)
      (syntax-case stx ()
        [(k x n)
          (let ([kv (syntax->datum #'n)])
            (with-syntax ([m (datum->syntax #'k (- 32 kv))])
              #'(u32-rotr x m)))])))
  )
