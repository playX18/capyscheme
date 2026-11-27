;; 32-bit word arithmetic on fixnums.
;;
;; Fixnums on this platform are 32-bit signed (range [-2^31, 2^31)), so an
;; unsigned 32-bit word does not fit. A word is therefore held as its signed
;; two's-complement value. The wraparound natives (`fx+/wraparound`,
;; `fxsrl`, `fxrotr32`) work on that 32-bit pattern directly, and the Opt
;; JIT inlines each of them to a single machine instruction when the shift
;; count is a literal.

(library (capy crypto u32)
  (export u32+ u32-rotl u32-rotr u32-shr u32->signed)
  (import (rnrs)
    (only (capy) fx+/wraparound fxsrl fxrotr32))

  ;; Unsigned 32-bit constant -> stored signed word.
  (define (u32->signed k)
    (if (>= k #x80000000) (- k #x100000000) k))

  ;; Sum of any number (at least two) of words, modulo 2^32.
  (define-syntax u32+
    (syntax-rules ()
      [(_ a b) (fx+/wraparound a b)]
      [(_ a b c ...) (u32+ (fx+/wraparound a b) c ...)]))

  ;; Logical right shift, 0 < n < 32.
  (define-syntax u32-shr
    (syntax-rules ()
      [(_ x n) (fxsrl x n)]))

  ;; Rotate right, 0 < n < 32.
  (define-syntax u32-rotr
    (syntax-rules ()
      [(_ x n) (fxrotr32 x n)]))

  ;; Rotate left, 0 < n < 32.
  (define-syntax u32-rotl
    (lambda (stx)
      (syntax-case stx ()
        [(k x n)
          (with-syntax ([m (datum->syntax #'k (- 32 (syntax->datum #'n)))])
            #'(fxrotr32 x m))])))
  )
