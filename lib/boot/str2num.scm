; Copyright Lightship Software, Incorporated.
;
; $Id$
;
; A parser for numeric constants in CapyScheme.
; Accepts the union of R6RS and ERR5RS syntax.
;
; The differences between R6RS and ERR5RS syntax are:
;
;     ERR5RS does not allow the imaginary part of a
;         rectangular notation to be an infinity or NaN
;
;     ERR5RS does not allow mantissa widths
;
;     ERR5RS allows trailing decimal digits to be #
;
;     ERR5RS allows 3+4I.  Despite an erratum, it is
;         still unclear whether the R6RS allows that.
;         It is also unclear whether the R6RS allows
;         +NaN.0 or -InF.0.
;
; ERR5RS also allows extensions, but the R6RS doesn't.
;
; Uses a procedure named bellerophon, which should implement
; Algorithm Bellerophon for reading floating point numbers perfectly.
;
; The following syntax is from the R6RS, without ERR5RS
; extensions.
;
; <number> -> <num 2> | <num 8>
;          | <num 10> | <num 16>
; <num R> -> <prefix R> <complex R>
; <complex R> -> <real R> | <real R> @ <real R>
;          | <real R> + <ureal R> i | <real R> - <ureal R> i
;          | <real R> + <naninf> i | <real R> - <naninf> i
;          | <real R> + i | <real R> - i
;          | + <ureal R> i | - <ureal R> i
;          | + <naninf> i | - <naninf> i
;          | + i | - i
; <real R> -> <sign> <ureal R>
;          | + <naninf> | - <naninf>
; <naninf> -> nan.0 | inf.0
; <ureal R> -> <uinteger R>
;          | <uinteger R> / <uinteger R>
;          | <decimal R> <mantissa width>
; <decimal 10> -> <uinteger 10> <suffix>
;          | . <digit 10>+ <suffix>
;          | <digit 10>+ . <digit 10>* <suffix>
;          | <digit 10>+ . <suffix>
; <uinteger R> -> <digit R>+
; <prefix R> -> <radix R> <exactness>
;          | <exactness> <radix R>
;
; <suffix> -> <empty>
;          | <exponent marker> <sign> <digit 10>+
; <exponent marker> -> e | E | s | S | f | F
;          | d | D | l | L
; <mantissa width> -> <empty>
;          | | <digit 10>+
; <sign> -> <empty> | + | -
; <exactness> -> <empty>
;          | #i| #I | #e| #E
; <radix 2> -> #b| #B
; <radix 8> -> #o| #O
; <radix 10> -> <empty> | #d | #D
; <radix 16> -> #x| #X
; <digit 2> -> 0 | 1
; <digit 8> -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
; <digit 10> -> <digit>
; <digit 16> -> <hex digit>

(define make-flonum
  (let ([two^52 4503599627370496]
        [two^63 9223372036854775808])
    (lambda (s m e)
      (let ([t (+ (if (zero? s) 0 two^63)
                (* (+ e 1023) two^52)
                (remainder m two^52))])
        (bits->flonum t)))))

(define bellerophon
  (let ()

    (define (bellerophon f e)
      (cond ((negative? f) (- (bellerophon (- f) e)))
        ((zero? f) flonum:zero)
        ((<= bellerophon:big-f f) (fail0 f e))
        ((< e bellerophon:small-e) (fail0 f e))
        ((< bellerophon:big-e e) flonum:infinity)
        ((and (< f two^n) (>= e 0) (< e log5-of-two^n))
          (* (exact->inexact f)
            (exact->inexact (expt 10 e))))
        ((and (< f two^n) (< e 0) (< (- e) log5-of-two^n))
          (/ (exact->inexact f)
            (exact->inexact (expt 10 (- e)))))
        (else (multiply-and-test
               f
               e
               (cond ((< e -216) slop-216)
                 ((< e -108) slop-108)
                 ((< e -54) slop-54)
                 ((< e -27) slop-27)
                 ((< e 0) slop0)
                 ((<= e 27) slop27)
                 ((<= e 54) slop54)
                 ((<= e 108) slop108)
                 ((<= e 216) slop216)
                 (else slop324))))))

    (define (multiply-and-test f e slop)
      (let ((x (integer->extended f))
            (y (ten-to-e e)))
        (let ((z (extended-multiply x y)))
          (if (<= (abs (- (extended-lowbits z) two^p-n-1)) slop)
            (fail f e z)
            (extended->flonum z)))))

    (define (fail0 f e) (AlgorithmM f e))

    (define (fail f e z) (AlgorithmM f e))

    ; Powers of ten are computed from a small table containing
    ; the best extended precision approximations to
    ; 10^-216, 10^-108, 10^-54, 10^-27, 10^27, 10^54, 10^108, 10^216.

    ; The table of powers of ten; see assignments below.

    (define ten^-216 0)
    (define ten^-108 0)
    (define ten^-54 0)
    (define ten^-27 0)
    (define ten^27 0)
    (define ten^54 0)
    (define ten^108 0)
    (define ten^216 0)

    (define (ten-to-e e)
      (cond ((< e -432)
             (error "Impossible case 1 in bellerophon: " e)
             #t)
        ((< e -216)
          (extended-multiply ten^-216
            (extended-multiply ten^-216
              (ten-to-e (+ e 432)))))
        ((< e -108)
          (extended-multiply ten^-216 (ten-to-e (+ e 216))))
        ((< e -54)
          (extended-multiply ten^-108 (ten-to-e (+ e 108))))
        ((< e -27)
          (extended-multiply ten^-54 (ten-to-e (+ e 54))))
        ((< e 0)
          (extended-multiply ten^-27 (ten-to-e (+ e 27))))
        ((<= e 27) (integer->extended (expt 10 e)))
        ((<= e 54)
          (extended-multiply ten^27 (ten-to-e (- e 27))))
        ((<= e 108)
          (extended-multiply ten^54 (ten-to-e (- e 54))))
        ((<= e 216)
          (extended-multiply ten^108 (ten-to-e (- e 108))))
        ((<= e 324)
          (extended-multiply ten^216 (ten-to-e (- e 216))))
        (else
          (error "Impossible case 2 in bellerophon: " e)
          #t)))

    ; These slop factors assume that f can be represented exactly
    ; as an extended precision number, so the slop factor is exactly
    ; twice the maximum error in the approximation to 10^e.

    (define slop-216 45)
    (define slop-108 9)
    (define slop-54 3)
    (define slop-27 3)
    (define slop0 1)
    (define slop27 0)
    (define slop54 1)
    (define slop108 3)
    (define slop216 9)
    (define slop324 21)

    (define n 53)
    (define two^n-1 4503599627370496) ; (expt 2 (- n 1))
    (define two^n 9007199254740992) ; (expt 2 n)
    (define p 64)
    (define two^p-1 9223372036854775808) ; (expt 2 (- p 1))
    (define two^p 18446744073709551616) ; (expt 2 p)
    (define two^p-n-1 1024) ; (expt 2 (- p n 1))
    (define two^p-n 2048) ; (expt 2 (- p n))

    (define flonum:zero 0.0)
    (define flonum:infinity 1e500)
    (define flonum:minexponent -1023)
    (define flonum:minexponent-51 -1074)
    (define bellerophon:big-f 18446744073709551616) ; (expt 2 64)
    (define bellerophon:small-e -306)
    (define bellerophon:big-e 309)

    ; Precomputed so we don't have to rely on LOG in initialization phase.
    ;    (define log5-of-two^n (inexact->exact (ceiling (/ (log two^n) (log 5)))))
    (define log5-of-two^n 23)

    (define (slow-ten-to-e e)
      (define (loop1 y s guardbit)
        (cond ((< y two^p)
               (make-extended (if (zero? guardbit) y (+ y 1)) s))
          (else (loop1 (quotient y 2) (+ s 1) (remainder y 2)))))
      (define (loop2 y s)
        (cond ((<= two^p-1 y)
               (make-extended y s))
          (else (loop2 (* 2 y) (- s 1)))))
      (define (loop3 x y n)
        (cond ((>= x (* y two^p-1))
               (loop3-help x y (- n)))
          (else (loop3 (* 2 x) y (+ n 1)))))
      (define (loop3-help x y n)
        (let* ((q (quotient x y))
               (r (- x (* q y)))
               (y-r (- y r)))
          (make-extended (cond ((< r y-r) q)
                          ((> r y-r) (+ q 1))
                          ((zero? (remainder q 2)) q)
                          (else (+ q 1)))
            n)))
      (if (negative? e)
        (loop3 1 (expt 10 (- e)) 0)
        (let ((ten^e (expt 10 e)))
          (cond ((>= ten^e two^p)
                 (loop1 ten^e 0 0))
            (else (loop2 ten^e 0))))))

    ; Extended precision floating point, implemented entirely
    ; in Scheme for portability and ease of maintenance.
    ;
    ; The following operations are used directly by Algorithm Bellerophon:
    ;
    ;   (integer->extended m)
    ;   (make-extended m q)
    ;   (extended->flonum ex)
    ;   (multiply-extended ex1 ex2)
    ;   (extended-lowbits ex)
    ;
    ; All numbers are positive; negative numbers and zero are
    ; not supported.
    ;
    ; An extended is represented as a pair (x t) where x and t are
    ; exact integers.  Its value is x * 2^t.  A normalized extended
    ; is an extended for which 2^{p-1} <= x < 2^p.

    (define make-extended list)
    (define (extended-significand f) (car f))
    (define (extended-exponent f) (cadr f))

    ; This flag is set by some operations to indicate whether
    ; any accuracy was lost during the operation.

    (define inexact-flag #f)

    ; (expt 2 (- (* 2 p) 1))
    (define two^2p-1 170141183460469231731687303715884105728)

    ; (expt 2 (- (* 2 n) 1))
    (define two^2n-1 40564819207303340847894502572032)

    (define (integer->extended n)
      (cond ((< n two^p-1)
             (extended-normalize (make-extended n 0) two^p-1))
        ((>= n two^p)
          (extended-normalize-and-round (make-extended n 0) two^p))
        (else (make-extended n 0))))

    ; Given an extended whose significand is less than two^p-1,
    ; returns the normalized extended having the same value.
    ; Because two^p-1 is a parameter, this can be used for various
    ; precisions other than p.

    (define (extended-normalize f two^p-1)
      (let ((x (extended-significand f))
            (t (extended-exponent f)))
        (if (<= two^p-1 x)
          (begin (set! inexact-flag #f)
            (make-extended x t))
          (extended-normalize (make-extended (* 2 x) (- t 1)) two^p-1))))

    ; Given an extended whose significand is greater than two^p,
    ; returns the normalized extended closest to the given float,
    ; rounding to even in case of ties.
    ; two^p is a parameter so this can be used for different
    ; precisions.

    (define (extended-normalize-and-round f two^p)
      (do ((x (extended-significand f) (quotient x 2))
           (guard 0 (remainder x 2))
           (sticky 0 (max sticky guard))
           (t (extended-exponent f) (+ t 1)))
        ((< x two^p)
          ; The result is probably inexact.
          ; This setting will be changed if incorrect.
          (set! inexact-flag #t)
          (cond ((zero? guard)
                 (set! inexact-flag (not (zero? sticky)))
                 (make-extended x t))
            ((and (zero? sticky) (even? x))
              (make-extended x t))
            ((= x (- two^p 1))
              (make-extended (quotient two^p 2)
                (+ t 1)))
            (else (make-extended (+ x 1) t))))))

    ; Given an extended, round it to the nearest flonum
    ; (with n bits of precision instead of p).

    (define (extended->flonum f)
      (let ((ff (extended-normalize-and-round f two^n)))
        (make-float (extended-significand ff)
          (extended-exponent ff))))

    ; Given normalized extendeds, return their normalized product.

    (define (extended-multiply f1 f2)
      (let ((f (* (extended-significand f1)
                (extended-significand f2)))
            (t (+ (extended-exponent f1)
                (extended-exponent f2)
                p)))
        ; Set flag for most common case.
        (set! inexact-flag #t)
        (if (<= two^2p-1 f)
          (let ((q (quotient f two^p))
                (r (remainder f two^p)))
            (cond ((< r two^p-1)
                   (if (zero? r)
                     (set! inexact-flag #f))
                   (make-extended q t))
              ((> r two^p-1)
                (make-extended (+ q 1) t))
              ((even? q)
                (make-extended q t))
              (else (make-extended (+ q 1) t))))
          (let ((q (quotient f two^p-1))
                (r (remainder f two^p-1)))
            (cond ((< r two^p-1)
                   (if (zero? r)
                     (set! inexact-flag #f))
                   (make-extended q (- t 1)))
              ((> r two^p-1)
                (make-extended (+ q 1) (- t 1)))
              ((even? f)
                (make-extended q (- t 1)))
              ((= q (- two^p-1 1))
                (make-extended two^p-1 t))
              (else (make-extended (+ q 1) (- t 1))))))))

    (define (extended-lowbits ex)
      (remainder (extended-significand ex)
        two^p-n))

    ; End of extended precision number implementation.

    ; Backup algorithm.
    ; I'm using an extremely slow backup algorithm, mainly because
    ; the slow algorithm is needed anyway for denormalized numbers
    ; and I'm trying to keep things simple.

    ; Given exact integers f and e, with f nonnegative,
    ; returns the floating point number closest to f * 10^e.

    (define (AlgorithmM f e)

      ; f * 10^e = u/v * 2^k

      (define (loop u v k)
        (let ((x (quotient u v)))
          (cond ((and (<= two^n-1 x) (< x two^n))
                 (ratio->float u v k))
            ((< x two^n-1)
              (loop (* 2 u) v (- k 1)))
            ((<= two^n x)
              (loop u (* 2 v) (+ k 1))))))

      (if (negative? e)
        (loop f (expt 10 (- e)) 0)
        (loop (* f (expt 10 e)) 1 0)))

    ; Given exact positive integers p and q with
    ; 2^(n-1) <= u/v < 2^n, and exact integer k,
    ; returns the float closest to u/v * 2^k.

    (define (ratio->float u v k)
      (let* ((q (quotient u v))
             (r (- u (* q v)))
             (v-r (- v r)))
        (cond ((< r v-r) (make-float q k))
          ((> r v-r) (make-float (+ q 1) k))
          ((zero? (remainder q 2)) (make-float q k))
          (else (make-float (+ q 1) k)))))

    ; END OF ALGORITHM MultiplyByTwos

    ; Primitive operations on flonums.

    (define (make-float m q)
      (if (< q flonum:minexponent)
        (make-float (* .5 m) (+ q 1))
        (* (+ m 0.0) (expt 2.0 q))))

    ; (define (float-significand x)
    ;   (cond ((and (<= .5 x) (< x 1.0))
    ;          (inexact->exact (* x (expt 2.0 53))))
    ;         ((< x .5) (float-significand (* 2.0 x)))
    ;         ((<= 1.0 x) (float-significand (* .5 x)))))
    ;
    ; (define (float-exponent x)
    ;   (define (loop x k)
    ;     (cond ((and (<= .5 x) (< x 1.0)) (- k 53))
    ;           ((< x .5) (loop (* 2.0 x) (- k 1)))
    ;           ((<= 1.0 x) (loop (* .5 x) (+ k 1)))))
    ;   (loop x 0))

    ; slow-ten-to-e is _really_ slow in Larceny.
    ;
    ;    (set! ten^-216 (slow-ten-to-e -216))
    ;    (set! ten^-108 (slow-ten-to-e -108))
    ;    (set! ten^-54  (slow-ten-to-e -54))
    ;    (set! ten^-27  (slow-ten-to-e -27))
    ;    (set! ten^27   (slow-ten-to-e 27))
    ;    (set! ten^54   (slow-ten-to-e 54))
    ;    (set! ten^108  (slow-ten-to-e 108))
    ;    (set! ten^216  (slow-ten-to-e 216))

    ; precomputed by slow-ten-to-e
    (set! ten^-216 '(12718228212127407597 -781))
    (set! ten^-108 '(10830740992659433045 -422))
    (set! ten^-54 '(14134776518227074637 -243))
    (set! ten^-27 '(11417981541647679048 -153))
    (set! ten^27 '(14901161193847656250 26))
    (set! ten^54 '(12037062152420224082 116))
    (set! ten^108 '(15709099088952724970 295))
    (set! ten^216 '(13377742608693866209 654))

    bellerophon))
(define (string->number s . rest)

  (let ((radix (cond ((null? rest) #f)
                ((null? (cdr rest)) (car rest))
                (else (assertion-violation 'string->number
                       "too many arguments"
                       (cons s rest)))))
        (n (string-length s)))

    ;; This is a procedure because at the time we wish to call make-flonum,
    ;; generic arithmetic is not yet fully operational.

    (define (flonum:nan) (make-flonum 0 1 1024))

    ;; This is a procedure because flonum:nan is.

    (define (flonum:inf) 1e500)

    (define (flonum:zero) 1e-999)

    (define (decimal-digit? c)
      (char<=? #\0 c #\9))

    (define (decimal-value c)
      (- (char->integer c) (char->integer #\0)))

    (define (parse-number)
      (if (= n 0)
        #f
        (case (string-ref s 0)
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+)
            (parse-complex 0 #f (or radix 10)))
          ((#\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
            (case radix
              ((16) (parse-complex 0 #f 16))
              (else #f)))
          ((#\.)
            (case radix
              ((#f 10) (parse-complex 0 #f 10))
              (else #f)))
          ((#\#)
            (parse-prefix 0 #f #f))
          (else #f))))

    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, 16 or #f if no explicit radix prefix read yet

    (define (parse-prefix i exactness radix)
      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\#)
            (let ((i (+ i 1)))
              (if (>= i n)
                #f
                (case (string-ref s i)
                  ((#\e #\E)
                    (if exactness #f (parse-prefix (+ i 1) 'e radix)))
                  ((#\i #\I)
                    (if exactness #f (parse-prefix (+ i 1) 'i radix)))
                  ((#\b #\B)
                    (if radix #f (parse-prefix (+ i 1) exactness 2)))
                  ((#\o #\O)
                    (if radix #f (parse-prefix (+ i 1) exactness 8)))
                  ((#\d #\D)
                    (if radix #f (parse-prefix (+ i 1) exactness 10)))
                  ((#\x #\X)
                    (if radix #f (parse-prefix (+ i 1) exactness 16)))
                  (else #f)))))
          (else (parse-complex i exactness (or radix 10))))))

    ;; Prefix has been consumed, but not anything else.
    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, or 16

    (define (parse-complex i exactness radix)

      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\-)
            (parse-ucomplex (+ i 1) exactness radix -1))
          ((#\+)
            (parse-ucomplex (+ i 1) exactness radix 1))
          (else
            (call-with-values
              (lambda () (parse-ureal i exactness radix 1))
              (lambda (areal i)
                (and areal
                  (parse-imaginary i exactness radix areal))))))))

    ;; An explicit sign has just been read.
    ;; sign = 1 or -1

    (define (parse-ucomplex i exactness radix sign)
      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\i #\I) ; FIXME: does the R6RS allow #\I here?
            (cond ((= (+ i 1) n)
                   (coerce-exactness exactness (make-rectangular 0 sign)))
              ((and (<= (+ i 5) n)
                  (char=? #\n (string-ref s (+ i 1)))
                  (char=? #\f (string-ref s (+ i 2)))
                  (char=? #\. (string-ref s (+ i 3)))
                  (char=? #\0 (string-ref s (+ i 4))))
                (parse-imaginary2 (+ i 5)
                  exactness
                  radix
                  (* sign (flonum:inf))))
              (else #f)))
          ((#\n)
            (cond ((and (<= (+ i 5) n)
                     (char=? #\a (string-ref s (+ i 1)))
                     (char=? #\n (string-ref s (+ i 2)))
                     (char=? #\. (string-ref s (+ i 3)))
                     (char=? #\0 (string-ref s (+ i 4))))
                   ; FIXME
                   (parse-imaginary2 (+ i 5)
                     exactness
                     radix
                     (* sign (flonum:nan))))
              (else #f)))
          (else
            (call-with-values
              (lambda () (parse-ureal i exactness radix sign))
              (lambda (areal i)
                (and areal
                  (parse-imaginary2 i exactness radix areal))))))))

    ;; An unsigned real part has just been read.
    ;; Note that 45i is not allowed, but +45i is.

    (define (parse-imaginary i exactness radix areal)
      (if (>= i n)
        (coerce-exactness exactness areal)
        (case (string-ref s i)
          ((#\+)
            (parse-uimaginary (+ i 1) exactness radix areal 1))
          ((#\-)
            (parse-uimaginary (+ i 1) exactness radix areal -1))
          ((#\@)
            (parse-angle (+ i 1) exactness radix areal))
          (else #f))))

    ;; An explicitly signed real part has just been read.
    ;; parse-imaginary2 is like parse-imaginary except it
    ;; allows i as a terminating character.

    (define (parse-imaginary2 i exactness radix areal)
      (if (>= i n)
        (parse-imaginary i exactness radix areal)
        (case (string-ref s i)
          ((#\i #\I) ; FIXME
            (if (= (+ i 1) n)
              (coerce-exactness exactness
                (make-rectangular 0 areal))
              #f))
          (else
            (parse-imaginary i exactness radix areal)))))

    ;; The real part and an explicit sign have just been read.

    (define (parse-uimaginary i exactness radix areal sign)
      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\i #\I) ; FIXME
            (cond ((= (+ i 1) n)
                   (coerce-exactness exactness
                     (make-rectangular areal sign)))
              ((and (<= (+ i 6) n)
                  (char=? #\n (string-ref s (+ i 1)))
                  (char=? #\f (string-ref s (+ i 2)))
                  (char=? #\. (string-ref s (+ i 3)))
                  (char=? #\0 (string-ref s (+ i 4)))
                  (char=? #\i (string-ref s (+ i 5))))
                (coerce-exactness exactness
                  (make-rectangular areal
                    (* sign (flonum:inf)))))
              (else #f)))
          ((#\n)
            (cond ((and (<= (+ i 6) n)
                     (char=? #\a (string-ref s (+ i 1)))
                     (char=? #\n (string-ref s (+ i 2)))
                     (char=? #\. (string-ref s (+ i 3)))
                     (char=? #\0 (string-ref s (+ i 4)))
                     (char=? #\i (string-ref s (+ i 5))))
                   (coerce-exactness exactness
                     (make-rectangular areal
                       ; FIXME
                       (* sign (flonum:nan)))))
              (else #f)))
          (else
            (call-with-values
              (lambda () (parse-ureal i exactness radix sign))
              (lambda (imag i)
                (and imag
                  (= (+ i 1) n)
                  (char=? #\i (char-downcase (string-ref s i)))
                  (coerce-exactness exactness
                    (make-rectangular areal
                      imag)))))))))

    ;; The real part and an @-sign have just been read.

    (define (parse-angle i exactness radix areal)
      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\+)
            (parse-angle2 (+ i 1) exactness radix areal 1))
          ((#\-)
            (parse-angle2 (+ i 1) exactness radix areal -1))
          ((#\i #\I #\n #\N)
            #f)
          (else
            (parse-angle2 i exactness radix areal 1)))))

    ;; The real part and an @-sign have just been read.
    ;; Either an explicit sign has just been read, or
    ;; lookahead has determined that there is no sign.

    (define (parse-angle2 i exactness radix areal sign)
      (if (>= i n)
        #f
        (case (string-ref s i)
          ((#\i)
            (cond ((and (= (+ i 5) n)
                     (char=? #\n (string-ref s (+ i 1)))
                     (char=? #\f (string-ref s (+ i 2)))
                     (char=? #\. (string-ref s (+ i 3)))
                     (char=? #\0 (string-ref s (+ i 4))))
                   (coerce-exactness exactness
                     (make-polar areal (* sign (flonum:inf)))))
              (else #f)))
          ((#\n)
            (cond ((and (= (+ i 5) n)
                     (char=? #\a (string-ref s (+ i 1)))
                     (char=? #\n (string-ref s (+ i 2)))
                     (char=? #\. (string-ref s (+ i 3)))
                     (char=? #\0 (string-ref s (+ i 4))))
                   ; FIXME
                   (coerce-exactness exactness
                     (make-polar areal (* sign (flonum:nan)))))
              (else #f)))
          (else
            (call-with-values
              (lambda () (parse-ureal i exactness radix sign))
              (lambda (angle i)
                (and angle
                  (= i n)
                  (coerce-exactness exactness
                    (make-polar areal
                      angle)))))))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; parse-ureal
    ;;
    ;; This procedure is called during parsing of the real part,
    ;; imaginary part, and angle.
    ;;
    ;; exactness = the symbol e if #e has been read
    ;;             the symbol i if #i has been read
    ;;             otherwise #f
    ;; radix = 2, 8, 10, or 16
    ;; sign = 1 or -1
    ;;
    ;; Returns two values:
    ;;     #f or a real value parsed from s
    ;;     index of the first unconsumed character in s
    ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (parse-ureal i exactness radix sign)
      (if (= radix 10)
        (parse-ureal10 i exactness sign)
        (call-with-values
          (lambda () (parse-uinteger i radix sign))
          (lambda (numerator i)
            (cond ((not numerator)
                   (values #f i))
              ((and (< i n)
                  (char=? #\/ (string-ref s i)))
                (call-with-values
                  (lambda () (parse-uinteger (+ i 1) radix sign))
                  (lambda (denominator i)
                    (values (and denominator
                             (create-number exactness
                               sign
                               numerator
                               denominator
                               0))
                      i))))
              (else
                (values (create-number exactness sign numerator 1 0)
                  i)))))))

    (define (parse-uinteger i radix sign)
      (define (loop i k)
        (if (>= i n)
          (values k i)
          (let* ((c (string-ref s i)))
            (case c
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (let ((j (decimal-value c)))
                  (if (< j radix)
                    (loop (+ i 1) (+ (* radix k) j))
                    (values k i))))
              ((#\a #\b #\c #\d #\e #\f #\A #\B #\C #\D #\E #\F)
                (if (= radix 16)
                  (let ((j (+ 10 (- (char->integer (char-downcase c))
                                  (char->integer #\a)))))
                    (loop (+ i 1) (+ (* radix k) j)))
                  (values k i)))
              (else
                (values k i))))))
      (if (>= i n)
        (values #f i)
        (let ((c (string-ref s i)))
          (case radix
            ((2)
              (if (char<=? #\0 c #\1)
                (loop i 0)
                (values #f i)))
            ((8)
              (if (char<=? #\0 c #\7)
                (loop i 0)
                (values #f i)))
            ((10)
              (if (char<=? #\0 c #\9)
                (loop i 0)
                (values #f i)))
            ((16)
              (if (or (char<=? #\0 c #\9)
                   (char<=? #\a (char-downcase c) #\f))
                (loop i 0)
                (values #f i)))
            (else
              (values #f i))))))

    (define (parse-ureal10 i e sign)
      (if (>= i n)
        (values #f i)
        (let ((c (string-ref s i)))
          (cond ((char=? c #\.)
                 (let ((i (+ i 1)))
                   (if (and (< i n)
                        (decimal-digit? (string-ref s i)))
                     (parse-scientific-fraction i
                       (or e 'i)
                       sign
                       0
                       0
                       #f)
                     (values #f i))))
            ((decimal-digit? c)
              (parse-ureal10a (+ i 1) e sign (decimal-value c)))
            (else
              (values #f i))))))

    ;; A nonempty sequence of decimal digits has been read,
    ;; and their value is (* sign k).

    (define (parse-ureal10a i e sign k)
      (if (>= i n)
        (values (create-number e sign k 1 0) i)
        (let ((c (string-ref s i)))
          (if (decimal-digit? c)
            (parse-ureal10a (+ i 1)
              e
              sign
              (+ (* 10 k) (decimal-value c)))
            (parse-ureal10part2 i e sign k #f)))))

    (define (parse-ureal10part2 i e sign k sharps-only?)
      (if (>= i n)
        (values (create-number e sign k 1 0) i)
        (case (string-ref s i)
          ((#\#)
            (parse-ureal10part2 (+ i 1) (or e 'i) sign (* 10 k) #t))
          ((#\.)
            (parse-scientific-fraction (+ i 1)
              (or e 'i)
              sign
              k
              0
              sharps-only?))
          ((#\i #\I #\+ #\- #\@) ; FIXME
            (values (create-number e sign k 1 0) i))
          ((#\/)
            (call-with-values
              (lambda () (parse-uinteger (+ i 1) 10 1))
              (lambda (denominator i)
                (values (and denominator
                         (create-number e sign k denominator 0))
                  i))))
          ((#\d #\e #\f #\l #\s #\D #\E #\F #\L #\S)
            (let ((precision (char-downcase (string-ref s i))))
              (parse-exponent (+ i 1) (or e 'i) sign k 0 precision)))
          ((#\|)
            (parse-mantissawidth (+ i 1) (or e 'i) sign k 0 #f))
          (else
            (values (create-number e sign k 1 0) i)))))

    ;; A decimal point has been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; sharps-only? = a boolean

    (define (parse-scientific-fraction i e sign p exponent sharps-only?)
      (if (>= i n)
        (values (create-number e sign p 1 exponent) i)
        (let ((c (string-ref s i)))
          (case c
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
              (if sharps-only?
                (values (create-number e sign p 1 exponent) i)
                (let ((p (+ (* 10 p) (decimal-value c))))
                  (parse-scientific-fraction
                    (+ i 1)
                    e
                    sign
                    p
                    (- exponent 1)
                    sharps-only?))))
            ((#\#)
              (parse-scientific-fraction
                (+ i 1)
                e
                sign
                (* 10 p)
                (- exponent 1)
                #t))
            ((#\d #\e #\f #\l #\s #\D #\E #\F #\L #\S)
              (let ((precision (char-downcase (string-ref s i))))
                (parse-exponent (+ i 1) (or e 'i) sign p exponent precision)))
            ((#\|)
              (parse-mantissawidth (+ i 1) (or e 'i) sign p exponent #f))
            (else
              (values (create-number e sign p 1 exponent) i))))))

    ;; An exponent marker has been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; precision = #f or #\d or #\e or #\f or #\l or #\s
    ;;
    ;; FIXME: what happens to the precision when we create a number?

    (define (parse-exponent i e sign p exponent precision)
      (define (loop i k ksign)
        (if (>= i n)
          (values (create-number e sign p 1 (+ exponent (* ksign k)))
            i)
          (let ((c (string-ref s i)))
            (cond ((decimal-digit? c)
                   (loop (+ i 1)
                     (+ (* 10 k) (decimal-value c))
                     ksign))
              ((char=? c #\|)
                (let ((exponent (+ exponent (* ksign k))))
                  (parse-mantissawidth
                    (+ i 1)
                    e
                    sign
                    p
                    exponent
                    precision)))
              (else
                (values (create-number e sign
                         p
                         1
                         (+ exponent (* ksign k)))
                  i))))))
      (if (>= i n)
        (values #f i)
        (case (string-ref s i)
          ((#\+ #\-)
            (let* ((sign (string-ref s i))
                   (i (+ i 1)))
              (if (and (< i n)
                   (decimal-digit? (string-ref s i)))
                (if (and (char=? #\0 (string-ref s i))
                     (char=? sign #\-)
                     (eq? e 'i)
                     (= p 1)
                     (eq? 'extremely (larceny:r7strict)))
                  (call-with-values
                    (lambda () (loop i 0 -1))
                    (lambda (x i)
                      (values (if (= x (flonum:zero))
                               (create-number 'i -1 0 1 0)
                               x)
                        i)))
                  (loop i 0 (if (char=? sign #\+) 1 -1)))
                (values #f i))))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (loop i 0 1))
          (else
            (values #f i)))))

    ;; A vertical bar has just been read, and the value of what
    ;; has been read so far is (* sign p (expt 10 exponent)).
    ;;
    ;; e = the symbol e or the symbol i
    ;; sign = 1 or -1
    ;; p = an exact integer >= 0
    ;; exponent = an exact integer
    ;; precision = #f or #\d or #\e or #\f or #\l or #\s
    ;;
    ;; FIXME: what happens to the precision when we create a number?

    (define (parse-mantissawidth i e sign p exponent precision)
      (define (loop i k)
        (if (>= i n)
          (values (create-number e sign p 1 exponent)
            i)
          (let ((c (string-ref s i)))
            (cond ((decimal-digit? c)
                   (loop (+ i 1)
                     (+ (* 10 k) (decimal-value c))))
              (else
                ; FIXME: the mantissa width is k, but we ignore it
                (values (create-number e sign p 1 exponent)
                  i))))))
      (if (>= i n)
        (values #f i)
        (case (string-ref s i)
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
            (loop i 0))
          (else
            (values #f i)))))

    ; FIXME: these two procedures are no longer used.
    ; Maybe they should be.

    (define (radix-digit? c r)
      (if (eq? r 16)
        (or (decimal-digit? c)
          (let ((c (char-downcase c)))
            (and (char<=? #\a c) (char<=? c #\f))))
        (and (char<=? #\0 c)
          (char<? c (integer->char (+ (char->integer #\0) r))))))

    (define (radix-digit-value c r)
      (cond ((not (eq? r 16))
             (- (char->integer c) (char->integer #\0)))
        ((char<=? c #\9)
          (radix-digit-value c 10))
        (else (+ 10 (- (char->integer (char-downcase c))
                     (char->integer #\a))))))

    ;----------------------------------------------------------------
    ;
    ; The arguments to create-number contain all the information needed to
    ; create a real number of the correct sign, magnitude, and exactness.
    ;
    ;   exactness   = a symbol, e or i
    ;   sign        = 1 or -1
    ;   numerator   = an exact integer >= 0
    ;   denominator = an exact integer >= 0
    ;   exponent    = an exact integer

    (define (create-number exactness sign numerator denominator exponent)
      (cond ((not (eq? denominator 1))
             ; exponent must be 0
             (if (eq? denominator 0)
               (if (eq? exactness 'i)
                 (/ (exact->inexact (* sign numerator))
                   (exact->inexact 0))
                 #f)
               (coerce-exactness exactness
                 (/ (* sign numerator) denominator))))
        ((eq? exactness 'i)
          (* sign (bellerophon numerator exponent)))
        ((zero? exponent)
          (* sign numerator))
        ((negative? exponent)
          (/ (* sign numerator)
            (expt 10 (- exponent))))
        (else (* sign numerator (expt 10 exponent)))))

    ; Given an exactness (e or i or #f) and an exact number x,
    ; coerces x to the specified exactness.  #f means e.

    (define (coerce-exactness exactness x)
      (cond ((eq? exactness 'i)
             (if (inexact? x) x (exact->inexact x)))
        (else x)))

    (if (not (or (not radix)
              (memv radix '(2 8 10 16))))
      (assertion-violation 'string->number "illegal radix" radix))

    (parse-number)))
