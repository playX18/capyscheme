;; Base64 (RFC 4648): standard and URL-safe alphabets, with or without padding.
;;
;; (base64-encode bv #:url? #f #:padding? #t) -> string
;; (base64-decode str #:url? #f #:padding? #t) -> bytevector
;;
;; Decoding is strict about characters: whitespace and characters outside the
;; selected alphabet raise an assertion violation. With #:padding? #t the input
;; must be padded to a multiple of four characters; with #:padding? #f any '='
;; is rejected.

(library (capy encoding base64)
  (export base64-encode base64-decode)
  (import (except (rnrs) define)
    (rnrs mutable-strings)
    (rename (only (core kw) new-define) (new-define define)))

  (define std-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
  (define url-alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

  ;; Reverse tables: byte -> sextet, or -1 when the byte is not in the alphabet.
  (define (make-reverse-table alphabet)
    (let ([table (make-vector 256 -1)])
      (do ([i 0 (fx+ i 1)])
          ((fx=? i 64) table)
        (vector-set! table (char->integer (string-ref alphabet i)) i))))

  (define std-reverse (make-reverse-table std-alphabet))
  (define url-reverse (make-reverse-table url-alphabet))

  (define (base64-encode bv #:url? [url? #f] #:padding? [padding? #t])
    "Encode bytevector `bv` as a base64 string."
    (let* ([alphabet (if url? url-alphabet std-alphabet)]
           [n (bytevector-length bv)]
           [full (fxdiv n 3)]
           [rem (fxmod n 3)]
           [out-len (cond
                      [(fx=? rem 0) (fx* 4 full)]
                      [padding? (fx* 4 (fx+ full 1))]
                      [(fx=? rem 1) (fx+ (fx* 4 full) 2)]
                      [else (fx+ (fx* 4 full) 3)])]
           [out (make-string out-len #\=)])
      (define (put! i sextet)
        (string-set! out i (string-ref alphabet sextet)))
      (do ([g 0 (fx+ g 1)])
          ((fx=? g full))
        (let* ([i (fx* g 3)]
               [b0 (bytevector-u8-ref bv i)]
               [b1 (bytevector-u8-ref bv (fx+ i 1))]
               [b2 (bytevector-u8-ref bv (fx+ i 2))]
               [o (fx* g 4)])
          (put! o (fxarithmetic-shift-right b0 2))
          (put! (fx+ o 1) (fxior (fxarithmetic-shift-left (fxand b0 3) 4)
                            (fxarithmetic-shift-right b1 4)))
          (put! (fx+ o 2) (fxior (fxarithmetic-shift-left (fxand b1 15) 2)
                            (fxarithmetic-shift-right b2 6)))
          (put! (fx+ o 3) (fxand b2 63))))
      (let ([i (fx* 3 full)] [o (fx* 4 full)])
        (cond
          [(fx=? rem 1)
            (let ([b0 (bytevector-u8-ref bv i)])
              (put! o (fxarithmetic-shift-right b0 2))
              (put! (fx+ o 1) (fxarithmetic-shift-left (fxand b0 3) 4)))]
          [(fx=? rem 2)
            (let ([b0 (bytevector-u8-ref bv i)]
                  [b1 (bytevector-u8-ref bv (fx+ i 1))])
              (put! o (fxarithmetic-shift-right b0 2))
              (put! (fx+ o 1) (fxior (fxarithmetic-shift-left (fxand b0 3) 4)
                                (fxarithmetic-shift-right b1 4)))
              (put! (fx+ o 2) (fxarithmetic-shift-left (fxand b1 15) 2)))]
          [else (if #f #f)]))
      out))

  (define (base64-decode str #:url? [url? #f] #:padding? [padding? #t])
    "Decode base64 string `str` into a bytevector."
    (let* ([table (if url? url-reverse std-reverse)]
           [len (string-length str)]
           [pad (cond
                  [(and (fx>=? len 2) (char=? (string-ref str (fx- len 1)) #\=)
                     (char=? (string-ref str (fx- len 2)) #\=))
                    2]
                  [(and (fx>=? len 1) (char=? (string-ref str (fx- len 1)) #\=)) 1]
                  [else 0])]
           [data (fx- len pad)])
      (unless (if padding?
                (fx=? 0 (fxmod len 4))
                (fx=? pad 0))
        (assertion-violation 'base64-decode "invalid base64 padding" str))
      (let ([rem (fxmod data 4)])
        (when (fx=? rem 1)
          (assertion-violation 'base64-decode "invalid base64 length" str))
        (let ([out (make-bytevector (fx+ (fx* 3 (fxdiv data 4))
                                      (cond [(fx=? rem 2) 1] [(fx=? rem 3) 2] [else 0]))
                     0)])
          (define (sextet i)
            (let* ([c (char->integer (string-ref str i))]
                   [v (if (fx<? c 256) (vector-ref table c) -1)])
              (when (fx<? v 0)
                (assertion-violation 'base64-decode "invalid base64 character" str i))
              v))
          (do ([g 0 (fx+ g 1)])
              ((fx=? g (fxdiv data 4)))
            (let* ([i (fx* g 4)]
                   [s0 (sextet i)] [s1 (sextet (fx+ i 1))]
                   [s2 (sextet (fx+ i 2))] [s3 (sextet (fx+ i 3))]
                   [o (fx* g 3)])
              (bytevector-u8-set! out o
                (fxior (fxarithmetic-shift-left s0 2) (fxarithmetic-shift-right s1 4)))
              (bytevector-u8-set! out (fx+ o 1)
                (fxand 255 (fxior (fxarithmetic-shift-left s1 4) (fxarithmetic-shift-right s2 2))))
              (bytevector-u8-set! out (fx+ o 2)
                (fxand 255 (fxior (fxarithmetic-shift-left s2 6) s3)))))
          (let ([i (fx* 4 (fxdiv data 4))] [o (fx* 3 (fxdiv data 4))])
            (cond
              [(fx=? rem 2)
                (let ([s0 (sextet i)] [s1 (sextet (fx+ i 1))])
                  (bytevector-u8-set! out o
                    (fxior (fxarithmetic-shift-left s0 2) (fxarithmetic-shift-right s1 4))))]
              [(fx=? rem 3)
                (let ([s0 (sextet i)] [s1 (sextet (fx+ i 1))] [s2 (sextet (fx+ i 2))])
                  (bytevector-u8-set! out o
                    (fxior (fxarithmetic-shift-left s0 2) (fxarithmetic-shift-right s1 4)))
                  (bytevector-u8-set! out (fx+ o 1)
                    (fxand 255 (fxior (fxarithmetic-shift-left s1 4) (fxarithmetic-shift-right s2 2)))))]
              [else (if #f #f)]))
          out))))
)
