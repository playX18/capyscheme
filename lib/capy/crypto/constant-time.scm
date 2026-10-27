;; Comparison whose running time does not depend on where the inputs differ.

(library (capy crypto constant-time)
  (export bytevector=?/constant-time)
  (import (rnrs))

  (define (bytevector=?/constant-time a b)
    "Return #t when bytevectors `a` and `b` are equal. The loop never exits early, so timing reveals only the length."
    (let ([n (bytevector-length a)])
      (and (fx=? n (bytevector-length b))
        (let loop ([i 0] [acc 0])
          (if (fx=? i n)
            (fx=? acc 0)
            (loop (fx+ i 1)
              (fxior acc (fxxor (bytevector-u8-ref a i) (bytevector-u8-ref b i))))))))))
