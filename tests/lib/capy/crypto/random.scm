(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy crypto random)
  (capy encoding base64))

(test-begin "capy crypto random")

(define (for-all-in-string? str pred)
  (let loop ([i 0])
    (or (= i (string-length str))
      (and (pred (string-ref str i)) (loop (+ i 1))))))


(test-group "random-bytes"
  (test-equal "length" 32 (bytevector-length (random-bytes 32)))
  (test-equal "zero length" 0 (bytevector-length (random-bytes 0)))
  (test-assert "two calls differ" (not (equal? (bytevector->u8-list (random-bytes 32))
                                         (bytevector->u8-list (random-bytes 32)))))
  (test-assert "large request fills all bytes" 
    (let ([bv (random-bytes 100000)])
      (let loop ([i 0] [nonzero 0])
        (cond
          [(= i 100000) (> nonzero 99000)]
          [else (loop (+ i 1) (if (zero? (bytevector-u8-ref bv i)) nonzero (+ nonzero 1)))])))))

(test-group "random-token"
  (test-equal "hex token is twice the byte count"
    32
    (string-length (random-token 16)))
  (test-assert "hex token uses only hex digits"
    (for-all-in-string? (random-token 16)
      (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\f)))))
  (test-equal "base64url token has no padding and no unsafe characters"
    #t
    (let ([t (random-token 16 #:encoding 'base64url)])
      (and (= 22 (string-length t))
        (for-all-in-string? t
          (lambda (c) (or (char<=? #\0 c #\9) (char<=? #\a c #\z) (char<=? #\A c #\Z)
                        (char=? c #\-) (char=? c #\_)))))))
  (test-assert "tokens differ"
    (not (string=? (random-token 16) (random-token 16)))))

(test-end "capy crypto random")
