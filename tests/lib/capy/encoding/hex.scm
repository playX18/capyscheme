(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy encoding hex))

(test-begin "capy encoding hex")

(define (raises? thunk)
  (guard (e [(assertion-violation? e) #t] [#t #f])
    (thunk)
    #f))

(test-group "bytevector->hex-string"
  (test-equal "empty" "" (bytevector->hex-string (make-bytevector 0)))
  (test-equal "lowercase digits" "00ff1a" (bytevector->hex-string (u8-list->bytevector '(0 255 26)))))

(test-group "hex-string->bytevector"
  (test-equal "decodes lowercase" '(0 255 26) (bytevector->u8-list (hex-string->bytevector "00ff1a")))
  (test-equal "decodes uppercase" '(171 205) (bytevector->u8-list (hex-string->bytevector "ABcD")))
  (test-equal "empty" 0 (bytevector-length (hex-string->bytevector "")))
  (test-assert "odd length" (raises? (lambda () (hex-string->bytevector "abc"))))
  (test-assert "non hex digit" (raises? (lambda () (hex-string->bytevector "zz")))))

(test-group "round trip"
  (test-equal "bytes 0..255"
    (bytevector->u8-list (let ([bv (make-bytevector 256)])
                           (do ([i 0 (+ i 1)]) ((= i 256) bv) (bytevector-u8-set! bv i i))))
    (bytevector->u8-list (hex-string->bytevector
                           (bytevector->hex-string
                             (let ([bv (make-bytevector 256)])
                               (do ([i 0 (+ i 1)]) ((= i 256) bv) (bytevector-u8-set! bv i i))))))))

(test-end "capy encoding hex")
