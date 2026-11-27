(import (except (rnrs) define)
  (only (scheme base) modulo)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy encoding base64))

(test-begin "capy encoding base64")

(define (ascii str) (string->utf8 str))
(define (text bv) (utf8->string bv))

(define (raises? thunk)
  (guard (e [(assertion-violation? e) #t] [#t #f])
    (thunk)
    #f))

;; RFC 4648 section 10.
(test-group "RFC 4648 vectors"
  (for-each
    (lambda (pair)
      (test-equal (string-append "encode " (car pair))
        (cdr pair)
        (base64-encode (ascii (car pair))))
      (test-equal (string-append "decode " (cdr pair))
        (car pair)
        (text (base64-decode (cdr pair)))))
    '(("" . "")
      ("f" . "Zg==")
      ("fo" . "Zm8=")
      ("foo" . "Zm9v")
      ("foob" . "Zm9vYg==")
      ("fooba" . "Zm9vYmE=")
      ("foobar" . "Zm9vYmFy"))))

(test-group "alphabets and padding"
  (test-equal "url-safe alphabet uses - and _"
    "-_-_"
    (base64-encode (u8-list->bytevector '(251 255 191)) #:url? #t))
  (test-equal "standard alphabet uses + and /"
    "+/+/"
    (base64-encode (u8-list->bytevector '(251 255 191))))
  (test-equal "unpadded encoding of one byte"
    "Zg"
    (base64-encode (ascii "f") #:padding? #f))
  (test-equal "unpadded encoding of two bytes"
    "Zm8"
    (base64-encode (ascii "fo") #:padding? #f))
  (test-equal "url-safe unpadded decode"
    (u8-list->bytevector '(251 255 191))
    (base64-decode "-_-_" #:url? #t))
  (test-equal "unpadded decode"
    "foob"
    (text (base64-decode "Zm9vYg" #:padding? #f)))
  (test-assert "padding required by default"
    (raises? (lambda () (base64-decode "Zm9vYg"))))
  (test-assert "padding rejected when disabled"
    (raises? (lambda () (base64-decode "Zm9vYg==" #:padding? #f))))
  (test-assert "standard alphabet rejects url characters"
    (raises? (lambda () (base64-decode "-_-_"))))
  (test-assert "url alphabet rejects standard characters"
    (raises? (lambda () (base64-decode "+/+/" #:url? #t)))))

(test-group "errors"
  (test-assert "invalid character"
    (raises? (lambda () (base64-decode "Zm9v!"))))
  (test-assert "whitespace is not skipped"
    (raises? (lambda () (base64-decode "Zm9v\nYmFy"))))
  (test-assert "a single leftover character is invalid"
    (raises? (lambda () (base64-decode "Zm9vY"))))
  (test-assert "padding in the middle"
    (raises? (lambda () (base64-decode "Zg==Zm8="))))
  )

(test-group "round trip"
  (test-assert "all byte values and lengths 0..40"
    (let loop ([n 0] [ok #t])
      (if (> n 40)
        ok
        (let* ([bv (make-bytevector n 0)])
          (do ([i 0 (+ i 1)]) ((= i n))
            (bytevector-u8-set! bv i (modulo (* i 91) 256)))
          (loop (+ n 1)
            (and ok
              (equal? (bytevector->u8-list (base64-decode (base64-encode bv)))
                (bytevector->u8-list bv)))))))))

(test-end "capy encoding base64")
