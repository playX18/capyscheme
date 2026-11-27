(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy crypto hmac)
  (capy encoding hex))

(test-begin "capy crypto hmac")

(define (ascii str) (string->utf8 str))
(define (hex-of bv) (bytevector->hex-string bv))

;; RFC 4231 test cases 1 and 2 (HMAC-SHA-256) and RFC 2202 (HMAC-SHA-1).
(test-group "RFC vectors"
  (test-equal "HMAC-SHA-256 case 1"
    "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
    (hex-of (hmac-sha256 (make-bytevector 20 #x0b) (ascii "Hi There"))))

  (test-equal "HMAC-SHA-256 case 2"
    "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
    (hex-of (hmac-sha256 (ascii "Jefe") (ascii "what do ya want for nothing?"))))

  (test-equal "HMAC-SHA-256 with a key longer than the block size"
    "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
    (hex-of (hmac-sha256 (make-bytevector 131 #xaa)
              (ascii "Test Using Larger Than Block-Size Key - Hash Key First"))))

  (test-equal "HMAC-SHA-1 case 2"
    "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79"
    (hex-of (hmac-sha1 (ascii "Jefe") (ascii "what do ya want for nothing?"))))

  (test-equal "HMAC-SHA-1 with a key longer than the block size"
    "90d0dace1c1bdc957339307803160335bde6df2b"
    (hex-of (hmac-sha1 (make-bytevector 131 #xaa)
              (ascii "Test Using Larger Than Block-Size Key - Hash Key First")))))

(test-end "capy crypto hmac")
