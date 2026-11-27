(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy crypto constant-time))

(test-begin "capy crypto constant-time")

(define (ascii str) (string->utf8 str))

(test-group "bytevector=?/constant-time"
  (test-assert "equal contents" (bytevector=?/constant-time (ascii "secret") (ascii "secret")))
  (test-assert "empty bytevectors are equal" (bytevector=?/constant-time (make-bytevector 0) (make-bytevector 0)))
  (test-assert "difference in the first byte" (not (bytevector=?/constant-time (ascii "Xecret") (ascii "secret"))))
  (test-assert "difference in the last byte" (not (bytevector=?/constant-time (ascii "secreX") (ascii "secret"))))
  (test-assert "different lengths" (not (bytevector=?/constant-time (ascii "secret") (ascii "secre")))))

(test-end "capy crypto constant-time")
