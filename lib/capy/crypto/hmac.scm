;; HMAC (RFC 2104)
;;
;; (hmac-sha256 key message) -> 32-byte bytevector
;; (hmac-sha1 key message)   -> 20-byte bytevector

(library (capy crypto hmac)
  (export hmac-sha1 hmac-sha256)
  (import (rnrs)
    (capy crypto sha1)
    (capy crypto sha256))

  (define (hmac make-state update! finish block-size key message)
    (let* ([key (if (fx>? (bytevector-length key) block-size)
                  (let ([st (make-state)])
                    (update! st key 0 (bytevector-length key))
                    (finish st))
                  key)]
           [klen (bytevector-length key)]
           [ipad (make-bytevector block-size #x36)]
           [opad (make-bytevector block-size #x5c)])
      (do ([i 0 (fx+ i 1)])
          ((fx>=? i klen))
        (let ([k (bytevector-u8-ref key i)])
          (bytevector-u8-set! ipad i (fxxor k #x36))
          (bytevector-u8-set! opad i (fxxor k #x5c))))
      (let ([inner (make-state)])
        (update! inner ipad 0 block-size)
        (update! inner message 0 (bytevector-length message))
        (let ([inner-digest (finish inner)]
              [outer (make-state)])
          (update! outer opad 0 block-size)
          (update! outer inner-digest 0 (bytevector-length inner-digest))
          (finish outer)))))

  (define (hmac-sha1 key message)
    "Return HMAC-SHA1 of `message` under `key` (both bytevectors)."
    (hmac make-sha1-state sha1-update! sha1-finish sha1-block-size key message))

  (define (hmac-sha256 key message)
    "Return HMAC-SHA256 of `message` under `key` (both bytevectors)."
    (hmac make-sha256-state sha256-update! sha256-finish sha256-block-size key message)))
