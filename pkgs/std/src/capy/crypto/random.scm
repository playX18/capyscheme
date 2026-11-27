;; Cryptographically strong random bytes from the operating system.
;;
;; `os-random-fill!` asks the kernel directly (getentropy/getrandom), so a
;; draw is one system call with no port, lock or file descriptor.

(library (capy crypto random)
  (export random-bytes random-token)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (only (capy) os-random-fill!)
    (capy encoding hex)
    (capy encoding base64))

  (define (random-bytes n)
    "Return a fresh bytevector of `n` random bytes."
    (let ([bv (make-bytevector n 0)])
      (when (fx>? n 0)
        (os-random-fill! bv))
      bv))

  (define (random-token n #:encoding [encoding 'hex])
    "Return a random token made from `n` random bytes. `encoding` is 'hex (default) or 'base64url (unpadded)."
    (let ([bv (random-bytes n)])
      (case encoding
        [(hex) (bytevector->hex-string bv)]
        [(base64url) (base64-encode bv #:url? #t #:padding? #f)]
        [else (assertion-violation 'random-token "unknown encoding" encoding)])))
  )
