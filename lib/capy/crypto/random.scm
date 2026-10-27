;; Cryptographically strong random bytes from the operating system (/dev/urandom).

(library (capy crypto random)
  (export random-bytes random-token)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (core thread)
    (capy encoding hex)
    (capy encoding base64))

  (define source-lock (make-mutex))
  (define source-port #f)

  (define (read-urandom! bv)
    (with-mutex source-lock
      (unless source-port
        (set! source-port (open-file-input-port "/dev/urandom")))
      (let ([n (bytevector-length bv)])
        (let loop ([filled 0])
          (when (fx<? filled n)
            (let ([got (get-bytevector-n! source-port bv filled (fx- n filled))])
              (when (eof-object? got)
                (assertion-violation 'random-bytes "/dev/urandom ended unexpectedly"))
              (loop (fx+ filled got))))))))

  (define (random-bytes n)
    "Return a fresh bytevector of `n` random bytes."
    (let ([bv (make-bytevector n 0)])
      (when (fx>? n 0)
        (read-urandom! bv))
      bv))

  (define (random-token n #:encoding [encoding 'hex])
    "Return a random token made from `n` random bytes. `encoding` is 'hex (default) or 'base64url (unpadded)."
    (let ([bv (random-bytes n)])
      (case encoding
        [(hex) (bytevector->hex-string bv)]
        [(base64url) (base64-encode bv #:url? #t #:padding? #f)]
        [else (assertion-violation 'random-token "unknown encoding" encoding)])))
  )
