(import (except (rnrs) define)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy crypto sha1)
  (capy encoding hex))

(test-begin "capy crypto sha1")

(define (hex-of bv) (bytevector->hex-string bv))

;; Pattern used for the multi-block vectors: byte i is (i*7) mod 256.
(define (pattern n)
  (let ([bv (make-bytevector n 0)])
    (do ([i 0 (+ i 1)]) ((= i n) bv)
      (bytevector-u8-set! bv i (modulo (* i 7) 256)))))

(test-group "fips vectors"
  (test-equal "empty" "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (hex-of (sha1 (make-bytevector 0))))
  (test-equal "abc" "a9993e364706816aba3e25717850c26c9cd0d89d"
    (hex-of (sha1 (string->utf8 "abc"))))
  (test-equal "448-bit message" "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
    (hex-of (sha1 (string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
  (test-equal "896-bit message" "a49b2446a02c645bf419f995b67091253a04a259"
    (hex-of (sha1 (string->utf8 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))))
  (test-equal "one million a" "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    (hex-of (sha1 (make-bytevector 1000000 97)))))

(test-group "block boundaries"
  (for-each
    (lambda (row)
      (test-equal (string-append "length " (number->string (car row)))
        (cadr row)
        (hex-of (sha1 (pattern (car row))))))
    '(
    (55 "aecd1643c9903b9bae8cb94f53c50f8a4e18605b" "576a1bf8d4478657e6dc4af9398544765c2a92cde28478b019235cfed315fc09")
    (56 "f5d65c621c02cc8e785159feff8088e3072da1bc" "9b20501dfd1d99161c257950f3444f3e49230c351c5c8e0943ef369f85f5205d")
    (63 "4952f0fe097e4d6410ae9eab4855aa836caf3bff" "30b345906b493f06f69444b6521113511c242f30e29840462950035043682f1e")
    (64 "1e17ae1fc093e5daca033553c97a5192ca164486" "d8bc63b4fc1156e5e7d95a418b9bf54cd3174bedbc2db40f74895349b229b3c0")
    (65 "ac44f5dbe3e9b5d2733fc9537fcad715c3c20bc3" "1ee23b0fbcaecc1aff4a9e8f1645f35ab2c8e13609cd73b68df8b5e3f63ce073")
    (127 "374f47c1c8d4700fc2588e71f6d1e861b431b5c3" "67d79933e3c9aa8e89f481e071cfca1e9a16c09d7263b5efa3bb01f1a9f8d065")
    (128 "516846cd40bd1fe431119c3b0a0f362957992ee8" "54c9eb041badfd7064645067b107661fed6113197ce2dd066ba69618abd3732f")
    (1000 "38f3aa587f4aa04965a359f9151092759b3a4c2a" "89f4ff56a25dd1db06a4ce6033603775d705fb96f30f8693733fef602a1ca532")
    ))
  )

(test-group "streaming"
  (test-equal "split updates equal one-shot"
    (hex-of (sha1 (pattern 300)))
    (let ([st (make-sha1-state)] [bv (pattern 300)])
      (sha1-update! st bv 0 1)
      (sha1-update! st bv 1 64)
      (sha1-update! st bv 64 65)
      (sha1-update! st bv 65 300)
      (hex-of (sha1-finish st)))))

(test-end "capy crypto sha1")
