(import (except (rnrs) define)
  (only (scheme base) modulo)
  (rename (only (core kw) new-define) (new-define define))
  (srfi 64)
  (capy crypto sha256)
  (capy encoding hex))

(test-begin "capy crypto sha256")

(define (hex-of bv) (bytevector->hex-string bv))

;; Pattern used for the multi-block vectors: byte i is (i*7) mod 256.
(define (pattern n)
  (let ([bv (make-bytevector n 0)])
    (do ([i 0 (+ i 1)]) ((= i n) bv)
      (bytevector-u8-set! bv i (modulo (* i 7) 256)))))

(test-group "fips vectors"
  (test-equal "empty" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    (hex-of (sha256 (make-bytevector 0))))
  (test-equal "abc" "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (hex-of (sha256 (string->utf8 "abc"))))
  (test-equal "448-bit message" "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    (hex-of (sha256 (string->utf8 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"))))
  (test-equal "896-bit message" "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"
    (hex-of (sha256 (string->utf8 "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))))
  (test-equal "one million a" "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
    (hex-of (sha256 (make-bytevector 1000000 97)))))

(test-group "block boundaries"
  (for-each
    (lambda (row)
      (test-equal (string-append "length " (number->string (car row)))
        (cadr row)
        (hex-of (sha256 (pattern (car row))))))
    '(
    (55 "576a1bf8d4478657e6dc4af9398544765c2a92cde28478b019235cfed315fc09")
    (56 "9b20501dfd1d99161c257950f3444f3e49230c351c5c8e0943ef369f85f5205d")
    (63 "30b345906b493f06f69444b6521113511c242f30e29840462950035043682f1e")
    (64 "d8bc63b4fc1156e5e7d95a418b9bf54cd3174bedbc2db40f74895349b229b3c0")
    (65 "1ee23b0fbcaecc1aff4a9e8f1645f35ab2c8e13609cd73b68df8b5e3f63ce073")
    (127 "67d79933e3c9aa8e89f481e071cfca1e9a16c09d7263b5efa3bb01f1a9f8d065")
    (128 "54c9eb041badfd7064645067b107661fed6113197ce2dd066ba69618abd3732f")
    (1000 "89f4ff56a25dd1db06a4ce6033603775d705fb96f30f8693733fef602a1ca532")
    )))

(test-group "streaming"
  (test-equal "split updates equal one-shot"
    (hex-of (sha256 (pattern 300)))
    (let ([st (make-sha256-state)] [bv (pattern 300)])
      (sha256-update! st bv 0 1)
      (sha256-update! st bv 1 64)
      (sha256-update! st bv 64 65)
      (sha256-update! st bv 65 300)
      (hex-of (sha256-finish st)))))

(test-end "capy crypto sha256")
