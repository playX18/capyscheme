(import (capy)
  (rnrs mutable-strings)
  (srfi 64))

(test-begin "string unicode edge cases")

;; Narrow strings hold Latin-1: U+0080..U+00FF are single bytes, not UTF-8.
(let ([s (string-append "" (make-string 1 (integer->char 199)))])
  (test-equal 1 (string-length s))
  (test-equal (integer->char 199) (string-ref s 0))
  (test-equal "\x00c7;" (call-with-string-output-port (lambda (p) (display s p)))))
(test-equal 3 (string-length (symbol->string (string->symbol (string #\a (integer->char 233) #\b)))))

;; Zero-length strings of a wide character.
(test-equal 0 (string-length (make-string 0 (integer->char #x1F600))))
(test-equal 2 (string-length (make-string 2 (integer->char #x1F600))))
(test-equal (integer->char #x1F600) (string-ref (make-string 2 (integer->char #x1F600)) 1))

;; Widening on string-set! / string-fill!.
(let ([s (make-string 3 #\a)])
  (string-set! s 1 (integer->char #x1F600))
  (test-equal (integer->char #x1F600) (string-ref s 1))
  (test-equal #\a (string-ref s 2)))
(let ([s (make-string 3 #\a)])
  (string-fill! s (integer->char #x1F600))
  (test-equal (integer->char #x1F600) (string-ref s 2)))

;; Titlecase uses the titlecase mapping, not uppercase.
(test-equal (string (integer->char #x1C5) #\space #\X)
  (string-titlecase (string (integer->char #x1C6) #\space #\x)))
(test-equal (integer->char #x1C5) (char-titlecase (integer->char #x1C6)))
(test-equal (integer->char #x1C5) (char-titlecase (integer->char #x1C4)))
(test-equal "Hello World" (string-titlecase "hELLO wORLD"))

(test-end "string unicode edge cases")
