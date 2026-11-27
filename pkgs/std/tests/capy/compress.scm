(import (rnrs)
        (srfi 64)
        (only (capy) microsecond)
        (capy compress)
        (capy process))

(test-begin "capy compress")

;; ---- Test data --------------------------------------------------------------------------

(define (lcg-bytes n seed)
  (let ([bv (make-bytevector n)])
    (let loop ([i 0] [x seed])
      (if (= i n)
        bv
        (let ([x (mod (+ (* x 1103515245) 12345) 2147483648)])
          (bytevector-u8-set! bv i (bitwise-and (bitwise-arithmetic-shift-right x 16) 255))
          (loop (+ i 1) x))))))

(define (repeat-string s n)
  (let loop ([k 0] [acc '()])
    (if (= k n) (string->utf8 (apply string-append acc)) (loop (+ k 1) (cons s acc)))))

;; Text-like: words drawn from a small vocabulary, so there are matches at all distances.
(define (text-bytes n seed)
  (let ([words '#("the" "quick" "brown" "fox" "jumps" "over" "lazy" "dog" "compress"
                  "scheme" "window" "huffman" "lambda" "\n" ", " "deflate" "inflate")]
        [rnd (lcg-bytes n seed)])
    (let loop ([i 0] [acc '()] [len 0])
      (if (>= len n)
        (let ([bv (string->utf8 (apply string-append (reverse acc)))])
          (let ([out (make-bytevector n)]) (bytevector-copy! bv 0 out 0 n) out))
        (let ([w (vector-ref words (mod (bytevector-u8-ref rnd i) (vector-length words)))])
          (loop (+ i 1) (cons (string-append w " ") acc) (+ len (string-length w) 1)))))))

(define (bytevector-append* parts)
  (let* ([n (fold-left + 0 (map bytevector-length parts))] [out (make-bytevector n)])
    (let loop ([parts parts] [pos 0])
      (if (null? parts) out
        (begin (bytevector-copy! (car parts) 0 out pos (bytevector-length (car parts)))
               (loop (cdr parts) (+ pos (bytevector-length (car parts)))))))))

(define (bytevector-slice bv start end)
  (let ([out (make-bytevector (- end start))]) (bytevector-copy! bv start out 0 (- end start)) out))

;; A fixed-code block with a match at distance 2 after one byte of output.
(define (deflate-fixed-far)
  ;; Bits, LSB first: BFINAL 1, BTYPE 01; literal 'a' (8 bits: 0x30 + 97 = 0x91, reversed);
  ;; length 3 (symbol 257, 7 bits 0000001 reversed); distance code 1 (5 bits 00001 reversed);
  ;; end of block (7 zero bits).
  (let ([bits '()])
    (define (put! v n) ; MSB-first code -> pushed LSB first in stream order
      (do ([k (- n 1) (- k 1)]) ((< k 0))
        (set! bits (cons (bitwise-and 1 (bitwise-arithmetic-shift-right v k)) bits))))
    (define (put-lsb! v n)
      (do ([k 0 (+ k 1)]) ((= k n))
        (set! bits (cons (bitwise-and 1 (bitwise-arithmetic-shift-right v k)) bits))))
    (put-lsb! 1 1) (put-lsb! 1 2)
    (put! (+ #x30 97) 8)
    (put! 1 7)
    (put! 1 5)
    (put! 0 7)
    (let* ([bits (reverse bits)]
           [n (div (+ (length bits) 7) 8)]
           [bv (make-bytevector n 0)])
      (let loop ([bits bits] [i 0])
        (unless (null? bits)
          (when (= 1 (car bits))
            (let ([byte (div i 8)])
              (bytevector-u8-set! bv byte (bitwise-ior (bytevector-u8-ref bv byte)
                                                       (bitwise-arithmetic-shift-left 1 (mod i 8))))))
          (loop (cdr bits) (+ i 1))))
      bv)))


(define samples
  (list
    (cons "empty" (make-bytevector 0))
    (cons "one byte" #vu8(42))
    (cons "short text" (string->utf8 "hello, hello, hello world"))
    (cons "zeros" (make-bytevector 100000 0))
    (cons "random" (lcg-bytes 70000 7))
    (cons "text" (text-bytes 200000 3))
    (cons "lorem" (repeat-string "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " 500))
    (cons "ramp" (let ([bv (make-bytevector 70000)])
                   (do ([i 0 (+ i 1)]) ((= i 70000) bv)
                     (bytevector-u8-set! bv i (mod (* i i) 251)))))))

(define big (bytevector-append* (list (text-bytes 400000 11) (lcg-bytes 100000 5) (make-bytevector 50000 7))))

(define (compress-error-raised? thunk)
  (guard (c [(compress-error? c) #t] [else #f])
    (thunk)
    #f))

;; ---- Checksums --------------------------------------------------------------------------

(test-group "checksums"
  (test-equal "crc32 check value" #xCBF43926 (crc32 (string->utf8 "123456789")))
  (test-equal "crc32 empty" 0 (crc32 #vu8()))
  (test-equal "crc32 of 'a'" #xE8B7BE43 (crc32 (string->utf8 "a")))
  (test-equal "crc32 range" (crc32 (string->utf8 "345")) (crc32 (string->utf8 "123456789") 2 5))
  (test-equal "crc32-update" (crc32 (string->utf8 "123456789"))
    (crc32-update (crc32 (string->utf8 "1234")) (string->utf8 "56789")))
  (test-equal "crc32 of long data, by pieces"
    (crc32 big)
    (let loop ([pos 0] [crc 0])
      (if (>= pos (bytevector-length big))
        crc
        (let ([end (min (bytevector-length big) (+ pos 9999))])
          (loop end (crc32-update crc big pos end))))))
  (test-equal "adler32 Wikipedia" #x11E60398 (adler32 (string->utf8 "Wikipedia")))
  (test-equal "adler32 empty" 1 (adler32 #vu8()))
  (test-equal "adler32 of a large run (s2 reductions)" #x86AF0001 (adler32 (make-bytevector 100000 0)))
  (test-equal "adler32-update" (adler32 big)
    (adler32-update (adler32 big 0 12345) big 12345)))

;; ---- Round trips ------------------------------------------------------------------------

(test-group "round trips"
  (for-each
    (lambda (sample)
      (let ([name (car sample)] [data (cdr sample)])
        (do ([level 0 (+ level 1)]) ((= level 10))
          (test-equal (string-append name ", level " (number->string level))
            data (inflate (deflate data #:level level))))
        (test-equal (string-append name ", fixed codes")
          data (inflate (deflate data #:strategy 'fixed)))
        (test-equal (string-append name ", huffman only")
          data (inflate (deflate data #:strategy 'huffman-only)))
        (test-equal (string-append name ", zlib") data (zlib-decompress (zlib-compress data)))
        (test-equal (string-append name ", gzip") data (gzip-decompress (gzip-compress data)))))
    samples)
  (test-equal "big, level 1" big (inflate (deflate big #:level 1)))
  (test-equal "big, level 9" big (inflate (deflate big #:level 9)))
  (test-equal "compress / decompress detect the format"
    '(#t #t #t)
    (let ([data (cdr (list-ref samples 5))])
      (map (lambda (format) (equal? data (decompress (compress data #:format format))))
           '(gzip zlib deflate)))))

(test-group "compression ratio"
  (let ([text (cdr (list-ref samples 5))])
    (test-assert "level 9 is no larger than level 1"
      (<= (bytevector-length (deflate text #:level 9)) (bytevector-length (deflate text #:level 1))))
    (test-assert "text compresses to under a third"
      (< (* 3 (bytevector-length (deflate text))) (bytevector-length text))))
  (test-assert "random data grows by at most 5 bytes per block of 16 Ki tokens"
    (let* ([data (cdr (list-ref samples 4))] [n (bytevector-length data)])
      (<= (bytevector-length (deflate data)) (+ n 5 (* 5 (div n 16384))))))
  (test-equal "the empty stream" #vu8(3 0) (deflate #vu8()))
  (test-equal "the zlib header of level 6" '(#x78 #x9C)
    (let ([z (zlib-compress #vu8(1 2 3))]) (list (bytevector-u8-ref z 0) (bytevector-u8-ref z 1)))))

;; ---- Known streams ----------------------------------------------------------------------

(test-group "known streams"
  ;; "hello" with the fixed code, from zlib.
  (test-equal "fixed-code stream" (string->utf8 "hello")
    (inflate #vu8(#xcb #x48 #xcd #xc9 #xc9 #x07 #x00)))
  ;; A stored block: BFINAL=1, BTYPE=00, LEN=3, NLEN, "abc".
  (test-equal "stored block" (string->utf8 "abc")
    (inflate #vu8(1 3 0 #xfc #xff 97 98 99)))
  ;; zlib's compress("hello world").
  (test-equal "zlib stream" (string->utf8 "hello world")
    (zlib-decompress #vu8(#x78 #x9c #xcb #x48 #xcd #xc9 #xc9 #x57 #x28 #xcf #x2f #xca #x49 #x01
                          #x00 #x1a #x0b #x04 #x5d)))
  (test-equal "two gzip members" (string->utf8 "hello, hello, hello world")
    (gzip-decompress (bytevector-append* (list (gzip-compress (string->utf8 "hello, "))
                                               (gzip-compress (string->utf8 "hello, hello world"))))))
  (test-equal "gzip with a name and mtime" (string->utf8 "data")
    (gzip-decompress (gzip-compress (string->utf8 "data") #:name "notes.txt" #:mtime 1700000000))))

;; ---- Errors -----------------------------------------------------------------------------

(test-group "errors"
  (let ([gz (gzip-compress (cdr (list-ref samples 5)))]
        [z (zlib-compress (cdr (list-ref samples 5)))])
    (test-assert "truncated gzip"
      (compress-error-raised? (lambda () (gzip-decompress (bytevector-slice gz 0 (- (bytevector-length gz) 100))))))
    (test-assert "truncated in the trailer"
      (compress-error-raised? (lambda () (gzip-decompress (bytevector-slice gz 0 (- (bytevector-length gz) 3))))))
    (test-assert "bad gzip CRC"
      (compress-error-raised?
        (lambda ()
          (let ([bad (bytevector-copy gz)])
            (bytevector-u8-set! bad (- (bytevector-length bad) 6)
                                (bitwise-xor 1 (bytevector-u8-ref bad (- (bytevector-length bad) 6))))
            (gzip-decompress bad)))))
    (test-assert "bad Adler-32"
      (compress-error-raised?
        (lambda ()
          (let ([bad (bytevector-copy z)])
            (bytevector-u8-set! bad (- (bytevector-length bad) 1)
                                (bitwise-xor 1 (bytevector-u8-ref bad (- (bytevector-length bad) 1))))
            (zlib-decompress bad)))))
    (test-assert "not gzip" (compress-error-raised? (lambda () (gzip-decompress z))))
    (test-assert "not zlib" (compress-error-raised? (lambda () (zlib-decompress gz)))))
  (test-assert "block type 3" (compress-error-raised? (lambda () (inflate #vu8(7 0 0)))))
  (test-assert "stored LEN/NLEN mismatch" (compress-error-raised? (lambda () (inflate #vu8(1 3 0 0 0 97 98 99)))))
  (test-assert "distance too far back"
    ;; Fixed code: literal 'a', then length 3 at distance 2.
    (compress-error-raised? (lambda () (inflate (deflate-fixed-far)))))
  (test-assert "corrupt data never loops"
    (let loop ([seed 1] [ok #t])
      (if (= seed 40)
        ok
        (let ([junk (lcg-bytes 300 seed)])
          (loop (+ seed 1)
                (and ok (guard (c [(compress-error? c) #t] [else #f]) (inflate junk) #t)))))))
  (test-error "bad level" (deflate #vu8(1) #:level 10))
  (test-error "bad format" (compress #vu8(1) #:format 'zip)))


;; ---- Ports ------------------------------------------------------------------------------

(define (compress-through-port data format level chunk)
  (let-values ([(sink get) (open-bytevector-output-port)])
    (let ([out (open-compress-output-port sink #:format format #:level level #:close? #f)])
      (let loop ([pos 0])
        (when (< pos (bytevector-length data))
          (let ([end (min (bytevector-length data) (+ pos chunk))])
            (put-bytevector out data pos (- end pos))
            (loop end))))
      (close-port out)
      (get))))

(define (decompress-through-port bytes format chunk)
  (let ([in (open-decompress-input-port (open-bytevector-input-port bytes) #:format format)])
    (let-values ([(sink get) (open-bytevector-output-port)])
      (let loop ()
        (let ([piece (get-bytevector-n in chunk)])
          (unless (eof-object? piece)
            (put-bytevector sink piece)
            (loop))))
      (close-port in)
      (get))))

(test-group "ports"
  (for-each
    (lambda (format)
      (test-equal (string-append "compress port, " (symbol->string format))
        big (decompress (compress-through-port big format 6 30000) #:format format))
      (test-equal (string-append "decompress port, " (symbol->string format))
        big (decompress-through-port (compress big #:format format) format 7777)))
    '(gzip zlib deflate))
  (test-equal "compress port, level 0, one byte at a time" (cdr (list-ref samples 2))
    (gzip-decompress (compress-through-port (cdr (list-ref samples 2)) 'gzip 0 1)))
  (test-equal "compress port, level 1, odd chunks" big
    (inflate (compress-through-port big 'deflate 1 65537)))
  (test-equal "decompress port, auto, multi-member" (string->utf8 "abcdef")
    (decompress-through-port (bytevector-append* (list (gzip-compress (string->utf8 "abc"))
                                                       (gzip-compress (string->utf8 "def"))))
                             'auto 1))
  (test-equal "empty through ports" #vu8()
    (decompress-through-port (compress-through-port #vu8() 'gzip 6 10) 'auto 10))
  (test-assert "decompress port reports corrupt data"
    (compress-error-raised?
      (lambda ()
        (let ([bad (bytevector-copy (gzip-compress big))])
          (bytevector-u8-set! bad 5000 (bitwise-xor 255 (bytevector-u8-ref bad 5000)))
          (decompress-through-port bad 'gzip 4096))))))

;; ---- Interoperability with the system gzip ----------------------------------------------

(define (have? program)
  (guard (c [#t #f])
    (let ([p (spawn "sh" (list "-c" (string-append "command -v " program))
                    #:stdout 'null #:stderr 'null)])
      (process-wait p)
      (process-success? p))))

(define tmp (string-append "/tmp/capy-compress-test-" (number->string (microsecond))))

(define (write-file path bv)
  (when (file-exists? path) (delete-file path))
  (call-with-port (open-file-output-port path) (lambda (p) (put-bytevector p bv))))

(define (run-bytes program args stdin-path)
  (let ([p (spawn program args #:stdin stdin-path #:stdout 'bytes)])
    (process-wait p)
    (unless (process-success? p) (error 'run-bytes "command failed" program args))
    (process-stdout-bytes p)))

(when (have? "gzip")
  (test-group "system gzip"
    (let ([data (bytevector-append* (list (text-bytes 300000 21) (lcg-bytes 20000 2)))])
      (write-file tmp data)
      (for-each
        (lambda (level)
          (test-equal (string-append "decode gzip -" (number->string level))
            data (gzip-decompress (run-bytes "gzip" (list "-c" "-n" (string-append "-" (number->string level))) tmp))))
        '(1 6 9))
      (for-each
        (lambda (level)
          (write-file (string-append tmp ".gz") (gzip-compress data #:level level #:name "data"))
          (test-equal (string-append "gzip -d decodes level " (number->string level))
            data (run-bytes "gzip" '("-d" "-c") (string-append tmp ".gz"))))
        '(0 1 4 6 9))
      (write-file (string-append tmp ".gz") (compress-through-port data 'gzip 6 4096))
      (test-equal "gzip -d decodes the compress port" data
        (run-bytes "gzip" '("-d" "-c") (string-append tmp ".gz")))
      (test-equal "decompress port over gzip's output" data
        (decompress-through-port (run-bytes "gzip" '("-c" "-n") tmp) 'auto 5000))
      (test-assert "gzip -t accepts fixed-code output"
        (begin
          (write-file (string-append tmp ".gz") (gzip-compress data #:strategy 'fixed))
          (let ([p (spawn "gzip" (list "-t" (string-append tmp ".gz")))])
            (process-wait p)
            (process-success? p))))
      (delete-file tmp)
      (delete-file (string-append tmp ".gz")))))

(when (have? "python3")
  (test-group "system zlib (python3)"
    (let ([data (text-bytes 100000 33)])
      (write-file tmp data)
      (test-equal "decode zlib.compress level 9" data
        (zlib-decompress
          (run-bytes "python3" '("-c" "import sys,zlib; sys.stdout.buffer.write(zlib.compress(sys.stdin.buffer.read(), 9))") tmp)))
      (test-equal "decode raw deflate, wbits -15" data
        (inflate
          (run-bytes "python3" '("-c" "import sys,zlib; c=zlib.compressobj(6, zlib.DEFLATED, -15); sys.stdout.buffer.write(c.compress(sys.stdin.buffer.read())+c.flush())") tmp)))
      (write-file tmp (zlib-compress data #:level 9))
      (test-equal "zlib.decompress decodes level 9" data
        (run-bytes "python3" '("-c" "import sys,zlib; sys.stdout.buffer.write(zlib.decompress(sys.stdin.buffer.read()))") tmp))
      (delete-file tmp))))

(test-end "capy compress")
