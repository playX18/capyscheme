;;; Compression: DEFLATE (RFC 1951), zlib (RFC 1950) and gzip (RFC 1952), in Scheme.
;;;
;;; Bytevector to bytevector:
;;;
;;;   (gzip-compress (string->utf8 "hello, hello, hello"))       ; a .gz file's bytes
;;;   (gzip-decompress bytes)                                    ; and back
;;;   (zlib-compress bv #:level 9)    (zlib-decompress bv)       ; the zlib format (PNG, HTTP)
;;;   (deflate bv)                    (inflate bv)               ; raw DEFLATE, no framing
;;;   (compress bv #:format 'zlib)    (decompress bv)            ; decompress detects the format
;;;
;;; Ports, for data that does not fit in memory or arrives in pieces:
;;;
;;;   (call-with-port (open-compress-output-port (open-file-output-port "log.gz"))
;;;     (lambda (out) (put-bytevector out data)))                ; closing it ends the stream
;;;
;;;   (let ([in (open-decompress-input-port (open-file-input-port "log.gz"))])
;;;     (get-bytevector-all in))
;;;
;;; Keyword arguments:
;;;
;;;   #:level     0 (stored, no compression) to 9 (smallest, slowest); 6 by default, as zlib.
;;;               1-3 take the first good match, 4-9 also try the next byte (lazy matching).
;;;   #:strategy  default, fixed (fixed Huffman codes only) or huffman-only (no matches).
;;;   #:format    gzip, zlib or deflate (raw); `decompress` and the input port also take
;;;               auto (the default), which recognizes gzip and zlib headers.
;;;   #:name      gzip: the file name stored in the header (a string), #f for none.
;;;   #:mtime     gzip: the modification time stored in the header, in seconds since 1970.
;;;   #:close?    ports: close the underlying port when this one is closed; #t by default.
;;;
;;; gzip-decompress reads every member of a multi-member file and checks each CRC-32 and size.
;;; zlib-decompress checks the Adler-32. Bytes after the end of the data are ignored.
;;; Malformed input raises a condition that satisfies `compress-error?`.
;;;
;;; The checksums are here too: `crc32` and `adler32` take a bytevector and an optional range,
;;; and `crc32-update` and `adler32-update` continue a running value.

(library (capy compress)
  (export deflate inflate
    zlib-compress zlib-decompress
    gzip-compress gzip-decompress
    compress decompress
    open-compress-output-port open-decompress-input-port
    crc32 crc32-update adler32 adler32-update
    &compress-error compress-error?)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (capy compress checksum)
    (capy compress tables)
    (capy compress inflate)
    (capy compress deflate))

  ;; ---- Arguments -------------------------------------------------------------------------

  (define (check-bytevector who bv)
    (unless (bytevector? bv)
      (assertion-violation who "expected a bytevector" bv)))

  (define (check-level who level)
    (unless (and (fixnum? level) (fx<=? 0 level 9))
      (assertion-violation who "#:level must be an integer from 0 to 9" level))
    level)

  (define (check-strategy who strategy)
    (unless (memq strategy '(default fixed huffman-only))
      (assertion-violation who "#:strategy must be default, fixed or huffman-only" strategy))
    strategy)

  (define (check-format who format auto?)
    (unless (memq format (if auto? '(auto gzip zlib deflate) '(gzip zlib deflate)))
      (assertion-violation who
        (if auto?
          "#:format must be auto, gzip, zlib or deflate"
          "#:format must be gzip, zlib or deflate")
        format))
    format)

  (define (bytevector-concat parts)
    (let* ([n (fold-left (lambda (n bv) (+ n (bytevector-length bv))) 0 parts)]
           [out (make-bytevector n)])
      (let loop ([parts parts] [pos 0])
        (if (null? parts)
          out
          (let ([bv (car parts)])
            (bytevector-copy! bv 0 out pos (bytevector-length bv))
            (loop (cdr parts) (+ pos (bytevector-length bv))))))))

  ;; ---- Headers and trailers --------------------------------------------------------------

  (define (zlib-header level)
    (let* ([cmf #x78]
           [flevel (cond [(<= level 1) 0] [(<= level 5) 1] [(= level 6) 2] [else 3])]
           [flg (* flevel 64)]
           [flg (+ flg (- 31 (mod (+ (* cmf 256) flg) 31)))])
      (u8-list->bytevector (list cmf flg))))

  (define (u32-be x)
    (let ([bv (make-bytevector 4)]) (bytevector-u32-set! bv 0 x (endianness big)) bv))

  (define (u32-le x)
    (let ([bv (make-bytevector 4)]) (bytevector-u32-set! bv 0 x (endianness little)) bv))

  (define (latin-1 who s)
    (let ([bv (make-bytevector (string-length s))])
      (do ([i 0 (+ i 1)]) ((= i (string-length s)) bv)
        (let ([c (char->integer (string-ref s i))])
          (when (or (= c 0) (> c 255))
            (assertion-violation who "#:name must be a string of Latin-1 characters other than NUL" s))
          (bytevector-u8-set! bv i c)))))

  (define (gzip-header who level name mtime)
    (unless (or (not name) (string? name))
      (assertion-violation who "#:name must be a string or #f" name))
    (unless (and (integer? mtime) (exact? mtime) (<= 0 mtime #xFFFFFFFF))
      (assertion-violation who "#:mtime must be an exact integer from 0 to 2^32 - 1" mtime))
    (let ([h (make-bytevector 10 0)])
      (bytevector-u8-set! h 0 #x1f)
      (bytevector-u8-set! h 1 #x8b)
      (bytevector-u8-set! h 2 8)
      (bytevector-u8-set! h 3 (if name 8 0))
      (bytevector-u32-set! h 4 mtime (endianness little))
      (bytevector-u8-set! h 8 (cond [(= level 9) 2] [(= level 1) 4] [else 0]))
      (bytevector-u8-set! h 9 255)
      (if name
        (bytevector-concat (list h (latin-1 who name) #vu8(0)))
        h)))

  ;; ---- Compression -----------------------------------------------------------------------

  ;; Raw DEFLATE data for BV.
  ;;
  ;;   (deflate (make-bytevector 1000 65))   ; => a 10-byte bytevector
  (define (deflate bv #:level [level 6] #:strategy [strategy 'default])
    (check-bytevector 'deflate bv)
    (deflate-bytevector bv 0 (bytevector-length bv)
                        (check-level 'deflate level) (check-strategy 'deflate strategy)))

  ;; BV in the zlib format: a 2-byte header, DEFLATE data and the Adler-32 of BV.
  (define (zlib-compress bv #:level [level 6] #:strategy [strategy 'default])
    (check-bytevector 'zlib-compress bv)
    (check-level 'zlib-compress level)
    (bytevector-concat
      (list (zlib-header level)
            (deflate-bytevector bv 0 (bytevector-length bv) level (check-strategy 'zlib-compress strategy))
            (u32-be (adler32 bv)))))

  ;; BV as a gzip file of one member.
  ;;
  ;;   (gzip-compress bv #:name "notes.txt" #:mtime 1700000000)
  (define (gzip-compress bv #:level [level 6] #:strategy [strategy 'default]
                         #:name [name #f] #:mtime [mtime 0])
    (check-bytevector 'gzip-compress bv)
    (check-level 'gzip-compress level)
    (bytevector-concat
      (list (gzip-header 'gzip-compress level name mtime)
            (deflate-bytevector bv 0 (bytevector-length bv) level (check-strategy 'gzip-compress strategy))
            (u32-le (crc32 bv))
            (u32-le (bitwise-and (bytevector-length bv) #xFFFFFFFF)))))

  ;; BV compressed in FORMAT: gzip (the default), zlib or deflate.
  (define (compress bv #:format [format 'gzip] #:level [level 6] #:strategy [strategy 'default])
    (case (check-format 'compress format #f)
      [(gzip) (gzip-compress bv #:level level #:strategy strategy)]
      [(zlib) (zlib-compress bv #:level level #:strategy strategy)]
      [else (deflate bv #:level level #:strategy strategy)]))

  ;; ---- Decompression ---------------------------------------------------------------------
  ;;
  ;; A reader is the input side: a buffer `in` with unread bytes [ip, iend), filled from a
  ;; port, or one bytevector with nothing more to come. A decoder reads a header, runs the
  ;; inflater over the body and checks the trailer, member after member for gzip.

  (define-record-type reader
    (fields (mutable in) (mutable ip) (mutable iend) (mutable eof?) port))

  (define (bytevector-reader bv) (make-reader bv 0 (bytevector-length bv) #t #f))

  (define (port-reader port) (make-reader (make-bytevector 65536 0) 0 0 #f port))

  ;; Reads until at least MIN unread bytes are buffered or the port ends.
  (define (reader-fill! r min)
    (let loop ()
      (when (and (not (reader-eof? r)) (< (- (reader-iend r) (reader-ip r)) min))
        (let ([chunk (get-bytevector-some (reader-port r))])
          (if (eof-object? chunk)
            (reader-eof?-set! r #t)
            (let* ([in (reader-in r)]
                   [ip (reader-ip r)]
                   [left (- (reader-iend r) ip)]
                   [need (+ left (bytevector-length chunk))]
                   [dest (if (> need (bytevector-length in))
                           (make-bytevector (max need (* 2 (bytevector-length in))) 0)
                           in)])
              (bytevector-copy! in ip dest 0 left)
              (bytevector-copy! chunk 0 dest left (bytevector-length chunk))
              (reader-in-set! r dest)
              (reader-ip-set! r 0)
              (reader-iend-set! r need)
              (loop)))))))

  (define (reader-u8! r who)
    (reader-fill! r 1)
    (let ([ip (reader-ip r)])
      (when (>= ip (reader-iend r))
        (compress-error who "compressed data ends early"))
      (reader-ip-set! r (+ ip 1))
      (bytevector-u8-ref (reader-in r) ip)))

  (define (reader-u16le! r who)
    (let* ([a (reader-u8! r who)] [b (reader-u8! r who)]) (+ a (* 256 b))))

  (define (reader-u32! r who big?)
    (let* ([a (reader-u8! r who)] [b (reader-u8! r who)] [c (reader-u8! r who)] [d (reader-u8! r who)])
      (if big?
        (+ (* a #x1000000) (* b #x10000) (* c #x100) d)
        (+ a (* b #x100) (* c #x10000) (* d #x1000000)))))

  ;; The next two bytes without consuming them, or #f if fewer remain.
  (define (reader-peek2 r)
    (reader-fill! r 2)
    (let ([ip (reader-ip r)])
      (and (<= (+ ip 2) (reader-iend r))
           (cons (bytevector-u8-ref (reader-in r) ip)
                 (bytevector-u8-ref (reader-in r) (+ ip 1))))))

  (define (gzip-magic? p) (and p (= (car p) #x1f) (= (cdr p) #x8b)))

  (define (zlib-magic? p)
    (and p (= (bitwise-and (car p) 15) 8) (<= (bitwise-arithmetic-shift-right (car p) 4) 7)
         (= 0 (mod (+ (* 256 (car p)) (cdr p)) 31))))

  (define (read-gzip-header! r who)
    (let* ([id1 (reader-u8! r who)] [id2 (reader-u8! r who)])
      (unless (and (= id1 #x1f) (= id2 #x8b))
        (compress-error who "not gzip data: bad magic number" id1 id2)))
    (let* ([cm (reader-u8! r who)] [flg (reader-u8! r who)])
      (unless (= cm 8)
        (compress-error who "unsupported gzip compression method" cm))
      (unless (= 0 (bitwise-and flg #xE0))
        (compress-error who "invalid gzip header: reserved flags set" flg))
      (do ([i 0 (+ i 1)]) ((= i 6)) (reader-u8! r who))      ; MTIME, XFL, OS
      (when (logbit? flg 2)                                    ; FEXTRA
        (let ([xlen (reader-u16le! r who)])
          (do ([i 0 (+ i 1)]) ((= i xlen)) (reader-u8! r who))))
      (when (logbit? flg 3) (skip-zero-terminated! r who))     ; FNAME
      (when (logbit? flg 4) (skip-zero-terminated! r who))     ; FCOMMENT
      (when (logbit? flg 1) (reader-u16le! r who))))           ; FHCRC

  (define (logbit? x k) (not (= 0 (bitwise-and x (bitwise-arithmetic-shift-left 1 k)))))

  (define (skip-zero-terminated! r who)
    (let loop () (unless (= 0 (reader-u8! r who)) (loop))))

  (define (read-zlib-header! r who)
    (let* ([cmf (reader-u8! r who)] [flg (reader-u8! r who)])
      (unless (zlib-magic? (cons cmf flg))
        (compress-error who "not zlib data: bad header" cmf flg))
      (when (logbit? flg 5)
        (compress-error who "zlib data with a preset dictionary is not supported"))))

  ;; The decoder. FORMAT is gzip, zlib or deflate once the first header is read. GROW? makes
  ;; the output buffer grow (one-shot) instead of sliding (ports).
  (define-record-type decoder
    (fields who reader (mutable format) grow?
            (mutable phase)        ; header, body, trailer or end
            (mutable inflater)
            (mutable out) (mutable op)
            (mutable ostart)       ; output before this was delivered
            (mutable ck)           ; running CRC-32 or Adler-32 of the member
            (mutable ck-pos)       ; output before this is in ck
            (mutable size)))       ; bytes of output in the member, modulo 2^32

  (define (new-decoder who r format grow? out-size)
    (make-decoder who r format grow? 'header #f (make-bytevector out-size 0) 0 0 0 0 0))

  (define (start-member! d)
    (let ([r (decoder-reader d)] [who (decoder-who d)])
      (when (eq? (decoder-format d) 'auto)
        (let ([p (reader-peek2 r)])
          (decoder-format-set! d (cond [(gzip-magic? p) 'gzip] [(zlib-magic? p) 'zlib] [else 'deflate]))))
      (case (decoder-format d)
        [(gzip) (read-gzip-header! r who) (decoder-ck-set! d 0)]
        [(zlib) (read-zlib-header! r who) (decoder-ck-set! d 1)]
        [else #f])
      (decoder-ck-pos-set! d (decoder-op d))
      (decoder-size-set! d 0)
      (reader-fill! r 1100)
      (decoder-inflater-set! d (make-inflater (reader-in r) (reader-ip r) (reader-iend r) (reader-eof? r)
                                              (decoder-out d)))
      (inflater-set-op! (decoder-inflater d) (decoder-op d))
      (decoder-phase-set! d 'body)))

  ;; Brings the checksum and size up to the output produced so far.
  (define (account! d)
    (let ([from (decoder-ck-pos d)] [to (decoder-op d)])
      (when (> to from)
        (case (decoder-format d)
          [(gzip)
           (decoder-ck-set! d (crc32-update (decoder-ck d) (decoder-out d) from to))
           (decoder-size-set! d (bitwise-and (+ (decoder-size d) (- to from)) #xFFFFFFFF))]
          [(zlib) (decoder-ck-set! d (adler32-update (decoder-ck d) (decoder-out d) from to))]
          [else #f])
        (decoder-ck-pos-set! d to))))

  ;; Makes room in the output buffer: grows it, or keeps its last 32 KiB (the window) once
  ;; everything has been delivered.
  (define (make-room! d)
    (let* ([out (decoder-out d)] [op (decoder-op d)] [st (decoder-inflater d)])
      (if (decoder-grow? d)
        (let ([bigger (make-bytevector (* 2 (bytevector-length out)) 0)])
          (bytevector-copy! out 0 bigger 0 op)
          (decoder-out-set! d bigger)
          (inflater-set-output! st bigger op))
        (let ([shift (- op 32768)])
          (when (> shift 0)
            (bytevector-copy! out shift out 0 32768)
            (decoder-op-set! d 32768)
            (decoder-ostart-set! d (- (decoder-ostart d) shift))
            (decoder-ck-pos-set! d (- (decoder-ck-pos d) shift))
            (inflater-set-output! st out 32768))))))

  (define (run-body! d)
    (let* ([r (decoder-reader d)] [st (decoder-inflater d)]
           [status (begin
                     (inflater-set-input! st (reader-in r) (reader-ip r) (reader-iend r) (reader-eof? r))
                     (inflater-run! st))])
      (reader-ip-set! r (inflater-ip st))
      (decoder-op-set! d (inflater-op st))
      (account! d)
      (case status
        [(need-input) (reader-fill! r 1100)]
        [(need-output) (when (or (decoder-grow? d) (= (decoder-ostart d) (decoder-op d))) (make-room! d))]
        [(done)
         (reader-ip-set! r (inflater-align! st))
         (decoder-phase-set! d 'trailer)])))

  (define (check-trailer! d)
    (let ([r (decoder-reader d)] [who (decoder-who d)])
      (case (decoder-format d)
        [(gzip)
         (let* ([crc (reader-u32! r who #f)] [size (reader-u32! r who #f)])
           (unless (= crc (decoder-ck d))
             (compress-error who "gzip data is corrupt: CRC-32 mismatch" crc (decoder-ck d)))
           (unless (= size (decoder-size d))
             (compress-error who "gzip data is corrupt: size mismatch" size (decoder-size d)))
           ;; Another member may follow.
           (decoder-phase-set! d (if (gzip-magic? (reader-peek2 r)) 'header 'end)))]
        [(zlib)
         (let ([adler (reader-u32! r who #t)])
           (unless (= adler (decoder-ck d))
             (compress-error who "zlib data is corrupt: Adler-32 mismatch" adler (decoder-ck d)))
           (decoder-phase-set! d 'end))]
        [else (decoder-phase-set! d 'end)])))

  ;; Runs the decoder until it has undelivered output or ends. Returns #f at the end.
  (define (decoder-pull! d)
    (let loop ()
      (cond
        [(< (decoder-ostart d) (decoder-op d)) #t]
        [else
         (case (decoder-phase d)
           [(header) (start-member! d) (loop)]
           [(body) (run-body! d) (loop)]
           [(trailer) (check-trailer! d) (loop)]
           [else #f])])))

  (define (decode-all who bv format)
    (check-bytevector who bv)
    (let ([d (new-decoder who (bytevector-reader bv) format #t
                          (max 1024 (min (* 4 (bytevector-length bv)) 67108864)))])
      (let loop ()
        (case (decoder-phase d)
          [(end)
           (let ([result (make-bytevector (decoder-op d))])
             (bytevector-copy! (decoder-out d) 0 result 0 (decoder-op d))
             result)]
          [(header) (start-member! d) (loop)]
          [(body) (run-body! d) (loop)]
          [else (check-trailer! d) (loop)]))))

  ;; The bytes of the raw DEFLATE data BV.
  (define (inflate bv)
    (decode-all 'inflate bv 'deflate))

  ;; The bytes of the zlib data BV. The Adler-32 is checked.
  (define (zlib-decompress bv)
    (decode-all 'zlib-decompress bv 'zlib))

  ;; The bytes of the gzip data BV: every member, concatenated. CRC-32s and sizes are checked.
  (define (gzip-decompress bv)
    (decode-all 'gzip-decompress bv 'gzip))

  ;; The bytes of BV in FORMAT: auto (the default: gzip and zlib by their headers, else raw
  ;; DEFLATE), gzip, zlib or deflate.
  (define (decompress bv #:format [format 'auto])
    (decode-all 'decompress bv (check-format 'decompress format #t)))

  ;; ---- Ports -----------------------------------------------------------------------------

  ;; A binary input port of the decompressed bytes of PORT, a binary input port. It reads
  ;; ahead, so bytes after the compressed data are consumed from PORT too.
  ;;
  ;;   (get-bytevector-all (open-decompress-input-port (open-file-input-port "a.gz")))
  (define (open-decompress-input-port port #:format [format 'auto] #:close? [close? #t])
    (unless (and (input-port? port) (binary-port? port))
      (assertion-violation 'open-decompress-input-port "expected a binary input port" port))
    (let ([d (new-decoder 'open-decompress-input-port (port-reader port)
                          (check-format 'open-decompress-input-port format #t) #f (+ 32768 65536 1024))])
      (make-custom-binary-input-port
        "decompress"
        (lambda (bv start count)
          (if (and (> count 0) (decoder-pull! d))
            (let* ([ostart (decoder-ostart d)]
                   [n (min count (- (decoder-op d) ostart))])
              (bytevector-copy! (decoder-out d) ostart bv start n)
              (decoder-ostart-set! d (+ ostart n))
              n)
            0))
        #f #f
        (lambda () (when close? (close-port port))))))

  ;; A binary output port that compresses what is written to it into PORT, a binary output
  ;; port. Closing it finishes the stream (and closes PORT if #:close? is true); data written
  ;; to it is complete in PORT only then.
  ;;
  ;;   (let ([out (open-compress-output-port (open-file-output-port "out.gz") #:level 9)])
  ;;     (put-bytevector out data)
  ;;     (close-port out))
  (define (open-compress-output-port port #:format [format 'gzip] #:level [level 6]
                                     #:strategy [strategy 'default]
                                     #:name [name #f] #:mtime [mtime 0] #:close? [close? #t])
    (define who 'open-compress-output-port)
    (unless (and (output-port? port) (binary-port? port))
      (assertion-violation who "expected a binary output port" port))
    (check-format who format #f)
    (check-level who level)
    (check-strategy who strategy)
    (let ([st (make-deflater level strategy #t)]
          [ck (if (eq? format 'zlib) 1 0)]
          [size 0]
          [closed? #f])
      (define (drain!)
        (let ([bytes (deflater-take-output! st)])
          (when (> (bytevector-length bytes) 0)
            (put-bytevector port bytes))))
      (case format
        [(gzip) (put-bytevector port (gzip-header who level name mtime))]
        [(zlib) (put-bytevector port (zlib-header level))]
        [else #f])
      (make-custom-binary-output-port
        "compress"
        (lambda (bv start count)
          (when closed?
            (assertion-violation who "write to a closed compress port"))
          (case format
            [(gzip)
             (set! ck (crc32-update ck bv start (+ start count)))
             (set! size (bitwise-and (+ size count) #xFFFFFFFF))]
            [(zlib) (set! ck (adler32-update ck bv start (+ start count)))]
            [else #f])
          (deflater-write! st bv start (+ start count))
          (drain!)
          count)
        #f #f
        (lambda ()
          (unless closed?
            (set! closed? #t)
            (deflater-finish! st)
            (drain!)
            (case format
              [(gzip) (put-bytevector port (u32-le ck)) (put-bytevector port (u32-le size))]
              [(zlib) (put-bytevector port (u32-be ck))]
              [else #f])
            (flush-output-port port)
            (when close? (close-port port))))))))
