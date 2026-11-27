;;; DEFLATE decompression (RFC 1951) as a resumable state machine.
;;;
;;; An inflater reads compressed bytes from in[ip, iend) and writes the decompressed bytes to
;;; out[op, ...). `inflater-run!` runs until one of three things happens:
;;;
;;;   done          the final block ended
;;;   need-output   out is (nearly) full: the caller empties or grows it, then runs again
;;;   need-input    in is nearly used up: the caller moves in[ip, iend) to the front, reads
;;;                 more after it, then runs again
;;;
;;; When the inflater is told that no more input will come (EOF), it reads zeros past iend and
;;; reports data that ends early as an error, so a one-shot decompression never stops for
;;; input. Matches refer back into out, so a caller that empties out must keep its last 32 KiB.
;;;
;;; Huffman codes are decoded with one flat table per code, indexed by the next MAXLEN bits of
;;; input (MAXLEN is the longest code length, at most 15). An entry packs
;;;
;;;   bits 0-3    the code length (0: no code, the input is invalid)
;;;   bits 4-7    the number of extra bits (length and distance codes)
;;;   bits 8-9    the kind: 0 literal, 1 length, 2 end of block (literal/length code only)
;;;   bits 10..   the literal byte, or the base length or distance
;;;
;;; The bit buffer is a fixnum holding BC bits, least significant first. It is refilled two
;;; bytes at a time when it holds fewer than 16 bits, so it never holds more than 31: every
;;; value in the decoder is a fixnum.

(library (capy compress inflate)
  (export make-inflater inflater-run! inflater-align!
    inflater-in inflater-ip inflater-iend inflater-eof? inflater-out inflater-op
    inflater-set-input! inflater-set-output! inflater-set-op!
    inflate-range)
  (import (rnrs)
    (capy compress tables))


  (define-syntax define-fields
    (syntax-rules ()
      [(_ (name index) ...)
       (begin (define-syntax name (identifier-syntax index)) ...)]))

  (define-fields
    (f:in 0) (f:ip 1) (f:iend 2) (f:eof 3) (f:out 4) (f:op 5) (f:bb 6) (f:bc 7) (f:mode 8)
    (f:final 9) (f:ltab 10) (f:lmask 11) (f:dtab 12) (f:dmask 13) (f:remaining 14)
    (f:lscratch 15) (f:dscratch 16) (f:lens 17) (f:cltab 18))

  ;; Modes.
  (define-syntax m:header (identifier-syntax 0))
  (define-syntax m:codes (identifier-syntax 1))
  (define-syntax m:stored (identifier-syntax 2))
  (define-syntax m:done (identifier-syntax 3))

  (define (make-inflater in ip iend eof? out)
    (let ([st (make-vector 19 0)])
      (vector-set! st f:in in)
      (vector-set! st f:ip ip)
      (vector-set! st f:iend iend)
      (vector-set! st f:eof eof?)
      (vector-set! st f:out out)
      (vector-set! st f:op 0)
      (vector-set! st f:mode m:header)
      (vector-set! st f:final #f)
      (vector-set! st f:ltab #f)
      (vector-set! st f:dtab #f)
      (vector-set! st f:lscratch #f)
      (vector-set! st f:dscratch #f)
      (vector-set! st f:lens (make-vector 320 0))
      (vector-set! st f:cltab (make-vector 128 0))
      st))

  (define (inflater-in st) (vector-ref st f:in))
  (define (inflater-ip st) (vector-ref st f:ip))
  (define (inflater-iend st) (vector-ref st f:iend))
  (define (inflater-eof? st) (vector-ref st f:eof))
  (define (inflater-out st) (vector-ref st f:out))
  (define (inflater-op st) (vector-ref st f:op))

  (define (inflater-set-input! st in ip iend eof?)
    (vector-set! st f:in in)
    (vector-set! st f:ip ip)
    (vector-set! st f:iend iend)
    (vector-set! st f:eof eof?))

  (define (inflater-set-output! st out op)
    (vector-set! st f:out out)
    (vector-set! st f:op op))

  (define (inflater-set-op! st op) (vector-set! st f:op op))

  ;; After `done`: drop the bits of the last byte and give back the whole bytes still in the
  ;; bit buffer, so that ip is the position just after the compressed data.
  (define (inflater-align! st)
    (let* ([bc (vector-ref st f:bc)]
           [ip (fx- (vector-ref st f:ip) (fxarithmetic-shift-right bc 3))])
      (vector-set! st f:bb 0)
      (vector-set! st f:bc 0)
      (vector-set! st f:ip ip)
      (when (fx>? ip (vector-ref st f:iend))
        (compress-error 'inflate "compressed data ends early"))
      ip))

  (define (peek16-slow in ip iend)
    (fxior (if (fx<? ip iend) (bytevector-u8-ref in ip) 0)
           (fxarithmetic-shift-left
             (if (fx<? (fx+ ip 1) iend) (bytevector-u8-ref in (fx+ ip 1)) 0)
             8)))

  ;; The next two bytes, little endian, with zeros past iend.
  (define-syntax peek16
    (syntax-rules ()
      [(_ in ip iend)
       (if (fx<? (fx+ ip 1) iend)
         (bytevector-u16-ref in ip (endianness little))
         (peek16-slow in ip iend))]))

  ;; Makes sure the bit buffer holds at least 16 bits, then evaluates BODY with BB, BC and IP
  ;; rebound. K is a join point: the compiler contifies it, so this is one branch.
  (define-syntax refill
    (syntax-rules ()
      [(_ (bb bc ip) in iend body ...)
       (let ([k (lambda (bb bc ip) body ...)])
         (if (fx<? bc 16)
           (k (fxior bb (fxarithmetic-shift-left (peek16 in ip iend) bc)) (fx+ bc 16) (fx+ ip 2))
           (k bb bc ip)))]))

  (define-syntax low-bits
    (syntax-rules ()
      [(_ bb n) (fxand bb (fx- (fxarithmetic-shift-left 1 n) 1))]))

  ;; ---- Decoding tables -------------------------------------------------------------------

  (define (lit-entry sym len)
    (cond
      [(fx<? sym 256) (fxior (fxarithmetic-shift-left sym 10) len)]
      [(fx=? sym 256) (fxior (fxarithmetic-shift-left 2 8) len)]
      [(fx<? sym 286)
       (let ([i (fx- sym 257)])
         (fxior (fxarithmetic-shift-left (vector-ref length-base i) 10)
                (fxarithmetic-shift-left 1 8)
                (fxarithmetic-shift-left (vector-ref length-extra i) 4)
                len))]
      [else 0]))

  (define (dist-entry sym len)
    (if (fx<? sym 30)
      (fxior (fxarithmetic-shift-left (vector-ref dist-base sym) 10)
             (fxarithmetic-shift-left (vector-ref dist-extra sym) 4)
             len)
      0))

  (define (clen-entry sym len)
    (fxior (fxarithmetic-shift-left sym 10) len))

  ;; Fills TABLE for the canonical code with lengths lens[start, start + count) and returns
  ;; the index mask. ENTRY makes the entry of a symbol. An over-subscribed code is an error; an
  ;; incomplete one leaves zero entries, which are an error only if the input reaches them.
  (define (build-table! who table lens start count entry)
    (let ([bl-count (make-vector 16 0)]
          [next (make-vector 16 0)])
      (do ([i 0 (fx+ i 1)]) ((fx=? i count))
        (let ([l (vector-ref lens (fx+ start i))])
          (vector-set! bl-count l (fx+ 1 (vector-ref bl-count l)))))
      (vector-set! bl-count 0 0)
      (let ([maxlen (let loop ([l 15]) (if (and (fx>? l 0) (fx=? 0 (vector-ref bl-count l))) (loop (fx- l 1)) l))])
        (let kraft ([l 1] [left 1])
          (when (fx<=? l 15)
            (let ([left (fx- (fx* left 2) (vector-ref bl-count l))])
              (when (fx<? left 0)
                (compress-error who "invalid Huffman code: over-subscribed code lengths"))
              (when (and (fx=? l 15) (fx>? left 0))
                ;; Incomplete: clear the entries no code will cover.
                (do ([i 0 (fx+ i 1)])
                    ((fx=? i (fxarithmetic-shift-left 1 (fxmax maxlen 1))))
                  (vector-set! table i 0)))
              (kraft (fx+ l 1) left))))
        (let loop ([l 1] [code 0])
          (when (fx<=? l 15)
            (let ([code (fx* 2 (fx+ code (vector-ref bl-count (fx- l 1))))])
              (vector-set! next l code)
              (loop (fx+ l 1) code))))
        (let* ([bits (fxmax maxlen 1)]
               [size (fxarithmetic-shift-left 1 bits)])
          (do ([sym 0 (fx+ sym 1)]) ((fx=? sym count))
            (let ([l (vector-ref lens (fx+ start sym))])
              (when (fx>? l 0)
                (let* ([code (vector-ref next l)]
                       [e (entry sym l)]
                       [step (fxarithmetic-shift-left 1 l)])
                  (vector-set! next l (fx+ code 1))
                  (let fill ([j (reverse-bits code l)])
                    (when (fx<? j size)
                      (vector-set! table j e)
                      (fill (fx+ j step))))))))
          (fx- size 1)))))

  (define-values (fixed-ltab fixed-lmask fixed-dtab fixed-dmask)
    (let ([lens (make-vector 288 0)]
          [ltab (make-vector 512 0)]
          [dtab (make-vector 32 0)])
      (do ([s 0 (fx+ s 1)]) ((fx=? s 288))
        (vector-set! lens s (cond [(fx<? s 144) 8] [(fx<? s 256) 9] [(fx<? s 280) 7] [else 8])))
      (let ([lmask (build-table! 'inflate ltab lens 0 288 lit-entry)])
        (vector-fill! lens 5)
        (let ([dmask (build-table! 'inflate dtab lens 0 30 dist-entry)])
          (values ltab lmask dtab dmask)))))

  ;; The bit reader of the cold dynamic-header code: a vector #(bb bc ip) and the input.
  ;; (Captured variables updated with set! here made Tier 2 compiles on the background worker
  ;; read stale bit counts now and then; a vector does not.)
  (define (header-refill! r in iend)
    (let ([bc (vector-ref r 1)])
      (when (fx<? bc 16)
        (let ([ip (vector-ref r 2)])
          (vector-set! r 0 (fxior (vector-ref r 0) (fxarithmetic-shift-left (peek16 in ip iend) bc)))
          (vector-set! r 1 (fx+ bc 16))
          (vector-set! r 2 (fx+ ip 2))))))

  (define (header-bits! r in iend n)
    (header-refill! r in iend)
    (let* ([bb (vector-ref r 0)] [v (low-bits bb n)])
      (vector-set! r 0 (fxarithmetic-shift-right bb n))
      (vector-set! r 1 (fx- (vector-ref r 1) n))
      v))

  (define (header-decode! r in iend table mask)
    (header-refill! r in iend)
    (let* ([bb (vector-ref r 0)]
           [e (vector-ref table (fxand bb mask))]
           [n (fxand e 15)])
      (when (fx=? n 0)
        (compress-error 'inflate "invalid code-length code"))
      (vector-set! r 0 (fxarithmetic-shift-right bb n))
      (vector-set! r 1 (fx- (vector-ref r 1) n))
      (fxarithmetic-shift-right e 10)))

  ;; Reads the code lengths of a dynamic block (RFC 1951, 3.2.7) and builds its tables into
  ;; the state. Returns the new bit position. Cold: the header is at most ~600 bytes.
  (define (read-dynamic-header! st in bb bc ip iend)
    (define r (vector bb bc ip))
    (define (bits! n) (header-bits! r in iend n))
    (define (decode! table mask) (header-decode! r in iend table mask))
    (let* ([hlit (fx+ 257 (bits! 5))]
           [hdist (fx+ 1 (bits! 5))]
           [hclen (fx+ 4 (bits! 4))]
           [lens (vector-ref st f:lens)]
           [cltab (vector-ref st f:cltab)])
      (when (or (fx>? hlit 286) (fx>? hdist 30))
        (compress-error 'inflate "invalid block header: too many length or distance codes" hlit hdist))
      (vector-fill! lens 0)
      (do ([i 0 (fx+ i 1)]) ((fx=? i hclen))
        (vector-set! lens (vector-ref clen-order i) (bits! 3)))
      (let ([clmask (build-table! 'inflate cltab lens 0 19 clen-entry)]
            [n (fx+ hlit hdist)])
        (vector-fill! lens 0)
        (let loop ([i 0])
          (when (fx<? i n)
            (let ([sym (decode! cltab clmask)])
              (define (repeat! value count)
                (when (fx>? (fx+ i count) n)
                  (compress-error 'inflate "invalid code lengths: a repeat runs past the end"))
                (do ([k 0 (fx+ k 1)]) ((fx=? k count))
                  (vector-set! lens (fx+ i k) value))
                (loop (fx+ i count)))
              (cond
                [(fx<? sym 16) (vector-set! lens i sym) (loop (fx+ i 1))]
                [(fx=? sym 16)
                 (when (fx=? i 0)
                   (compress-error 'inflate "invalid code lengths: repeat with no previous length"))
                 (let ([prev (vector-ref lens (fx- i 1))])
                   (repeat! prev (fx+ 3 (bits! 2))))]
                [(fx=? sym 17) (repeat! 0 (fx+ 3 (bits! 3)))]
                [else (repeat! 0 (fx+ 11 (bits! 7)))]))))
        (when (fx=? 0 (vector-ref lens 256))
          (compress-error 'inflate "invalid block: no end-of-block code"))
        (let ([ltab (or (vector-ref st f:lscratch)
                        (let ([t (make-vector 32768 0)]) (vector-set! st f:lscratch t) t))]
              [dtab (or (vector-ref st f:dscratch)
                        (let ([t (make-vector 32768 0)]) (vector-set! st f:dscratch t) t))])
          ;; The distance lengths follow the literal/length lengths in `lens`; build-table!
          ;; numbers the symbols from START.
          (vector-set! st f:lmask (build-table! 'inflate ltab lens 0 hlit lit-entry))
          (vector-set! st f:dmask (build-table! 'inflate dtab lens hlit hdist dist-entry))
          (vector-set! st f:ltab ltab)
          (vector-set! st f:dtab dtab)
          (values (vector-ref r 0) (vector-ref r 1) (vector-ref r 2))))))

  (define (inflater-run! st)
    (let* ([in (vector-ref st f:in)]
           [iend (vector-ref st f:iend)]
           [eof? (vector-ref st f:eof)]
           [out (vector-ref st f:out)]
           [ilimit (if eof? (fx+ iend 4) (fx- iend 1024))]
           [olimit (fx- (bytevector-length out) 266)])

      (define (suspend mode bb bc ip op status)
        (vector-set! st f:mode mode)
        (vector-set! st f:bb bb)
        (vector-set! st f:bc bc)
        (vector-set! st f:ip ip)
        (vector-set! st f:op op)
        status)

      (define (out-of-input mode bb bc ip op)
        (if eof?
          (compress-error 'inflate "compressed data ends early")
          (suspend mode bb bc ip op 'need-input)))

      (define (header bb bc ip op)
        (cond
          [(vector-ref st f:final) (suspend m:done bb bc ip op 'done)]
          [(fx>? ip ilimit) (out-of-input m:header bb bc ip op)]
          [else
           (refill (bb bc ip) in iend
             (let ([final? (fx=? 1 (fxand bb 1))]
                   [type (fxand (fxarithmetic-shift-right bb 1) 3)]
                   [bb (fxarithmetic-shift-right bb 3)]
                   [bc (fx- bc 3)])
               (vector-set! st f:final final?)
               (case type
                 [(0)
                  ;; Stored: skip to a byte boundary, read LEN and NLEN.
                  (let* ([drop (fxand bc 7)]
                         [bb (fxarithmetic-shift-right bb drop)]
                         [bc (fx- bc drop)])
                    (refill (bb bc ip) in iend
                      (let ([len (fxand bb #xFFFF)]
                            [bb (fxarithmetic-shift-right bb 16)]
                            [bc (fx- bc 16)])
                        (refill (bb bc ip) in iend
                          (let ([nlen (fxand bb #xFFFF)]
                                [bb (fxarithmetic-shift-right bb 16)]
                                [bc (fx- bc 16)])
                            (unless (fx=? len (fxxor nlen #xFFFF))
                              (compress-error 'inflate "invalid stored block: LEN and NLEN do not match" len nlen))
                            ;; Give back the whole bytes still buffered: the data is copied
                            ;; straight from `in`.
                            (stored 0 0 (fx- ip (fxarithmetic-shift-right bc 3)) op len))))))]
                 [(1)
                  (vector-set! st f:ltab fixed-ltab)
                  (vector-set! st f:lmask fixed-lmask)
                  (vector-set! st f:dtab fixed-dtab)
                  (vector-set! st f:dmask fixed-dmask)
                  (codes bb bc ip op)]
                 [(2)
                  (let-values ([(bb bc ip) (read-dynamic-header! st in bb bc ip iend)])
                    (codes bb bc ip op))]
                 [else (compress-error 'inflate "invalid block type 3")])))]))

      (define (stored bb bc ip op remaining)
        (let ([n (fxmax 0 (fxmin remaining (fxmin (fx- iend ip) (fx- (bytevector-length out) op))))])
          (when (fx>? n 0)
            (bytevector-copy! in ip out op n))
          (let ([ip (fx+ ip n)] [op (fx+ op n)] [remaining (fx- remaining n)])
            (cond
              [(fx=? remaining 0) (header bb bc ip op)]
              [(fx>=? ip iend)
               (vector-set! st f:remaining remaining)
               (out-of-input m:stored bb bc ip op)]
              [else
               (vector-set! st f:remaining remaining)
               (suspend m:stored bb bc ip op 'need-output)]))))

      (define (codes bb bc ip op)
        (let ([ltab (vector-ref st f:ltab)]
              [lmask (vector-ref st f:lmask)]
              [dtab (vector-ref st f:dtab)]
              [dmask (vector-ref st f:dmask)])
          (let loop ([bb bb] [bc bc] [ip ip] [op op])
            (cond
              [(fx>? op olimit) (suspend m:codes bb bc ip op 'need-output)]
              [(fx>? ip ilimit) (out-of-input m:codes bb bc ip op)]
              [else
               (refill (bb bc ip) in iend
                 (let* ([e (vector-ref ltab (fxand bb lmask))]
                        [n (fxand e 15)]
                        [bb (fxarithmetic-shift-right bb n)]
                        [bc (fx- bc n)]
                        [kind (fxand (fxarithmetic-shift-right e 8) 3)])
                   (cond
                     [(fx=? kind 0)
                      (when (fx=? n 0)
                        (compress-error 'inflate "invalid literal/length code"))
                      (bytevector-u8-set! out op (fxarithmetic-shift-right e 10))
                      (loop bb bc ip (fx+ op 1))]
                     [(fx=? kind 1)
                      (refill (bb bc ip) in iend
                        (let* ([x (fxand (fxarithmetic-shift-right e 4) 15)]
                               [len (fx+ (fxarithmetic-shift-right e 10) (low-bits bb x))]
                               [bb (fxarithmetic-shift-right bb x)]
                               [bc (fx- bc x)])
                          (refill (bb bc ip) in iend
                            (let* ([d (vector-ref dtab (fxand bb dmask))]
                                   [n (fxand d 15)]
                                   [bb (fxarithmetic-shift-right bb n)]
                                   [bc (fx- bc n)])
                              (when (fx=? n 0)
                                (compress-error 'inflate "invalid distance code"))
                              (refill (bb bc ip) in iend
                                (let* ([x (fxand (fxarithmetic-shift-right d 4) 15)]
                                       [dist (fx+ (fxarithmetic-shift-right d 10) (low-bits bb x))]
                                       [bb (fxarithmetic-shift-right bb x)]
                                       [bc (fx- bc x)]
                                       [from (fx- op dist)]
                                       [end (fx+ op len)])
                                  (when (fx<? from 0)
                                    (compress-error 'inflate "invalid distance: too far back" dist))
                                  (if (fx>=? dist 8)
                                    ;; Eight bytes at a time (Tier 2 inlines the u64 accessors,
                                    ;; not the u32 store); may write up to 7 bytes past END,
                                    ;; which the 266 bytes of slack above olimit allow.
                                    (let copy ([s from] [o op])
                                      (if (fx<? o end)
                                        (begin
                                          (bytevector-u64-native-set! out o (bytevector-u64-native-ref out s))
                                          (copy (fx+ s 8) (fx+ o 8)))
                                        (loop bb bc ip end)))
                                    (let copy ([s from] [o op])
                                      (if (fx<? o end)
                                        (begin
                                          (bytevector-u8-set! out o (bytevector-u8-ref out s))
                                          (copy (fx+ s 1) (fx+ o 1)))
                                        (loop bb bc ip end))))))))))]
                     [(fx=? kind 2) (header bb bc ip op)]
                     [else (compress-error 'inflate "invalid literal/length code")])))]))))

      (let ([mode (vector-ref st f:mode)]
            [bb (vector-ref st f:bb)]
            [bc (vector-ref st f:bc)]
            [ip (vector-ref st f:ip)]
            [op (vector-ref st f:op)])
        (cond
          [(fx=? mode m:header) (header bb bc ip op)]
          [(fx=? mode m:codes) (codes bb bc ip op)]
          [(fx=? mode m:stored) (stored bb bc ip op (vector-ref st f:remaining))]
          [else 'done]))))

  ;; ---- One shot --------------------------------------------------------------------------

  ;; Decompresses the DEFLATE data at in[start, end). Returns the bytes and the position just
  ;; after the compressed data.
  (define (inflate-range in start end)
    (let ([st (make-inflater in start end #t
                             (make-bytevector (fxmax 1024 (fxmin (fx* 4 (fx- end start)) 67108864)) 0))])
      (let loop ()
        (case (inflater-run! st)
          [(need-output)
           (let* ([out (vector-ref st f:out)]
                  [bigger (make-bytevector (fx* 2 (bytevector-length out)) 0)])
             (bytevector-copy! out 0 bigger 0 (vector-ref st f:op))
             (vector-set! st f:out bigger)
             (loop))]
          [(done)
           (let* ([ip (inflater-align! st)]
                  [op (vector-ref st f:op)]
                  [result (make-bytevector op)])
             (bytevector-copy! (vector-ref st f:out) 0 result 0 op)
             (values result ip))]
          [else (compress-error 'inflate "internal error: inflater stopped for input")])))))
