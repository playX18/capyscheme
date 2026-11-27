;;; DEFLATE compression (RFC 1951).
;;;
;;; LZ77 over a 32 KiB window with hash chains on 3-byte prefixes, then Huffman coding. The
;;; levels follow zlib's table: 1-3 take the first good match (greedy), 4-9 look one byte
;;; ahead for a longer match (lazy), and the higher levels follow longer chains. Level 0 sends
;;; stored blocks. Every 16 Ki tokens a block is sent in whichever of the three block types
;;; (stored, fixed Huffman, dynamic Huffman) is smallest, counted exactly in bits.
;;;
;;; A deflater either compresses one bytevector in place (`deflate-bytevector`) or owns a
;;; window buffer that `deflater-write!` appends to. In the second case, when the buffer is
;;; full the deflater compresses what it can, sends the block, and slides the buffer down by a
;;; multiple of 32 KiB so the last 32 KiB stay available for matches.
;;;
;;; The bit writer holds fewer than 16 bits between writes and writes two bytes at a time, so
;;; every value stays a 31-bit fixnum. The hot loops carry their state as loop variables and
;;; use macros rather than small procedures, which Tier 2 does not inline into large callers.

(library (capy compress deflate)
  (export make-deflater deflater-write! deflater-finish! deflater-take-output!
    deflate-bytevector)
  (import (rnrs)
    (capy compress tables))

  (define-syntax define-constants
    (syntax-rules ()
      [(_ (name value) ...)
       (begin (define-syntax name (identifier-syntax value)) ...)]))

  (define-constants
    (W 32768) (WMASK 32767) (HSIZE 32768) (HMASK 32767) (NIL -100000)
    (TOKMAX 16384) (BUFSIZE 262144) (LOOKAHEAD 262) (TOO-FAR 4096))

  (define-constants
    (d:win 0) (d:wend 1) (d:strstart 2) (d:block-start 3) (d:head 4) (d:prev 5) (d:toks 6)
    (d:ntok 7) (d:lfreq 8) (d:dfreq 9) (d:out 10) (d:op 11) (d:bb 12) (d:bc 13) (d:level 14)
    (d:strategy 15) (d:plen 16) (d:pdist 17) (d:avail 18) (d:size 19))

  ;; good lazy nice chain, as in zlib's configuration_table.
  (define level-table
    '#(#(0 0 0 0) #(4 4 8 4) #(4 5 16 8) #(4 6 32 32) #(4 4 16 16) #(8 16 32 32)
       #(8 16 128 128) #(8 32 128 256) #(32 128 258 1024) #(32 258 258 4096)))

  ;; ---- Static code tables ----------------------------------------------------------------

  ;; Match length 3..258 -> length symbol - 257.
  (define length-code
    (let ([t (make-vector 259 0)])
      (do ([c 0 (fx+ c 1)]) ((fx=? c 29) t)
        (let ([end (if (fx=? c 28) 259 (vector-ref length-base (fx+ c 1)))])
          (do ([l (vector-ref length-base c) (fx+ l 1)]) ((fx>=? l end))
            (vector-set! t l c))))))

  (define (slow-dist-code d)
    (let loop ([c 29]) (if (fx<=? (vector-ref dist-base c) d) c (loop (fx- c 1)))))

  ;; Distance 1..256 by distance - 1; 257..32768 by (distance - 1) >> 7.
  (define dist-small
    (let ([t (make-vector 256 0)])
      (do ([d 0 (fx+ d 1)]) ((fx=? d 256) t) (vector-set! t d (slow-dist-code (fx+ d 1))))))
  (define dist-large
    (let ([t (make-vector 256 0)])
      (do ([k 0 (fx+ k 1)]) ((fx=? k 256) t) (vector-set! t k (slow-dist-code (fx+ (fx* k 128) 1))))))

  (define-syntax dist->code
    (syntax-rules ()
      [(_ ds dl d)
       (if (fx<=? d 256)
         (vector-ref ds (fx- d 1))
         (vector-ref dl (fxarithmetic-shift-right (fx- d 1) 7)))]))

  (define fixed-llens
    (let ([v (make-vector 288 0)])
      (do ([s 0 (fx+ s 1)]) ((fx=? s 288) v)
        (vector-set! v s (cond [(fx<? s 144) 8] [(fx<? s 256) 9] [(fx<? s 280) 7] [else 8])))))
  (define fixed-dlens (make-vector 30 5))

  ;; ---- Huffman code construction ---------------------------------------------------------

  ;; Code lengths, at most LIMIT, of a prefix code for the N symbols with frequencies FREQS.
  ;; Unused symbols get 0. At least two symbols always get a code (some decoders reject a
  ;; code with one). A Huffman tree gives the depths; if one is too deep, zlib's fix moves
  ;; leaves up until the Kraft sum fits, and the longest lengths go to the rarest symbols.
  (define (code-lengths freqs n limit)
    (let* ([lens (make-vector n 0)]
           [used (let loop ([i (fx- n 1)] [acc '()])
                   (if (fx<? i 0) acc (loop (fx- i 1) (if (fx>? (vector-ref freqs i) 0) (cons i acc) acc))))])
      (cond
        [(null? used) (vector-set! lens 0 1) (vector-set! lens 1 1) lens]
        [(null? (cdr used))
         (vector-set! lens (car used) 1)
         (vector-set! lens (if (fx=? (car used) 0) 1 0) 1)
         lens]
        [else
         (let* ([sorted (list->vector
                          (list-sort (lambda (a b)
                                       (let ([fa (vector-ref freqs a)] [fb (vector-ref freqs b)])
                                         (or (fx<? fa fb) (and (fx=? fa fb) (fx<? a b)))))
                                     used))]
                [m (vector-length sorted)]
                [weight (make-vector (fx* 2 m) 0)]
                [parent (make-vector (fx* 2 m) 0)]
                [depth (make-vector (fx* 2 m) 0)]
                [bl-count (make-vector 64 0)])
           (do ([i 0 (fx+ i 1)]) ((fx=? i m))
             (vector-set! weight i (vector-ref freqs (vector-ref sorted i))))
           ;; Two queues: leaves [li, m) and internal nodes [qi, next), both in nondecreasing
           ;; weight order.
           (let loop ([li 0] [qi m] [next m])
             (when (fx>? (fx+ (fx- m li) (fx- next qi)) 1)
               (let*-values ([(a li qi) (if (and (fx<? li m)
                                                 (or (fx=? qi next)
                                                     (fx<=? (vector-ref weight li) (vector-ref weight qi))))
                                          (values li (fx+ li 1) qi)
                                          (values qi li (fx+ qi 1)))]
                             [(b li qi) (if (and (fx<? li m)
                                                 (or (fx=? qi next)
                                                     (fx<=? (vector-ref weight li) (vector-ref weight qi))))
                                          (values li (fx+ li 1) qi)
                                          (values qi li (fx+ qi 1)))])
                 (vector-set! weight next (fx+ (vector-ref weight a) (vector-ref weight b)))
                 (vector-set! parent a next)
                 (vector-set! parent b next)
                 (loop li qi (fx+ next 1)))))
           ;; The root is node 2m - 2; parents have larger indices than their children.
           (do ([k (fx- (fx* 2 m) 3) (fx- k 1)]) ((fx<? k 0))
             (vector-set! depth k (fx+ 1 (vector-ref depth (vector-ref parent k)))))
           (let ([overflow 0])
             (do ([i 0 (fx+ i 1)]) ((fx=? i m))
               (let ([d (vector-ref depth i)])
                 (if (fx>? d limit)
                   (begin (set! overflow (fx+ overflow 1))
                          (vector-set! bl-count limit (fx+ 1 (vector-ref bl-count limit))))
                   (vector-set! bl-count d (fx+ 1 (vector-ref bl-count d))))))
             (let fix ()
               (when (fx>? overflow 0)
                 (let ([bits (let find ([b (fx- limit 1)])
                               (if (fx=? 0 (vector-ref bl-count b)) (find (fx- b 1)) b))])
                   (vector-set! bl-count bits (fx- (vector-ref bl-count bits) 1))
                   (vector-set! bl-count (fx+ bits 1) (fx+ (vector-ref bl-count (fx+ bits 1)) 2))
                   (vector-set! bl-count limit (fx- (vector-ref bl-count limit) 1))
                   (set! overflow (fx- overflow 2))
                   (fix)))))
           ;; The rarest symbols (front of `sorted`) get the longest codes.
           (let assign ([bits (fxmin limit 63)] [i 0])
             (when (fx>? bits 0)
               (let ([count (vector-ref bl-count bits)])
                 (do ([k 0 (fx+ k 1)]) ((fx=? k count))
                   (vector-set! lens (vector-ref sorted (fx+ i k)) bits))
                 (assign (fx- bits 1) (fx+ i count)))))
           lens)])))

  ;; Canonical codes for LENS, bit-reversed for sending (RFC 1951, 3.2.2).
  (define (canonical-codes lens)
    (let* ([n (vector-length lens)]
           [bl-count (make-vector 16 0)]
           [next (make-vector 16 0)]
           [codes (make-vector n 0)])
      (do ([i 0 (fx+ i 1)]) ((fx=? i n))
        (let ([l (vector-ref lens i)])
          (vector-set! bl-count l (fx+ 1 (vector-ref bl-count l)))))
      (vector-set! bl-count 0 0)
      (let loop ([l 1] [code 0])
        (when (fx<=? l 15)
          (let ([code (fx* 2 (fx+ code (vector-ref bl-count (fx- l 1))))])
            (vector-set! next l code)
            (loop (fx+ l 1) code))))
      (do ([i 0 (fx+ i 1)]) ((fx=? i n) codes)
        (let ([l (vector-ref lens i)])
          (when (fx>? l 0)
            (vector-set! codes i (reverse-bits (vector-ref next l) l))
            (vector-set! next l (fx+ 1 (vector-ref next l))))))))

  (define fixed-lcodes (canonical-codes fixed-llens))
  (define fixed-dcodes (canonical-codes fixed-dlens))

  ;; The number of leading entries of LENS to send: trailing zeros dropped, at least MINIMUM.
  (define (trim-count lens minimum)
    (let loop ([k (vector-length lens)])
      (if (and (fx>? k minimum) (fx=? 0 (vector-ref lens (fx- k 1)))) (loop (fx- k 1)) k)))

  ;; Run-length codes the code lengths LENS[0, n) with the symbols 16, 17 and 18, as zlib's
  ;; scan_tree does. Returns a list of (symbol . extra-bits-value) in order.
  (define (rle-lengths lens n)
    (let loop ([i 0] [prev -1] [acc '()])
      (if (fx>=? i n)
        (reverse acc)
        (let* ([l (vector-ref lens i)]
               [run (let r ([j (fx+ i 1)]) (if (and (fx<? j n) (fx=? (vector-ref lens j) l)) (r (fx+ j 1)) (fx- j i)))])
          (cond
            [(and (fx=? l 0) (fx>=? run 11))
             (let ([k (fxmin run 138)]) (loop (fx+ i k) 0 (cons (cons 18 (fx- k 11)) acc)))]
            [(and (fx=? l 0) (fx>=? run 3))
             (loop (fx+ i run) 0 (cons (cons 17 (fx- run 3)) acc))]
            [(and (fx>? l 0) (fx=? l prev) (fx>=? run 3))
             (let ([k (fxmin run 6)]) (loop (fx+ i k) l (cons (cons 16 (fx- k 3)) acc)))]
            [(and (fx>? l 0) (fx>=? run 4))
             ;; Send the length once, then repeat it.
             (let ([k (fxmin (fx- run 1) 6)])
               (loop (fx+ i (fx+ 1 k)) l (cons (cons 16 (fx- k 3)) (cons (cons l 0) acc))))]
            [else (loop (fx+ i 1) l (cons (cons l 0) acc))])))))

  ;; ---- State -----------------------------------------------------------------------------

  ;; A deflater for LEVEL 0..9 and STRATEGY default, fixed or huffman-only. WINDOW? makes it
  ;; own a window buffer for `deflater-write!`.
  (define (make-deflater level strategy window?)
    (let ([st (make-vector d:size 0)])
      (vector-set! st d:win (if window? (make-bytevector BUFSIZE 0) #f))
      (vector-set! st d:wend 0)
      (vector-set! st d:strstart 0)
      (vector-set! st d:block-start 0)
      (vector-set! st d:head (make-vector HSIZE NIL))
      (vector-set! st d:prev (make-vector W NIL))
      (vector-set! st d:toks (make-vector TOKMAX 0))
      (vector-set! st d:ntok 0)
      (vector-set! st d:lfreq (make-vector 286 0))
      (vector-set! st d:dfreq (make-vector 30 0))
      (vector-set! st d:out (make-bytevector 65536 0))
      (vector-set! st d:op 0)
      (vector-set! st d:bb 0)
      (vector-set! st d:bc 0)
      (vector-set! st d:level level)
      (vector-set! st d:strategy strategy)
      (vector-set! st d:plen 2)
      (vector-set! st d:pdist 0)
      (vector-set! st d:avail #f)
      st))

  ;; The compressed bytes so far; empties the output buffer.
  (define (deflater-take-output! st)
    (let* ([op (vector-ref st d:op)]
           [result (make-bytevector op)])
      (bytevector-copy! (vector-ref st d:out) 0 result 0 op)
      (vector-set! st d:op 0)
      result))

  (define (ensure-output! st nbytes)
    (let ([out (vector-ref st d:out)] [op (vector-ref st d:op)])
      (when (fx>? (fx+ op nbytes) (bytevector-length out))
        (let ([bigger (make-bytevector (fxmax (fx+ op nbytes) (fx* 2 (bytevector-length out))) 0)])
          (bytevector-copy! out 0 bigger 0 op)
          (vector-set! st d:out bigger)))))

  ;; ---- Bit output ------------------------------------------------------------------------

  ;; Adds the N low bits of V to the bit buffer (BB, BC) and writes two bytes to OUT at OP once
  ;; it holds 16 bits; then evaluates BODY with BB, BC and OP rebound. OUT must have room.
  (define-syntax put
    (syntax-rules ()
      [(_ (bb bc op) out v n body)
       (let* ([bb (fxior bb (fxarithmetic-shift-left v bc))]
              [bc (fx+ bc n)])
         (let ([k (lambda (bb bc op) body)])
           (if (fx>=? bc 16)
             (begin
               (bytevector-u8-set! out op (fxand bb 255))
               (bytevector-u8-set! out (fx+ op 1) (fxand (fxarithmetic-shift-right bb 8) 255))
               (k (fxarithmetic-shift-right bb 16) (fx- bc 16) (fx+ op 2)))
             (k bb bc op))))]))

  ;; The same for cold code, on the state.
  (define (put-bits! st v n)
    (let ([out (vector-ref st d:out)]
          [bb (vector-ref st d:bb)]
          [bc (vector-ref st d:bc)]
          [op (vector-ref st d:op)])
      (put (bb bc op) out v n
        (begin (vector-set! st d:bb bb) (vector-set! st d:bc bc) (vector-set! st d:op op)))))

  ;; Pads the bit buffer to a byte boundary and writes it out.
  (define (align-bits! st)
    (let ([out (vector-ref st d:out)]
          [bb (vector-ref st d:bb)]
          [bc (vector-ref st d:bc)]
          [op (vector-ref st d:op)])
      (cond
        [(fx>? bc 8)
         (bytevector-u8-set! out op (fxand bb 255))
         (bytevector-u8-set! out (fx+ op 1) (fxand (fxarithmetic-shift-right bb 8) 255))
         (vector-set! st d:op (fx+ op 2))]
        [(fx>? bc 0)
         (bytevector-u8-set! out op (fxand bb 255))
         (vector-set! st d:op (fx+ op 1))])
      (vector-set! st d:bb 0)
      (vector-set! st d:bc 0)))

  ;; ---- Blocks ----------------------------------------------------------------------------

  ;; Stored blocks for win[start, end); the last has BFINAL set if FINAL?.
  (define (send-stored! st win start end final?)
    (let loop ([pos start])
      (let* ([len (fxmin 65535 (fx- end pos))]
             [last? (fx=? (fx+ pos len) end)])
        (ensure-output! st (fx+ len 8))
        (put-bits! st (if (and last? final?) 1 0) 3)
        (align-bits! st)
        (let ([out (vector-ref st d:out)] [op (vector-ref st d:op)])
          (bytevector-u16-set! out op len (endianness little))
          (bytevector-u16-set! out (fx+ op 2) (fxxor len #xFFFF) (endianness little))
          (bytevector-copy! win pos out (fx+ op 4) len)
          (vector-set! st d:op (fx+ op (fx+ 4 len))))
        (unless last? (loop (fx+ pos len))))))

  ;; Sends the tokens with the given codes.
  (define (send-tokens! st ntok lcodes llens dcodes dlens)
    (let ([out (vector-ref st d:out)]
          [toks (vector-ref st d:toks)]
          [lcode length-code]
          [lbase length-base]
          [lextra length-extra]
          [dbase dist-base]
          [dextra dist-extra]
          [ds dist-small]
          [dl dist-large])
      (let loop ([k 0] [bb (vector-ref st d:bb)] [bc (vector-ref st d:bc)] [op (vector-ref st d:op)])
        (if (fx=? k ntok)
          (begin (vector-set! st d:bb bb) (vector-set! st d:bc bc) (vector-set! st d:op op))
          (let ([t (vector-ref toks k)])
            (if (fx<? t 256)
              (put (bb bc op) out (vector-ref lcodes t) (vector-ref llens t)
                (loop (fx+ k 1) bb bc op))
              (let* ([len (fxarithmetic-shift-right t 16)]
                     [dist (fxand t #xFFFF)]
                     [c (vector-ref lcode len)]
                     [s (fx+ 257 c)]
                     [dc (dist->code ds dl dist)])
                (put (bb bc op) out (vector-ref lcodes s) (vector-ref llens s)
                  (put (bb bc op) out (fx- len (vector-ref lbase c)) (vector-ref lextra c)
                    (put (bb bc op) out (vector-ref dcodes dc) (vector-ref dlens dc)
                      (put (bb bc op) out (fx- dist (vector-ref dbase dc)) (vector-ref dextra dc)
                        (loop (fx+ k 1) bb bc op))))))))))))

  (define (weighted-sum freqs lens n)
    (let loop ([i 0] [sum 0])
      (if (fx=? i n) sum (loop (fx+ i 1) (fx+ sum (fx* (vector-ref freqs i) (vector-ref lens i)))))))

  ;; Sends the pending tokens, which cover win[block-start, covered), as one block of the
  ;; smallest type, and starts the next block at COVERED.
  (define (flush-block! st win ntok covered final?)
    (let* ([lfreq (vector-ref st d:lfreq)]
           [dfreq (vector-ref st d:dfreq)]
           [start (vector-ref st d:block-start)]
           [raw (fx- covered start)]
           [strategy (vector-ref st d:strategy)])
      (vector-set! lfreq 256 1)
      (let* ([extra (fx+ (let loop ([i 0] [sum 0])
                           (if (fx=? i 29) sum
                             (loop (fx+ i 1) (fx+ sum (fx* (vector-ref lfreq (fx+ 257 i)) (vector-ref length-extra i))))))
                         (weighted-sum dfreq dist-extra 30))]
             [fixed-bits (fx+ 3 (fx+ extra (fx+ (weighted-sum lfreq fixed-llens 286) (weighted-sum dfreq fixed-dlens 30))))]
             [stored-bits (fx+ (fx* 8 raw) (fx* 48 (fx+ 1 (fxdiv raw 65535))))])
        (if (eq? strategy 'fixed)
          (send-huffman! st ntok final? fixed-lcodes fixed-llens fixed-dcodes fixed-dlens fixed-bits #f)
          (let* ([llens (code-lengths lfreq 286 15)]
                 [dlens (code-lengths dfreq 30 15)]
                 [nlit (trim-count llens 257)]
                 [ndist (trim-count dlens 1)]
                 [all (let ([v (make-vector (fx+ nlit ndist) 0)])
                        (do ([i 0 (fx+ i 1)]) ((fx=? i nlit)) (vector-set! v i (vector-ref llens i)))
                        (do ([i 0 (fx+ i 1)]) ((fx=? i ndist)) (vector-set! v (fx+ nlit i) (vector-ref dlens i)))
                        v)]
                 [runs (rle-lengths all (vector-length all))]
                 [clfreq (let ([v (make-vector 19 0)])
                           (for-each (lambda (r) (vector-set! v (car r) (fx+ 1 (vector-ref v (car r))))) runs)
                           v)]
                 [cllens (code-lengths clfreq 19 7)]
                 [nclen (let loop ([k 19])
                          (if (and (fx>? k 4) (fx=? 0 (vector-ref cllens (vector-ref clen-order (fx- k 1)))))
                            (loop (fx- k 1))
                            k))]
                 [header-bits (fold-left (lambda (sum r)
                                           (fx+ sum (fx+ (vector-ref cllens (car r))
                                                         (case (car r) [(16) 2] [(17) 3] [(18) 7] [else 0]))))
                                         (fx+ 17 (fx* 3 nclen))
                                         runs)]
                 [dynamic-bits (fx+ header-bits
                                    (fx+ extra (fx+ (weighted-sum lfreq llens 286) (weighted-sum dfreq dlens 30))))])
            (cond
              [(and (fx<=? stored-bits fixed-bits) (fx<=? stored-bits dynamic-bits))
               (send-stored! st win start covered final?)]
              [(fx<=? fixed-bits dynamic-bits)
               (send-huffman! st ntok final? fixed-lcodes fixed-llens fixed-dcodes fixed-dlens fixed-bits #f)]
              [else
               (send-huffman! st ntok final? (canonical-codes llens) llens (canonical-codes dlens) dlens
                              dynamic-bits
                              (list nlit ndist nclen cllens runs))]))))
      (vector-fill! lfreq 0)
      (vector-fill! dfreq 0)
      (vector-set! st d:ntok 0)
      (vector-set! st d:block-start covered)))

  ;; A Huffman block. HEADER is #f for the fixed code, or the dynamic code's description.
  (define (send-huffman! st ntok final? lcodes llens dcodes dlens bits header)
    (ensure-output! st (fx+ 16 (fxarithmetic-shift-right bits 3)))
    (put-bits! st (if final? 1 0) 1)
    (put-bits! st (if header 2 1) 2)
    (when header
      (let ([nlit (car header)] [ndist (cadr header)] [nclen (caddr header)]
            [cllens (cadddr header)] [runs (car (cddddr header))])
        (let ([clcodes (canonical-codes cllens)])
          (put-bits! st (fx- nlit 257) 5)
          (put-bits! st (fx- ndist 1) 5)
          (put-bits! st (fx- nclen 4) 4)
          (do ([k 0 (fx+ k 1)]) ((fx=? k nclen))
            (put-bits! st (vector-ref cllens (vector-ref clen-order k)) 3))
          (for-each (lambda (r)
                      (let ([sym (car r)])
                        (put-bits! st (vector-ref clcodes sym) (vector-ref cllens sym))
                        (case sym
                          [(16) (put-bits! st (cdr r) 2)]
                          [(17) (put-bits! st (cdr r) 3)]
                          [(18) (put-bits! st (cdr r) 7)]
                          [else #f])))
                    runs))))
    (send-tokens! st ntok lcodes llens dcodes dlens)
    (put-bits! st (vector-ref lcodes 256) (vector-ref llens 256)))

  ;; ---- Matching --------------------------------------------------------------------------

  (define-syntax hash3
    (syntax-rules ()
      [(_ win i)
       (fxand (fxxor (fxarithmetic-shift-left (bytevector-u8-ref win i) 10)
                     (fxxor (fxarithmetic-shift-left (bytevector-u8-ref win (fx+ i 1)) 5)
                            (bytevector-u8-ref win (fx+ i 2))))
              HMASK)]))

  ;; Inserts position I into the hash chains and evaluates to the previous head of its chain.
  (define-syntax insert!
    (syntax-rules ()
      [(_ win head prev i)
       (let* ([h (hash3 win i)]
              [old (vector-ref head h)])
         (vector-set! prev (fxand i WMASK) old)
         (vector-set! head h i)
         old)]))

  ;; The number of equal bytes at A and B, at most LIMIT. Compares four bytes at a time.
  (define-syntax match-length
    (syntax-rules ()
      [(_ win a b limit)
       (let words ([k 0])
         (if (and (fx<=? (fx+ k 4) limit)
                  (fx=? (bytevector-s32-ref win (fx+ a k) (endianness little))
                        (bytevector-s32-ref win (fx+ b k) (endianness little))))
           (words (fx+ k 4))
           (let bytes ([k k])
             (if (and (fx<? k limit)
                      (fx=? (bytevector-u8-ref win (fx+ a k)) (bytevector-u8-ref win (fx+ b k))))
               (bytes (fx+ k 1))
               k))))]))

  ;; The longest match for position I, longer than BEST, among the chain from CAND. Follows at
  ;; most CHAIN links and stops at a match of NICE bytes. Returns (values length distance);
  ;; the length is BEST if nothing longer was found.
  (define (longest-match win prev i cand best chain nice wend)
    (let ([limit (fxmin 258 (fx- wend i))])
      (if (fx>=? best limit)
        (values best 0)
        (let loop ([cand cand] [chain chain] [best best] [dist 0])
          (if (or (fx>=? (fx- i cand) W) (fx=? chain 0))
            (values best dist)
            (let ([next (vector-ref prev (fxand cand WMASK))])
              (if (and (fx=? (bytevector-u8-ref win (fx+ cand best)) (bytevector-u8-ref win (fx+ i best)))
                       (fx=? (bytevector-u8-ref win cand) (bytevector-u8-ref win i))
                       (fx=? (bytevector-u8-ref win (fx+ cand 1)) (bytevector-u8-ref win (fx+ i 1))))
                (let ([len (match-length win cand i limit)])
                  (cond
                    [(fx<=? len best) (loop next (fx- chain 1) best dist)]
                    [(or (fx>=? len nice) (fx>=? len limit)) (values len (fx- i cand))]
                    [else (loop next (fx- chain 1) len (fx- i cand))]))
                (loop next (fx- chain 1) best dist))))))))

  ;; ---- Compression loops -----------------------------------------------------------------
  ;;
  ;; Each processes positions [strstart, limit) of win, with matches allowed up to wend, and
  ;; saves its position (and the lazy match state) in the deflater.

  (define-syntax add-literal
    (syntax-rules ()
      [(_ toks lfreq ntok b)
       (begin
         (vector-set! toks ntok b)
         (vector-set! lfreq b (fx+ 1 (vector-ref lfreq b))))]))

  (define-syntax add-match
    (syntax-rules ()
      [(_ toks lfreq dfreq lcode ds dl ntok len dist)
       (begin
         (vector-set! toks ntok (fxior (fxarithmetic-shift-left len 16) dist))
         (let ([lc (fx+ 257 (vector-ref lcode len))])
           (vector-set! lfreq lc (fx+ 1 (vector-ref lfreq lc))))
         (let ([dc (dist->code ds dl dist)])
           (vector-set! dfreq dc (fx+ 1 (vector-ref dfreq dc)))))]))

  ;; After adding a token: the token count, sending the block if it is full.
  (define-syntax next-ntok
    (syntax-rules ()
      [(_ st win ntok covered)
       (let ([n (fx+ ntok 1)])
         (if (fx=? n TOKMAX)
           (begin (flush-block! st win n covered #f) 0)
           n))]))

  (define (run-greedy! st win wend limit)
    (let* ([level (vector-ref st d:level)]
           [config (vector-ref level-table level)]
           [max-insert (vector-ref config 1)]
           [nice (vector-ref config 2)]
           [chain (vector-ref config 3)]
           [head (vector-ref st d:head)]
           [prev (vector-ref st d:prev)]
           [toks (vector-ref st d:toks)]
           [lfreq (vector-ref st d:lfreq)]
           [dfreq (vector-ref st d:dfreq)]
           [lcode length-code]
           [ds dist-small]
           [dl dist-large])
      (let loop ([i (vector-ref st d:strstart)] [ntok (vector-ref st d:ntok)])
        (if (fx>=? i limit)
          (begin (vector-set! st d:strstart i) (vector-set! st d:ntok ntok))
          (let* ([cand (if (fx<=? (fx+ i 3) wend) (insert! win head prev i) NIL)])
            (let-values ([(len dist) (if (fx<? (fx- i cand) W)
                                       (longest-match win prev i cand 2 chain nice wend)
                                       (values 2 0))])
              (if (fx>=? len 3)
                (let ([end (fx+ i len)])
                  (add-match toks lfreq dfreq lcode ds dl ntok len dist)
                  (when (fx<=? len max-insert)
                    (let ins ([p (fx+ i 1)])
                      (when (and (fx<? p end) (fx<=? (fx+ p 3) wend))
                        (insert! win head prev p)
                        (ins (fx+ p 1)))))
                  (loop end (next-ntok st win ntok end)))
                (begin
                  (add-literal toks lfreq ntok (bytevector-u8-ref win i))
                  (loop (fx+ i 1) (next-ntok st win ntok (fx+ i 1)))))))))))

  (define (run-lazy! st win wend limit)
    (let* ([level (vector-ref st d:level)]
           [config (vector-ref level-table level)]
           [good (vector-ref config 0)]
           [max-lazy (vector-ref config 1)]
           [nice (vector-ref config 2)]
           [chain (vector-ref config 3)]
           [head (vector-ref st d:head)]
           [prev (vector-ref st d:prev)]
           [toks (vector-ref st d:toks)]
           [lfreq (vector-ref st d:lfreq)]
           [dfreq (vector-ref st d:dfreq)]
           [lcode length-code]
           [ds dist-small]
           [dl dist-large])
      ;; plen and pdist: the match at i - 1, if avail (else plen is 2).
      (let loop ([i (vector-ref st d:strstart)]
                 [plen (vector-ref st d:plen)]
                 [pdist (vector-ref st d:pdist)]
                 [avail (vector-ref st d:avail)]
                 [ntok (vector-ref st d:ntok)])
        (if (fx>=? i limit)
          (begin
            (vector-set! st d:strstart i)
            (vector-set! st d:plen plen)
            (vector-set! st d:pdist pdist)
            (vector-set! st d:avail avail)
            (vector-set! st d:ntok ntok))
          (let* ([cand (if (fx<=? (fx+ i 3) wend) (insert! win head prev i) NIL)])
            (let-values ([(len dist) (if (and (fx<? (fx- i cand) W) (fx<? plen max-lazy))
                                       (longest-match win prev i cand plen
                                                      (if (fx>=? plen good) (fxarithmetic-shift-right chain 2) chain)
                                                      nice wend)
                                       (values 2 0))])
              ;; A found match is longer than plen; else len = plen (or 2) and dist = 0.
              (let ([len (if (or (fx=? dist 0) (and (fx=? len 3) (fx>? dist TOO-FAR))) 2 len)])
                (cond
                  [(and (fx>=? plen 3) (fx<=? len plen))
                   ;; The match at i - 1 wins.
                   (let ([end (fx+ (fx- i 1) plen)])
                     (add-match toks lfreq dfreq lcode ds dl ntok plen pdist)
                     (let ins ([p (fx+ i 1)])
                       (when (and (fx<? p end) (fx<=? (fx+ p 3) wend))
                         (insert! win head prev p)
                         (ins (fx+ p 1))))
                     (loop end 2 0 #f (next-ntok st win ntok end)))]
                  [avail
                   (add-literal toks lfreq ntok (bytevector-u8-ref win (fx- i 1)))
                   (loop (fx+ i 1) len dist #t (next-ntok st win ntok i))]
                  [else (loop (fx+ i 1) len dist #t ntok)]))))))))

  (define (run-literals! st win wend limit)
    (let ([toks (vector-ref st d:toks)] [lfreq (vector-ref st d:lfreq)])
      (let loop ([i (vector-ref st d:strstart)] [ntok (vector-ref st d:ntok)])
        (if (fx>=? i limit)
          (begin (vector-set! st d:strstart i) (vector-set! st d:ntok ntok))
          (begin
            (add-literal toks lfreq ntok (bytevector-u8-ref win i))
            (loop (fx+ i 1) (next-ntok st win ntok (fx+ i 1))))))))

  ;; Compresses win[strstart, limit). FINAL? finishes the stream: the last block, then
  ;; padding to a byte boundary.
  (define (compress! st win wend limit final?)
    (let ([level (vector-ref st d:level)]
          [strategy (vector-ref st d:strategy)])
      (cond
        [(fx=? level 0)
         (vector-set! st d:strstart limit)
         (when (or final? (fx>? (fx- limit (vector-ref st d:block-start)) 0))
           (send-stored! st win (vector-ref st d:block-start) limit final?)
           (vector-set! st d:block-start limit))]
        [else
         (cond
           [(eq? strategy 'huffman-only) (run-literals! st win wend limit)]
           [(fx<=? level 3) (run-greedy! st win wend limit)]
           [else (run-lazy! st win wend limit)])
         (when final?
           (let ([i (vector-ref st d:strstart)] [ntok (vector-ref st d:ntok)])
             ;; A literal still waiting for the lazy match decision.
             (let ([ntok (if (vector-ref st d:avail)
                           (begin
                             (add-literal (vector-ref st d:toks) (vector-ref st d:lfreq) ntok
                                          (bytevector-u8-ref win (fx- i 1)))
                             (vector-set! st d:avail #f)
                             (fx+ ntok 1))
                           ntok)])
               (flush-block! st win ntok i #t))))])
      (when final? (align-bits! st))))

  ;; ---- Driving ---------------------------------------------------------------------------

  ;; Raw DEFLATE data for bv[start, end).
  (define (deflate-bytevector bv start end level strategy)
    (let ([st (make-deflater level strategy #f)])
      (vector-set! st d:strstart start)
      (vector-set! st d:block-start start)
      (compress! st bv end end #t)
      (deflater-take-output! st)))

  ;; Moves the window down by a multiple of 32 KiB, keeping 32 KiB before strstart.
  (define (slide! st)
    (let* ([win (vector-ref st d:win)]
           [wend (vector-ref st d:wend)]
           [strstart (vector-ref st d:strstart)]
           [shift (fx* W (fxdiv (fx- strstart W) W))])
      (when (fx>? shift 0)
        (bytevector-copy! win shift win 0 (fx- wend shift))
        (let ([head (vector-ref st d:head)] [prev (vector-ref st d:prev)])
          (do ([h 0 (fx+ h 1)]) ((fx=? h HSIZE))
            (let ([v (fx- (vector-ref head h) shift)])
              (vector-set! head h (if (fx<? v 0) NIL v))))
          (do ([h 0 (fx+ h 1)]) ((fx=? h W))
            (let ([v (fx- (vector-ref prev h) shift)])
              (vector-set! prev h (if (fx<? v 0) NIL v)))))
        (vector-set! st d:wend (fx- wend shift))
        (vector-set! st d:strstart (fx- strstart shift))
        (vector-set! st d:block-start (fx- (vector-ref st d:block-start) shift)))))

  ;; Appends bv[start, end) to the stream.
  (define (deflater-write! st bv start end)
    (let loop ([start start])
      (when (fx<? start end)
        (let* ([win (vector-ref st d:win)]
               [wend (vector-ref st d:wend)]
               [n (fxmin (fx- end start) (fx- BUFSIZE wend))])
          (bytevector-copy! bv start win wend n)
          (vector-set! st d:wend (fx+ wend n))
          (when (fx=? (fx+ wend n) BUFSIZE)
            (compress! st win BUFSIZE (fx- BUFSIZE LOOKAHEAD) #f)
            ;; The block must not refer to bytes the slide drops.
            (let ([covered (if (vector-ref st d:avail)
                             (fx- (vector-ref st d:strstart) 1)
                             (vector-ref st d:strstart))])
              (when (fx>? covered (vector-ref st d:block-start))
                (flush-block! st win (vector-ref st d:ntok) covered #f)))
            (slide! st))
          (loop (fx+ start n))))))

  ;; Compresses the rest of the window and ends the stream.
  (define (deflater-finish! st)
    (let ([wend (vector-ref st d:wend)])
      (compress! st (vector-ref st d:win) wend wend #t))))
