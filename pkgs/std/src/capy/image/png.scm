;;; PNG (ISO/IEC 15948) decoding and encoding for (capy image).
;;;
;;; Decoding takes every PNG: color types 0 (gray), 2 (RGB), 3 (palette), 4 (gray and
;;; alpha) and 6 (RGBA), every bit depth they allow (1, 2, 4, 8, 16), the five filters,
;;; Adam7 interlacing and tRNS transparency. 16-bit samples keep their high byte; samples
;;; below 8 bits are scaled to 0-255. Chunk CRCs are checked; ancillary chunks other than
;;; tRNS are skipped, and an unknown critical chunk is an error.
;;;
;;; Encoding writes 8-bit, non-interlaced PNGs. #:color-type auto (the default) picks the
;;; smallest of gray, gray-alpha, rgb and rgba that holds the image exactly; each row gets
;;; the filter with the smallest sum of absolute residuals.

(library (capy image png)
  (export png->image image->png png-bytes?)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (capy compress)
    (capy image base))

  (define signature #vu8(137 80 78 71 13 10 26 10))

  ;; Whether BV starts with the PNG signature.
  (define (png-bytes? bv)
    (and (bytevector? bv)
         (>= (bytevector-length bv) 8)
         (let loop ([i 0])
           (or (= i 8)
               (and (= (bytevector-u8-ref bv i) (bytevector-u8-ref signature i)) (loop (+ i 1)))))))

  (define (bad message . irritants)
    (apply image-format-error 'png->image message irritants))

  ;; ---- Decoding --------------------------------------------------------------------------

  (define (chunk-type bv pos)
    (let ([s (make-string 4)])
      (do ([i 0 (+ i 1)]) ((= i 4) s)
        (string-set! s i (integer->char (bytevector-u8-ref bv (+ pos i)))))))

  (define (channels-of ctype)
    (case ctype [(0) 1] [(2) 3] [(3) 1] [(4) 2] [(6) 4] [else (bad "invalid color type" ctype)]))

  (define (valid-depth? ctype depth)
    (memv depth (case ctype [(0) '(1 2 4 8 16)] [(3) '(1 2 4 8)] [else '(8 16)])))

  (define (concat-ranges bv ranges)
    (let* ([n (fold-left (lambda (n r) (+ n (- (cdr r) (car r)))) 0 ranges)]
           [out (make-bytevector n)])
      (let loop ([rs ranges] [pos 0])
        (if (null? rs)
          out
          (let ([len (- (cdar rs) (caar rs))])
            (bytevector-copy! bv (caar rs) out pos len)
            (loop (cdr rs) (+ pos len)))))))

  ;; The image of the PNG file BV.
  (define (png->image bv)
    (unless (png-bytes? bv) (bad "not a PNG file: bad signature"))
    (let loop ([pos 8] [ihdr #f] [palette #f] [trns #f] [idat '()])
      (when (> (+ pos 12) (bytevector-length bv))
        (bad "the file ends before the IEND chunk"))
      (let* ([len (bytevector-u32-ref bv pos (endianness big))]
             [type (chunk-type bv (+ pos 4))]
             [data (+ pos 8)]
             [end (+ data len)])
        (when (> (+ end 4) (bytevector-length bv))
          (bad "a chunk runs past the end of the file" type))
        (unless (= (crc32 bv (+ pos 4) end) (bytevector-u32-ref bv end (endianness big)))
          (bad "CRC mismatch in a chunk" type))
        (let ([next (+ end 4)])
          (cond
            [(string=? type "IHDR")
             (unless (= len 13) (bad "IHDR has the wrong length" len))
             (loop next data palette trns idat)]
            [(not ihdr) (bad "the first chunk is not IHDR" type)]
            [(string=? type "PLTE") (loop next ihdr (cons data len) trns idat)]
            [(string=? type "tRNS") (loop next ihdr palette (cons data len) idat)]
            [(string=? type "IDAT") (loop next ihdr palette trns (cons (cons data end) idat))]
            [(string=? type "IEND")
             (when (null? idat) (bad "no IDAT chunk"))
             (decode bv ihdr palette trns (concat-ranges bv (reverse idat)))]
            [(zero? (bitwise-and (bytevector-u8-ref bv (+ pos 4)) 32))
             (bad "unknown critical chunk" type)]
            [else (loop next ihdr palette trns idat)])))))

  (define (decode bv ihdr palette trns compressed)
    (let ([w (bytevector-u32-ref bv ihdr (endianness big))]
          [h (bytevector-u32-ref bv (+ ihdr 4) (endianness big))]
          [depth (bytevector-u8-ref bv (+ ihdr 8))]
          [ctype (bytevector-u8-ref bv (+ ihdr 9))]
          [interlace (bytevector-u8-ref bv (+ ihdr 12))])
      (when (or (= w 0) (= h 0) (> (* w h) #x10000000))
        (bad "invalid or too large image size" w h))
      (unless (valid-depth? ctype depth) (bad "invalid bit depth for the color type" depth ctype))
      (unless (and (= 0 (bytevector-u8-ref bv (+ ihdr 10))) (= 0 (bytevector-u8-ref bv (+ ihdr 11))))
        (bad "unknown compression or filter method"))
      (unless (memv interlace '(0 1)) (bad "unknown interlace method" interlace))
      (when (and (= ctype 3) (not palette)) (bad "a palette image has no PLTE chunk"))
      (let* ([raw (zlib-decompress compressed)]
             [img (make-image w h)]
             [pal (and palette (palette-rgba bv palette (and (= ctype 3) trns)))]
             [key (and trns (memv ctype '(0 2)) (trns-key bv trns ctype))]
             [bits (* depth (channels-of ctype))]
             [bpp (max 1 (div bits 8))])
        (define (pass! offset x0 y0 dx dy)
          ;; One (sub)image: returns the offset after its rows.
          (let ([pw (div (+ (- w x0) (- dx 1)) dx)]
                [ph (div (+ (- h y0) (- dy 1)) dy)])
            (if (or (= pw 0) (= ph 0))
              offset
              (let* ([rowbytes (div (+ (* pw bits) 7) 8)]
                     [end (+ offset (* ph (+ rowbytes 1)))])
                (when (> end (bytevector-length raw))
                  (bad "the image data is too short"))
                (unfilter! raw offset ph rowbytes bpp)
                (do ([r 0 (+ r 1)]) ((= r ph))
                  (convert-row! raw (+ offset (* r (+ rowbytes 1)) 1) pw ctype depth pal key
                                (image-data img) (* 4 (+ x0 (* w (+ y0 (* r dy))))) (* 4 dx)))
                end))))
        (if (= interlace 0)
          (pass! 0 0 0 1 1)
          (fold-left (lambda (offset p) (apply pass! offset p))
                     0
                     '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))))
        img)))

  ;; The palette as 256 RGBA entries of 4 bytes, alpha from tRNS. Indices past the
  ;; palette are transparent black.
  (define (palette-rgba bv palette trns)
    (let* ([start (car palette)] [n (div (cdr palette) 3)]
           [out (make-bytevector (* 4 256) 0)])
      (do ([i 0 (+ i 1)]) ((= i (min n 256)))
        (bytevector-u8-set! out (* 4 i) (bytevector-u8-ref bv (+ start (* 3 i))))
        (bytevector-u8-set! out (+ (* 4 i) 1) (bytevector-u8-ref bv (+ start (* 3 i) 1)))
        (bytevector-u8-set! out (+ (* 4 i) 2) (bytevector-u8-ref bv (+ start (* 3 i) 2)))
        (bytevector-u8-set! out (+ (* 4 i) 3)
                            (if (and trns (< i (cdr trns))) (bytevector-u8-ref bv (+ (car trns) i)) 255)))
      out))

  ;; The transparent sample values (at the image's bit depth) of a gray or RGB image.
  (define (trns-key bv trns ctype)
    (let ([n (if (= ctype 0) 1 3)])
      (when (< (cdr trns) (* 2 n)) (bad "tRNS is too short"))
      (let ([v (make-vector n 0)])
        (do ([i 0 (+ i 1)]) ((= i n) v)
          (vector-set! v i (bytevector-u16-ref bv (+ (car trns) (* 2 i)) (endianness big)))))))

  (define-syntax fxabs
    (syntax-rules () [(_ x) (let ([v x]) (if (fx<? v 0) (fx- 0 v) v))]))

  (define (paeth a b c)
    (let* ([p (fx- (fx+ a b) c)]
           [pa (fxabs (fx- p a))] [pb (fxabs (fx- p b))] [pc (fxabs (fx- p c))])
      (cond [(and (fx<=? pa pb) (fx<=? pa pc)) a]
            [(fx<=? pb pc) b]
            [else c])))

  ;; Reverses the filters of ROWS rows of ROWBYTES bytes, each after its filter byte, starting
  ;; at raw[start]. In place: each row's prior row is already reconstructed.
  (define (unfilter! raw start rows rowbytes bpp)
    (let ([stride (fx+ rowbytes 1)])
      (do ([r 0 (fx+ r 1)]) ((fx=? r rows))
        (let* ([pos (fx+ start (fx* r stride))]
               [line (fx+ pos 1)]
               [first? (fx=? r 0)]
               [end (fx+ line rowbytes)])
          (case (bytevector-u8-ref raw pos)
            [(0) #f]
            [(1)
             (do ([i (fx+ line bpp) (fx+ i 1)]) ((fx>=? i end))
               (bytevector-u8-set! raw i (fxand 255 (fx+ (bytevector-u8-ref raw i)
                                                         (bytevector-u8-ref raw (fx- i bpp))))))]
            [(2)
             (unless first?
               (do ([i line (fx+ i 1)]) ((fx=? i end))
                 (bytevector-u8-set! raw i (fxand 255 (fx+ (bytevector-u8-ref raw i)
                                                           (bytevector-u8-ref raw (fx- i stride)))))))]
            [(3)
             (do ([i line (fx+ i 1)]) ((fx=? i end))
               (let ([a (if (fx<? (fx- i line) bpp) 0 (bytevector-u8-ref raw (fx- i bpp)))]
                     [b (if first? 0 (bytevector-u8-ref raw (fx- i stride)))])
                 (bytevector-u8-set! raw i (fxand 255 (fx+ (bytevector-u8-ref raw i)
                                                           (fxarithmetic-shift-right (fx+ a b) 1))))))]
            [(4)
             (do ([i line (fx+ i 1)]) ((fx=? i end))
               (let* ([left? (fx>=? (fx- i line) bpp)]
                      [a (if left? (bytevector-u8-ref raw (fx- i bpp)) 0)]
                      [b (if first? 0 (bytevector-u8-ref raw (fx- i stride)))]
                      [c (if (and left? (not first?)) (bytevector-u8-ref raw (fx- (fx- i stride) bpp)) 0)])
                 (bytevector-u8-set! raw i (fxand 255 (fx+ (bytevector-u8-ref raw i) (paeth a b c))))))]
            [else (bad "unknown filter type" (bytevector-u8-ref raw pos))])))))

  ;; Sample I of a row at raw[rs] of DEPTH-bit samples, at its own depth.
  (define-syntax sample
    (syntax-rules ()
      [(_ raw rs i depth)
       (case depth
         [(8) (bytevector-u8-ref raw (fx+ rs i))]
         [(16) (bytevector-u16-ref raw (fx+ rs (fx* 2 i)) (endianness big))]
         [else
          (let* ([bit (fx* i depth)]
                 [byte (bytevector-u8-ref raw (fx+ rs (fxarithmetic-shift-right bit 3)))]
                 [shift (fx- (fx- 8 depth) (fxand bit 7))])
            (fxand (fxarithmetic-shift-right byte shift) (fx- (fxarithmetic-shift-left 1 depth) 1)))])]))

  ;; A sample scaled to 8 bits.
  (define-syntax to8
    (syntax-rules ()
      [(_ v depth)
       (case depth
         [(8) v]
         [(16) (fxarithmetic-shift-right v 8)]
         [(1) (fx* v 255)]
         [(2) (fx* v 85)]
         [else (fx* v 17)])]))

  ;; Writes the PW pixels of the row at raw[rs] to out[o], o + step, ...
  (define (convert-row! raw rs pw ctype depth pal key out o step)
    (case ctype
      [(0)
       (let ([k (and key (vector-ref key 0))])
         (do ([i 0 (fx+ i 1)] [o o (fx+ o step)]) ((fx=? i pw))
           (let* ([s (sample raw rs i depth)] [v (to8 s depth)])
             (bytevector-u8-set! out o v)
             (bytevector-u8-set! out (fx+ o 1) v)
             (bytevector-u8-set! out (fx+ o 2) v)
             (bytevector-u8-set! out (fx+ o 3) (if (eqv? s k) 0 255)))))]
      [(2)
       (do ([i 0 (fx+ i 1)] [o o (fx+ o step)]) ((fx=? i pw))
         (let ([r (sample raw rs (fx* 3 i) depth)]
               [g (sample raw rs (fx+ 1 (fx* 3 i)) depth)]
               [b (sample raw rs (fx+ 2 (fx* 3 i)) depth)])
           (bytevector-u8-set! out o (to8 r depth))
           (bytevector-u8-set! out (fx+ o 1) (to8 g depth))
           (bytevector-u8-set! out (fx+ o 2) (to8 b depth))
           (bytevector-u8-set! out (fx+ o 3)
                               (if (and key (fx=? r (vector-ref key 0)) (fx=? g (vector-ref key 1))
                                        (fx=? b (vector-ref key 2)))
                                 0 255))))]
      [(3)
       (do ([i 0 (fx+ i 1)] [o o (fx+ o step)]) ((fx=? i pw))
         (bytevector-copy! pal (fx* 4 (sample raw rs i depth)) out o 4))]
      [(4)
       (do ([i 0 (fx+ i 1)] [o o (fx+ o step)]) ((fx=? i pw))
         (let ([v (to8 (sample raw rs (fx* 2 i) depth) depth)])
           (bytevector-u8-set! out o v)
           (bytevector-u8-set! out (fx+ o 1) v)
           (bytevector-u8-set! out (fx+ o 2) v)
           (bytevector-u8-set! out (fx+ o 3) (to8 (sample raw rs (fx+ 1 (fx* 2 i)) depth) depth))))]
      [else
       (if (and (fx=? depth 8) (fx=? step 4))
         (bytevector-copy! raw rs out o (fx* 4 pw))
         (do ([i 0 (fx+ i 1)] [o o (fx+ o step)]) ((fx=? i pw))
           (do ([c 0 (fx+ c 1)]) ((fx=? c 4))
             (bytevector-u8-set! out (fx+ o c) (to8 (sample raw rs (fx+ c (fx* 4 i)) depth) depth)))))]))

  ;; ---- Encoding --------------------------------------------------------------------------

  ;; The smallest color type that holds every pixel of DATA exactly.
  (define (auto-color-type data)
    (let ([n (bytevector-length data)])
      (let loop ([o 0] [gray? #t] [opaque? #t])
        (if (or (fx=? o n) (not (or gray? opaque?)))
          (cond [(and gray? opaque?) 'gray] [gray? 'gray-alpha] [opaque? 'rgb] [else 'rgba])
          (let ([r (bytevector-u8-ref data o)])
            (loop (fx+ o 4)
                  (and gray? (fx=? r (bytevector-u8-ref data (fx+ o 1))) (fx=? r (bytevector-u8-ref data (fx+ o 2))))
                  (and opaque? (fx=? 255 (bytevector-u8-ref data (fx+ o 3))))))))))

  (define (ctype-code type)
    (case type [(gray) 0] [(rgb) 2] [(gray-alpha) 4] [else 6]))

  ;; Rows of the image packed as TYPE, without filter bytes.
  (define (packed img type)
    (image->bytevector img #:channels (case type [(gray) 1] [(gray-alpha) 2] [(rgb) 3] [else 4])))

  ;; The filtered scanlines: each row is a filter byte and the residuals of FILTER, or with
  ;; filter adaptive, of whichever filter gives the smallest sum of absolute residuals.
  (define (filter-rows px rows rowbytes bpp filter)
    (let* ([stride (fx+ rowbytes 1)]
           [out (make-bytevector (fx* rows stride) 0)]
           [cand (make-bytevector (fx* 5 rowbytes) 0)])
      (do ([r 0 (fx+ r 1)]) ((fx=? r rows) out)
        (let ([line (fx* r rowbytes)]
              [first? (fx=? r 0)])
          (define (residual f i)
            (let* ([x (bytevector-u8-ref px (fx+ line i))]
                   [left? (fx>=? i bpp)]
                   [a (if left? (bytevector-u8-ref px (fx- (fx+ line i) bpp)) 0)]
                   [b (if first? 0 (bytevector-u8-ref px (fx- (fx+ line i) rowbytes)))]
                   [c (if (and left? (not first?)) (bytevector-u8-ref px (fx- (fx- (fx+ line i) rowbytes) bpp)) 0)])
              (fxand 255 (fx- x (case f
                                  [(0) 0]
                                  [(1) a]
                                  [(2) b]
                                  [(3) (fxarithmetic-shift-right (fx+ a b) 1)]
                                  [else (paeth a b c)])))))
          (define (fill! f)
            ;; Returns the sum of absolute (signed) residuals.
            (let ([base (fx* f rowbytes)])
              (let loop ([i 0] [sum 0])
                (if (fx=? i rowbytes)
                  sum
                  (let ([v (residual f i)])
                    (bytevector-u8-set! cand (fx+ base i) v)
                    (loop (fx+ i 1) (fx+ sum (if (fx<? v 128) v (fx- 256 v)))))))))
          (let ([best (if (eq? filter 'adaptive)
                        (let pick ([f 0] [best 0] [cost (greatest-fixnum)])
                          (if (fx=? f 5)
                            best
                            (let ([c (fill! f)])
                              (if (fx<? c cost) (pick (fx+ f 1) f c) (pick (fx+ f 1) best cost)))))
                        (let ([f (case filter [(none) 0] [(sub) 1] [(up) 2] [(average) 3] [else 4])])
                          (fill! f)
                          f))])
            (bytevector-u8-set! out (fx* r stride) best)
            (bytevector-copy! cand (fx* best rowbytes) out (fx+ (fx* r stride) 1) rowbytes))))))

  (define (u32be x) (let ([bv (make-bytevector 4)]) (bytevector-u32-set! bv 0 x (endianness big)) bv))

  (define (chunk type data)
    (let* ([t (string->utf8 type)]
           [body (make-bytevector (+ 4 (bytevector-length data)))])
      (bytevector-copy! t 0 body 0 4)
      (bytevector-copy! data 0 body 4 (bytevector-length data))
      (list (u32be (bytevector-length data)) body (u32be (crc32 body)))))

  (define (concat parts)
    (let* ([n (fold-left (lambda (n p) (+ n (bytevector-length p))) 0 parts)]
           [out (make-bytevector n)])
      (let loop ([ps parts] [pos 0])
        (if (null? ps)
          out
          (begin (bytevector-copy! (car ps) 0 out pos (bytevector-length (car ps)))
                 (loop (cdr ps) (+ pos (bytevector-length (car ps)))))))))

  ;; The PNG file of IMG.
  ;;
  ;;   #:level       the zlib level, 0-9 (default 6)
  ;;   #:color-type  auto (default), gray, gray-alpha, rgb or rgba; gray from a color image
  ;;                 is its luma, and a type without alpha drops it
  ;;   #:filter      adaptive (default), none, sub, up, average or paeth
  (define (image->png img #:level [level 6] #:color-type [color-type 'auto] #:filter [filter 'adaptive])
    (unless (image? img) (assertion-violation 'image->png "expected an image" img))
    (unless (memq color-type '(auto gray gray-alpha rgb rgba))
      (assertion-violation 'image->png "#:color-type must be auto, gray, gray-alpha, rgb or rgba" color-type))
    (unless (memq filter '(adaptive none sub up average paeth))
      (assertion-violation 'image->png "#:filter must be adaptive, none, sub, up, average or paeth" filter))
    (let* ([w (image-width img)] [h (image-height img)])
      (when (or (= w 0) (= h 0))
        (assertion-violation 'image->png "a PNG cannot be empty" w h))
      (let* ([type (if (eq? color-type 'auto) (auto-color-type (image-data img)) color-type)]
             [bpp (case type [(gray) 1] [(gray-alpha) 2] [(rgb) 3] [else 4])]
             [rowbytes (* bpp w)]
             [ihdr (make-bytevector 13 0)])
        (bytevector-u32-set! ihdr 0 w (endianness big))
        (bytevector-u32-set! ihdr 4 h (endianness big))
        (bytevector-u8-set! ihdr 8 8)
        (bytevector-u8-set! ihdr 9 (ctype-code type))
        (concat
          (append
            (list signature)
            (chunk "IHDR" ihdr)
            (chunk "IDAT" (zlib-compress (filter-rows (packed img type) h rowbytes bpp filter) #:level level))
            (chunk "IEND" #vu8())))))))
