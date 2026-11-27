;;; Windows bitmaps (BMP) for (capy image).
;;;
;;; Reading takes the OS/2 core header and the Windows headers (BITMAPINFOHEADER and its V2
;;; to V5 extensions); 1, 4 and 8 bits per pixel with a palette, uncompressed or RLE8/RLE4;
;;; 16 and 32 bits with the default layout or BI_BITFIELDS masks (alpha too); 24 bits; rows
;;; bottom-up or top-down. A 32-bit image without an alpha mask is opaque.
;;;
;;; Writing makes a 24-bit BMP when every pixel is opaque, else a 32-bit one with a V4 header
;;; and an alpha mask, rows bottom-up.

(library (capy image bmp)
  (export bmp->image image->bmp bmp-bytes?)
  (import (rnrs)
    (capy image base))

  (define (bad message . irritants)
    (apply image-format-error 'bmp->image message irritants))

  (define (bmp-bytes? bv)
    (and (bytevector? bv) (>= (bytevector-length bv) 26)
         (= (bytevector-u8-ref bv 0) 66) (= (bytevector-u8-ref bv 1) 77)))

  (define (u16 bv i) (bytevector-u16-ref bv i (endianness little)))
  (define (u32 bv i) (bytevector-u32-ref bv i (endianness little)))
  (define (s32 bv i) (bytevector-s32-ref bv i (endianness little)))

  ;; A channel mask: (shift . bits), or #f for none. Contiguous masks only.
  (define (mask-info m)
    (if (= m 0)
      #f
      (let loop ([shift 0])
        (if (odd? (bitwise-arithmetic-shift-right m shift))
          (let count ([bits 0])
            (if (odd? (bitwise-arithmetic-shift-right m (+ shift bits)))
              (count (+ bits 1))
              (begin
                (unless (= m (* (- (expt 2 bits) 1) (expt 2 shift)))
                  (bad "a channel mask is not contiguous" m))
                (cons shift bits))))
          (loop (+ shift 1))))))

  ;; The 8-bit value of channel INFO in pixel V.
  (define (channel v info default)
    (if info
      (let* ([bits (cdr info)]
             [x (bitwise-and (bitwise-arithmetic-shift-right v (car info)) (- (expt 2 bits) 1))])
        (cond [(= bits 8) x]
              [(> bits 8) (bitwise-arithmetic-shift-right x (- bits 8))]
              [else (div (+ (* x 255) (div (- (expt 2 bits) 1) 2)) (- (expt 2 bits) 1))]))
      default))

  ;; The image of the BMP file BV.
  (define (bmp->image bv)
    (unless (bmp-bytes? bv) (bad "not a BMP file"))
    (let* ([data-offset (u32 bv 10)]
           [hsize (u32 bv 14)]
           [core? (= hsize 12)])
      (unless (memv hsize '(12 40 52 56 64 108 124)) (bad "unknown BMP header size" hsize))
      (when (> (+ 14 hsize) (bytevector-length bv)) (bad "the header is truncated"))
      (let* ([w (if core? (u16 bv 18) (s32 bv 18))]
             [raw-h (if core? (u16 bv 20) (s32 bv 22))]
             [top-down? (< raw-h 0)]
             [h (abs raw-h)]
             [bpp (if core? (u16 bv 24) (u16 bv 28))]
             [compression (if core? 0 (u32 bv 30))]
             [colors-used (if (or core? (< hsize 40)) 0 (u32 bv 46))])
        (when (or (<= w 0) (= h 0) (> (* w h) #x10000000)) (bad "invalid or too large image size" w h))
        (unless (memv bpp '(1 4 8 16 24 32)) (bad "unsupported bits per pixel" bpp))
        (unless (memv compression '(0 1 2 3 6)) (bad "unsupported compression" compression))
        (let* (;; The masks follow a 40-byte header and are inside longer ones: at 54 either way.
               [masks-at 54]
               [masks (and (memv compression '(3 6))
                           (list (u32 bv masks-at) (u32 bv (+ masks-at 4)) (u32 bv (+ masks-at 8))
                                 (if (or (>= hsize 56) (= compression 6)) (u32 bv (+ masks-at 12)) 0)))]
               [palette-at (+ 14 hsize (if (and (= hsize 40) masks) (if (= compression 6) 16 12) 0))]
               [entry (if core? 3 4)]
               [ncolors (if (<= bpp 8) (if (> colors-used 0) colors-used (expt 2 bpp)) 0)]
               [palette (make-bytevector (* 4 256) 0)]
               [img (make-image w h)]
               [out (image-data img)]
               [stride (* 4 (div (+ (* bpp w) 31) 32))])
          ;; The output row of file row r.
          (define (out-row r) (if top-down? r (- h 1 r)))
          (do ([i 0 (+ i 1)]) ((= i (min ncolors 256)))
            (let ([p (+ palette-at (* entry i))])
              (when (> (+ p 3) (bytevector-length bv)) (bad "the palette is truncated"))
              (bytevector-u8-set! palette (* 4 i) (bytevector-u8-ref bv (+ p 2)))
              (bytevector-u8-set! palette (+ (* 4 i) 1) (bytevector-u8-ref bv (+ p 1)))
              (bytevector-u8-set! palette (+ (* 4 i) 2) (bytevector-u8-ref bv p))
              (bytevector-u8-set! palette (+ (* 4 i) 3) 255)))
          (cond
            [(memv compression '(1 2))
             (decode-rle bv data-offset w h (= compression 2) palette out out-row)]
            [else
             (when (> (+ data-offset (* stride h)) (bytevector-length bv)) (bad "the pixel data is truncated"))
             (let ([masks (or masks
                              (case bpp
                                [(16) '(#x7C00 #x3E0 #x1F 0)]
                                [(32) '(#xFF0000 #xFF00 #xFF 0)]
                                [else #f]))])
               (do ([r 0 (+ r 1)]) ((= r h))
                 (let ([row (+ data-offset (* r stride))]
                       [o (* 4 w (out-row r))])
                   (case bpp
                     [(24)
                      (do ([x 0 (fx+ x 1)]) ((fx=? x w))
                        (let ([s (fx+ row (fx* 3 x))] [d (fx+ o (fx* 4 x))])
                          (bytevector-u8-set! out d (bytevector-u8-ref bv (fx+ s 2)))
                          (bytevector-u8-set! out (fx+ d 1) (bytevector-u8-ref bv (fx+ s 1)))
                          (bytevector-u8-set! out (fx+ d 2) (bytevector-u8-ref bv s))
                          (bytevector-u8-set! out (fx+ d 3) 255)))]
                     [(16 32)
                      (decode-masked-row! bv row w (= bpp 32) masks out o)]
                     [else
                      (do ([x 0 (+ x 1)]) ((= x w))
                        (let* ([bit (* x bpp)]
                               [byte (bytevector-u8-ref bv (+ row (div bit 8)))]
                               [index (bitwise-and (bitwise-arithmetic-shift-right byte (- 8 bpp (mod bit 8)))
                                                   (- (expt 2 bpp) 1))])
                          (bytevector-copy! palette (* 4 index) out (+ o (* 4 x)) 4)))]))))])
          img))))

  (define (decode-masked-row! bv row w wide? masks out o)
    (let ([r (mask-info (car masks))] [g (mask-info (cadr masks))]
          [b (mask-info (caddr masks))] [a (mask-info (cadddr masks))])
      (if (and wide? (equal? (list r g b) '((16 . 8) (8 . 8) (0 . 8))) (or (not a) (equal? a '(24 . 8))))
        ;; The common BGRA / BGRX layout: bytes, no arithmetic on 32-bit words.
        (do ([x 0 (fx+ x 1)]) ((fx=? x w))
          (let ([s (fx+ row (fx* 4 x))] [d (fx+ o (fx* 4 x))])
            (bytevector-u8-set! out d (bytevector-u8-ref bv (fx+ s 2)))
            (bytevector-u8-set! out (fx+ d 1) (bytevector-u8-ref bv (fx+ s 1)))
            (bytevector-u8-set! out (fx+ d 2) (bytevector-u8-ref bv s))
            (bytevector-u8-set! out (fx+ d 3) (if a (bytevector-u8-ref bv (fx+ s 3)) 255))))
        (do ([x 0 (+ x 1)]) ((= x w))
          (let ([v (if wide? (u32 bv (+ row (* 4 x))) (u16 bv (+ row (* 2 x))))]
                [d (+ o (* 4 x))])
            (bytevector-u8-set! out d (channel v r 0))
            (bytevector-u8-set! out (+ d 1) (channel v g 0))
            (bytevector-u8-set! out (+ d 2) (channel v b 0))
            (bytevector-u8-set! out (+ d 3) (channel v a 255)))))))

  ;; RLE8 (RLE4 if four?): runs, absolute runs and escapes, into palette colors. Pixels the
  ;; data skips stay transparent.
  (define (decode-rle bv pos w h four? palette out out-row)
    (define (put! x r index)
      (when (and (< x w) (< r h))
        (bytevector-copy! palette (* 4 index) out (* 4 (+ x (* w (out-row r)))) 4)))
    (define (byte p)
      (if (< p (bytevector-length bv)) (bytevector-u8-ref bv p) (bad "the RLE data is truncated")))
    (let loop ([p pos] [x 0] [r 0])
      (when (< r h)
        (let ([count (byte p)] [value (byte (+ p 1))])
          (cond
            [(> count 0)
             (do ([k 0 (+ k 1)]) ((= k count))
               (put! (+ x k) r (if four?
                                 (if (even? k) (bitwise-arithmetic-shift-right value 4) (bitwise-and value 15))
                                 value)))
             (loop (+ p 2) (+ x count) r)]
            [(= value 0) (loop (+ p 2) 0 (+ r 1))]          ; end of line
            [(= value 1) #f]                                 ; end of bitmap
            [(= value 2) (loop (+ p 4) (+ x (byte (+ p 2))) (+ r (byte (+ p 3))))]
            [else
             ;; An absolute run of VALUE pixels, padded to a 16-bit boundary.
             (let ([nbytes (if four? (div (+ value 1) 2) value)])
               (do ([k 0 (+ k 1)]) ((= k value))
                 (put! (+ x k) r
                       (if four?
                         (let ([b (byte (+ p 2 (div k 2)))])
                           (if (even? k) (bitwise-arithmetic-shift-right b 4) (bitwise-and b 15)))
                         (byte (+ p 2 k)))))
               (loop (+ p 2 nbytes (mod nbytes 2)) (+ x value) r))])))))

  ;; IMG as a BMP file.
  (define (image->bmp img)
    (unless (image? img) (assertion-violation 'image->bmp "expected an image" img))
    (let* ([w (image-width img)] [h (image-height img)]
           [data (image-data img)]
           [opaque? (let loop ([o 3]) (or (>= o (bytevector-length data))
                                          (and (= 255 (bytevector-u8-ref data o)) (loop (+ o 4)))))]
           [bpp (if opaque? 24 32)]
           [hsize (if opaque? 40 108)]
           [stride (* 4 (div (+ (* bpp w) 31) 32))]
           [offset (+ 14 hsize)]
           [size (+ offset (* stride h))]
           [bv (make-bytevector size 0)])
      (bytevector-u8-set! bv 0 66)
      (bytevector-u8-set! bv 1 77)
      (bytevector-u32-set! bv 2 size (endianness little))
      (bytevector-u32-set! bv 10 offset (endianness little))
      (bytevector-u32-set! bv 14 hsize (endianness little))
      (bytevector-s32-set! bv 18 w (endianness little))
      (bytevector-s32-set! bv 22 h (endianness little))
      (bytevector-u16-set! bv 26 1 (endianness little))
      (bytevector-u16-set! bv 28 bpp (endianness little))
      (bytevector-u32-set! bv 30 (if opaque? 0 3) (endianness little))
      (bytevector-u32-set! bv 34 (* stride h) (endianness little))
      (bytevector-u32-set! bv 38 2835 (endianness little))   ; 72 dpi
      (bytevector-u32-set! bv 42 2835 (endianness little))
      (unless opaque?
        (bytevector-u32-set! bv 54 #x00FF0000 (endianness little))
        (bytevector-u32-set! bv 58 #x0000FF00 (endianness little))
        (bytevector-u32-set! bv 62 #x000000FF (endianness little))
        (bytevector-u32-set! bv 66 #xFF000000 (endianness little))
        (bytevector-u32-set! bv 70 #x73524742 (endianness little)))   ; LCS_sRGB, "sRGB"
      (do ([y 0 (+ y 1)]) ((= y h) bv)
        (let ([row (+ offset (* stride (- h 1 y)))]
              [src (* 4 w y)])
          (do ([x 0 (fx+ x 1)]) ((fx=? x w))
            (let ([s (fx+ src (fx* 4 x))]
                  [d (fx+ row (fx* (if opaque? 3 4) x))])
              (bytevector-u8-set! bv d (bytevector-u8-ref data (fx+ s 2)))
              (bytevector-u8-set! bv (fx+ d 1) (bytevector-u8-ref data (fx+ s 1)))
              (bytevector-u8-set! bv (fx+ d 2) (bytevector-u8-ref data s))
              (unless opaque?
                (bytevector-u8-set! bv (fx+ d 3) (bytevector-u8-ref data (fx+ s 3)))))))))))
