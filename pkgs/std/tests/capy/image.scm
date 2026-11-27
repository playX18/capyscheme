(import (except (rnrs) define)
        (rename (only (core kw) new-define) (new-define define))
        (srfi 64)
        (only (capy) microsecond)
        (capy image)
        (capy compress)
        (capy process))

(test-begin "capy image")

;; ---- Helpers ----------------------------------------------------------------------------

(define (bv-concat parts)
  (let* ([n (fold-left + 0 (map bytevector-length parts))] [out (make-bytevector n)])
    (let loop ([ps parts] [pos 0])
      (if (null? ps) out
        (begin (bytevector-copy! (car ps) 0 out pos (bytevector-length (car ps)))
               (loop (cdr ps) (+ pos (bytevector-length (car ps)))))))))

(define (bv-slice bv start . end)
  (let* ([end (if (null? end) (bytevector-length bv) (car end))]
         [out (make-bytevector (- end start))])
    (bytevector-copy! bv start out 0 (- end start))
    out))

(define (u32be x) (let ([b (make-bytevector 4)]) (bytevector-u32-set! b 0 x (endianness big)) b))

(define (format-error? thunk)
  (guard (c [(image-format-error? c) #t] [(compress-error? c) #t] [else #f])
    (thunk)
    #f))

;; A test pattern: every pixel different, with some transparency.
(define (pattern w h)
  (let ([img (make-image w h)])
    (do ([y 0 (+ y 1)]) ((= y h) img)
      (do ([x 0 (+ x 1)]) ((= x w))
        (image-set-rgba! img x y (mod (* x 7) 256) (mod (* y 11) 256) (mod (* (+ x y) 13) 256)
                         #:alpha (if (= 0 (mod (+ x (* 3 y)) 5)) (mod (* x y) 256) 255))))))

(define (opaque img)
  (let ([c (image-copy img)])
    (do ([o 3 (+ o 4)]) ((>= o (bytevector-length (image-data c))) c)
      (bytevector-u8-set! (image-data c) o 255))))

(define (gray img)
  (let* ([c (image-copy img)] [d (image-data c)])
    (do ([o 0 (+ o 4)]) ((>= o (bytevector-length d)) c)
      (bytevector-u8-set! d (+ o 1) (bytevector-u8-ref d o))
      (bytevector-u8-set! d (+ o 2) (bytevector-u8-ref d o)))))

;; ---- The image type ---------------------------------------------------------------------

(test-group "pixels"
  (let ([img (make-image 4 3)])
    (test-equal "size" '(4 3 48) (list (image-width img) (image-height img) (bytevector-length (image-data img))))
    (test-equal "transparent black by default" 0 (image-ref img 3 2))
    (image-set! img 1 2 #xFF8000C0)
    (test-equal "image-ref" #xFF8000C0 (image-ref img 1 2))
    (test-equal "image-rgba" '(255 128 0 192)
      (call-with-values (lambda () (image-rgba img 1 2)) list))
    (test-equal "layout" '(255 128 0 192)
      (map (lambda (i) (bytevector-u8-ref (image-data img) (+ (* 4 (+ 1 (* 4 2))) i))) '(0 1 2 3)))
    (test-error "outside" (image-ref img 4 0))
    (test-error "bad color" (image-set! img 0 0 -1)))
  (test-equal "rgba" #xFF8000FF (rgba 255 128 0))
  (test-equal "rgba with alpha" #x01020304 (rgba 1 2 3 #:alpha 4))
  (test-equal "color parts" '(18 52 86 120)
    (list (color-red #x12345678) (color-green #x12345678) (color-blue #x12345678) (color-alpha #x12345678)))
  (test-equal "make-image #:color" #x336699FF (image-ref (make-image 3 3 #:color #x336699FF) 2 2))
  (test-equal "bytevector->image, gray" #x7F7F7FFF (image-ref (bytevector->image 2 1 #vu8(0 127) #:channels 1) 1 0))
  (test-equal "bytevector->image, rgb" #x010203FF (image-ref (bytevector->image 1 1 #vu8(1 2 3) #:channels 3) 0 0))
  (test-equal "image->bytevector, rgb" #vu8(1 2 3)
    (image->bytevector (bytevector->image 1 1 #vu8(1 2 3 4)) #:channels 3))
  (test-equal "image->bytevector, gray is luma" #vu8(77 149 29)
    (image->bytevector (bytevector->image 3 1 #vu8(255 0 0 255 0 255 0 255 0 0 255 255)) #:channels 1))
  (test-error "bytevector->image, wrong length" (bytevector->image 2 2 #vu8(1 2 3))))

(test-group "fill, blit, crop, flip"
  (let ([img (make-image 10 10 #:color #x000000FF)])
    (image-fill! img #xFF0000FF #:x 8 #:y -2 #:width 5 #:height 4)
    (test-equal "fill is clipped" '(#xFF0000FF #xFF0000FF #x000000FF #x000000FF)
      (list (image-ref img 8 0) (image-ref img 9 1) (image-ref img 7 0) (image-ref img 9 2))))
  (let ([dst (make-image 6 6 #:color #x000000FF)]
        [src (make-image 3 3 #:color #x00FF00FF)])
    (image-set! src 0 0 #x0000FFFF)
    (image-blit! dst src #:x -1 #:y 4)
    (test-equal "blit is clipped" '(#x00FF00FF #x00FF00FF #x000000FF #x000000FF)
      (list (image-ref dst 0 4) (image-ref dst 1 5) (image-ref dst 2 4) (image-ref dst 0 3)))
    (image-blit! dst src #:x 3 #:y 0 #:src-x 0 #:src-y 0 #:width 1 #:height 1)
    (test-equal "blit a rectangle" '(#x0000FFFF #x000000FF) (list (image-ref dst 3 0) (image-ref dst 4 0))))
  (let ([dst (make-image 1 1 #:color #x0000FFFF)]
        [src (make-image 1 1 #:color #xFF000080)])
    (image-blit! dst src #:blend? #t)
    (test-equal "blend half red over blue" '(128 0 127 255)
      (call-with-values (lambda () (image-rgba dst 0 0)) list)))
  (let ([dst (make-image 1 1)] [src (make-image 1 1 #:color #xFF000080)])
    (image-blit! dst src #:blend? #t)
    (test-equal "blend over transparent keeps the color" '(255 0 0 128)
      (call-with-values (lambda () (image-rgba dst 0 0)) list)))
  (let* ([p (pattern 9 7)] [c (image-crop p 2 3 4 2)])
    (test-equal "crop" (list 4 2 (image-ref p 2 3) (image-ref p 5 4))
      (list (image-width c) (image-height c) (image-ref c 0 0) (image-ref c 3 1)))
    (test-error "crop outside" (image-crop p 6 0 4 1))
    (test-equal "flip horizontal" (image-ref p 0 2) (image-ref (image-flip p) 8 2))
    (test-equal "flip vertical" (image-ref p 3 0) (image-ref (image-flip p #:direction 'vertical) 3 6))))

(test-group "scale"
  (let* ([p (pattern 5 4)] [n (image-scale p 10 8 #:filter 'nearest)])
    (test-equal "nearest 2x" (list (image-ref p 0 0) (image-ref p 2 1) (image-ref p 4 3))
      (list (image-ref n 1 1) (image-ref n 4 3) (image-ref n 9 7))))
  (test-equal "nearest 1x is the identity" (image-data (pattern 6 5))
    (image-data (image-scale (pattern 6 5) 6 5 #:filter 'nearest)))
  (test-equal "bilinear of a flat image is flat" #x406080FF
    (image-ref (image-scale (make-image 7 3 #:color #x406080FF) 20 11) 13 5))
  (let ([two (bytevector->image 2 1 #vu8(0 0 0 255 200 100 50 255))])
    ;; Destination pixel 1 samples source x = 0.25.
    (test-equal "bilinear weights" '(50 25 13 255)
      (call-with-values (lambda () (image-rgba (image-scale two 4 1) 1 0)) list)))
  (let ([half (bytevector->image 2 1 #vu8(255 0 0 255 0 255 0 0))])
    (test-equal "bilinear ignores the color of transparent pixels" '(255 0 0)
      (call-with-values (lambda () (image-rgba (image-scale half 4 1) 1 0))
        (lambda (r g b a) (list r g b))))))

;; ---- PNG --------------------------------------------------------------------------------

(define (chunk type data)
  (let ([body (bv-concat (list (string->utf8 type) data))])
    (bv-concat (list (u32be (bytevector-length data)) body (u32be (crc32 body))))))

;; A PNG built independently of (capy image): filter 0 only, samples from (SAMPLE x y c) at
;; DEPTH bits, packed MSB first; Adam7 passes when INTERLACE is 1.
(define (make-png w h ctype depth interlace sample #:plte [plte #f] #:trns [trns #f])
  (let ([channels (case ctype [(0 3) 1] [(2) 3] [(4) 2] [else 4])])
    (define (scanlines x0 y0 dx dy)
      (let ([pw (div (+ (- w x0) (- dx 1)) dx)] [ph (div (+ (- h y0) (- dy 1)) dy)])
        (if (or (= pw 0) (= ph 0))
          '()
          (let ([rowbytes (div (+ (* pw channels depth) 7) 8)])
            (let rows ([r 0] [acc '()])
              (if (= r ph)
                (reverse acc)
                (let ([row (make-bytevector (+ 1 rowbytes) 0)])
                  (do ([i 0 (+ i 1)]) ((= i pw))
                    (do ([c 0 (+ c 1)]) ((= c channels))
                      (let ([v (sample (+ x0 (* i dx)) (+ y0 (* r dy)) c)]
                            [k (+ c (* i channels))])
                        (if (= depth 16)
                          (bytevector-u16-set! row (+ 1 (* 2 k)) v (endianness big))
                          (let* ([bit (* k depth)] [byte (+ 1 (div bit 8))])
                            (bytevector-u8-set! row byte
                              (bitwise-ior (bytevector-u8-ref row byte)
                                           (bitwise-arithmetic-shift-left v (- 8 depth (mod bit 8))))))))))
                  (rows (+ r 1) (cons row acc)))))))))
    (let ([ihdr (make-bytevector 13 0)])
      (bytevector-u32-set! ihdr 0 w (endianness big))
      (bytevector-u32-set! ihdr 4 h (endianness big))
      (bytevector-u8-set! ihdr 8 depth)
      (bytevector-u8-set! ihdr 9 ctype)
      (bytevector-u8-set! ihdr 12 interlace)
      (bv-concat
        (append
          (list #vu8(137 80 78 71 13 10 26 10) (chunk "IHDR" ihdr))
          (if plte (list (chunk "PLTE" plte)) '())
          (if trns (list (chunk "tRNS" trns)) '())
          (list (chunk "tEXt" (string->utf8 "Comment\x0;ignored"))
                (chunk "IDAT" (zlib-compress
                                (bv-concat
                                  (apply append
                                    (if (= interlace 0)
                                      (list (scanlines 0 0 1 1))
                                      (map (lambda (p) (apply scanlines p))
                                           '((0 0 8 8) (4 0 8 8) (0 4 4 8) (2 0 4 4) (0 2 2 4) (1 0 2 2) (0 1 1 2))))))))
                (chunk "IEND" #vu8())))))))

(define (scale8 v depth)
  (case depth [(16) (div v 256)] [(8) v] [else (div (* v 255) (- (expt 2 depth) 1))]))

(define (test-sample x y c depth) (mod (+ (* x 3) (* y 5) (* c 7)) (expt 2 depth)))

(test-group "PNG decoding: every color type, depth and interlace"
  (for-each
    (lambda (spec)
      (let ([ctype (car spec)])
        (for-each
          (lambda (depth)
            (for-each
              (lambda (interlace)
                (let* ([w 13] [h 11]
                       [png (make-png w h ctype depth interlace (lambda (x y c) (test-sample x y c depth))
                                      #:plte (and (= ctype 3)
                                                  (let ([p (make-bytevector (* 3 (expt 2 depth)))])
                                                    (do ([i 0 (+ i 1)]) ((= i (bytevector-length p)) p)
                                                      (bytevector-u8-set! p i (mod (* i 37) 256))))))]
                       [img (png->image png)]
                       [expected
                        (lambda (x y)
                          (let ([s (lambda (c) (scale8 (test-sample x y c depth) depth))])
                            (case ctype
                              [(0) (list (s 0) (s 0) (s 0) 255)]
                              [(2) (list (s 0) (s 1) (s 2) 255)]
                              [(3) (let ([i (test-sample x y 0 depth)])
                                     (list (mod (* 37 (* 3 i)) 256) (mod (* 37 (+ 1 (* 3 i))) 256)
                                           (mod (* 37 (+ 2 (* 3 i))) 256) 255))]
                              [(4) (list (s 0) (s 0) (s 0) (s 1))]
                              [else (list (s 0) (s 1) (s 2) (s 3))])))])
                  (test-assert (string-append "color type " (number->string ctype) ", depth " (number->string depth)
                                              (if (= interlace 1) ", Adam7" ""))
                    (let loop ([y 0])
                      (or (= y h)
                          (and (let xs ([x 0])
                                 (or (= x w)
                                     (and (equal? (call-with-values (lambda () (image-rgba img x y)) list)
                                                  (expected x y))
                                          (xs (+ x 1)))))
                               (loop (+ y 1))))))))
              '(0 1)))
          (cdr spec))))
    '((0 1 2 4 8 16) (2 8 16) (3 1 2 4 8) (4 8 16) (6 8 16))))

(test-group "PNG transparency"
  (test-equal "gray tRNS"
    '((0 0 0 0) (255 255 255 255))
    (let ([img (png->image (make-png 2 1 0 1 0 (lambda (x y c) x) #:trns #vu8(0 0)))])
      (list (call-with-values (lambda () (image-rgba img 0 0)) list)
            (call-with-values (lambda () (image-rgba img 1 0)) list))))
  (test-equal "RGB tRNS, 16-bit, compared at full depth"
    '(0 255)
    (let ([img (png->image (make-png 2 1 2 16 0 (lambda (x y c) (if (= x 0) 4660 4661))
                                     #:trns #vu8(#x12 #x34 #x12 #x34 #x12 #x34)))])
      (list (color-alpha (image-ref img 0 0)) (color-alpha (image-ref img 1 0)))))
  (test-equal "palette tRNS"
    '(#x0A141E10 #x28323CFF)
    (let ([img (png->image (make-png 2 1 3 8 0 (lambda (x y c) x)
                                     #:plte #vu8(10 20 30 40 50 60) #:trns #vu8(16)))])
      (list (image-ref img 0 0) (image-ref img 1 0)))))

(test-group "PNG encoding"
  (let ([p (pattern 37 23)])
    (for-each
      (lambda (case)
        (let* ([img ((cadr case) p)]
               [png (image->png img)])
          (test-equal (string-append "round trip, " (car case)) (image-data img) (image-data (png->image png)))
          (test-equal (string-append "color type, " (car case)) (caddr case) (bytevector-u8-ref png 25))))
      (list (list "rgba" (lambda (i) i) 6)
            (list "rgb" opaque 2)
            (list "gray" (lambda (i) (opaque (gray i))) 0)
            (list "gray-alpha" gray 4)))
    (for-each
      (lambda (filter)
        (test-equal (string-append "filter " (symbol->string filter)) (image-data p)
          (image-data (png->image (image->png p #:filter filter #:level 1)))))
      '(none sub up average paeth))
    (test-equal "forced rgba of an opaque image" 6 (bytevector-u8-ref (image->png (opaque p) #:color-type 'rgba) 25))
    (test-assert "a flat image compresses well"
      (< (bytevector-length (image->png (make-image 200 200 #:color #x123456FF))) 1000))))

(test-group "PNG errors"
  (let ([png (image->png (pattern 8 8))])
    (test-assert "bad signature" (format-error? (lambda () (png->image (bv-slice png 1)))))
    (test-assert "bad CRC"
      (format-error? (lambda ()
                       (let ([b (bytevector-copy png)])
                         (bytevector-u8-set! b 20 (bitwise-xor 1 (bytevector-u8-ref b 20)))
                         (png->image b)))))
    (test-assert "truncated" (format-error? (lambda () (png->image (bv-slice png 0 (- (bytevector-length png) 20))))))
    (test-assert "bad depth"
      (format-error? (lambda () (png->image (make-png 1 1 2 4 0 (lambda (x y c) 0))))))))

;; ---- Netpbm -----------------------------------------------------------------------------

(test-group "Netpbm"
  (define (rgba-list img x y) (call-with-values (lambda () (image-rgba img x y)) list))
  (test-equal "P1 with comments and no spaces"
    '((0 0 0 255) (255 255 255 255))
    (let ([img (pnm->image (string->utf8 "P1\n# a comment\n3 1\n101"))])
      (list (rgba-list img 0 0) (rgba-list img 1 0))))
  (test-equal "P2 with maxval 15" '(17 17 17 255)
    (rgba-list (pnm->image (string->utf8 "P2 2 1 15 0 1")) 1 0))
  (test-equal "P3" '(1 2 3 255) (rgba-list (pnm->image (string->utf8 "P3\n1 1\n255\n1 2 3\n")) 0 0))
  (test-equal "P4" '((0 0 0 255) (255 255 255 255) (0 0 0 255))
    (let ([img (pnm->image (bv-concat (list (string->utf8 "P4\n10 1\n") #vu8(#b10000000 #b01000000))))])
      (list (rgba-list img 0 0) (rgba-list img 1 0) (rgba-list img 9 0))))
  (test-equal "P5, 16-bit" '(128 128 128 255)
    (rgba-list (pnm->image (bv-concat (list (string->utf8 "P5 1 1 65535\n") #vu8(#x80 #x00)))) 0 0))
  (let ([p (opaque (pattern 17 9))])
    (test-equal "P6 round trip" (image-data p) (image-data (pnm->image (image->ppm p))))
    (test-equal "P3 round trip" (image-data p) (image-data (pnm->image (image->ppm p #:plain? #t))))
    (test-equal "P5 is the luma" (image->bytevector p #:channels 1)
      (image->bytevector (pnm->image (image->pgm p)) #:channels 1))
    (test-equal "P2 round trip" (image->bytevector p #:channels 1)
      (image->bytevector (pnm->image (image->pgm p #:plain? #t)) #:channels 1)))
  (test-assert "sample above maxval" (format-error? (lambda () (pnm->image (string->utf8 "P2 1 1 7 9")))))
  (test-assert "short raster" (format-error? (lambda () (pnm->image (string->utf8 "P6 2 2 255\nabc"))))))

;; ---- BMP --------------------------------------------------------------------------------

(define (make-bmp w h bpp compression pixels #:palette [palette #vu8()] #:masks [masks #f] #:top-down? [top? #f])
  (let* ([hsize 40]
         [mask-bytes (if masks (bv-concat (map (lambda (m) (let ([b (make-bytevector 4)])
                                                              (bytevector-u32-set! b 0 m (endianness little)) b))
                                               masks))
                       #vu8())]
         [offset (+ 14 hsize (bytevector-length mask-bytes) (bytevector-length palette))]
         [hdr (make-bytevector (+ 14 hsize) 0)])
    (bytevector-u8-set! hdr 0 66) (bytevector-u8-set! hdr 1 77)
    (bytevector-u32-set! hdr 2 (+ offset (bytevector-length pixels)) (endianness little))
    (bytevector-u32-set! hdr 10 offset (endianness little))
    (bytevector-u32-set! hdr 14 hsize (endianness little))
    (bytevector-s32-set! hdr 18 w (endianness little))
    (bytevector-s32-set! hdr 22 (if top? (- h) h) (endianness little))
    (bytevector-u16-set! hdr 26 1 (endianness little))
    (bytevector-u16-set! hdr 28 bpp (endianness little))
    (bytevector-u32-set! hdr 30 compression (endianness little))
    (bytevector-u32-set! hdr 46 (div (bytevector-length palette) 4) (endianness little))
    (bv-concat (list hdr mask-bytes palette pixels))))

(test-group "BMP"
  (define (rgba-list img x y) (call-with-values (lambda () (image-rgba img x y)) list))
  (let ([p (pattern 13 7)])
    (test-equal "32-bit round trip keeps alpha" (image-data p) (image-data (bmp->image (image->bmp p))))
    (test-equal "opaque images are 24-bit" 24 (bytevector-u16-ref (image->bmp (opaque p)) 28 (endianness little)))
    (test-equal "24-bit round trip" (image-data (opaque p)) (image-data (bmp->image (image->bmp (opaque p))))))
  ;; 2x2, 8-bit palette, bottom-up: the first file row is the bottom row. Rows pad to 4 bytes.
  (let ([img (bmp->image (make-bmp 2 2 8 0 #vu8(0 1 0 0  1 0 0 0)
                                   #:palette #vu8(255 0 0 0  0 0 255 0)))])
    (test-equal "8-bit palette, bottom-up" '((0 0 255 255) (255 0 0 255) (255 0 0 255) (0 0 255 255))
      (list (rgba-list img 0 1) (rgba-list img 1 1) (rgba-list img 0 0) (rgba-list img 1 0))))
  (let ([img (bmp->image (make-bmp 3 1 4 0 #vu8(#x01 #x20 0 0) #:palette #vu8(0 0 0 0  255 255 255 0  0 255 0 0)
                                   #:top-down? #t))])
    (test-equal "4-bit, top-down" '((0 0 0 255) (255 255 255 255) (0 255 0 255))
      (list (rgba-list img 0 0) (rgba-list img 1 0) (rgba-list img 2 0))))
  (let ([img (bmp->image (make-bmp 9 1 1 0 #vu8(#b10000000 #b10000000 0 0) #:palette #vu8(0 0 0 0  255 255 255 0)))])
    (test-equal "1-bit" '((255 255 255 255) (0 0 0 255) (255 255 255 255))
      (list (rgba-list img 0 0) (rgba-list img 1 0) (rgba-list img 8 0))))
  (let ([img (bmp->image (make-bmp 1 1 16 3 #vu8(#x1F #xF8 0 0) #:masks '(#xF800 #x7E0 #x1F)))])
    (test-equal "16-bit 5-6-5 bitfields" '(255 0 255 255) (rgba-list img 0 0)))
  (let ([img (bmp->image (make-bmp 1 1 16 0 #vu8(#xE0 #x03 0 0)))])
    (test-equal "16-bit default 5-5-5" '(0 255 0 255) (rgba-list img 0 0)))
  ;; Bottom row: a run of 3 of index 1, end of line. Top row: an absolute run of 2 0 1 (padded
  ;; to an even count of bytes), end of bitmap. Pixel (3, 1) is never written.
  (let ([img (bmp->image (make-bmp 4 2 8 1 #vu8(3 1  0 0  0 3 2 0 1 0  0 1)
                                   #:palette #vu8(0 0 0 0  0 0 255 0  0 255 0 0)))])
    (test-equal "RLE8 runs and an absolute run"
      '((255 0 0 255) (255 0 0 255) (0 0 0 0) (0 255 0 255) (0 0 0 255) (255 0 0 255))
      (list (rgba-list img 0 1) (rgba-list img 2 1) (rgba-list img 3 1)
            (rgba-list img 0 0) (rgba-list img 1 0) (rgba-list img 2 0))))
  (test-assert "not a BMP" (format-error? (lambda () (bmp->image (string->utf8 "XX not a bitmap at all.....")))))
  (test-assert "truncated" (format-error? (lambda () (bmp->image (bv-slice (image->bmp (pattern 5 5)) 0 100))))))

;; ---- Files ------------------------------------------------------------------------------

(define tmp (string-append "/tmp/capy-image-test-" (number->string (microsecond))))

(test-group "files"
  (let ([p (pattern 21 12)])
    (for-each
      (lambda (ext)
        (let ([path (string-append tmp "." ext)])
          (write-image p path)
          (test-equal (string-append "write and read ." ext)
            (image-data (if (member ext '("png" "bmp")) p (opaque p)))
            (image-data (let ([img (read-image path)]) (if (string=? ext "pgm") (opaque p) img))))
          (delete-file path)))
      '("png" "bmp" "ppm"))
    (test-equal "decode-image recognizes each format" '(#t #t #t)
      (map (lambda (fmt) (= 21 (image-width (decode-image (encode-image p fmt))))) '(png bmp pgm)))
    (test-error "unknown extension" (write-image p (string-append tmp ".xyz")))))

;; ---- Interoperability with PIL ----------------------------------------------------------

(define (have-pil?)
  (guard (c [#t #f])
    (let ([p (spawn "python3" '("-c" "import PIL") #:stdout 'null #:stderr 'null)])
      (process-wait p)
      (process-success? p))))

(define (python script . args)
  (let ([p (spawn "python3" (cons "-c" (cons script args)) #:stdout 'string #:stderr 'string)])
    (process-wait p)
    (unless (process-success? p) (error 'python (process-stderr-string p)))
    (process-stdout-string p)))

(define (read-bytes path) (call-with-port (open-file-input-port path) get-bytevector-all))

(define pil-write "
import sys
from PIL import Image
d = sys.argv[1]
w, h = 29, 17
img = Image.new('RGBA', (w, h))
img.putdata([((x * 9) % 256, (y * 15) % 256, ((x + y) * 7) % 256, 255 if (x + y) % 4 else (x * 20) % 256)
             for y in range(h) for x in range(w)])
cases = {
  'rgba.png': img, 'rgb.png': img.convert('RGB'), 'l.png': img.convert('L'), 'la.png': img.convert('LA'),
  'one.png': img.convert('1'), 'p.png': img.convert('RGB').quantize(16),
  'i16.png': img.convert('I').point(lambda v: v * 257).convert('I;16'),
  'rgb.bmp': img.convert('RGB'), 'p.bmp': img.convert('RGB').quantize(200), 'one.bmp': img.convert('1'),
  'rgb.ppm': img.convert('RGB'), 'l.pgm': img.convert('L'),
}
for name, im in cases.items():
    kw = {'bits': 4} if name == 'p.png' else {}
    im.save(d + '/' + name, **kw)
    # PIL clamps 16-bit gray to 255 when it converts to RGBA; the reference for it is the
    # 8-bit image it was made from (v * 257 has v as its high byte).
    ref = img.convert('L') if name == 'i16.png' else Image.open(d + '/' + name)
    with open(d + '/' + name + '.rgba', 'wb') as f:
        f.write(ref.convert('RGBA').tobytes())
print(' '.join(cases))
")

(define pil-read "
import sys
from PIL import Image
for path in sys.argv[1:]:
    with open(path + '.rgba', 'wb') as f:
        f.write(Image.open(path).convert('RGBA').tobytes())
")

(when (have-pil?)
  (test-group "PIL"
    (let ([dir (string-append tmp "-pil")])
      (run "mkdir" (list "-p" dir))
      (for-each
        (lambda (name)
          (test-equal (string-append "decode PIL's " name)
            (read-bytes (string-append dir "/" name ".rgba"))
            (image-data (read-image (string-append dir "/" name)))))
        (let ([out (python pil-write dir)])
          (let loop ([words (string->list out)] [cur '()] [acc '()])
            (cond
              [(null? words) (reverse (if (null? cur) acc (cons (list->string (reverse cur)) acc)))]
              [(char-whitespace? (car words))
               (loop (cdr words) '() (if (null? cur) acc (cons (list->string (reverse cur)) acc)))]
              [else (loop (cdr words) (cons (car words) cur) acc)]))))
      (let ([p (pattern 31 19)])
        (for-each
          (lambda (name img) (write-image img (string-append dir "/ours-" name)))
          '("rgba.png" "gray.png" "alpha.bmp" "rgb.bmp" "rgb.ppm")
          (list p (opaque (gray p)) p (opaque p) (opaque p)))
        (python pil-read (string-append dir "/ours-rgba.png") (string-append dir "/ours-gray.png")
                (string-append dir "/ours-alpha.bmp") (string-append dir "/ours-rgb.bmp")
                (string-append dir "/ours-rgb.ppm"))
        (for-each
          (lambda (name img)
            (test-equal (string-append "PIL decodes our " name)
              (image-data img) (read-bytes (string-append dir "/ours-" name ".rgba"))))
          '("rgba.png" "gray.png" "alpha.bmp" "rgb.bmp" "rgb.ppm")
          (list p (opaque (gray p)) p (opaque p) (opaque p))))
      (run "rm" (list "-rf" dir)))))

(test-end "capy image")
