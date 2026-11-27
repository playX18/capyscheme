;;; The image type of (capy image) and the operations on pixels.
;;;
;;; An image is WIDTH x HEIGHT pixels of 8-bit RGBA, stored row by row from the top left in
;;; one bytevector (`image-data`): pixel (x, y) is the 4 bytes at 4 (y WIDTH + x), red first.
;;; The alpha is straight (not premultiplied).
;;;
;;; A color is an exact integer #xRRGGBBAA, as in CSS: #xFF0000FF is opaque red and 0 is
;;; transparent black. `rgba` builds one from its parts, and `color-red` ... `color-alpha`
;;; take it apart.

(library (capy image base)
  (export make-image image? image-width image-height image-data
    image-copy bytevector->image image->bytevector
    rgba color-red color-green color-blue color-alpha
    image-ref image-set! image-rgba image-set-rgba!
    image-fill! image-blit! image-crop image-scale image-flip
    image-error &image-format-error make-image-format-error image-format-error? image-format-error)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define)))

  (define-record-type (image %make-image image?)
    (fields (immutable width image-width)
            (immutable height image-height)
            (immutable data image-data)))

  ;; Bad arguments: an assertion violation.
  (define (image-error who message . irritants)
    (apply assertion-violation who message irritants))

  ;; Raised for a file that is not a valid image of its format.
  (define-condition-type &image-format-error &error
    make-image-format-error
    image-format-error?)

  (define (image-format-error who message . irritants)
    (raise
      (condition
        (make-image-format-error)
        (make-who-condition who)
        (make-message-condition message)
        (make-irritants-condition irritants))))

  (define (check-size who width height)
    (unless (and (fixnum? width) (fixnum? height) (fx>=? width 0) (fx>=? height 0)
                 (<= (* 4 width height) (greatest-fixnum)))
      (image-error who "invalid image size" width height)))

  (define (check-color who c)
    (unless (and (integer? c) (exact? c) (<= 0 c #xFFFFFFFF))
      (image-error who "expected a color #xRRGGBBAA" c)))

  (define (check-image who img)
    (unless (image? img) (image-error who "expected an image" img)))

  ;; ---- Colors ----------------------------------------------------------------------------

  ;; The color with red, green, blue and alpha R, G, B, A (0-255; A defaults to 255).
  ;;
  ;;   (rgba 255 128 0)       ; => #xFF8000FF
  (define (rgba r g b #:alpha [a 255])
    (for-each (lambda (v) (unless (and (fixnum? v) (fx<=? 0 v 255))
                            (image-error 'rgba "a color component must be from 0 to 255" v)))
              (list r g b a))
    (+ (* r #x1000000) (* g #x10000) (* b #x100) a))

  (define (color-red c) (bitwise-and (bitwise-arithmetic-shift-right c 24) 255))
  (define (color-green c) (bitwise-and (bitwise-arithmetic-shift-right c 16) 255))
  (define (color-blue c) (bitwise-and (bitwise-arithmetic-shift-right c 8) 255))
  (define (color-alpha c) (bitwise-and c 255))

  ;; ---- Construction ----------------------------------------------------------------------

  ;; A WIDTH x HEIGHT image filled with COLOR (transparent black by default).
  ;;
  ;;   (make-image 64 32 #:color #xFFFFFFFF)      ; opaque white
  (define (make-image width height #:color [color 0])
    (check-size 'make-image width height)
    (check-color 'make-image color)
    (let ([img (%make-image width height (make-bytevector (fx* 4 (fx* width height)) 0))])
      (unless (eqv? color 0) (image-fill! img color))
      img))

  (define (image-copy img)
    (check-image 'image-copy img)
    (%make-image (image-width img) (image-height img) (bytevector-copy (image-data img))))

  ;; An image from packed pixels: CHANNELS bytes per pixel, rows from the top. 1 is gray, 2 gray
  ;; and alpha, 3 RGB and 4 RGBA. With 4 channels the bytevector becomes the image's data
  ;; without a copy unless #:copy? is true.
  (define (bytevector->image width height bytes #:channels [channels 4] #:copy? [copy? #f])
    (check-size 'bytevector->image width height)
    (unless (memv channels '(1 2 3 4))
      (image-error 'bytevector->image "#:channels must be 1, 2, 3 or 4" channels))
    (unless (and (bytevector? bytes) (= (bytevector-length bytes) (* channels width height)))
      (image-error 'bytevector->image "the bytevector does not hold WIDTH x HEIGHT x CHANNELS bytes"
                   width height channels))
    (if (fx=? channels 4)
      (%make-image width height (if copy? (bytevector-copy bytes) bytes))
      (let* ([n (fx* width height)]
             [out (make-bytevector (fx* 4 n) 255)])
        (case channels
          [(1) (do ([i 0 (fx+ i 1)]) ((fx=? i n))
                 (let ([v (bytevector-u8-ref bytes i)] [o (fx* 4 i)])
                   (bytevector-u8-set! out o v)
                   (bytevector-u8-set! out (fx+ o 1) v)
                   (bytevector-u8-set! out (fx+ o 2) v)))]
          [(2) (do ([i 0 (fx+ i 1)]) ((fx=? i n))
                 (let ([v (bytevector-u8-ref bytes (fx* 2 i))] [o (fx* 4 i)])
                   (bytevector-u8-set! out o v)
                   (bytevector-u8-set! out (fx+ o 1) v)
                   (bytevector-u8-set! out (fx+ o 2) v)
                   (bytevector-u8-set! out (fx+ o 3) (bytevector-u8-ref bytes (fx+ 1 (fx* 2 i))))))]
          [else (do ([i 0 (fx+ i 1)]) ((fx=? i n))
                  (let ([s (fx* 3 i)] [o (fx* 4 i)])
                    (bytevector-u8-set! out o (bytevector-u8-ref bytes s))
                    (bytevector-u8-set! out (fx+ o 1) (bytevector-u8-ref bytes (fx+ s 1)))
                    (bytevector-u8-set! out (fx+ o 2) (bytevector-u8-ref bytes (fx+ s 2)))))])
        (%make-image width height out))))

  ;; The pixels packed with CHANNELS bytes each (see bytevector->image). Gray is the luma
  ;; (0.299 R + 0.587 G + 0.114 B); dropping alpha just drops it.
  (define (image->bytevector img #:channels [channels 4])
    (check-image 'image->bytevector img)
    (unless (memv channels '(1 2 3 4))
      (image-error 'image->bytevector "#:channels must be 1, 2, 3 or 4" channels))
    (let* ([data (image-data img)]
           [n (fx* (image-width img) (image-height img))])
      (if (fx=? channels 4)
        (bytevector-copy data)
        (let ([out (make-bytevector (fx* channels n) 0)])
          (do ([i 0 (fx+ i 1)]) ((fx=? i n) out)
            (let ([s (fx* 4 i)] [o (fx* channels i)])
              (case channels
                [(3)
                 (bytevector-u8-set! out o (bytevector-u8-ref data s))
                 (bytevector-u8-set! out (fx+ o 1) (bytevector-u8-ref data (fx+ s 1)))
                 (bytevector-u8-set! out (fx+ o 2) (bytevector-u8-ref data (fx+ s 2)))]
                [else
                 (bytevector-u8-set! out o (luma data s))
                 (when (fx=? channels 2)
                   (bytevector-u8-set! out (fx+ o 1) (bytevector-u8-ref data (fx+ s 3))))])))))))

  ;; Rec. 601 luma of the pixel at data[s], rounded: (77 R + 150 G + 29 B + 128) / 256.
  (define (luma data s)
    (fxarithmetic-shift-right
      (fx+ (fx+ (fx* 77 (bytevector-u8-ref data s)) (fx* 150 (bytevector-u8-ref data (fx+ s 1))))
           (fx+ (fx* 29 (bytevector-u8-ref data (fx+ s 2))) 128))
      8))

  ;; ---- Pixels ----------------------------------------------------------------------------

  (define (pixel-offset who img x y)
    (unless (and (fixnum? x) (fixnum? y)
                 (fx<=? 0 x) (fx<? x (image-width img)) (fx<=? 0 y) (fx<? y (image-height img)))
      (image-error who "pixel outside the image" x y (image-width img) (image-height img)))
    (fx* 4 (fx+ x (fx* y (image-width img)))))

  ;; The color of pixel (X, Y).
  (define (image-ref img x y)
    (check-image 'image-ref img)
    (let ([o (pixel-offset 'image-ref img x y)] [d (image-data img)])
      (+ (* (bytevector-u8-ref d o) #x1000000) (* (bytevector-u8-ref d (fx+ o 1)) #x10000)
         (* (bytevector-u8-ref d (fx+ o 2)) #x100) (bytevector-u8-ref d (fx+ o 3)))))

  ;; The red, green, blue and alpha of pixel (X, Y), as four values.
  (define (image-rgba img x y)
    (check-image 'image-rgba img)
    (let ([o (pixel-offset 'image-rgba img x y)] [d (image-data img)])
      (values (bytevector-u8-ref d o) (bytevector-u8-ref d (fx+ o 1))
              (bytevector-u8-ref d (fx+ o 2)) (bytevector-u8-ref d (fx+ o 3)))))

  (define (image-set! img x y color)
    (check-image 'image-set! img)
    (check-color 'image-set! color)
    (let ([o (pixel-offset 'image-set! img x y)] [d (image-data img)])
      (bytevector-u8-set! d o (color-red color))
      (bytevector-u8-set! d (fx+ o 1) (color-green color))
      (bytevector-u8-set! d (fx+ o 2) (color-blue color))
      (bytevector-u8-set! d (fx+ o 3) (color-alpha color))))

  (define (image-set-rgba! img x y r g b #:alpha [a 255])
    (check-image 'image-set-rgba! img)
    (let ([o (pixel-offset 'image-set-rgba! img x y)] [d (image-data img)])
      (bytevector-u8-set! d o r)
      (bytevector-u8-set! d (fx+ o 1) g)
      (bytevector-u8-set! d (fx+ o 2) b)
      (bytevector-u8-set! d (fx+ o 3) a)))

  ;; ---- Rectangles ------------------------------------------------------------------------

  ;; The part of the rectangle (x, y, w, h) inside a WIDTH x HEIGHT image, as
  ;; (values x0 y0 x1 y1) with x0 <= x1 and y0 <= y1.
  (define (clip x y w h width height)
    (let ([x0 (max 0 x)] [y0 (max 0 y)]
          [x1 (min width (+ x w))] [y1 (min height (+ y h))])
      (values x0 y0 (max x0 x1) (max y0 y1))))

  ;; Sets every pixel of the rectangle (#:x, #:y, #:width, #:height), by default the whole
  ;; image, to COLOR. The rectangle is clipped to the image.
  ;;
  ;;   (image-fill! img #xFF0000FF #:x 10 #:y 10 #:width 5 #:height 5)
  (define (image-fill! img color #:x [x 0] #:y [y 0] #:width [w #f] #:height [h #f])
    (check-image 'image-fill! img)
    (check-color 'image-fill! color)
    (let-values ([(x0 y0 x1 y1) (clip x y (or w (image-width img)) (or h (image-height img))
                                      (image-width img) (image-height img))])
      (let ([data (image-data img)]
            [stride (fx* 4 (image-width img))]
            [span (fx* 4 (fx- x1 x0))])
        (when (and (fx>? span 0) (fx<? y0 y1))
          (let ([first (fx+ (fx* y0 stride) (fx* 4 x0))])
            ;; One pixel, then doubling copies across the first row, then the row down.
            (bytevector-u8-set! data first (color-red color))
            (bytevector-u8-set! data (fx+ first 1) (color-green color))
            (bytevector-u8-set! data (fx+ first 2) (color-blue color))
            (bytevector-u8-set! data (fx+ first 3) (color-alpha color))
            (let loop ([done 4])
              (when (fx<? done span)
                (let ([n (fxmin done (fx- span done))])
                  (bytevector-copy! data first data (fx+ first done) n)
                  (loop (fx+ done n)))))
            (do ([row (fx+ y0 1) (fx+ row 1)]) ((fx=? row y1))
              (bytevector-copy! data first data (fx+ (fx* row stride) (fx* 4 x0)) span)))))))

  ;; Copies SRC, or its rectangle (#:src-x, #:src-y, #:width, #:height), into DST with its
  ;; top left at (#:x, #:y). Clipped to both images. With #:blend? the pixels are composited
  ;; over DST by their alpha (Porter-Duff "over") instead of replacing it.
  ;;
  ;;   (image-blit! canvas sprite #:x 32 #:y 16 #:blend? #t)
  (define (image-blit! dst src #:x [x 0] #:y [y 0] #:src-x [sx 0] #:src-y [sy 0]
                       #:width [w #f] #:height [h #f] #:blend? [blend? #f])
    (check-image 'image-blit! dst)
    (check-image 'image-blit! src)
    ;; Clip the source rectangle to SRC, then its image at (x, y) to DST.
    (let*-values ([(w) (or w (image-width src))]
                  [(h) (or h (image-height src))]
                  [(sx0 sy0 sx1 sy1) (clip sx sy w h (image-width src) (image-height src))]
                  [(dx0 dy0 dx1 dy1) (clip (+ x (- sx0 sx)) (+ y (- sy0 sy))
                                           (- sx1 sx0) (- sy1 sy0)
                                           (image-width dst) (image-height dst))])
      (let* ([sx (+ sx0 (- dx0 (+ x (- sx0 sx))))]
             [sy (+ sy0 (- dy0 (+ y (- sy0 sy))))]
             [cols (fx- dx1 dx0)]
             [sdata (image-data src)] [ddata (image-data dst)]
             [sstride (fx* 4 (image-width src))] [dstride (fx* 4 (image-width dst))])
        (when (fx>? cols 0)
          (do ([row 0 (fx+ row 1)]) ((fx=? row (fx- dy1 dy0)))
            (let ([s (fx+ (fx* (fx+ sy row) sstride) (fx* 4 sx))]
                  [d (fx+ (fx* (fx+ dy0 row) dstride) (fx* 4 dx0))])
              (if blend?
                (blend-row! sdata s ddata d cols)
                (bytevector-copy! sdata s ddata d (fx* 4 cols)))))))))

  ;; src over dst, straight alpha, for N pixels.
  (define (blend-row! sdata s ddata d n)
    (let loop ([k 0] [s s] [d d])
      (when (fx<? k n)
        (let ([sa (bytevector-u8-ref sdata (fx+ s 3))])
          (cond
            [(fx=? sa 255) (bytevector-copy! sdata s ddata d 4)]
            [(fx=? sa 0) #f]
            [else
             (let* ([da (bytevector-u8-ref ddata (fx+ d 3))]
                    [inv (fx- 255 sa)]
                    ;; out alpha * 255 = sa * 255 + da * inv
                    [oa255 (fx+ (fx* sa 255) (fx* da inv))]
                    [half (fxarithmetic-shift-right oa255 1)])
               (do ([c 0 (fx+ c 1)]) ((fx=? c 3))
                 (let ([sc (bytevector-u8-ref sdata (fx+ s c))]
                       [dc (bytevector-u8-ref ddata (fx+ d c))])
                   (bytevector-u8-set! ddata (fx+ d c)
                     (fxdiv (fx+ (fx+ (fx* (fx* sc sa) 255) (fx* (fx* dc da) inv)) half) oa255))))
               (bytevector-u8-set! ddata (fx+ d 3) (fxdiv (fx+ oa255 127) 255)))])
          (loop (fx+ k 1) (fx+ s 4) (fx+ d 4))))))

  ;; A new image of the rectangle (X, Y, WIDTH, HEIGHT) of IMG, which must lie inside it.
  (define (image-crop img x y width height)
    (check-image 'image-crop img)
    (unless (and (fixnum? x) (fixnum? y) (fixnum? width) (fixnum? height)
                 (fx>=? x 0) (fx>=? y 0) (fx>=? width 0) (fx>=? height 0)
                 (<= (+ x width) (image-width img)) (<= (+ y height) (image-height img)))
      (image-error 'image-crop "the rectangle is not inside the image"
                   x y width height (image-width img) (image-height img)))
    (let ([out (make-image width height)])
      (image-blit! out img #:src-x x #:src-y y #:width width #:height height)
      out))

  ;; A mirror image: #:direction is horizontal (left-right, the default) or vertical.
  (define (image-flip img #:direction [direction 'horizontal])
    (check-image 'image-flip img)
    (let* ([w (image-width img)] [h (image-height img)]
           [src (image-data img)] [stride (fx* 4 w)]
           [out (make-bytevector (bytevector-length src) 0)])
      (case direction
        [(vertical)
         (do ([y 0 (fx+ y 1)]) ((fx=? y h))
           (bytevector-copy! src (fx* y stride) out (fx* (fx- (fx- h 1) y) stride) stride))]
        [(horizontal)
         (do ([y 0 (fx+ y 1)]) ((fx=? y h))
           (do ([x 0 (fx+ x 1)]) ((fx=? x w))
             (bytevector-copy! src (fx+ (fx* y stride) (fx* 4 x))
                               out (fx+ (fx* y stride) (fx* 4 (fx- (fx- w 1) x))) 4)))]
        [else (image-error 'image-flip "#:direction must be horizontal or vertical" direction)])
      (%make-image w h out)))

  ;; ---- Scaling ---------------------------------------------------------------------------

  ;; IMG resized to WIDTH x HEIGHT. #:filter is bilinear (the default) or nearest.
  ;; Bilinear weights each neighbour by its alpha, so transparent pixels do not bleed their
  ;; color into the result.
  ;;
  ;;   (image-scale icon 256 256 #:filter 'nearest)    ; pixel art stays crisp
  (define (image-scale img width height #:filter [filter 'bilinear])
    (check-image 'image-scale img)
    (check-size 'image-scale width height)
    (let ([sw (image-width img)] [sh (image-height img)])
      (cond
        [(or (fx=? width 0) (fx=? height 0) (fx=? sw 0) (fx=? sh 0)) (make-image width height)]
        [(eq? filter 'nearest) (scale-nearest img width height)]
        [(eq? filter 'bilinear) (scale-bilinear img width height)]
        [else (image-error 'image-scale "#:filter must be bilinear or nearest" filter)])))

  (define (scale-nearest img width height)
    (let* ([sw (image-width img)] [sh (image-height img)]
           [src (image-data img)]
           [out (make-bytevector (fx* 4 (fx* width height)) 0)]
           [xs (make-vector width 0)])
      ;; The source column of each destination column: the one under its center.
      (do ([x 0 (fx+ x 1)]) ((fx=? x width))
        (vector-set! xs x (fx* 4 (exact (floor (/ (* (+ (* 2 x) 1) sw) (* 2 width)))))))
      (do ([y 0 (fx+ y 1)]) ((fx=? y height))
        (let ([srow (fx* (fx* 4 sw) (exact (floor (/ (* (+ (* 2 y) 1) sh) (* 2 height)))))]
              [drow (fx* (fx* 4 width) y)])
          (do ([x 0 (fx+ x 1)]) ((fx=? x width))
            (bytevector-copy! src (fx+ srow (vector-ref xs x)) out (fx+ drow (fx* 4 x)) 4))))
      (%make-image width height out)))

  ;; For each destination coordinate: the two source coordinates and the weight of the second
  ;; in 1/128ths, sampling at centers.
  (define (bilinear-axis dst src)
    (let ([lo (make-vector dst 0)] [hi (make-vector dst 0)] [wt (make-vector dst 0)])
      (do ([i 0 (fx+ i 1)]) ((fx=? i dst))
        (let* ([pos (- (/ (* (+ i 0.5) src) dst) 0.5)]
               [pos (max 0.0 (min (- src 1.0) pos))]
               [i0 (exact (floor pos))]
               [i1 (min (- src 1) (+ i0 1))])
          (vector-set! lo i i0)
          (vector-set! hi i i1)
          (vector-set! wt i (exact (round (* 128 (- pos i0)))))))
      (values lo hi wt)))

  (define (scale-bilinear img width height)
    (let*-values ([(sw) (image-width img)]
                  [(sh) (image-height img)]
                  [(src) (image-data img)]
                  [(out) (make-bytevector (fx* 4 (fx* width height)) 0)]
                  [(x0s x1s xws) (bilinear-axis width sw)]
                  [(y0s y1s yws) (bilinear-axis height sh)])
      (let ([stride (fx* 4 sw)])
        (do ([y 0 (fx+ y 1)]) ((fx=? y height))
          (let ([r0 (fx* stride (vector-ref y0s y))]
                [r1 (fx* stride (vector-ref y1s y))]
                [wy (vector-ref yws y)]
                [drow (fx* (fx* 4 width) y)])
            (do ([x 0 (fx+ x 1)]) ((fx=? x width))
              (let* ([c0 (fx* 4 (vector-ref x0s x))]
                     [c1 (fx* 4 (vector-ref x1s x))]
                     [wx (vector-ref xws x)]
                     [p00 (fx+ r0 c0)] [p01 (fx+ r0 c1)] [p10 (fx+ r1 c0)] [p11 (fx+ r1 c1)]
                     ;; Weights in 1/16384ths.
                     [w00 (fx* (fx- 128 wx) (fx- 128 wy))]
                     [w01 (fx* wx (fx- 128 wy))]
                     [w10 (fx* (fx- 128 wx) wy)]
                     [w11 (fx* wx wy)]
                     [a00 (fx* w00 (bytevector-u8-ref src (fx+ p00 3)))]
                     [a01 (fx* w01 (bytevector-u8-ref src (fx+ p01 3)))]
                     [a10 (fx* w10 (bytevector-u8-ref src (fx+ p10 3)))]
                     [a11 (fx* w11 (bytevector-u8-ref src (fx+ p11 3)))]
                     [asum (fx+ (fx+ a00 a01) (fx+ a10 a11))]
                     [o (fx+ drow (fx* 4 x))])
                (bytevector-u8-set! out (fx+ o 3) (fxarithmetic-shift-right (fx+ asum 8192) 14))
                (when (fx>? asum 0)
                  (let ([half (fxarithmetic-shift-right asum 1)])
                    (do ([c 0 (fx+ c 1)]) ((fx=? c 3))
                      (bytevector-u8-set! out (fx+ o c)
                        (fxdiv (fx+ (fx+ (fx+ (fx* a00 (bytevector-u8-ref src (fx+ p00 c)))
                                              (fx* a01 (bytevector-u8-ref src (fx+ p01 c))))
                                         (fx+ (fx* a10 (bytevector-u8-ref src (fx+ p10 c)))
                                              (fx* a11 (bytevector-u8-ref src (fx+ p11 c)))))
                                    half)
                               asum)))))))))
        (%make-image width height out)))))
