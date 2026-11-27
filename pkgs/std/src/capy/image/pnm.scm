;;; Netpbm images (PBM, PGM, PPM) for (capy image).
;;;
;;; Reading takes all six formats: P1 and P4 (bitmaps, 1 is black), P2 and P5 (gray), P3 and
;;; P6 (RGB), plain (ASCII) or raw (binary), with any maxval up to 65535, which is scaled to
;;; 0-255. Comments (# to the end of the line) may appear anywhere in the header.
;;;
;;; Writing makes P6 (`image->ppm`) or P5 (`image->pgm`, the luma), raw by default or plain
;;; with #:plain? #t. Netpbm has no alpha: it is dropped.

(library (capy image pnm)
  (export pnm->image image->ppm image->pgm pnm-bytes?)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (capy image base))

  (define (bad message . irritants)
    (apply image-format-error 'pnm->image message irritants))

  ;; Whether BV starts like a Netpbm file: P1 to P6 and whitespace.
  (define (pnm-bytes? bv)
    (and (bytevector? bv)
         (>= (bytevector-length bv) 3)
         (= (bytevector-u8-ref bv 0) 80)
         (<= 49 (bytevector-u8-ref bv 1) 54)
         (space? (bytevector-u8-ref bv 2))))

  (define (space? b) (or (= b 32) (= b 9) (= b 10) (= b 13) (= b 11) (= b 12)))
  (define (digit? b) (<= 48 b 57))

  ;; Skips whitespace and comments from POS; returns the next position.
  (define (skip bv pos)
    (let loop ([pos pos])
      (cond
        [(>= pos (bytevector-length bv)) pos]
        [(space? (bytevector-u8-ref bv pos)) (loop (+ pos 1))]
        [(= (bytevector-u8-ref bv pos) 35)
         (let eol ([pos pos])
           (if (or (>= pos (bytevector-length bv)) (= (bytevector-u8-ref bv pos) 10))
             (loop pos)
             (eol (+ pos 1))))]
        [else pos])))

  ;; The decimal number at POS (after whitespace): (values number next-position).
  (define (number bv pos what)
    (let ([pos (skip bv pos)])
      (unless (and (< pos (bytevector-length bv)) (digit? (bytevector-u8-ref bv pos)))
        (bad "expected a number" what))
      (let loop ([pos pos] [n 0])
        (if (and (< pos (bytevector-length bv)) (digit? (bytevector-u8-ref bv pos)))
          (loop (+ pos 1) (+ (* n 10) (- (bytevector-u8-ref bv pos) 48)))
          (values n pos)))))

  ;; The image of the Netpbm file BV.
  (define (pnm->image bv)
    (unless (pnm-bytes? bv) (bad "not a PBM, PGM or PPM file"))
    (let ([kind (- (bytevector-u8-ref bv 1) 48)])
      (let*-values ([(w pos) (number bv 2 'width)]
                    [(h pos) (number bv pos 'height)]
                    [(maxval pos) (if (memv kind '(1 4)) (values 1 pos) (number bv pos 'maxval))])
        (when (or (= w 0) (= h 0) (> (* w h) #x10000000)) (bad "invalid or too large image size" w h))
        (unless (<= 1 maxval 65535) (bad "maxval must be from 1 to 65535" maxval))
        (let* ([img (make-image w h)]
               [out (image-data img)]
               [n (* w h)]
               [channels (case kind [(3 6) 3] [else 1])]
               [scale (lambda (v)
                        (when (> v maxval) (bad "a sample is larger than maxval" v maxval))
                        (if (= maxval 255) v (div (+ (* v 255) (div maxval 2)) maxval)))])
          (define (put! i c v)
            ;; Sample C of pixel I: gray samples go to all three channels.
            (let ([o (* 4 i)])
              (if (= channels 1)
                (begin (bytevector-u8-set! out o v) (bytevector-u8-set! out (+ o 1) v)
                       (bytevector-u8-set! out (+ o 2) v))
                (bytevector-u8-set! out (+ o c) v))
              (bytevector-u8-set! out (+ o 3) 255)))
          (case kind
            [(1)
             ;; One character per pixel; whitespace between them is optional.
             (let loop ([i 0] [pos pos])
               (when (< i n)
                 (let ([pos (skip bv pos)])
                   (unless (and (< pos (bytevector-length bv)) (memv (bytevector-u8-ref bv pos) '(48 49)))
                     (bad "expected 0 or 1 in a plain PBM"))
                   (put! i 0 (if (= (bytevector-u8-ref bv pos) 49) 0 255))
                   (loop (+ i 1) (+ pos 1)))))]
            [(2 3)
             (let loop ([k 0] [pos pos])
               (when (< k (* n channels))
                 (let-values ([(v pos) (number bv pos 'sample)])
                   (put! (div k channels) (mod k channels) (scale v))
                   (loop (+ k 1) pos))))]
            [(4)
             (let* ([start (+ pos 1)] [rowbytes (div (+ w 7) 8)])
               (when (> (+ start (* rowbytes h)) (bytevector-length bv)) (bad "the raster is too short"))
               (do ([y 0 (+ y 1)]) ((= y h))
                 (do ([x 0 (+ x 1)]) ((= x w))
                   (let ([byte (bytevector-u8-ref bv (+ start (* y rowbytes) (div x 8)))])
                     (put! (+ x (* y w)) 0
                           (if (= 0 (bitwise-and byte (bitwise-arithmetic-shift-right 128 (mod x 8)))) 255 0))))))]
            [else
             (let* ([start (+ pos 1)] [size (if (< maxval 256) 1 2)] [count (* n channels)])
               (when (> (+ start (* size count)) (bytevector-length bv)) (bad "the raster is too short"))
               (if (and (= size 1) (= maxval 255) (= channels 3))
                 (do ([i 0 (fx+ i 1)]) ((fx=? i n))
                   (bytevector-copy! bv (fx+ start (fx* 3 i)) out (fx* 4 i) 3)
                   (bytevector-u8-set! out (fx+ 3 (fx* 4 i)) 255))
                 (do ([k 0 (+ k 1)]) ((= k count))
                   (put! (div k channels) (mod k channels)
                         (scale (if (= size 1)
                                  (bytevector-u8-ref bv (+ start k))
                                  (bytevector-u16-ref bv (+ start (* 2 k)) (endianness big))))))))])
          img))))

  (define (header magic w h maxval?)
    (string->utf8 (string-append magic "\n" (number->string w) " " (number->string h) "\n"
                                 (if maxval? "255\n" ""))))

  (define (plain-body samples per-line)
    ;; Decimal samples separated by spaces, PER-LINE to a line (lines stay under 70 chars).
    (let-values ([(port get) (open-string-output-port)])
      (let ([n (bytevector-length samples)])
        (do ([i 0 (+ i 1)]) ((= i n))
          (put-string port (number->string (bytevector-u8-ref samples i)))
          (put-char port (if (or (= (mod (+ i 1) per-line) 0) (= i (- n 1))) #\newline #\space))))
      (string->utf8 (get))))

  (define (concat a b)
    (let ([out (make-bytevector (+ (bytevector-length a) (bytevector-length b)))])
      (bytevector-copy! a 0 out 0 (bytevector-length a))
      (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
      out))

  ;; IMG as a PPM file: P6, or P3 with #:plain? #t.
  (define (image->ppm img #:plain? [plain? #f])
    (unless (image? img) (assertion-violation 'image->ppm "expected an image" img))
    (let ([rgb (image->bytevector img #:channels 3)])
      (concat (header (if plain? "P3" "P6") (image-width img) (image-height img) #t)
              (if plain? (plain-body rgb 15) rgb))))

  ;; IMG as a PGM file of its luma: P5, or P2 with #:plain? #t.
  (define (image->pgm img #:plain? [plain? #f])
    (unless (image? img) (assertion-violation 'image->pgm "expected an image" img))
    (let ([gray (image->bytevector img #:channels 1)])
      (concat (header (if plain? "P2" "P5") (image-width img) (image-height img) #t)
              (if plain? (plain-body gray 17) gray)))))
