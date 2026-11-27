;;; Images: an RGBA8 image type, pixel operations, and PNG, BMP and Netpbm files.
;;;
;;;   (define img (make-image 320 200 #:color #x000000FF))       ; opaque black
;;;   (image-fill! img #xFF0000FF #:x 10 #:y 10 #:width 100 #:height 50)
;;;   (image-set! img 0 0 (rgba 255 255 0))
;;;   (image-ref img 0 0)                                        ; => #xFFFF00FF
;;;   (write-image img "out.png")                                ; the format from the name
;;;
;;;   (define photo (read-image "photo.png"))                    ; PNG, BMP, PPM, PGM, PBM
;;;   (define thumb (image-scale photo 64 64))                   ; bilinear; #:filter 'nearest
;;;   (image-blit! img thumb #:x 8 #:y 8 #:blend? #t)            ; alpha-composited
;;;   (image-crop photo 0 0 32 32)
;;;
;;; An image is width x height pixels of 8-bit RGBA, row by row from the top left, in one
;;; bytevector, `image-data`: pixel (x, y) is the 4 bytes at 4 (y width + x), red first,
;;; straight (not premultiplied) alpha. `bytevector->image` wraps packed pixels (gray, gray and
;;; alpha, RGB or RGBA), and `image->bytevector` packs them again, for handing to other code
;;; such as a texture upload.
;;;
;;; A color is an exact integer #xRRGGBBAA; `rgba` builds one and `color-red` ...
;;; `color-alpha` take it apart. `image-rgba` and `image-set-rgba!` work on the four parts.
;;;
;;; Files: `decode-image` takes the bytes of any supported file and `encode-image` makes one;
;;; `read-image` and `write-image` do the same with a path. Format-specific procedures:
;;; `png->image`, `image->png` (#:level, #:color-type, #:filter), `bmp->image`, `image->bmp`,
;;; `pnm->image`, `image->ppm` and `image->pgm` (#:plain?). A malformed file raises a
;;; condition that satisfies `image-format-error?`.

(library (capy image)
  (export make-image image? image-width image-height image-data
    image-copy bytevector->image image->bytevector
    rgba color-red color-green color-blue color-alpha
    image-ref image-set! image-rgba image-set-rgba!
    image-fill! image-blit! image-crop image-scale image-flip
    png->image image->png bmp->image image->bmp pnm->image image->ppm image->pgm
    decode-image encode-image read-image write-image
    &image-format-error image-format-error?)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (capy image base)
    (capy image png)
    (capy image pnm)
    (capy image bmp))

  ;; The image of the file bytes BV, of any supported format, recognized by its first bytes.
  (define (decode-image bv)
    (unless (bytevector? bv) (assertion-violation 'decode-image "expected a bytevector" bv))
    (cond
      [(png-bytes? bv) (png->image bv)]
      [(bmp-bytes? bv) (bmp->image bv)]
      [(pnm-bytes? bv) (pnm->image bv)]
      [else (image-format-error 'decode-image "not a PNG, BMP or Netpbm image")]))

  ;; IMG as the bytes of a FORMAT file: png, bmp, ppm or pgm. #:level is the PNG level.
  (define (encode-image img format #:level [level 6])
    (case format
      [(png) (image->png img #:level level)]
      [(bmp) (image->bmp img)]
      [(ppm) (image->ppm img)]
      [(pgm) (image->pgm img)]
      [else (assertion-violation 'encode-image "the format must be png, bmp, ppm or pgm" format)]))

  (define (read-file path)
    (call-with-port (open-file-input-port path) get-bytevector-all))

  ;; The image in the file at PATH.
  (define (read-image path)
    (let ([bv (read-file path)])
      (if (eof-object? bv)
        (image-format-error 'read-image "the file is empty" path)
        (decode-image bv))))

  (define (extension path)
    (let loop ([i (- (string-length path) 1)])
      (cond
        [(< i 0) ""]
        [(char=? (string-ref path i) #\.) (string-downcase (substring path (+ i 1) (string-length path)))]
        [(char=? (string-ref path i) #\/) ""]
        [else (loop (- i 1))])))

  ;; Writes IMG to the file at PATH, replacing it. #:format is png, bmp, ppm or pgm; by default
  ;; it comes from the extension (.png, .bmp, .ppm, .pgm, .pnm).
  ;;
  ;;   (write-image img "frame.bmp")
  ;;   (write-image img "out" #:format 'png #:level 9)
  (define (write-image img path #:format [format #f] #:level [level 6])
    (let ([format (or format
                      (let ([ext (extension path)])
                        (cond
                          [(string=? ext "png") 'png]
                          [(string=? ext "bmp") 'bmp]
                          [(member ext '("ppm" "pnm")) 'ppm]
                          [(string=? ext "pgm") 'pgm]
                          [else (assertion-violation 'write-image
                                  "cannot tell the format from the file name: pass #:format" path)])))])
      (let ([bytes (encode-image img format #:level level)])
        (when (file-exists? path) (delete-file path))
        (call-with-port (open-file-output-port path)
          (lambda (port) (put-bytevector port bytes)))))))
