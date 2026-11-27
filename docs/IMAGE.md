# Images

`(capy image)` is a simple RGBA image type with pixel operations, plus reading and writing PNG,
BMP and Netpbm (PBM, PGM, PPM) files. It is written in Scheme. PNG data goes through
[`(capy compress)`](COMPRESS.md).

```scheme
(import (rnrs) (capy image))

(define img (make-image 320 200 #:color #x000000FF))          ; opaque black
(image-fill! img #xFF0000FF #:x 10 #:y 10 #:width 100 #:height 50)
(image-set! img 0 0 (rgba 255 255 0))
(image-ref img 0 0)                                           ; => #xFFFF00FF
(write-image img "out.png")                                   ; the format from the name

(define photo (read-image "photo.png"))                       ; PNG, BMP, PPM, PGM or PBM
(image-blit! img (image-scale photo 64 64) #:x 8 #:y 8 #:blend? #t)
(write-image (image-crop photo 0 0 32 32) "corner.bmp")
```

## The image type

An image has a width, a height and one bytevector, `image-data`, of 8-bit RGBA pixels. Rows run
from the top, and each row runs from the left. Pixel (x, y) is the 4 bytes at `4 (y width + x)`,
red first. The alpha is straight (not premultiplied). The bytevector is the image itself, not a
copy, so it can go straight to code that wants RGBA8, such as a texture upload. Code that loops
over pixels is fastest working on it directly.

A color is an exact integer `#xRRGGBBAA`, as in CSS: `#xFF0000FF` is opaque red, and `0` is
transparent black.

| Procedure | Result |
|-----------|--------|
| `(make-image width height #:color)` | A new image, transparent black unless `#:color` is given. |
| `(image? x)`, `(image-width img)`, `(image-height img)`, `(image-data img)` | |
| `(image-copy img)` | A copy with its own data. |
| `(bytevector->image width height bytes #:channels #:copy?)` | An image of packed pixels: 1 (gray), 2 (gray, alpha), 3 (RGB) or 4 (RGBA, the default) bytes each. With 4, `bytes` becomes the image's data unless `#:copy? #t`. |
| `(image->bytevector img #:channels)` | The pixels packed again. Gray is the Rec. 601 luma. |
| `(rgba r g b #:alpha)` | A color from its parts, 0–255. The alpha defaults to 255. |
| `(color-red c)`, `(color-green c)`, `(color-blue c)`, `(color-alpha c)` | The parts of a color. |
| `(image-ref img x y)`, `(image-set! img x y color)` | One pixel, as a color. |
| `(image-rgba img x y)`, `(image-set-rgba! img x y r g b #:alpha)` | One pixel, as four values. |
| `(image-fill! img color #:x #:y #:width #:height)` | Fills a rectangle (by default, the whole image). It is clipped to the image. |
| `(image-blit! dst src #:x #:y #:src-x #:src-y #:width #:height #:blend?)` | Copies `src`, or a rectangle of it, into `dst` with its top left corner at (`#:x`, `#:y`). It is clipped to both images. `#:blend? #t` composites by alpha (Porter–Duff "over") instead of replacing. |
| `(image-crop img x y width height)` | A new image of the rectangle, which must lie inside `img`. |
| `(image-flip img #:direction)` | A mirror image: `horizontal` (the default) or `vertical`. |
| `(image-scale img width height #:filter)` | A resized copy. `#:filter` is `bilinear` (the default) or `nearest`. Bilinear weights each neighbor by its alpha, so the color of a transparent pixel does not bleed. |

Pixel coordinates outside the image, bad colors and bad sizes are assertion violations.

## Files

| Procedure | Result |
|-----------|--------|
| `(read-image path)` | The image in the file. The format comes from its first bytes. |
| `(write-image img path #:format #:level)` | Writes the file, replacing it. The format comes from the extension (`.png`, `.bmp`, `.ppm`, `.pnm`, `.pgm`) unless `#:format` is `png`, `bmp`, `ppm` or `pgm`. `#:level` is the PNG compression level. |
| `(decode-image bytes)`, `(encode-image img format #:level)` | The same, with bytevectors. |
| `(png->image bytes)`, `(image->png img #:level #:color-type #:filter)` | PNG. |
| `(bmp->image bytes)`, `(image->bmp img)` | BMP. |
| `(pnm->image bytes)`, `(image->ppm img #:plain?)`, `(image->pgm img #:plain?)` | PBM, PGM and PPM. |

A file that is not valid for its format raises a condition that satisfies
`image-format-error?`, a subtype of `&error`. A corrupt PNG data stream raises a
`compress-error?` from `(capy compress)`.

### PNG

The decoder reads every PNG:

- All color types (gray, RGB, palette, gray with alpha, RGBA) and all bit depths they allow
  (1, 2, 4, 8 and 16).
- The five filters and Adam7 interlacing.
- tRNS transparency. For gray and RGB images it is compared at the full sample depth.

16-bit samples keep their high byte, and samples narrower than 8 bits are scaled to 0–255.
Chunk CRCs are checked. Ancillary chunks other than tRNS (text, gamma, color profiles) are
skipped, and an unknown critical chunk is an error.

The encoder writes 8-bit, non-interlaced files:

- `#:color-type auto` (the default) picks the smallest of `gray`, `gray-alpha`, `rgb` and `rgba`
  that holds the image exactly. Naming a type forces it: gray from a color image is the luma,
  and a type without alpha drops it.
- `#:filter adaptive` (the default) gives each row the filter with the smallest sum of absolute
  residuals. `none`, `sub`, `up`, `average` and `paeth` force one filter.
- `#:level` is the zlib level, 0–9 (default 6).

### BMP

The reader handles:

- The OS/2 core header and the Windows headers (`BITMAPINFOHEADER` and V2–V5).
- 1, 4 and 8 bits per pixel with a palette, uncompressed or RLE8/RLE4.
- 16 and 32 bits, with the default layouts or `BI_BITFIELDS` masks, alpha included.
- 24 bits.
- Rows bottom-up or top-down.

A 32-bit file without an alpha mask is opaque. Pixels that RLE data skips stay transparent.

The writer makes a 24-bit file when every pixel is opaque. Otherwise it makes a 32-bit file
with a V4 header and an alpha mask.

### Netpbm

The reader takes P1–P6, plain (ASCII) and raw (binary), with comments anywhere in the header
and any maxval up to 65535, which is scaled to 0–255. In a bitmap, 1 is black. `image->ppm`
writes P6, or P3 with `#:plain? #t`. `image->pgm` writes the luma as P5, or P2. Netpbm has no
alpha, so it is dropped.

## Performance

For a 1024×1024 image on an Apple M5 Pro, with Tier 2 warmed up, PNG encoding takes about
120 ms (level 6, adaptive filters) and decoding about 33 ms. Writing a BMP takes 6 ms, and a
bilinear scale to 512×512 takes 26 ms.

## Tests

`pkgs/std/tests/capy/image.scm` decodes every PNG color type, bit depth and interlace mode,
built by an encoder in the test itself that does not use `(capy image)`. When `python3` with
PIL is installed, it also decodes PIL's PNG, BMP, PPM and PGM files and has PIL decode ours,
comparing every pixel.
