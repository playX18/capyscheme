;;; A line index over a string: conversions between character offsets, byte offsets (of the
;;; UTF-8 text) and (line, character) positions, where `character` counts in the units of a
;;; position encoding: 'utf-8 (bytes), 'utf-16 (code units, a character above U+FFFF counts 2)
;;; or 'utf-32 (code points). These are the encodings of the language server protocol.
;;;
;;;   (define idx (make-line-index "a\r\nb😀c"))
;;;   (line-index-line-count idx)                 => 2
;;;   (line-index-offset->position idx 5 'utf-16) => 1, 3      (two values: line, character)
;;;   (line-index-position->offset idx 1 4 'utf-16) => 5
;;;
;;; Lines are numbered from 0 and end at `\n`, `\r\n` or a lone `\r`; the text after the last
;;; terminator is a line, possibly empty. The table of line starts makes each query a binary
;;; search plus a scan of one line (O(1) for 'utf-32 positions). Positions out of range are
;;; clamped: a line past the end is the last line, a character past the end of a line is the
;;; end of the line (before its terminator), and one that falls inside a character (the middle
;;; of a surrogate pair or of a multi-byte sequence) rounds down to the start of that character.
;;;
;;; An index is immutable; `line-index-update` makes the index of the text after a replacement by
;;; re-scanning only the lines the replacement touches (the tables are copied, the text is
;;; spliced, so an edit costs a memory copy but no per-character work over the whole file).

(library (capy text line-index)
  (export make-line-index
    line-index?
    line-index-text
    line-index-length
    line-index-byte-length
    line-index-line-count
    line-index-line-start
    line-index-line-end
    line-index-offset->line
    line-index-offset->position
    line-index-position->offset
    line-index-offset->byte
    line-index-byte->offset
    line-index-update
    position-encoding?)
  (import (rnrs))

  (define-record-type (<line-index> %make-line-index line-index?)
    (fields
      (immutable text line-index-text)
      (immutable starts %starts) ; char offset of every line start
      (immutable bstarts %bstarts) ; byte offset of every line start
      (immutable bytes line-index-byte-length)))

  (define (position-encoding? x)
    (and (memq x '(utf-8 utf-16 utf-32)) #t))

  (define (check-encoding who enc)
    (unless (position-encoding? enc)
      (assertion-violation who "not a position encoding (utf-8, utf-16 or utf-32)" enc)))

  (define (utf8-units ch)
    (let ([n (char->integer ch)])
      (cond [(<= n #x7F) 1] [(<= n #x7FF) 2] [(<= n #xFFFF) 3] [else 4])))

  (define (char-units ch enc)
    (case enc
      [(utf-8) (utf8-units ch)]
      [(utf-16) (if (> (char->integer ch) #xFFFF) 2 1)]
      [else 1]))

  ;; The line starts p of TEXT with FROM < p <= LIMIT, as two lists (character and byte offsets),
  ;; in order. BYTES is the byte offset of FROM. A start follows `\n`, and `\r` unless a `\n`
  ;; comes next.
  (define (scan-starts text from bytes limit)
    (let ([len (string-length text)])
      (let loop ([i from] [b bytes] [cs '()] [bs '()])
        (if (>= i limit)
          (values (reverse cs) (reverse bs))
          (let* ([c (string-ref text i)]
                 [b (+ b (utf8-units c))])
            (if (or (char=? c #\newline)
                  (and (char=? c #\return)
                    (or (= (+ i 1) len) (not (char=? (string-ref text (+ i 1)) #\newline)))))
              (loop (+ i 1) b (cons (+ i 1) cs) (cons b bs))
              (loop (+ i 1) b cs bs)))))))

  (define (utf8-length-of text from to)
    (let loop ([i from] [b 0])
      (if (>= i to) b (loop (+ i 1) (+ b (utf8-units (string-ref text i)))))))

  (define (make-line-index text)
    (let ([len (string-length text)])
      (let-values ([(cs bs) (scan-starts text 0 0 len)])
        (%make-line-index text
          (list->vector (cons 0 cs))
          (list->vector (cons 0 bs))
          (utf8-length-of text 0 len)))))

  (define (line-index-length idx) (string-length (line-index-text idx)))

  (define (line-index-line-count idx) (vector-length (%starts idx)))

  ;; The index of the last element of the sorted vector V that is <= X (V starts with 0, X >= 0).
  (define (floor-search v x)
    (let loop ([lo 0] [hi (- (vector-length v) 1)])
      (if (>= lo hi)
        lo
        (let ([mid (quotient (+ lo hi 1) 2)])
          (if (<= (vector-ref v mid) x) (loop mid hi) (loop lo (- mid 1)))))))

  (define (clamp-line idx line)
    (max 0 (min line (- (line-index-line-count idx) 1))))

  ;; The offset where LINE starts (the last line for a LINE past the end).
  (define (line-index-line-start idx line)
    (vector-ref (%starts idx) (clamp-line idx line)))

  ;; The offset where LINE ends, before its terminator.
  (define (line-index-line-end idx line)
    (let* ([line (clamp-line idx line)]
           [text (line-index-text idx)]
           [start (vector-ref (%starts idx) line)])
      (if (= (+ line 1) (line-index-line-count idx))
        (string-length text)
        (let* ([next (vector-ref (%starts idx) (+ line 1))]
               [last (string-ref text (- next 1))])
          (cond
            [(and (char=? last #\newline) (> (- next 1) start)
               (char=? (string-ref text (- next 2)) #\return))
              (- next 2)]
            [(or (char=? last #\newline) (char=? last #\return)) (- next 1)]
            [else next])))))

  (define (clamp-offset idx off)
    (max 0 (min off (line-index-length idx))))

  ;; The line that holds character offset OFF (clamped to the text).
  (define (line-index-offset->line idx off)
    (floor-search (%starts idx) (clamp-offset idx off)))

  ;; Whether LINE (a valid line) has only ASCII characters: as many bytes as characters, so a
  ;; column is the same count in every encoding.
  (define (ascii-line? idx line)
    (let ([starts (%starts idx)] [bstarts (%bstarts idx)])
      (if (= (+ line 1) (vector-length starts))
        (= (- (line-index-byte-length idx) (vector-ref bstarts line))
           (- (string-length (line-index-text idx)) (vector-ref starts line)))
        (= (- (vector-ref bstarts (+ line 1)) (vector-ref bstarts line))
           (- (vector-ref starts (+ line 1)) (vector-ref starts line))))))

  ;; Two values: the line and the character (in ENC units) of offset OFF.
  (define (line-index-offset->position idx off enc)
    (check-encoding 'line-index-offset->position enc)
    (let* ([off (clamp-offset idx off)]
           [line (floor-search (%starts idx) off)]
           [start (vector-ref (%starts idx) line)])
      (values line
        (if (or (eq? enc 'utf-32) (ascii-line? idx line))
          (- off start)
          (let ([text (line-index-text idx)])
            (let loop ([i start] [n 0])
              (if (>= i off) n (loop (+ i 1) (+ n (char-units (string-ref text i) enc))))))))))

  ;; The character offset of (LINE, CHARACTER in ENC units), clamped as described above.
  (define (line-index-position->offset idx line character enc)
    (check-encoding 'line-index-position->offset enc)
    (let* ([line (clamp-line idx line)]
           [start (vector-ref (%starts idx) line)]
           [end (line-index-line-end idx line)]
           [character (max 0 character)])
      (if (or (eq? enc 'utf-32) (ascii-line? idx line))
        (min end (+ start character))
        (let ([text (line-index-text idx)])
          (let loop ([i start] [n 0])
            (if (>= i end)
              end
              (let ([n (+ n (char-units (string-ref text i) enc))])
                (if (> n character) i (loop (+ i 1) n)))))))))

  ;; The byte offset (in the UTF-8 encoding of the text) of character offset OFF.
  (define (line-index-offset->byte idx off)
    (let* ([off (clamp-offset idx off)]
           [line (floor-search (%starts idx) off)]
           [start (vector-ref (%starts idx) line)])
      (+ (vector-ref (%bstarts idx) line)
        (utf8-length-of (line-index-text idx) start off))))

  ;; The character offset of byte offset BYTE; inside a multi-byte character it is the start of
  ;; that character.
  (define (line-index-byte->offset idx byte)
    (let* ([byte (max 0 (min byte (line-index-byte-length idx)))]
           [line (floor-search (%bstarts idx) byte)]
           [text (line-index-text idx)]
           [len (string-length text)])
      (let loop ([i (vector-ref (%starts idx) line)] [b (vector-ref (%bstarts idx) line)])
        (if (>= i len)
          len
          (let ([b (+ b (utf8-units (string-ref text i)))])
            (if (> b byte) i (loop (+ i 1) b)))))))

  ;; The index of the text with the characters [START, END) replaced by NEW-TEXT. Only the
  ;; lines around the replacement are scanned again: the line tables before them are kept and
  ;; the ones after them are shifted.
  (define (line-index-update idx start end new-text)
    (let* ([old (line-index-text idx)]
           [start (clamp-offset idx start)]
           [end (max start (clamp-offset idx end))]
           [text (string-append (substring old 0 start) new-text
                  (substring old end (string-length old)))]
           [new-end (+ start (string-length new-text))]
           [delta (- (string-length new-text) (- end start))]
           [bdelta (- (utf8-length-of new-text 0 (string-length new-text))
                    (utf8-length-of old start end))]
           [starts (%starts idx)]
           [bstarts (%bstarts idx)]
           ;; A `\r` just before START may now pair with a `\n`, so rescan from its line.
           [first (floor-search starts (max 0 (- start 1)))]
           [after (+ (floor-search starts end) 1)]
           [count (vector-length starts)])
      (let-values ([(cs bs) (scan-starts text (vector-ref starts first)
                             (vector-ref bstarts first) new-end)])
        (let* ([middle (length cs)]
               [n (+ first 1 middle (- count after))]
               [new-starts (make-vector n 0)]
               [new-bstarts (make-vector n 0)])
          (do ([i 0 (+ i 1)]) ((> i first))
            (vector-set! new-starts i (vector-ref starts i))
            (vector-set! new-bstarts i (vector-ref bstarts i)))
          (let loop ([i (+ first 1)] [cs cs] [bs bs])
            (unless (null? cs)
              (vector-set! new-starts i (car cs))
              (vector-set! new-bstarts i (car bs))
              (loop (+ i 1) (cdr cs) (cdr bs))))
          (do ([i after (+ i 1)] [j (+ first 1 middle) (+ j 1)]) ((>= i count))
            (vector-set! new-starts j (+ (vector-ref starts i) delta))
            (vector-set! new-bstarts j (+ (vector-ref bstarts i) bdelta)))
          (%make-line-index text new-starts new-bstarts
            (+ (line-index-byte-length idx) bdelta))))))
  )
