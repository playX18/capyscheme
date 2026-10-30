;;; R6RS textual input and output operations (rnrs io ports, 8.2.9 and 8.2.12)
;;; plus string ports, end-of-line processing, and UTF-8/UTF-16 decoding.
;;;
;;; Textual ports are binary ports carrying a transcoder.  Characters
;;; are decoded in place from the port's input buffer: peeking at a
;;; character never consumes bytes, so lookahead needs no push-back.
;;; Bulk operations (get-line, get-string-*, put-string) hand whole
;;; buffer runs to the native helpers in runtime/vm/portio.rs.
;;;
;;; Decoding errors are handled according to the transcoder's mode:
;;; 'ignore drops the offending bytes, 'replace delivers U+FFFD (kept in
;;; the port's pending-char field until read), 'raise signals an
;;; &i/o-decoding condition.  In every case the offending bytes (the
;;; maximal invalid subpart) are consumed when the error is detected.


;;; Creates a textual port over the binary port p, sharing p's
;;; underlying source and its buffers, then closes p without
;;; closing the underlying source (R6RS 8.2.6).
(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (late (boot port-api) current-input-port))
(|#%provide| all-defined)

(define (%ports/make-textual-port p t)
  (when (%ports/output-port? p)
    (%ports/flush-buffer p))
  (let* ([codec (%ports/codec-code (transcoder-codec t))]
         [input? (%ports/input-port? p)]
         [custom? (%port-custom? p)]
         [np (%make-port-tuple
               (%port-name p)
               (%port-direction p)
               t
               (%port-buffer-mode p)
               (%port-flush-datum? p)
               'open
               (%port-handler p)
               (%port-in-buf p)
               (%port-in-ptr p)
               (%port-in-lim p)
               (%port-out-buf p)
               (%port-out-lim p)
               0
               0
               0
               (if (eqv? codec 2) (vector 'utf16 #f #f) #f)
               '()
               #f
               (%port-fd p)
               custom?
               codec
               (transcoder-eol-style t)
               (transcoder-error-handling-mode t)
               (if (and input? (not custom?)) 4096 %ports/never)
               (%port-memory p)
               (%port-hw p)
               #f)])
    ;; close the original port without closing the shared handler
    (%port-state-set! p 'closed)
    (when (%port-fd p)
      (%ports/unregister-output! p)
      (when (%ports/output-port? np)
        (%ports/register-output! np)))
    np))

(define (transcoded-port p t)
  (if (and (port? p)
       (%ports/binary? p)
       (%ports/open? p)
       (%transcoder? t)
       (memq (transcoder-codec t) '(latin-1 utf-8 utf-16))
       (memq (transcoder-eol-style t) '(none lf cr crlf nel crnel ls))
       (memq (transcoder-error-handling-mode t) '(ignore replace raise))
       (if (%ports/input-output? p)
         (and (eq? (transcoder-codec t) 'latin-1)
           (eq? (transcoder-eol-style t) 'none))
         #t))
    (%ports/make-textual-port p t)
    (assertion-violation 'transcoded-port "illegal argument(s)" p t)))

;;; ---------------------------------------------------------------
;;; Decoding.
;;;
;;; The decoders look at the bytes starting `off` bytes past the input
;;; pointer, refilling as needed, and consume nothing.  They return
;;;   c >= 0       the code point
;;;   -1           end of stream
;;;   -(2 + n)     invalid input; n bytes make up the offending subpart
;;; ---------------------------------------------------------------
(define (%ports/decode-utf8-at p off)
  (if (%ports/ensure-input p (+ off 1))
    (let ([b (bytevector-u8-ref (%port-in-buf p) (+ (%port-in-ptr p) off))])
      (if (< b 128)
        b
        (let ([need (cond ((< b #xc2) 1)
                      ((< b #xe0) 2)
                      ((< b #xf0) 3)
                      ((< b #xf5) 4)
                      (else 1))])
          (%ports/ensure-input p (+ off need))
          (let* ([pos (+ (%port-in-ptr p) off)]
                 [lim (%port-in-lim p)]
                 [r (%utf8-decode-char (%port-in-buf p) pos lim)])
            (if (eq? r -1)
              ;; a valid prefix cut off by the end of the stream
              (- -2 (- lim pos))
              r)))))
    -1))

(define (%ports/utf16-state p)
  (or (%port-codec-state p)
    (let ([s (vector 'utf16 #f #f)])
      (%port-codec-state-set! p s)
      s)))

;;; Reads the 16-bit unit at byte offset k of the buffer.
(define (%ports/utf16-unit buf k st)
  (let ([b1 (bytevector-u8-ref buf k)]
        [b2 (bytevector-u8-ref buf (+ k 1))])
    (if (eq? (vector-ref st 1) 'little)
      (logior (ash b2 8) b1)
      (logior (ash b1 8) b2))))

(define (%ports/decode-utf16-at p off)
  (let ([st (%ports/utf16-state p)])
    ;; sniff the byte-order mark before the first unit
    (when (not (vector-ref st 1))
      (if (%ports/ensure-input p 2)
        (let* ([buf (%port-in-buf p)]
               [ptr (%port-in-ptr p)]
               [b1 (bytevector-u8-ref buf ptr)]
               [b2 (bytevector-u8-ref buf (+ ptr 1))])
          (cond ((and (= b1 #xfe) (= b2 #xff))
                 (vector-set! st 1 'big)
                 (%ports/advance! p 2))
            ((and (= b1 #xff) (= b2 #xfe))
              (vector-set! st 1 'little)
              (%ports/advance! p 2))
            (else
              (vector-set! st 1 'big))))
        (vector-set! st 1 'big)))
    (if (%ports/ensure-input p (+ off 2))
      (let* ([buf (%port-in-buf p)]
             [k (+ (%port-in-ptr p) off)]
             [u (%ports/utf16-unit buf k st)])
        (cond ((and (>= u #xd800) (<= u #xdbff))
               (if (%ports/ensure-input p (+ off 4))
                 (let* ([k (+ (%port-in-ptr p) off)]
                        [u2 (%ports/utf16-unit (%port-in-buf p) (+ k 2) st)])
                   (if (and (>= u2 #xdc00) (<= u2 #xdfff))
                     (+ #x10000 (logior (ash (- u #xd800) 10) (- u2 #xdc00)))
                     -4))
                 -4))
          ((and (>= u #xdc00) (<= u #xdfff)) -4)
          (else u)))
      (let ([avail (- (%port-in-lim p) (+ (%port-in-ptr p) off))])
        (if (<= avail 0) -1 (- -2 avail))))))

(define (%ports/decode-at p off)
  (let ([codec (%port-codec p)])
    (cond ((eq? codec 1) (%ports/decode-utf8-at p off))
      ((eq? codec 0)
        (if (%ports/ensure-input p (+ off 1))
          (bytevector-u8-ref (%port-in-buf p) (+ (%port-in-ptr p) off))
          -1))
      (else (%ports/decode-utf16-at p off)))))

;;; The number of bytes the encoding of code point c occupies.
(define (%ports/code-len p c)
  (let ([codec (%port-codec p)])
    (cond ((eq? codec 1)
           (cond ((< c #x80) 1)
             ((< c #x800) 2)
             ((< c #x10000) 3)
             (else 4)))
      ((eq? codec 0) 1)
      (else (if (> c #xffff) 4 2)))))

;;; Returns the next code point without consuming it, or -1 at the end
;;; of the stream, applying the error-handling mode to invalid input.

(define (%ports/peek-code p)
  (let ([pc (%port-pending-char p)])
    (or pc
      (let loop ()
        (let ([c (%ports/decode-at p 0)])
          (cond ((>= c 0) c)
            ((eq? c -1) -1)
            (else
              (%ports/advance! p (- -2 c))
              (let ([mode (%port-errmode p)])
                (cond ((eq? mode 'ignore) (loop))
                  ((eq? mode 'replace)
                    (%port-pending-char-set! p #xfffd)
                    #xfffd)
                  (else
                    (raise-i/o-decoding-error 'get-char "decoding error" p)))))))))))

;;; Consumes the code point c just returned by %ports/peek-code.

(define (%ports/consume-code! p c)
  (if (%port-pending-char p)
    (%port-pending-char-set! p #f)
    (%ports/advance! p (%ports/code-len p c))))

;;; Position bookkeeping after a character / line ending was consumed.

(define (%ports/note-char! p)
  (let ([n (+ (%port-char-pos p) 1)])
    (%port-char-pos-set! p n)
    (when (>= n (%port-anchor-next p))
      (%ports/record-anchor! p))))

(define (%ports/note-newline! p)
  (let ([n (+ (%port-char-pos p) 1)])
    (%port-char-pos-set! p n)
    (%port-lines-read-set! p (+ (%port-lines-read p) 1))
    (%port-line-start-set! p n)
    (when (>= n (%port-anchor-next p))
      (%ports/record-anchor! p))))

;;; General path of get-char / lookahead-char: end-of-line translation,
;;; error modes, and every codec.  If lookahead? is true nothing is
;;; consumed.
(define (%ports/read-char-slow p lookahead?)
  (let ([c (%ports/peek-code p)])
    (cond ((< c 0) (eof-object))
      ((eq? c 10)
        (unless lookahead?
          (%ports/consume-code! p c)
          (%ports/note-newline! p))
        #\newline)
      ((and (not (eq? (%port-eol p) 'none))
          (or (eq? c 13) (eq? c #x85) (eq? c #x2028)))
        (unless lookahead?
          (%ports/consume-code! p c)
          (when (eq? c 13)
            ;; carriage return followed by linefeed or NEL is one line ending
            (let ([c2 (%ports/decode-at p 0)])
              (when (or (eq? c2 10) (eq? c2 #x85))
                (%ports/advance! p (%ports/code-len p c2)))))
          (%ports/note-newline! p))
        #\newline)
      (else
        (unless lookahead?
          (%ports/consume-code! p c)
          (%ports/note-char! p))
        (integer->char c)))))

;;; Returns the next character (or eof).  If lookahead? is true, the
;;; port position is not advanced.  The common case, one ASCII byte of
;;; a UTF-8 input-only port, is handled inline.
(define (%ports/read-char p lookahead?)
  (when (eq? (%port-direction p) 'input-output)
    (%ports/flush-buffer p))
  (let ([ptr (%port-in-ptr p)])
    (if (and (eq? (%port-codec p) 1)
         (< ptr (%port-in-lim p))
         (not (%port-pending-char p)))
      (let ([b (bytevector-u8-ref (%port-in-buf p) ptr)])
        (if (and (< b 128) (not (eq? b 10)) (not (eq? b 13)))
          (begin
            (unless lookahead?
              (%port-in-ptr-set! p (+ ptr 1))
              (%ports/note-char! p))
            (integer->char b))
          (%ports/read-char-slow p lookahead?)))
      (%ports/read-char-slow p lookahead?))))

(define (get-char p)
  (if (%ports/textual-input? p)
    (%ports/read-char p #f)
    (assertion-violation 'get-char "not an open textual input port" p)))

(define (lookahead-char p)
  (if (%ports/textual-input? p)
    (%ports/read-char p #t)
    (assertion-violation 'lookahead-char "not an open textual input port" p)))

;;; ---------------------------------------------------------------
;;; Bulk input.
;;; ---------------------------------------------------------------

(define %ports/unlimited 1099511627776)

;;; True when whole buffer runs can be decoded by the native helper.
(define (%ports/chunkable? p)
  (and (eq? (%port-direction p) 'input)
    (or (eq? (%port-codec p) 1) (eq? (%port-codec p) 0))
    (not (%port-pending-char p))))

;;; Decodes a run of characters straight from the input buffer.
;;; Returns (values string chars newlines): the decoded text, how many
;;; characters it accounts for (newline-mode 2 consumes a linefeed that
;;; the text does not contain), and how many linefeeds were consumed.
;;; chars = 0 means no progress was possible and the caller must take
;;; the single-character path.
;;;
;;; nl-mode: 0 = run through linefeeds, 1 = stop after one (kept in the
;;; text), 2 = stop after one (dropped from the text).  stop-eol? makes
;;; the run end before characters needing end-of-line translation.
(define (%ports/chunk! p max-chars nl-mode stop-eol?)
  (call-with-values
    (lambda ()
      (%decode-string (%port-codec p)
        (%port-in-buf p)
        (%port-in-ptr p)
        (%port-in-lim p)
        max-chars
        nl-mode
        stop-eol?))
    (lambda (s bytes newlines tail)
      (if (= bytes 0)
        (values s 0 0)
        (let* ([chars (if (= nl-mode 2)
                       (+ (string-length s) newlines)
                       (string-length s))]
               [pos (+ (%port-char-pos p) chars)])
          (%ports/advance! p bytes)
          (%port-char-pos-set! p pos)
          (when (> newlines 0)
            (%port-lines-read-set! p (+ (%port-lines-read p) newlines))
            (%port-line-start-set! p (- pos tail)))
          (when (>= pos (%port-anchor-next p))
            (%ports/record-anchor! p))
          (values s chars newlines))))))

(define (%ports/string-concat parts)
  ;; parts is in reverse order
  (cond ((null? parts) "")
    ((null? (cdr parts)) (car parts))
    (else (apply string-append (reverse parts)))))

;;; Reads up to count characters (all remaining ones for #f).  Returns
;;; the text as a string, or the eof object if the stream was at its end
;;; and count > 0.
(define (%ports/read-text p count)
  (let ([limit (or count %ports/unlimited)]
        [stop-eol? (not (eq? (%port-eol p) 'none))])
    (let loop ([parts '()] [have 0])
      (if (>= have limit)
        (%ports/string-concat parts)
        (if (and (%ports/chunkable? p)
             (or (< (%port-in-ptr p) (%port-in-lim p))
               (%ports/ensure-input p 1)))
          (call-with-values
            (lambda () (%ports/chunk! p (- limit have) 0 stop-eol?))
            (lambda (s chars newlines)
              (if (> chars 0)
                (loop (cons s parts) (+ have chars))
                (let ([c (%ports/read-char p #f)])
                  (if (eof-object? c)
                    (if (= have 0) (eof-object) (%ports/string-concat parts))
                    (loop (cons (make-string 1 c) parts) (+ have 1)))))))
          (let ([c (%ports/read-char p #f)])
            (if (eof-object? c)
              (if (= have 0) (eof-object) (%ports/string-concat parts))
              (loop (cons (make-string 1 c) parts) (+ have 1)))))))))

(define (get-string-n! p s start count)
  (if (and (%ports/textual-input? p)
       (string? s)
       (fixnum? start)
       (fixnum? count)
       (<= 0 start)
       (<= 0 count)
       (<= (+ start count) (string-length s)))
    (if (= count 0)
      0
      (let ([text (%ports/read-text p count)])
        (if (eof-object? text)
          text
          (let ([n (string-length text)])
            (let loop ([i 0])
              (when (< i n)
                (string-set! s (+ start i) (string-ref text i))
                (loop (+ i 1))))
            n))))
    (assertion-violation 'get-string-n! "illegal arguments" p s start count)))

(define (get-string-n p count)
  (if (and (%ports/textual-input? p)
       (fixnum? count)
       (<= 0 count))
    (if (= count 0)
      ""
      (%ports/read-text p count))
    (assertion-violation 'get-string-n "illegal arguments" p count)))

(define (get-string-all p)
  (if (%ports/textual-input? p)
    (%ports/read-text p #f)
    (assertion-violation 'get-string-all "not an open textual input port" p)))

;;; Reads one line.  r7? selects R7RS read-line semantics, where a bare
;;; carriage return also ends a line even if the port does not translate
;;; line endings; for R6RS get-line only linefeed does (after whatever
;;; translation the transcoder applies).
(define (%ports/read-line p r7?)
  (let ([stop-eol? (or r7? (not (eq? (%port-eol p) 'none)))])
    (define (finish parts) (%ports/string-concat parts))
    (define (step parts any?)
      (let ([c (%ports/read-char p #f)])
        (cond ((eof-object? c)
               (if any? (finish parts) (eof-object)))
          ((char=? c #\newline) (finish parts))
          ((and r7? (char=? c #\return))
            (let ([c2 (%ports/read-char p #t)])
              (when (and (char? c2) (char=? c2 #\newline))
                (%ports/read-char p #f)))
            (finish parts))
          (else (loop (cons (make-string 1 c) parts) #t)))))
    (define (loop parts any?)
      (if (and (%ports/chunkable? p)
           (or (< (%port-in-ptr p) (%port-in-lim p))
             (%ports/ensure-input p 1)))
        (call-with-values
          (lambda () (%ports/chunk! p %ports/unlimited 2 stop-eol?))
          (lambda (s chars newlines)
            (cond ((> newlines 0) (finish (cons s parts)))
              ((> chars 0) (loop (cons s parts) #t))
              (else (step parts any?)))))
        (step parts any?)))
    (loop '() #f)))

;;; R6RS get-line: reads through the linefeed and returns the text
;;; without it.
(define (get-line p)
  (if (%ports/textual-input? p)
    (%ports/read-line p #f)
    (assertion-violation 'get-line "not an open textual input port" p)))

;;; R7RS read-line: an end of line is a linefeed, a carriage return,
;;; or a carriage return followed by a linefeed.  The end-of-line
;;; sequence is not included in the result.
(define (read-line . rest)
  (define p (if (null? rest) (current-input-port) (car rest)))
  (if (%ports/textual-input? p)
    (%ports/read-line p #t)
    (assertion-violation 'read-line "not a textual input port" p)))

;;; ---------------------------------------------------------------
;;; Output.
;;; ---------------------------------------------------------------

;;; Encodes #\newline according to the port's eol style.
(define (%ports/put-eol p codec eol)
  (case eol
    ((none lf)
      (%ports/put-byte p 10))
    ((cr)
      (%ports/put-byte p 13))
    ((crlf)
      (%ports/put-byte p 13)
      (%ports/put-byte p 10))
    ((nel)
      (if (eqv? codec 0)
        (%ports/put-byte p #x85)
        (begin
          (%ports/put-byte p #xc2)
          (%ports/put-byte p #x85))))
    ((crnel)
      (if (eqv? codec 0)
        (begin
          (%ports/put-byte p 13)
          (%ports/put-byte p #x85))
        (begin
          (%ports/put-byte p 13)
          (%ports/put-byte p #xc2)
          (%ports/put-byte p #x85))))
    ((ls)
      (if (eqv? codec 0)
        (assertion-violation 'put-char
          "cannot encode line separator as latin-1" p)
        (begin
          (%ports/put-byte p #xe2)
          (%ports/put-byte p #x80)
          (%ports/put-byte p #xa8))))
    (else
      (assertion-violation 'put-char "internal error" p))))

;;; Encodes one character as UTF-16, writing a byte-order mark before
;;; the first character of a stream (unless the stream is a non-empty
;;; file being appended to).  Byte order is big-endian unless the port's
;;; codec state says otherwise.
(define (%ports/put-char-utf16 p c)
  (let* ([st (%ports/utf16-state p)]
         [little? (eq? (vector-ref st 1) 'little)])
    (define (put-unit u)
      (if little?
        (begin
          (%ports/put-byte p (logand u #xff))
          (%ports/put-byte p (ash u -8)))
        (begin
          (%ports/put-byte p (ash u -8))
          (%ports/put-byte p (logand u #xff)))))
    (when (not (vector-ref st 2))
      (let ([g (%ports/underlying-position p)])
        (when (and (or (not g) (zero? g)) (zero? (%port-out-lim p)))
          (put-unit #xfeff)))
      (vector-set! st 2 #t))
    (let ([sv (char->integer c)])
      (if (<= sv #xffff)
        (put-unit sv)
        (let ([x (- sv #x10000)])
          (put-unit (+ #xd800 (ash x -10)))
          (put-unit (+ #xdc00 (logand x #x3ff))))))
    (unspecified)))

;;; Writes one character to an open textual output port.
(define (%ports/put-char* p c)
  (when (eq? (%port-direction p) 'input-output)
    (%ports/prepare-write! p))
  (let ([codec (%port-codec p)]
        [sv (char->integer c)])
    (cond
      ((eq? sv 10)
       (%ports/put-eol p codec (%port-eol p))
       (let ([n (+ (%port-char-pos p) 1)])
         (%port-char-pos-set! p n)
         (%port-lines-read-set! p (+ (%port-lines-read p) 1))
         (%port-line-start-set! p n))
       (%ports/after-write! p #t))
      ((eq? codec 1)
       (%ports/reserve! p 4)
       (let ([lim (%port-out-lim p)])
         (if (< sv 128)
           (begin
             (bytevector-u8-set! (%port-out-buf p) lim sv)
             (%port-out-lim-set! p (+ lim 1)))
           (%port-out-lim-set! p (%utf8-encode-char! (%port-out-buf p) lim c))))
       (%port-char-pos-set! p (+ (%port-char-pos p) 1))
       (%ports/after-write! p #f))
      ((eq? codec 0)
       (if (<= sv #xff)
         (begin
           (%ports/put-byte p sv)
           (%port-char-pos-set! p (+ (%port-char-pos p) 1)))
         (let ([mode (%port-errmode p)])
           (cond ((eq? mode 'ignore) (unspecified))
             ((eq? mode 'replace)
               (%ports/put-byte p 63)
               (%port-char-pos-set! p (+ (%port-char-pos p) 1)))
             (else
               (raise-i/o-encoding-error 'put-char "encoding error" p c))))))
      (else
        (%ports/put-char-utf16 p c)
        (%port-char-pos-set! p (+ (%port-char-pos p) 1)))))
  (unspecified))

(define (put-char p c)
  (if (and (%ports/textual-output? p) (char? c))
    (%ports/put-char* p c)
    (assertion-violation 'put-char "not an open textual output port or invalid char" p c)))

;;; Writes s[start, end) to an open textual output port.
(define (%ports/put-string* p s start end)
  (let ([codec (%port-codec p)]
        [eol (%port-eol p)])
    (if (and (or (eq? codec 1) (eq? codec 0))
         (or (eq? eol 'none) (eq? eol 'lf)))
      (let ([saw-newline? #f]
            [memory? (%port-memory p)])
        (when (eq? (%port-direction p) 'input-output)
          (%ports/prepare-write! p))
        (let loop ([i start])
          (when (< i end)
            (%ports/reserve! p (if memory? (+ 4 (- end i)) 4))
            (call-with-values
              (lambda ()
                (%encode-string! codec (%port-out-buf p) (%port-out-lim p) s i end))
              (lambda (new-lim next newlines tail)
                (cond ((> next i)
                       (%port-out-lim-set! p new-lim)
                       (let ([pos (+ (%port-char-pos p) (- next i))])
                         (%port-char-pos-set! p pos)
                         (when (> newlines 0)
                           (set! saw-newline? #t)
                           (%port-lines-read-set! p (+ (%port-lines-read p) newlines))
                           (%port-line-start-set! p (- pos tail))))
                       (loop next))
                  (else
                    ;; a character the codec cannot encode: let the
                    ;; single-character path apply the error mode
                    (%ports/put-char* p (string-ref s i))
                    (loop (+ i 1))))))))
        (when (> end start)
          (%ports/after-write! p saw-newline?)))
      (let loop ([i start])
        (when (< i end)
          (%ports/put-char* p (string-ref s i))
          (loop (+ i 1))))))
  (unspecified))

(define (put-string p s . rest)
  (define (put p s start count)
    (if (and (%ports/textual-output? p)
         (string? s)
         (fixnum? start)
         (fixnum? count)
         (<= 0 start)
         (<= 0 count)
         (<= (+ start count) (string-length s)))
      (%ports/put-string* p s start (+ start count))
      (assertion-violation 'put-string "illegal arguments" p s start count)))
  (cond ((null? rest)
         (put p s 0 (if (string? s) (string-length s) 0)))
    ((null? (cdr rest))
      (put p s (car rest) (- (string-length s) (car rest))))
    ((null? (cddr rest))
      (put p s (car rest) (cadr rest)))
    (else
      (assertion-violation 'put-string "too many arguments" p s rest))))

;;; String ports.
;;;
;;; A string input port is a UTF-8 memory port over the encoded string;
;;; a string output port is a UTF-8 memory port that accumulates the
;;; encoded text, and get-output-string decodes it in one step.

(define %ports/string-transcoder
  (make-transcoder (utf-8-codec) 'none 'replace))

(define (string-output-port? p)
  (and (port? p) (eq? (%port-memory p) 'sout)))

(define (open-input-string s)
  (if (not (string? s))
    (assertion-violation 'open-input-string "illegal argument" s))
  (%ports/make-memory-input-port (string->utf8 s) %ports/string-transcoder))

(define (open-output-string)
  (%ports/make-memory-output-port %ports/string-transcoder 'sout))

(define (get-output-string p)
  (if (not (string-output-port? p))
    (assertion-violation 'get-output-string "illegal argument" p))
  (%utf8->string (%port-out-buf p) 0 (%ports/memory-size p)))

(define (reset-output-string p)
  (if (not (string-output-port? p))
    (assertion-violation 'reset-output-string "illegal argument" p))
  (%port-out-buf-set! p (make-bytevector %ports/memory-initial-size 0))
  (%port-out-lim-set! p 0)
  (%port-hw-set! p 0)
  (%port-char-pos-set! p 0)
  (%port-lines-read-set! p 0)
  (%port-line-start-set! p 0)
  (unspecified))

;;; R6RS open-string-output-port: returns the port and an extraction
;;; procedure.
(define (open-string-output-port)
  (let ([p (open-output-string)])
    (values
      p
      (lambda ()
        (let ([s (get-output-string p)])
          (reset-output-string p)
          s)))))

(define (call-with-string-output-port proc)
  (if (procedure? proc)
    (call-with-port
      (open-output-string)
      (lambda (out)
        (proc out)
        (get-output-string out)))
    (assertion-violation 'call-with-string-output-port "not a procedure" proc)))


(define make-transcoded-port transcoded-port)
(define make-string-input-port open-input-string)
(define make-string-output-port open-output-string)

;;; eof
