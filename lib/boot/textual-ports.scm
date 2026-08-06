;;; R6RS textual input and output operations (rnrs io ports, 8.2.9 and 8.2.12) 
;;; plus string ports, end-of-line processing, and UTF-8/UTF-16 decoding.
;;;
;;; Textual ports are binary ports carrying a transcoder.  Characters
;;; are decoded from the byte stream one at a time by the codec
;;; functions below; end-of-line translation and line counting are
;;; applied on top.  Lookahead is implemented by pushing the bytes of
;;; the decoded character back into the port's pending buffer.


;;; Creates a textual port over the binary port p, sharing p's
;;; underlying source and its buffered bytes, then closes p without
;;; closing the underlying source (R6RS 8.2.6).

(define (%ports/make-textual-port p t)
  (when (%ports/output-port? p)
    (%ports/flush-buffer p))
  (let ([np (%make-port
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
              (make-bytevector %ports/buffer-size 0)
              0
              (%port-pending p)
              (%port-pending-len p)
              0
              0
              0
              (if (eq? (transcoder-codec t) 'utf-16)
                (vector 'utf16 #f #f)
                #f)
              '()
              #f
              (%port-fd p)
              (%port-custom? p))])
    ;; close the original port without closing the shared handler
    (%port-state-set! p 'closed)
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


;;; Decodes the next character from the byte stream of a textual
;;; port.  Returns two values: the character (or the eof object) and
;;; the number of bytes consumed (0 for eof).  If scratch is a
;;; bytevector, the consumed bytes are recorded there so a lookahead
;;; can push them back.  Decoding errors are handled according to the
;;; transcoder's error-handling mode: 'ignore discards the offending
;;; bytes and retries, 'replace yields #\xfffd (or '?' for latin-1,
;;; which cannot fail to decode), 'raise signals an &i/o-decoding
;;; condition.
(define (%ports/decode-char p scratch)
  (define (rec b k)
    (when scratch
      (bytevector-u8-set! scratch k b)))
  (let ([codec (%ports/transcoder-codec-raw (%port-transcoder p))])
    (case codec
      ((latin-1)
        (let ([b (%ports/get-byte p)])
          (if (eof-object? b)
            (values (eof-object) 0)
            (begin
              (rec b 0)
              (values (integer->char b) 1)))))
      ((utf-8)
        (%ports/decode-utf8 p scratch))
      ((utf-16)
        (%ports/decode-utf16 p scratch))
      (else
        (assertion-violation 'get-char "internal error: unknown codec" codec p)))))

;;; Error dispatcher shared by the decoders.  For 'ignore, the
;;; offending bytes have already been consumed, so retrying cannot
;;; loop forever.
(define (%ports/decode-error p who)
  (case (%ports/transcoder-errmode-raw (%port-transcoder p))
    ((ignore)
      (call-with-values
        (lambda () (%ports/decode-char p #f))
        (lambda (c n) (values c n))))
    ((replace)
      (values #\xfffd 0))
    (else
      (raise-i/o-decoding-error who "decoding error" p))))

(define (%ports/decode-utf8 p scratch)
  (define (rec b k)
    (when scratch
      (bytevector-u8-set! scratch k b)))
  (let ([b1 (%ports/get-byte p)])
    (if (eof-object? b1)
      (values (eof-object) 0)
      (let ([len (cond ((<= b1 #x7f) 1)
                   ((<= b1 #xdf) 2)
                   ((<= b1 #xef) 3)
                   ((<= b1 #xf4) 4)
                   (else 0))])
        (cond ((= len 0)
               (%ports/decode-error p 'get-char))
          ((= len 1)
            (rec b1 0)
            (values (integer->char b1) 1))
          (else
            (rec b1 0)
            (let loop ([i 1]
                       [sv (logand b1
                             (case len ((2) #x1f) ((3) #x0f) (else #x07)))])
              (if (= i len)
                (if (<= #xd800 sv #xdfff) ; surrogate
                  (%ports/decode-error p 'get-char)
                  (values (integer->char sv) len))
                (let ([b (%ports/get-byte p)])
                  (cond ((eof-object? b) (%ports/decode-error p 'get-char))
                    ((not (<= #x80 b #xbf)) (%ports/decode-error p 'get-char))
                    ((and (= i 1)
                       (or (and (= b1 #xe0) (< b #xa0))
                         (and (= b1 #xf0) (< b #x90))
                         (and (= b1 #xf4) (> b #x8f))))
                      (%ports/decode-error p 'get-char))
                    (else
                      (rec b i)
                      (loop (+ i 1) (logior (ash sv 6) (logand b #x3f))))))))))))))

;;; Decodes the next UTF-16 character, sniffing the byte-order mark on
;;; the first character.  The BOM bytes are consumed but not recorded
;;; in scratch, so a lookahead push-back does not replay them.
(define (%ports/decode-utf16 p scratch)
  (define (rec b k)
    (when scratch
      (bytevector-u8-set! scratch k b)))
  (define st
    (or (%port-codec-state p)
      (let ([s (vector 'utf16 #f #f)])
        (%port-codec-state-set! p s)
        s)))
  (define (combine b1 b2)
    (if (eq? (vector-ref st 1) 'little)
      (logior (ash b2 8) b1)
      (logior (ash b1 8) b2)))
  (define (process-first-pair b1 b2)
    (let ([u (combine b1 b2)])
      (rec b1 0)
      (rec b2 1)
      (cond ((<= #xd800 u #xdbff)
             (let ([b3 (%ports/get-byte p)])
               (if (eof-object? b3)
                 (%ports/decode-error p 'get-char)
                 (let ([b4 (%ports/get-byte p)])
                   (if (eof-object? b4)
                     (%ports/decode-error p 'get-char)
                     (let ([u2 (combine b3 b4)])
                       (if (<= #xdc00 u2 #xdfff)
                         (begin
                           (rec b3 2)
                           (rec b4 3)
                           (values
                             (integer->char
                               (+ #x10000
                                 (logior (ash (- u #xd800) 10)
                                   (- u2 #xdc00))))
                             4))
                         (%ports/decode-error p 'get-char))))))))
        ((<= #xdc00 u #xdfff) ; lone low surrogate
          (%ports/decode-error p 'get-char))
        (else
          (values (integer->char u) 2)))))
  (define (read-pair)
    (let ([b1 (%ports/get-byte p)])
      (if (eof-object? b1)
        (values (eof-object) 0)
        (let ([b2 (%ports/get-byte p)])
          (if (eof-object? b2)
            (%ports/decode-error p 'get-char)
            (process-first-pair b1 b2))))))
  (if (not (vector-ref st 1))
    (let ([b1 (%ports/get-byte p)])
      (if (eof-object? b1)
        (values (eof-object) 0)
        (let ([b2 (%ports/get-byte p)])
          (if (eof-object? b2)
            (%ports/decode-error p 'get-char)
            (cond ((and (= b1 #xfe) (= b2 #xff))
                   (vector-set! st 1 'big)
                   (read-pair))
              ((and (= b1 #xff) (= b2 #xfe))
                (vector-set! st 1 'little)
                (read-pair))
              (else
                (vector-set! st 1 'big)
                (process-first-pair b1 b2)))))))
    (read-pair)))

;;; Decodes one character without end-of-line translation, pushing the
;;; consumed bytes back so a later read returns the same character.
(define (%ports/peek-raw-char p)
  (let ([scratch (make-bytevector 4 0)])
    (call-with-values
      (lambda () (%ports/decode-char p scratch))
      (lambda (c n)
        (if (eof-object? c)
          c
          (begin
            (%ports/push-back! p scratch n)
            c))))))

;;; Decodes one character without end-of-line translation, consuming it.
(define (%ports/get-raw-char p)
  (call-with-values
    (lambda () (%ports/decode-char p #f))
    (lambda (c n) c)))

;;; Records a (char-pos . byte-pos) anchor when the byte buffer is
;;; empty, used by set-port-position! on built-in textual ports.
;;; Anchors are recorded at most once every 64 characters so the list
;;; stays small.
(define (%ports/maybe-record-anchor! p)
  (if (and (= (%port-in-ptr p) (%port-in-lim p))
       (= (%port-pending-len p) 0))
    (let ([anchors (%port-anchors p)])
      (if (or (null? anchors)
           (> (- (%port-char-pos p) (caar anchors)) 64))
        (%ports/record-anchor! p))))
  (unspecified))


;;; Returns the next character (or eof).  If lookahead? is true, the
;;; port position is not advanced.
(define (%ports/read-char* p lookahead?)
  (define eol (%ports/transcoder-eol-raw (%port-transcoder p)))
  (define scratch (if lookahead? (make-bytevector 4 0) #f))
  (let loop ()
    (%ports/maybe-record-anchor! p)
    (call-with-values
      (lambda () (%ports/decode-char p scratch))
      (lambda (c n)
        (if (eof-object? c)
        (eof-object)
        (cond ((eq? eol 'none)
               (if lookahead?
                 (%ports/push-back! p scratch n)
                 (begin
                   (%port-char-pos-set! p (+ (%port-char-pos p) 1))
                   (when (char=? c #\newline)
                     (%port-lines-read-set! p (+ (%port-lines-read p) 1))
                     (%port-line-start-set! p (%port-char-pos p)))))
               c)
          ((char=? c #\return)
            (if lookahead?
              (begin
                (%ports/push-back! p scratch n)
                #\newline)
              (let ([c2 (%ports/peek-raw-char p)])
                (if (or (eof-object? c2)
                     (not (or (char=? c2 #\newline)
                            (char=? c2 #\x85))))
                  ; a bare carriage return is a line ending
                  (begin
                    (%port-char-pos-set! p (+ (%port-char-pos p) 1))
                    (%port-lines-read-set! p (+ (%port-lines-read p) 1))
                    (%port-line-start-set! p (%port-char-pos p))
                    #\newline)
                  ; carriage return followed by linefeed or NEL
                  (begin
                    (%ports/get-raw-char p)
                    (%port-char-pos-set! p (+ (%port-char-pos p) 2))
                    (%port-lines-read-set! p (+ (%port-lines-read p) 1))
                    (%port-line-start-set! p (%port-char-pos p))
                    #\newline)))))
          ((or (char=? c #\newline)
             (char=? c #\x85)
             (char=? c #\x2028))
            (if lookahead?
              (%ports/push-back! p scratch n)
              (begin
                (%port-char-pos-set! p (+ (%port-char-pos p) 1))
                (%port-lines-read-set! p (+ (%port-lines-read p) 1))
                (%port-line-start-set! p (%port-char-pos p))))
            #\newline)
          (else
            (if lookahead?
              (%ports/push-back! p scratch n)
              (%port-char-pos-set! p (+ (%port-char-pos p) 1)))
            c)))))))

(define (get-char p)
  (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
    (%ports/read-char* p #f)
    (assertion-violation 'get-char "not an open textual input port" p)))

(define (lookahead-char p)
  (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
    (%ports/read-char* p #t)
    (assertion-violation 'lookahead-char "not an open textual input port" p)))


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
      (if (eq? codec 'latin-1)
        (%ports/put-byte p #x85)
        (begin
          (%ports/put-byte p #xc2)
          (%ports/put-byte p #x85))))
    ((crnel)
      (if (eq? codec 'latin-1)
        (begin
          (%ports/put-byte p 13)
          (%ports/put-byte p #x85))
        (begin
          (%ports/put-byte p 13)
          (%ports/put-byte p #xc2)
          (%ports/put-byte p #x85))))
    ((ls)
      (if (eq? codec 'latin-1)
        (assertion-violation 'put-char
          "cannot encode line separator as latin-1" p)
        (begin
          (%ports/put-byte p #xe2)
          (%ports/put-byte p #x80)
          (%ports/put-byte p #xa8))))
    (else
      (assertion-violation 'put-char "internal error" p))))

;;; Encodes one character as UTF-16 (big-endian), writing a byte-order
;;; mark before the first character.
(define (%ports/put-char-utf16 p c)
  (let* ([st (%port-codec-state p)]
         [st (or st
              (let ([s (vector 'utf16 #f #f)])
                (%port-codec-state-set! p s)
                s))])
    (when (and (not (vector-ref st 2))
           (zero? (if (%ports/underlying-position p)
                    (%ports/underlying-position p)
                    0)))
      (%ports/put-byte p #xfe)
      (%ports/put-byte p #xff)
      (vector-set! st 2 #t))
    (let ([sv (char->integer c)])
      (if (<= sv #xffff)
        (begin
          (%ports/put-byte p (ash sv -8))
          (%ports/put-byte p (logand sv #xff)))
        (let ([x (- sv #x10000)])
          (let ([hi (+ #xd800 (ash x -10))]
                [lo (+ #xdc00 (logand x #x3ff))])
            (%ports/put-byte p (ash hi -8))
            (%ports/put-byte p (logand hi #xff))
            (%ports/put-byte p (ash lo -8))
            (%ports/put-byte p (logand lo #xff))))))
    (unspecified)))

(define (put-char p c)
  (if (and (port? p) (%ports/output-port? p) (%ports/textual? p) (char? c))
    (let* ([t (%port-transcoder p)]
           [codec (%ports/transcoder-codec-raw t)]
           [eol (%ports/transcoder-eol-raw t)]
           [errmode (%ports/transcoder-errmode-raw t)])
      (if (char=? c #\newline)
        (%ports/put-eol p codec eol)
        (case codec
          ((latin-1)
            (let ([sv (char->integer c)])
              (if (<= sv #xff)
                (%ports/put-byte p sv)
                (case errmode
                  ((ignore) (unspecified))
                  ((replace) (%ports/put-byte p 63))
                  (else
                    (raise-i/o-encoding-error 'put-char "encoding error" p c))))))
          ((utf-8)
            (let ([bv (%transcoder/encode-char 'utf-8 c)])
              (let loop ([i 0] [n (bytevector-length bv)])
                (if (< i n)
                  (begin
                    (%ports/put-byte p (bytevector-u8-ref bv i))
                    (loop (+ i 1) n))
                  (unspecified)))))
          ((utf-16)
            (%ports/put-char-utf16 p c))
          (else
            (assertion-violation 'put-char "internal error" p))))
      (%port-char-pos-set! p (+ (%port-char-pos p) 1))
      (when (char=? c #\newline)
        (%port-lines-read-set! p (+ (%port-lines-read p) 1))
        (%port-line-start-set! p (%port-char-pos p))
        (when (eq? (%port-buffer-mode p) 'line)
          (%ports/flush-buffer p))))
    (assertion-violation 'put-char "not an open textual output port or invalid char" p c)))

(define (get-string-n! p s start count)
  (if (and (port? p)
       (%ports/input-port? p)
       (%ports/textual? p)
       (string? s)
       (fixnum? start)
       (fixnum? count)
       (<= 0 start)
       (<= 0 count)
       (<= (+ start count) (string-length s)))
    (if (= count 0)
      0
      (let loop ([i start] [n (+ start count)])
        (cond ((= i n)
               (- i start))
          (else
            (let ([c (get-char p)])
              (cond ((eof-object? c)
                     (if (= i start) (eof-object) (- i start)))
                (else
                  (string-set! s i c)
                  (loop (+ i 1) n))))))))
    (assertion-violation 'get-string-n! "illegal arguments" p s start count)))

(define (get-string-n p count)
  (if (and (port? p)
       (%ports/input-port? p)
       (%ports/textual? p)
       (fixnum? count)
       (<= 0 count))
    (let* ([s (make-string count #\space)]
           [n (get-string-n! p s 0 count)])
      (cond ((eof-object? n) (eof-object))
        ((= n count) s)
        (else (substring s 0 n))))
    (assertion-violation 'get-string-n "illegal arguments" p count)))

(define (get-string-all p)
  (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
    (let loop ([chars '()])
      (let ([c (get-char p)])
        (if (eof-object? c)
          (if (null? chars)
            (eof-object)
            (list->string (reverse chars)))
          (loop (cons c chars)))))
    (assertion-violation 'get-string-all "not an open textual input port" p)))

;;; R6RS get-line: reads through the linefeed and returns the text
;;; without it.  With an eol style other than none, all line endings
;;; have already been translated to #\newline by get-char.
(define (get-line p)
  (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
    (let loop ([chars '()])
      (let ([c (get-char p)])
        (cond ((eof-object? c)
               (if (null? chars) (eof-object) (list->string (reverse chars))))
          ((char=? c #\newline)
            (list->string (reverse chars)))
          (else
            (loop (cons c chars))))))
    (assertion-violation 'get-line "not an open textual input port" p)))

;;; R7RS read-line: an end of line is a linefeed, a carriage return,
;;; or a carriage return followed by a linefeed.  The end-of-line
;;; sequence is not included in the result.
(define (read-line . rest)
  (define p (if (null? rest) (current-input-port) (car rest)))
  (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
    (let loop ([chars '()])
      (let ([c (get-char p)])
        (cond ((eof-object? c)
               (if (null? chars) (eof-object) (list->string (reverse chars))))
          ((char=? c #\newline)
            (list->string (reverse chars)))
          ((char=? c #\return)
            (let ([c2 (peek-char p)])
              (if (and (char? c2) (char=? c2 #\newline))
                (get-char p)))
            (list->string (reverse chars)))
          (else
            (loop (cons c chars))))))
    (assertion-violation 'read-line "not a textual input port" p)))

(define (put-string p s . rest)
  (define (put p s start count)
    (if (and (port? p)
         (%ports/output-port? p)
         (%ports/textual? p)
         (string? s)
         (fixnum? start)
         (fixnum? count)
         (<= 0 start)
         (<= 0 count)
         (<= (+ start count) (string-length s)))
      (let loop ([i start] [n (+ start count)])
        (if (< i n)
          (begin
            (put-char p (string-ref s i))
            (loop (+ i 1) n))
          (unspecified)))
      (assertion-violation 'put-string "illegal arguments" p s start count)))
  (cond ((null? rest)
         (put p s 0 (string-length s)))
    ((null? (cdr rest))
      (put p s (car rest) (- (string-length s) (car rest))))
    ((null? (cddr rest))
      (put p s (car rest) (cadr rest)))
    (else
      (assertion-violation 'put-string "too many arguments" p s rest))))

;;; String ports.
;;;
;;; A string input port is a UTF-8 transcoded bytevector input port.
;;; A string output port is a UTF-8 transcoded bytevector output port;
;;; get-output-string reads the accumulated UTF-8 bytes from the
;;; underlying bytevector data.

(define (string-output-port? p)
  (and (port? p)
    (%ports/textual? p)
    (let ([d (%handler-data (%port-handler p))])
      (and (vector? d)
        (> (vector-length d) 0)
        (eq? (vector-ref d 0) 'bytevector-output-port)))))

(define (open-input-string s)
  (if (not (string? s))
    (assertion-violation 'open-input-string "illegal argument" s))
  (transcoded-port
    (open-input-bytevector (string->utf8 s))
    (make-transcoder (utf-8-codec) 'none 'ignore)))

(define (open-output-string)
  (transcoded-port (open-output-bytevector) (default-transcoder)))

(define (get-output-string p)
  (if (not (string-output-port? p))
    (assertion-violation 'get-output-string "illegal argument" p))
  (flush-output-port p)
  (let* ([data (%handler-data (%port-handler p))]
         [bv (vector-ref data 1)]
         [limit (vector-ref data 3)])
    (utf8->string (bytevector-copy bv 0 limit))))

(define (reset-output-string p)
  (if (not (string-output-port? p))
    (assertion-violation 'reset-output-string "illegal argument" p))
  (flush-output-port p)
  (let ([data (%handler-data (%port-handler p))])
    (vector-set! data 1 (make-bytevector 32 0))
    (vector-set! data 2 0)
    (vector-set! data 3 0))
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
