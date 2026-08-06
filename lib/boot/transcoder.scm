;;; R6RS transcoder machinery (rnrs io ports, 8.2.4): codecs, eol styles, error-handling modes, and the transcoder record.

;;; A transcoder is an immutable record (codec, eol-style,
;;; error-handling-mode).  A codec is one of the symbols
;;; 'latin-1, 'utf-8, 'utf-16.  An eol style is one of
;;; 'lf 'cr 'crlf 'nel 'crnel 'ls 'none.  An error-handling mode is
;;; one of 'ignore 'raise 'replace.

(define <transcoder>
  (let* ([rtd (make-record-type-descriptor '<transcoder> #f #f #f #f '#(
            (immutable codec)
            (immutable eol-style)
            (immutable error-handling-mode)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '<transcoder> rtd rcd)))

(define %transcoder? (record-predicate (record-type-rtd <transcoder>)))
(define %make-transcoder (record-constructor (record-type-rcd <transcoder>)))
(define %transcoder-codec (record-accessor (record-type-rtd <transcoder>) 0))
(define %transcoder-eol-style (record-accessor (record-type-rtd <transcoder>) 1))
(define %transcoder-error-handling-mode (record-accessor (record-type-rtd <transcoder>) 2))

(define (%ports/transcoder-codec-raw t) (%transcoder-codec t))
(define (%ports/transcoder-eol-raw t) (%transcoder-eol-style t))
(define (%ports/transcoder-errmode-raw t) (%transcoder-error-handling-mode t))


(define (latin-1-codec) 'latin-1)
(define (utf-8-codec) 'utf-8)
(define (utf-16-codec) 'utf-16)

(define lf 'lf)
(define cr 'cr)
(define crlf 'crlf)
(define nel 'nel)
(define crnel 'crnel)
(define ls 'ls)

(define (eol-style style)
  (case style
    ((lf cr crlf nel crnel ls none) style)
    (else
      (assertion-violation 'eol-style "invalid eol style" style))))


(define (native-eol-style) 'none)

(define (make-transcoder codec . rest)
  (define (valid-codec? c)
    (memq c '(latin-1 utf-8 utf-16)))
  (define (valid-eol? s)
    (memq s '(lf cr crlf nel crnel ls none)))
  (define (valid-mode? m)
    (memq m '(ignore raise replace)))
  (if (and (valid-codec? codec)
       (valid-eol? (if (null? rest) 'none (car rest)))
       (valid-mode? (if (or (null? rest) (null? (cdr rest))) 'replace (cadr rest))))
    (%make-transcoder
      codec
      (if (null? rest) (native-eol-style) (car rest))
      (if (or (null? rest) (null? (cdr rest))) 'replace (cadr rest)))
    (assertion-violation 'make-transcoder "invalid transcoder argument(s)" (cons codec rest))))

(define (native-transcoder)
  (default-transcoder))

(define default-transcoder
  (make-parameter (%make-transcoder 'utf-8 'none 'replace) %transcoder?))

(define (transcoder-codec t)
  (if (%transcoder? t)
    (%transcoder-codec t)
    (assertion-violation 'transcoder-codec "not a transcoder" t)))

(define (transcoder-eol-style t)
  (if (%transcoder? t)
    (%transcoder-eol-style t)
    (assertion-violation 'transcoder-eol-style "not a transcoder" t)))

(define (transcoder-error-handling-mode t)
  (if (%transcoder? t)
    (%transcoder-error-handling-mode t)
    (assertion-violation 'transcoder-error-handling-mode "not a transcoder" t)))

  
;;; Encodes sv (a Unicode scalar value) as UTF-8, returning a
;;; bytevector of 1-4 bytes.

(define (%transcoder/utf8-encode sv)
  (cond ((<= sv #x7f)
         (let ([bv (make-bytevector 1 0)])
           (bytevector-u8-set! bv 0 sv)
           bv))
    ((<= sv #x7ff)
      (let ([bv (make-bytevector 2 0)])
        (bytevector-u8-set! bv 0 (logior #b11000000 (ash sv -6)))
        (bytevector-u8-set! bv 1 (logior #b10000000 (logand sv #b00111111)))
        bv))
    ((<= sv #xffff)
      (let ([bv (make-bytevector 3 0)])
        (bytevector-u8-set! bv 0 (logior #b11100000 (ash sv -12)))
        (bytevector-u8-set! bv 1 (logior #b10000000 (logand (ash sv -6) #b00111111)))
        (bytevector-u8-set! bv 2 (logior #b10000000 (logand sv #b00111111)))
        bv))
    (else
      (let ([bv (make-bytevector 4 0)])
        (bytevector-u8-set! bv 0 (logior #b11110000 (ash sv -18)))
        (bytevector-u8-set! bv 1 (logior #b10000000 (logand (ash sv -12) #b00111111)))
        (bytevector-u8-set! bv 2 (logior #b10000000 (logand (ash sv -6) #b00111111)))
        (bytevector-u8-set! bv 3 (logior #b10000000 (logand sv #b00111111)))
        bv))))

;;; Encodes one character with the given codec.  Returns a bytevector,
;;; or #f if the character cannot be encoded by that codec (only
;;; possible for latin-1).  UTF-16 is not handled here; it needs the
;;; per-port endianness state and lives in the textual port layer.

(define (%transcoder/encode-char codec c)
  (let ([sv (char->integer c)])
    (case codec
      ((latin-1)
        (if (<= sv #xff)
          (let ([bv (make-bytevector 1 0)])
            (bytevector-u8-set! bv 0 sv)
            bv)
          #f))
      ((utf-8)
        (%transcoder/utf8-encode sv))
      (else
        #f))))

;;; eof
