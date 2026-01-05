; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; Larceny -- I/O system.
;
; Design: the system is designed so that in the common case
; of an Ascii character being read from or written to a port
; whose buffer is neither empty nor full, very few procedure
; calls are executed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Port data structure and invariants.
;
; Latin-1 and UTF-8 transcoding is done on the fly.  UTF-16 and
; nonstandard transcoders are implemented only by custom ports,
; which translate to UTF-8, so UTF-16 and nonstandard transcoders
; are never seen by this level of abstraction.
;
; Every port has:
;
;     type            { binary, textual }
;                   x { closed, input, output, input/output }
;     state           { closed, error, eof, binary, textual, auxstart, auxend }
;     transcoder      { Latin-1, UTF-8 }
;                   x { none, lf, cr, crlf, nel, crnel, ls }
;                   x { raise, replace, ignore }
;     mainbuf
;     auxbuf          4-byte bytevector
;
; An input (but not input/output) port p can be in one of these states:
;
; closed          type = 0 & state = 'closed
;
; error           type > 0 & state = 'error
;
; eof             type > 0 & state = 'eof
;
; binary          type = <binary,input> & state = 'binary
;
; textual         (type = <textual,input> & state = 'textual
;                 & mainbuf[mainlim] = 255
;                 & auxptr = auxlim = 0)
;              or (type = <textual,output> & state = 'textual
;                 & mainptr = 0)
;
; auxstart        type = <textual,input> & state = 'auxstart
;                 & 0 = mainptr < mainlim & mainbuf[0] = 255
;                 & 0 <= auxptr < auxlim
;
; auxend          type = <textual,input> & state = 'auxend
;                 & mainbuf[mainlim] = 255
;                 & 0 <= auxptr < auxlim
;
; In the auxstart state, the contents of auxbuf precede mainbuf.
; In the auxend state, the contents of auxbuf follow mainbuf.
;
; In the textual state, the buffered bytes lie between mainptr
; and mainlim.
;
; Input/output ports preserve this invariant:
;
; closed          type = <_,input/output> & state = 'closed
;
; error           type > <_,input/output> & state = 'error
;
; eof             type = <_,input/output> & state = 'eof
;
; input/output    type = <_,input/output> & state = 'input/output
;                 & mainptr = 0
;                 & mainlim <= 4
;
; The buffer is empty iff mainptr = 0.  If mainlim > 0, then
; the buffer contains exactly one lookahead byte or character
; (in UTF-8).  Aside from the one byte or character of
; lookahead, input/output ports are never buffered.
;
; Input/output ports are different in several other ways:
;
;     Input/output ports must support set-port-position! so
;     they can undo lookahead buffering when switching from
;     input to output.  (See io/put-X-input/output.)
;
;     A binary input/output port's ioproc never reads or writes
;     more than one byte at a time.
;
;     A textual input/output port's ioproc never reads or writes
;     more than one character at a time.
;
;     If a textual input/output port is transcoded, then all
;     transcoding and end-of-line handling is done by the port's
;     ioproc, not by the on-the-fly transcoding in this file.
;
; Inlined read and write operations on input/output ports
; always perform a closed call to the main read and write
; routines defined in this file.
;
; FIXME:  For textual output ports, keeping the buffered bytes
; between 0 and mainptr *might* be better.  This might improve
; combined input/output ports.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The inlined operations (get-u8, get-char, put-u8, put-char)
; always access the mainbuf, calling out-of-line code for these
; specific cases:
;
;     get-u8      mainbuf is empty
;     put-u8      mainbuf is full
;     get-char    next byte is 13 or greater than 127
;     put-char    mainbuf is full or character is non-Ascii
;
; For a textual input port, the mainbuf always contains a
; sentinel that forces complicated situations to be handled
; by out-of-line code.  The other bytes of the mainbuf depend
; upon the transcoder:
;
;     Latin-1     mainbuf contains Latin-1
;     UTF-8       mainbuf contains UTF-8
;
; For output ports with a UTF-16 transcoder, trancoding from
; UTF-8 to UTF-16 will be performed out of line, when the buffer
; is flushed, or by a custom port.  (Haven't decided yet.)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The port type is a fixnum that encodes the binary/textual
; and input/output distinctions.  It is the inclusive or of
;
;     binary/textual:
;         0 means binary
;         1 means textual
;     direction:
;         0 means closed
;         2 means input
;         4 means output
;         6 means input/output

(define (errmsg msg . rest)
  (symbol->string msg))

(define type-mask:binary/textual  1)
(define type-mask:direction       6)

(define type:binary               0)
(define type:textual              1)
(define type:input                2)
(define type:output               4)

(define type:closed               0)
(define type:binary-input         2)
(define type:textual-input        3)
(define type:binary-output        4)
(define type:textual-output       5)
(define type:binary-input/output  6)
(define type:textual-input/output 7)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Fields and offsets of a port structure.
; The most frequently accessed fields are at the beginning
; of the port structure, in hope of improving cache performance.
;
; NOTE that you can *not* change the offsets without also changing
; them in Compiler/common.imp.sch, where they are likely to be
; inlined.  The offsets may also be used in the following files:
;
;     bytevectorio.sch
;     stringio.sch
;     transio.sch
;
; They should not be used in any other files.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define port.tag        0) ; port tag, used for port? procedure
(define port.type       1) ; fixnum: see above for encoding
(define port.mainbuf    2) ; bytevector: Latin-1 or UTF-8 encodings
(define port.mainptr    3) ; nonnegative fixnum: next loc in mainbuf (input)
(define port.mainlim    4) ; nonnegative fixnum: sentinel in mainbuf (input)

; For input ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainptr field.
;
; For output ports, port.mainpos holds the current byte
; or character position minus the current value of the
; port.mainlim field.

(define port.mainpos    5) ; integer: byte/char position - (mainptr or mainlim)
(define port.transcoder 6) ; fixnum: see comment at make-transcoder

; The state is always one of the following symbols:
;     closed error eof
;     binary
;     textual auxstart auxend
;     input/output

(define port.state      7) ; symbol: see above
(define port.iodata     8) ; port-specific data
(define port.ioproc     9) ; port*symbol -> varies-with-symbol

; output ports

(define port.bufmode    10) ; symbol: none, line, datum, block
(define port.wr-flush?  11) ; boolean: true iff bufmode is datum

; textual input ports

(define port.auxbuf     12) ; bytevector: 4 bytes before or after mainbuf
(define port.auxptr     13) ; fixnum: index of next byte in auxbuf
(define port.auxlim     14) ; fixnum: 1 + index of last byte in auxbuf
(define port.linesread  15) ; integer: number of line endings read so far
(define port.linestart  16) ; integer: character position after last line ending
(define port.wasreturn  17) ; boolean: last line ending was #\return
(define port.readmode   18) ; fixnum: see comment before default-read-mode

; all ports

(define port.setposn    19) ; boolean: true iff supports set-port-position!
(define port.alist      20) ; association list: used mainly by custom ports
(define port.r7rstype   21) ; copy of port.type but unaltered by closing
(define port.reader     22)
(define port.structure-size 24)      ; size of port structure

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Miscellaneous constants.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Default length of i/o buffer.

(define port.mainbuf-size    1024)

; Textual input uses 255 as a sentinel byte to force
; inline code to call a procedure for the general case.
; Note that 255 is not a legal code unit of UTF-8.

(define port.sentinel 255)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public low-level interface (may be called from code in other files)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (io/initialize)
  ; Nothing, for the time being.
  #t)

(define (io/finalize)
  ; Nothing, for the time being.
  #t)

; 'ioproc' is a procedure of one argument: a symbol that denotes the 
; operation to perform.  It returns a port-specific procedure that, when
; called, performs the operation.  The operations are:
;
;   read : iodata * buffer -> { fixnum, 'eof, 'error }
;   write : iodata * buffer * count -> { 'ok, 'error }
;   close : iodata -> { 'ok, 'error }
;   ready? : iodata -> boolean
;   name : iodata -> string
;
; If the port supports set-port-position!, then ioproc also performs
;
;   set-position! : iodata * posn -> { 'ok, 'error }
;
; Note: io/make-port defaults to textual (for backward compatibility)

(define (io/make-port ioproc iodata . rest)
  (let ((v (make-tuple port.structure-size #f))
        (input? #f)
        (output? #f)
        (binary? #f)
        (textual? #f)
        (set-position? #f))

    (tuple-set! v port.bufmode 'block) ; default buffer mode is block

    ; Parse keyword arguments.

    (for-each
     (lambda (keyword)
      (case keyword
        ((input)         (set! input? #t))
        ((output)        (set! output? #t))
        ((text)          (set! textual? #t))
        ((binary)        (set! binary? #t))
        ((set-position!) (set! set-position? #t))
        ((none)          (tuple-set! v port.bufmode 'none))
        ((line)          (tuple-set! v port.bufmode 'line))
        ((datum flush)   (tuple-set! v port.bufmode 'datum)
                         (tuple-set! v port.wr-flush? #t))
        ((block)         (tuple-set! v port.bufmode 'block))
        (else
         (assertion-violation 'io/make-port "bad attribute" (car rest))
         #t)))
     rest)

    (if (and binary? textual?)
        (assertion-violation 'io/make-port "binary incompatible with textual"))
    (tuple-set! v
                 port.type
                 (cond
                  ((and binary? input? output?)  type:binary-input/output)
                  ((and binary? input?)          type:binary-input)
                  ((and binary? output?)         type:binary-output)
                  ((and textual? input? output?) type:textual-input/output)
                  ((and textual? input?)         type:textual-input)
                  ((and textual? output?)        type:textual-output)
                  (input?                        type:textual-input)
                  (output?                       type:textual-output)
                  (else
                   (error
                    'io/make-port "neither input nor output" rest))))

    (tuple-set! v port.mainbuf (make-bytevector port.mainbuf-size))
    (tuple-set! v port.mainptr 0)
    (tuple-set! v port.mainlim 0)
    (tuple-set! v port.mainpos 0)
    (tuple-set! v port.transcoder
                   (if binary?
                       0
                       (native-transcoder)))
    (tuple-set! v port.state (if binary? 'binary 'textual))
    (tuple-set! v port.iodata iodata)
    (tuple-set! v port.ioproc ioproc)
    (tuple-set! v port.auxbuf (make-bytevector 4))
    (tuple-set! v port.auxlim 0)
    (tuple-set! v port.linesread 0)
    (tuple-set! v port.linestart 0)
    (tuple-set! v port.wasreturn #f)
    (tuple-set! v port.readmode
                   (if (not binary?)
                       (default-read-mode)
                       readmode:binary))

    (tuple-set! v port.setposn set-position?)
    (tuple-set! v port.alist '())
    (tuple-set! v port.r7rstype (tuple-ref v port.type))
    (tuple-set! v port.reader #f)
    (tuple-set! v 0 'type:port)
    (io/reset-buffers! v)                     ; inserts sentinel
    v))

; Port? is integrable.
; Eof-object? is integrable.

(define (port? x) 
  (and (tuple? x) (eq? (tuple-ref x 0) 'type:port)))

(define (io/input-port? p)
  (and (port? p)
       (let ((direction (logand type-mask:direction
                                  (tuple-ref p port.type))))
         (= type:input (logand direction type:input)))))

(define (io/output-port? p)
  (and (port? p)
       (let ((direction (logand type-mask:direction
                                  (tuple-ref p port.type))))
         (= type:output (logand direction type:output)))))

(define (io/r7rs-input-port? p)
  (and (port? p)
       (let ((direction (logand type-mask:direction
                                  (tuple-ref p port.r7rstype))))
         (= type:input (logand direction type:input)))))

(define (io/r7rs-output-port? p)
  (and (port? p)
       (let ((direction (logand type-mask:direction
                                  (tuple-ref p port.r7rstype))))
         (= type:output (logand direction type:output)))))

(define (io/open-port? p)
  (or (io/input-port? p) (io/output-port? p)))

(define (io/buffer-mode p)
  (assert (port? p))
  (tuple-ref p port.bufmode))

; FIXME:  For v0.94 only, read-char and peek-char can read
; from binary ports, treating them as Latin-1.
;
; FIXME:  After v0.94, io/read-char and io/peek-char should
; just delegate to io/get-char.
;
; FIXME:  For v0.963, uses of io/if have been removed, but
; their old locations are still identified by FIXME comments.

;(define-syntax io/if
;  (syntax-rules ()
;   ((_ expr) #f)         ;FIXME: disabled for 0.95
;   ((_ expr) expr)
;   ))

(define (io/read-char p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((or (eq? type type:textual-input)
                   (eq? type type:textual-input/output))
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (begin (tuple-set! p port.mainptr (+ ptr 1))
                            (integer->char unit))
                     (io/get-char p #f))))
              ((or (eq? type type:binary-input)   
                   (eq? type type:binary-input/output))
               (let ((x (io/get-u8 p #f)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'read-char "not a textual input port" p)
               #t)))
      (begin (error 'read-char "not a textual input port" p)
             #t)))

; FIXME:  See comments for io/read-char above.

(define (io/peek-char p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((or (eq? type type:textual-input)
                   (eq? type type:textual-input/output))
               (let ((unit (bytevector-ref buf ptr)))
                 (if (< unit 128)
                     (integer->char unit)
                     (io/get-char p #t))))
              ((or (eq? type type:binary-input)   
                   (eq? type type:binary-input/output))
               (let ((x (io/get-u8 p #t)))
                 (if (eof-object? x)
                     x
                     (integer->char x))))
              (else
               (error 'peek-char "not a textual input port" p)
               #t)))
      (begin (error 'peek-char "not a textual input port" p)
             #t)))

; This was dropped in R6RS because its semantics as specified
; by the R5RS aren't really useful.  See below.
;
; FIXME: reliable only when an Ascii character is ready on a
; textual port, which is a restriction permitted by the R5RS.
; The problem here is that a non-Ascii character might have
; been read in part, but attempting to read the full character
; might hang.  A more complex implementation is needed.
;
; FIXME: trusts the ioproc, which might be unwise.

(define (io/char-ready? p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (buf  (tuple-ref p port.mainbuf))
            (ptr  (tuple-ref p port.mainptr)))
        (cond ((or (eq? type type:textual-input)
                   (eq? type type:textual-input/output))
               (let ((unit (bytevector-ref buf ptr)))
                 (or (< unit 128)
                     (eq? (tuple-ref p port.state)
                          'eof)
                     (((tuple-ref p port.ioproc) 'ready?)
                      (tuple-ref p port.iodata)))))
              (else #f)))
      (error 'char-ready? (errmsg 'msg:nottextualinput) p)))

; FIXME: trusts the ioproc, which might be unwise.

(define (io/u8-ready? p)
  (if (port? p)
      (let ((type (tuple-ref p port.type))
            (ptr  (tuple-ref p port.mainptr))
            (lim  (tuple-ref p port.mainlim)))
        (cond ((or (eq? type type:binary-input)   
                   (eq? type type:binary-input/output))
               (or (< ptr lim)
                   (eq? (tuple-ref p port.state)
                        'eof)
                   (((tuple-ref p port.ioproc) 'ready?)
                    (tuple-ref p port.iodata))))
              (else #f)))
      (error 'u8-ready? (errmsg 'msg:notbinaryinput) p)))

; FIXME:  For v0.94 only, io/write-char can write to binary
; ports, treating them as Latin-1.
;
; FIXME:  After v0.94, io/write-char should just delegate
; to io/put-char.

(define (io/write-char c p)
  (unless (char? c)
    (error 'write-char "not a character" c))
  (if (port? p)
      (let ((type (tuple-ref p port.type)))
        (cond ((or (eq? type type:binary-output)   
                   (eq? type type:binary-input/output))
               (let ((sv (char->integer c)))
                 (if (< sv 256)
                     (io/put-u8 p sv)
                     (error 'write-char
                            "non-latin-1 character to binary port"
                            c p))))
              ((or (eq? type type:textual-output)
                   (eq? type type:textual-input/output))
               (io/put-char p c))
              (else
               (error 'write-char "not a textual output port" p)
               #t)))
      (begin (error 'write-char "not a textual output port" p)
             #t)))

; FIXME:  The name is misleading, since it now requires a bytevector.
; FIXME:  Asm/Shared/makefasl.sch uses this procedure, mainly
; to write codevectors.
;
; For short bytevectors, it might be more effective to copy rather than
; flush.  This procedure is really most useful for long bytevectors, and
; was written to speed up fasl file writing.
;
; With the advent of transcoded i/o in v0.94, this procedure
; is useful only for writing fasl files, which are either
; binary or Latin-1 with no end-of-line translation.

(define (io/write-bytevector-like bvl p)
  (assert (port? p))
  (assert (bytevector? bvl))
  (assert (let ((t (tuple-ref p port.transcoder)))
            (or (eq? t codec:binary)
                (eq? (io/transcoder-codec t) 'latin-1))))
  (let ((buf (tuple-ref p port.mainbuf))
        (tt  (typetag bvl)))
    (io/flush-buffer p)
    (tuple-set! p port.mainbuf bvl)
    (tuple-set! p port.mainlim (bytevector-length bvl))
    (io/flush-buffer p)
    (tuple-set! p port.mainbuf buf)
    (tuple-set! p port.mainlim 0)
    (unspecified)))
  
; When writing the contents of an entire string,
; we could do the error checking just once, but
; this should be fast enough for now.

(define (io/write-string s p)
  (let loop ((i 0)
             (n (string-length s)))
    (if (< i n)
        (begin
          (io/write-char (string-ref s i) p)
          (loop (+ i 1) n))
        (unspecified))))

(define (io/discretionary-flush p)
  (if (and (port? p) (io/output-port? p))
      (if (tuple-ref p port.wr-flush?)
          (io/flush-buffer p))
      (begin (error "io/discretionary-flush: not an output port: " p)
             #t)))

; Flushes output-only ports, but does not flush combined input/output
; ports because they are unbuffered on the output side.

(define (io/flush p)
  
  (if (and (port? p) (io/output-port? p))
      (if (not (io/input-port? p))
          (io/flush-buffer p))
      (begin (error "io/flush: not an output port: " p)
             #t)))

; FIXME:  Should release buffers.

(define (io/close-port p)
  (if (port? p)
      (begin
        (if (io/output-port? p)
            (io/flush-buffer p))
        (((tuple-ref p port.ioproc) 'close)
         
         (tuple-ref p port.iodata))
        (io/set-closed-state! p)
        (unspecified))
      (begin (error "io/close-port: not a port: " p)
             #t)))

(define (io/port-reader p)
  (if (not (port? p))
    (assertion-violation 'port-reader "not a port" p))
  (tuple-ref p port.reader))
(define (io/port-reader-set! p r)
  (if (not (port? p))
    (assertion-violation 'port-reader-set! "not a port" p))
  (tuple-set! p port.reader r))

(define (io/port-name p)
  (((tuple-ref p port.ioproc) 'name) (tuple-ref p port.iodata)))

(define (io/port-error-condition? p)
  (and (port? p) (eq? (tuple-ref p port.state) 'error)))

(define (io/port-at-eof? p)
  (and (port? p) (eq? (tuple-ref p port.state) 'eof)))

; The port alist was added to implement R6RS semantics for
; port-position and set-port-position!, and may eventually
; be used for other things also.

(define (io/port-alist p)
  (cond ((port? p)
         (tuple-ref p port.alist))
        (else
         (error 'io/port-alist (errmsg 'msg:illegal) p)
         #t)))

(define (io/port-alist-set! p alist)
  (cond ((not (port? p))
         (error 'io/port-alist (errmsg 'msg:illegal1) p))
        ((not (and (list? alist)
                   (every? pair? alist)))
         (error 'io/port-alist (errmsg 'msg:illegal2) p))
        (else
         (tuple-set! p port.alist alist)
         (unspecified))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Parameters.
;
; These parameters control how the read and get-datum procedures
; parse data from a textual input port.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If #t, symbols beginning with : are self-quoting.
;;
;; FIXME:  This affects macro expansion, but not the reader.

(define recognize-keywords? (make-parameter  #f))

(define recognize-javadot-symbols?
  (make-parameter #f boolean?))

(define case-sensitive? (make-parameter  #f boolean?))

; FIXME: deprecated

(define read-square-bracket-as-paren
  (make-parameter #t boolean?))

;; If #t, the reader keeps track of source locations.

(define datum-source-locations?
  (make-parameter  #f boolean?))

;; Enables flags such as #!r5rs and #!larceny

(define read-r6rs-flags?
  (make-parameter#t boolean?))

;; Enables:
;; # as an insignificant digit
;; #!null #!false #!true #!unspecified #!undefined
;; embedded vertical bars within symbols
;; #^B #^C #^F #^P #^G randomness (used in FASL files)

(define read-larceny-weirdness?
  (make-parameter  #t boolean?))

;; Enables:
;; vertical bars surrounding symbols
;; backslash escaping of symbols (disables *all* case-folding in symbol)
;; non-number implies symbol (at least for some things)
;; some nonstandard peculiar identifiers (-- -1+ 1+ 1-)
;; backslashes in strings before characters that don't have to be escaped
;; unconditional downcasing of the character following #
;; nonstandard character names (?)
;; #! ... !# comments (see lib/Standard/exec-comment.sch)
;; #.(...) read-time evaluation (see lib/Standard/sharp-dot.sch)
;; #&... (see lib/Standard/box.sch)

(define read-traditional-weirdness?
  (make-parameter #f boolean?))

;; Enables:
;; MzScheme #\uXX character notation
;; MzScheme #% randomness
;; #"..." randomness

(define read-mzscheme-weirdness?
  (make-parameter #f boolean?))

;; Enables:
;; R5RS lexical syntax, including the now-deprecated weird parts

(define read-r5rs-weirdness?
  (make-parameter  #t boolean?))

;; Enables:
;; R6RS lexical syntax

(define read-r6rs-weirdness?
  (make-parameter  #t boolean?))

;; Enables:
;; R7RS lexical syntax

(define read-r7rs-weirdness?
  (make-parameter  #t boolean?))

;; The following were supported in v0.93 but will not be in the
;; future:
;;
;; #r randomness (regular expressions?)
;; some kind of weirdness in flush-whitespace-until-rparen

;; This thunk is called whenever a #!fasl flag is read.
;; Its result is the value of the #!fasl flag.

(define fasl-evaluator
  (make-parameter  (lambda () (unspecified)) procedure?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Reader/writer mode of a textual input or output port.
;
; A binary port always has mode 0.
; The reader/writer mode of a textual port is encoded by a fixnum
; that is the inclusive or of
;
;     nofoldcase/foldcase:
;         0 means #!no-fold-case
;         1 means #!fold-case
;     source locations:
;         0 means don't record source locations
;         2 means record source locations
;     javadot:
;         0 means don't recognize JavaDot symbols
;         4 means recognize JavaDot symbols
;     flags:
;         0 means recognize all flags
;         8 means recognize #!r6rs flag only
;     weirdness:
;         0 means to enforce minimal lexical syntax
;        16 means to allow Larceny weirdness
;        32 means to allow traditional weirdness
;        64 means to allow MzScheme weirdness
;       128 means to allow R5RS weirdness
;       256 means to allow R6RS weirdness
;       512 means to allow R7RS weirdness
;
; If the weirdness is 0, then the port uses lexical conventions
; that approximate the intersection of R6RS and R7RS syntax.
; (For output ports, R7RS slashification may be necessary even
; when the weirdness is 0.)
;
; If the weirdness is 256, then strict R6RS lexical syntax
; should be enforced (modulo case and javadot, which may be
; allowed or disallowed independently).
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define readmode-mask:foldcase        1)
(define readmode-mask:locations       2)
(define readmode-mask:javadot         4)
(define readmode-mask:flags           8)
(define readmode-mask:weirdness    1008)    ; (+ 16 32 64 128 256 512)

(define readmode:binary               0)
(define readmode:nofoldcase           0)
(define readmode:foldcase             1)
(define readmode:nolocations          0)
(define readmode:locations            2)
(define readmode:nojavadot            0)
(define readmode:javadot              4)
(define readmode:noflags              0)
(define readmode:flags                8)
(define readmode:noweird              0)
(define readmode:larceny             16)
(define readmode:traditional         32)
(define readmode:mzscheme            64)
(define readmode:r5rs               128)
(define readmode:r6rs               256)
(define readmode:r7rs               512)

(define (default-read-mode)
  (define (default parameter iftrue iffalse)
    (if (parameter) iftrue iffalse))
  (+ (default case-sensitive?             readmode:nofoldcase
                                          readmode:foldcase)
     (default datum-source-locations?     readmode:locations
                                          readmode:nolocations)
     (default recognize-javadot-symbols?  readmode:javadot
                                          readmode:nojavadot)
     (default read-r6rs-flags?            readmode:flags
                                          readmode:noflags)
     (default read-larceny-weirdness?     readmode:larceny
                                          readmode:noweird)
     (default read-traditional-weirdness? readmode:traditional
                                          readmode:noweird)
     (default read-mzscheme-weirdness?    readmode:mzscheme
                                          readmode:noweird)
     (default read-r5rs-weirdness?        readmode:r5rs
                                          readmode:noweird)
     (default read-r6rs-weirdness?        readmode:r6rs
                                          readmode:noweird)
     (default read-r7rs-weirdness?        readmode:r7rs
                                          readmode:noweird)))


(define (io/complain-of-illegal-argument proc arg)
  (error proc "illegal argument" arg)
  #t)

(define (io/port-folds-case? p)
  (cond ((io/input-port? p)
         (eq? (logand readmode-mask:foldcase
                        (tuple-ref p port.readmode))
              readmode:foldcase))
        (else
         (io/complain-of-illegal-argument 'io/port-folds-case? p))))

(define (io/port-folds-case! p bool)
  (cond ((and (io/input-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (tuple-ref p port.readmode))
                (mode (logand mode (lognot readmode-mask:foldcase)))
                (mode (logior mode
                                (if bool
                                    readmode:foldcase
                                    readmode:nofoldcase))))
           (tuple-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          'io/port-folds-case!
          (if (boolean? bool) p bool)))))

(define (io/port-records-source-locations? p)
  (cond ((io/input-port? p)
         (eq? (logand readmode-mask:locations
                        (tuple-ref p port.readmode))
              readmode:locations))
        (else
         (io/complain-of-illegal-argument 'io/port-records-locations? p))))

(define (io/port-recognizes-javadot-symbols? p)
  (cond ((io/input-port? p)
         (eq? (logand readmode-mask:javadot
                        (tuple-ref p port.readmode))
              readmode:javadot))
        (else
         (io/complain-of-illegal-argument 'io/port-recognizes-javadot-symbols?
                                          p))))

(define (io/port-recognizes-javadot-symbols! p bool)
  (cond ((and (io/input-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (tuple-ref p port.readmode))
                (mode (logand mode (lognot readmode-mask:javadot)))
                (mode (logior mode
                                (if bool
                                    readmode:javadot
                                    readmode:nojavadot))))
           (tuple-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          'io/port-recognizes-javadot-symbols!
          (if (boolean? bool) p bool)))))

(define (io/port-allows-flags? p)
  (cond ((io/input-port? p)
         (eq? (logand readmode-mask:flags
                        (tuple-ref p port.readmode))
              readmode:flags))
        (else
         (io/complain-of-illegal-argument 'io/port-allows-flags? p))))

(define (allows-weirdness-getter p themode name)
  (cond ((and (io/open-port? p) (io/textual-port? p))
         (eq? (logand themode
                        (tuple-ref p port.readmode))
              themode))
        (else
         (io/complain-of-illegal-argument name p))))

(define (allows-weirdness-setter p bool themode name)
  (cond ((and (io/open-port? p) (io/textual-port? p) (boolean? bool))
         (let* ((mode (tuple-ref p port.readmode))
                (mode (logand mode (lognot themode)))
                (mode (logior mode
                                (if bool
                                    themode
                                    readmode:noweird))))
           (tuple-set! p port.readmode mode)))
        (else
         (io/complain-of-illegal-argument
          name
          (if (boolean? bool) p bool)))))

(define (io/port-allows-larceny-weirdness? p)
  (allows-weirdness-getter p
                           readmode:larceny
                           'io/port-allows-larceny-weirdness?))

(define (io/port-allows-larceny-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:larceny
                           'io/port-allows-larceny-weirdness!))

(define (io/port-allows-traditional-weirdness? p)
  (allows-weirdness-getter p
                           readmode:traditional
                           'io/port-allows-traditional-weirdness?))

(define (io/port-allows-traditional-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:traditional
                           'io/port-allows-traditional-weirdness!))

(define (io/port-allows-mzscheme-weirdness? p)
  (allows-weirdness-getter p
                           readmode:mzscheme
                           'io/port-allows-mzscheme-weirdness?))

(define (io/port-allows-mzscheme-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:mzscheme
                           'io/port-allows-mzscheme-weirdness!))

(define (io/port-allows-r5rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r5rs
                           'io/port-allows-r5rs-weirdness?))

(define (io/port-allows-r5rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r5rs
                           'io/port-allows-r5rs-weirdness!))

(define (io/port-allows-r6rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r6rs
                           'io/port-allows-r6rs-weirdness?))

(define (io/port-allows-r6rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r6rs
                           'io/port-allows-r6rs-weirdness!))

(define (io/port-allows-r7rs-weirdness? p)
  (allows-weirdness-getter p
                           readmode:r7rs
                           'io/port-allows-r7rs-weirdness?))

(define (io/port-allows-r7rs-weirdness! p bool)
  (allows-weirdness-setter p
                           bool
                           readmode:r7rs
                           'io/port-allows-r7rs-weirdness!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; R6RS i/o
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; R6RS port positions are a mess, mainly because they mimic
; a Posix feature that works only for file descriptors and
; whose semantics is specified in terms of bytes, not characters.
; As specified in Scheme, port positions also interact (badly)
; with other misfeatures, including input/output ports (whose
; semantics were modelled on Posix features that assume one
; byte per character) and custom ports (whose semantics mandates
; at least one character of lookahead buffering, but does not
; include any provision for buffer corrections when calculating
; port positions).
;
; In Larceny, all ports support the port-position operation.
; Binary ports report the position in bytes, and textual ports
; report the position in characters.
;
; For set-port-position!, the situation is more complex:
;
; some ports do not support set-port-position!
;     e.g. pipes and sockets
; binary file ports support set-port-position!
; custom ports may support set-port-position!
;     If so, then Larceny assumes their positions are reported
;     in bytes (for binary ports) or characters (for textual
;     ports) and relies on those assumptions to implement
;     buffer corrections.
; custom input/output (combined) ports must support set-port-position!
;     Larceny relies on set-port-position! to implement any buffer
;     correction required when switching from input to output.
; textual input file ports support set-port-position!
;     With Latin-1 or UTF-8 transcoding, the implementation
;     uses a cache and a port-position-in-bytes procedure.
;     If the position has not been cached, then a warning
;     is issued and the implementation proceeds as for UTF-16
;     transcoding.
;     With UTF-16 transcoding, set-port-position! is implemented
;     by resetting to the beginning of the file and reading the
;     specified number of characters.
; textual input/output file ports support set-port-position!
;     Implementation is the same as for input-only file ports.
; textual output-only files ports support set-port-position!
;     Buffer corrections are implemented using caching and
;     a port-position-in-bytes procedure that is part of the
;     port structure.
; bytevector and string ports support set-port-position!
;     They mimic file ports.
;
; For textual ports, if the argument to set-port-position! is
; nonzero and is also not the result of a previous call to
; port-position on the port, then an exception should be raised.
;
; A position cache that maps character positions to byte positions
; is maintained for textual ports that support set-port-position!.
; If the port uses variable-length transcoding (with a codec other
; than Latin-1, or an eol-style other than none), then the port
; must supply one of the following procedures in the port's alist:
;
;     port-position-in-bytes
;     port-position-in-chars (used by custom ports)

(define (io/port-position p)
  (if (io/binary-port? p)
      (io/port-position-nocache p)
      (let ((posn (io/port-position-nocache p)))

        (if (and (> posn 0)
                 (tuple-ref p port.setposn))

            ; Cache the position for future use by set-port-position!

            (let* ((posn (io/port-position-nocache p))
                   (t (io/port-transcoder p))
                   (alist (io/port-alist p))
                   (probe1 (assq 'port-position alist))
                   (port-position-in-chars (if probe1 (cdr probe1) #f))
                   (probe2 (assq 'port-position-in-bytes alist))
                   (port-position-in-bytes (if probe2 (cdr probe2) #f))
                   (probe3 (assq 'cached-positions alist))
                   (ht (if probe3
                           (cdr probe3)
                           (let* ((ht (make-core-hash-eqv))
                                  (entry (cons 'cached-positions ht)))
                             (io/port-alist-set! p (cons entry alist))
                             ht))))

              (cond ((and (eq? 'latin-1 (io/transcoder-codec t))
                          (eq? 'none (io/transcoder-eol-style t)))
                     (core-hash-set! ht posn posn))
                    (port-position-in-chars
                     (core-hash-set! ht posn posn))
                    (port-position-in-bytes
                     (let* ((byte-posn (port-position-in-bytes))
                            (byte-posn
                             (if (io/input-port? p)
                                 (- byte-posn
                                    (- (tuple-ref p port.mainlim)
                                       (tuple-ref p port.mainptr))
                                    (- (tuple-ref p port.auxlim)
                                       (tuple-ref p port.auxptr)))
                                 byte-posn)))
                       (core-hash-set! ht posn byte-posn)))
                    (else
                     (assertion-violation
                      'port-position
                      "internal error: no support for set-port-position!")))))

        posn)))

; Like io/port-position, but faster and more space-efficient
; because it doesn't cache.  The call to io/flush is necessary
; for correct caching when called by io/port-position above.

(define (io/port-position-nocache p)
  (cond ((io/input-port? p)
         (+ (tuple-ref p port.mainpos)
            (tuple-ref p port.mainptr)))
        ((io/output-port? p)
         (io/flush p)
         (+ (tuple-ref p port.mainpos)
            (tuple-ref p port.mainlim)))
        (else
         (error "io/port-position: " p " is not an open port.")
         #t)))

(define (io/port-lines-read p)
  (cond ((io/input-port? p)
         (tuple-ref p port.linesread))
        (else
         (error 'io/port-lines-read "not a textual input port" p)
         #t)))

(define (io/port-line-start p)
  (cond ((io/input-port? p)
         (tuple-ref p port.linestart))
        (else
         (error 'io/port-line-start "not a textual input port" p)
         #t)))

(define (io/port-has-set-port-position!? p)
  (cond ((port? p)
         (tuple-ref p port.setposn))
        (else
         (error 'io/has-set-port-position!? "illegal argument" p)
         #t)))

; FIXME: for textual output ports with variable-length encoding,
; any output operation should invalidate all cached positions
; that lie beyond the position of the output operation.

(define (io/set-port-position! p posn)
  (if (io/output-port? p)
      (io/flush p))
  (cond ((not (and (port? p)
                   (tuple-ref p port.setposn)))
         (error 'io/set-port-position! (errmsg 'msg:illegalarg1) p posn))
        ((eq? (tuple-ref p port.state) 'closed)
         (unspecified))
        ((not (and (exact? posn) (integer? posn)))
         (error 'io/set-port-position! "illegal argument" posn))
        ((or (= posn 0)
             (io/binary-port? p))
         (io/reset-buffers! p)
         (tuple-set! p port.mainpos posn)
         (io/set-port-position-as-binary! p posn))
        (else

         ; Lookup the corresponding byte position.

         (let* ((t (io/port-transcoder p))
                (codec (io/transcoder-codec t))
                (input? (io/input-port? p))
                (output? (io/output-port? p))
                (alist (io/port-alist p))
                (probe1 (assq 'port-position alist))
                (port-position-in-chars (if probe1 (cdr probe1) #f))
                (probe2 (assq 'port-position-in-bytes alist))
                (port-position-in-bytes (if probe2 (cdr probe2) #f))
                (probe3 (assq 'cached-positions alist))
                (ht (if probe3 (cdr probe3) #f))
                (byte-posn (and ht (core-hash-ref ht posn #f))))

           (define (reposition!)
             (io/reset-buffers! p)
             (tuple-set! p port.mainpos posn)
             (io/set-port-position-as-binary! p byte-posn))

           (define (read-chars-loop n)
             (if (> n 0)
                 (begin
                   (read-char p)
                   (read-chars-loop (- n 1)))))

           ; We can't enforce the R6RS restriction for combined
           ; input/output ports because it may be a lookahead correction.

           (cond ((or byte-posn
                      (and input? output?))
                  (reposition!))
                 (else
                  ; error case: posn > 0 and not in cache
                  (if (not (issue-deprecated-warnings?))
                      (assertion-violation 'set-port-position!
                                           (errmsg 'msg:uncachedposition)
                                           p posn)
                      ; FIXME: ad hoc warning message
                      (let ((out (current-error-port)))
                        (display "Warning from set-port-position!: " out)
                        (newline out)
                        (display (errmsg 'msg:uncachedposition) out)
                        (display ": " out)
                        (write posn out)
                        (newline out)
                        ; Attempt the operation anyway.  Hey, it might work.
                        (cond ((or port-position-in-chars
                                   (and
                                    (eq? 'latin-1 codec)
                                    (eq? 'none (io/transcoder-eol-style t))))
                               (reposition!))
                              ((io/input-port? p)
                               (io/set-port-position! p 0)
                               (read-chars-loop posn))
                              (else
                               (reposition!))))))))))

  (unspecified))

(define (io/set-port-position-as-binary! p posn)
  (let ((r (((tuple-ref p port.ioproc) 'set-position!)
            (tuple-ref p port.iodata)
            posn)))
    (cond ((eq? r 'ok)
           (if (eq? (tuple-ref p port.state) 'eof)
               (tuple-set!
                p
                port.state
                (if (binary-port? p) 'binary 'textual)))
           (unspecified))
          (else
           (error 'set-port-position! "io error" p posn)))))

(define (io/port-transcoder p)
  (assert (port? p))
  (tuple-ref p port.transcoder))

(define (io/textual-port? p)
  (assert (port? p))
  (not (= 0 (logand type-mask:binary/textual
                        (tuple-ref p port.type)))))

(define (io/r7rs-textual-port? p)
  (assert (port? p))
  (not (= 0 (logand type-mask:binary/textual
                        (tuple-ref p port.r7rstype)))))

(define (io/binary-port? p)
  (assert (port? p))
  (= 0 (logand type-mask:binary/textual (tuple-ref p port.type))))

(define (io/r7rs-binary-port? p)
  (assert (port? p))
  (= 0 (logand type-mask:binary/textual
                   (tuple-ref p port.r7rstype))))

; Transcoders et cetera.
;
; Internally, transcoders are represented as fixnums of the form
; xxxyyyzz, where xxx encodes a codec, yyy encodes an eol style,
; and zz encodes an error handling mode.
;
; codec (3 bits):
;     000 means none (port is binary)
;     001 means Latin-1
;     010 means UTF-8
;     011 means UTF-16
;     111 means UTF-16 in little-endian mode (internal only)
; 
; eol style (3 bits):
;     000 means none
;     001 means lf
;     010 means nel
;     011 means ls
;     100 means cr
;     101 means crlf
;     110 means crnel
;
; error handling mode (2 bits):
;     00 means ignore
;     01 means replace
;     10 means raise
;
; FIXME:  The external world should see a more self-explanatory
; representation of transcoders, and the three accessors for
; transcoders should return the original symbols instead of
; their canonical equivalents.

(define transcoder-mask:codec    #b11100000)
(define transcoder-mask:eolstyle #b00011100)
(define transcoder-mask:errmode  #b00000011)

(define codec:binary  0)
(define codec:latin-1 #b00100000)
(define codec:utf-8   #b01000000)
(define codec:utf-16  #b01100000)

(define eolstyle:none  #b00000)
(define eolstyle:lf    #b00100)
(define eolstyle:nel   #b01000)
(define eolstyle:ls    #b01100)
(define eolstyle:cr    #b10000)
(define eolstyle:crlf  #b10100)
(define eolstyle:crnel #b11000)

(define errmode:ignore  0)
(define errmode:replace 1)
(define errmode:raise   2)

; May be redefined at startup as specified by system-features.
; (See also command-line processing).

(define default-transcoder
  (make-parameter
                  codec:utf-8
                  (lambda (t)
                    (and (fixnum? t)
                         (<= codec:latin-1 t transcoder-mask:codec)))))

; In Larceny, *every* symbol names an end-of-line style,
; and *every* symbol names an error handling mode.

(define (io/make-transcoder codec eol-style handling-mode)
  (define (local-error msg irritant)
    (if (issue-deprecated-warnings?)
        (let ((out (current-error-port)))
          (display "Warning: " out)
          (display msg out)
          (display ": " out)
          (write irritant out)
          (newline out)
          (display "Using Larceny-specific interpretation." out)
          (newline out))))
  (let ((bits:codec (case codec
                     ((latin-1) codec:latin-1)
                     ((utf-8)   codec:utf-8)
                     ((utf-16)  codec:utf-16)
                     (else (local-error "nonstandard codec" codec)
                           codec:latin-1)))
        (bits:eol (case eol-style
                   ((none)  eolstyle:none)
                   ((lf)    eolstyle:lf)
                   ((nel)   eolstyle:nel)
                   ((ls)    eolstyle:ls)
                   ((cr)    eolstyle:cr)
                   ((crlf)  eolstyle:crlf)
                   ((crnel) eolstyle:crnel)
                   (else (local-error "nonstandard eol style" eol-style)
                         eolstyle:none)))
        (bits:ehm (case handling-mode
                   ((ignore)  errmode:ignore)
                   ((replace) errmode:replace)
                   ((raise)   errmode:raise)
                   (else (local-error "nonstandard error handling mode"
                                      handling-mode)
                         errmode:replace))))
    (+ bits:codec bits:eol bits:ehm)))

; Programmers should never see a transcoder with binary codec.

(define (io/transcoder-codec t)
  (let ((codec (logand t transcoder-mask:codec)))

    (cond ((= codec codec:binary)  'binary)
          ((= codec codec:latin-1) 'latin-1)
          ((= codec codec:utf-8)   'utf-8)
          ((= codec codec:utf-16)  'utf-16)
          (else
           (assertion-violation 'transcoder-codec
                                "weird transcoder" t)))))

(define (io/transcoder-eol-style t)
  (let ((style (logand t transcoder-mask:eolstyle)))
    (cond ((= style eolstyle:none)  'none)
          ((= style eolstyle:lf)    'lf)
          ((= style eolstyle:nel)   'nel)
          ((= style eolstyle:ls)    'ls)
          ((= style eolstyle:cr)    'cr)
          ((= style eolstyle:crlf)  'crlf)
          ((= style eolstyle:crnel) 'crnel)
          (else
           (assertion-violation 'transcoder-eol-style
                                "weird transcoder" t)))))

(define (io/transcoder-error-handling-mode t)
  (let ((mode (logand t transcoder-mask:errmode)))
    (cond ((= mode errmode:ignore)  'ignore)
          ((= mode errmode:replace) 'replace)
          ((= mode errmode:raise)   'raise)
          (else
           (assertion-violation 'transcoder-error-handling-mode
                                "weird transcoder" t)))))

; Like transcoded-port, but performs less error checking.

(define (io/transcoded-port p t)
  (assert (io/open-port? p))
  (assert (eq? 'binary (tuple-ref p port.state)))
  (if (io/output-port? p)
      (io/flush p))

  (if (not (memq (transcoder-codec t) '(latin-1 utf-8)))
      (io/transcoded-port-random p t)

      ; shallow copy

      (let ((newport (io/clone-port p)))
        (tuple-set! newport
                          port.type
                          (logior type:textual
                                    (tuple-ref p port.type)))
        (tuple-set! newport
                          port.r7rstype
                          (tuple-ref newport port.type))
        (tuple-set! newport port.transcoder t)
        (tuple-set! newport port.state 'textual)
        (tuple-set! newport port.readmode (default-read-mode))

        ; io/transcode-port! expects a newly filled mainbuf,
        ; so we have to fake it here.
        ;
        ; FIXME: Is the above really true?
    
        (let* ((mainbuf1 (tuple-ref p port.mainbuf))
               (mainptr1 (tuple-ref p port.mainptr))
               (mainlim1 (tuple-ref p port.mainlim))
               (mainbuf2 (make-bytevector (bytevector-length mainbuf1)))
               (mainlim2 (- mainlim1 mainptr1)))

          ; FIXME:  Unclear what port-position should do.

          (tuple-set! newport port.mainpos 0)

          (r6rs:bytevector-copy! mainbuf1
                                 mainptr1
                                 mainbuf2 0 mainlim2)
          (tuple-set! newport port.mainbuf mainbuf2)
          (tuple-set! newport port.mainptr 0)
          (tuple-set! newport port.mainlim mainlim2))

        ; close original port, destroying original mainbuf

        (io/set-closed-state! p)

        (cond ((and (io/input-port? newport)
                    (io/output-port? newport))
               (assert #t))
              ((io/input-port? newport)
               (io/transcode-port! newport)))
        newport)))

; Latin-1 and UTF-8 are transcoded on the fly, but other
; codecs are transcoded by interposing an extra binary port.

(define (io/transcoded-port-random p t)

  ; FIXME: for now, we support only Latin-1, UTF-8, and UTF-16.

  (assert (eq? (transcoder-codec t) 'utf-16))

  ; shallow copy

  (let* ((newport (io/clone-port p))
         (mainbuf1 (tuple-ref p port.mainbuf))
         (mainptr1 (tuple-ref p port.mainptr))
         (mainlim1 (tuple-ref p port.mainlim))
         (mainbuf2 (make-bytevector (bytevector-length mainbuf1))))

      (r6rs:bytevector-copy! mainbuf1
                             mainptr1
                             mainbuf2 0 (- mainlim1 mainptr1))
      (tuple-set! newport port.mainbuf mainbuf2)

      ; close original port, destroying original mainbuf

      (io/set-closed-state! p)

      (let* ((p (utf16/transcoded-binary-port newport))
             (t (make-transcoder (utf-8-codec)
                                 (transcoder-eol-style t)
                                 (transcoder-error-handling-mode t))))

        ; FIXME: this will look like it's transcoded as UTF-8

        (io/transcoded-port p t))))

; Like transcoded-port but preserves slightly different state
; and uses the correct transcoder for custom textual ports.

(define (io/custom-transcoded-port p)
  (assert (port? p))
  (let* ((t (make-transcoder (utf-8-codec) 'none 'ignore))
         (newport (io/transcoded-port p t)))
    newport))
