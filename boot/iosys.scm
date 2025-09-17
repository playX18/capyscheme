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
(define port.wr-flush? 11) ; boolean: true iff bufmode is datum

; textual input ports

(define port.auxbuf    12) ; bytevector: 4 bytes before or after mainbuf
(define port.auxptr    13) ; fixnum: index of next byte in auxbuf
(define port.auxlim    14) ; fixnum: 1 + index of last byte in auxbuf
(define port.linesread 15) ; integer: number of line endings read so far
(define port.linestart 16) ; integer: character position after last line ending
(define port.wasreturn 17) ; boolean: last line ending was #\return
(define port.readmode  18) ; fixnum: see comment before default-read-mode

; all ports

(define port.setposn   19) ; boolean: true iff supports set-port-position!
(define port.alist     20) ; association list: used mainly by custom ports
(define port.r7rstype  21) ; copy of port.type but unaltered by closing

(define port.structure-size 22)      ; size of port structure

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
    (let ([v (make-tuple port.structure-size #f)]
          [input? #f]
          [output? #f]
          [binary? #f]
          [textual? #f]
          [set-position? #f])
        (tuple-set! v port.bufmode 'block)
        (for-each 
            (lambda (keyword)
                (cond 
                 [(eq? keyword 'input) (set! input? #t)]
                 [(eq? keyword 'output) (set! output? #t)]
                 [(eq? keyword 'binary) (set! binary? #t)]
                 [(eq? keyword 'textual) (set! textual? #t)]
                 [(eq? keyword 'set-position!) (set! set-position? #t)]
                 [(eq? keyword 'none) (tuple-set! v port.bufmode 'none)]
                 [(eq? keyword 'line) (tuple-set! v port.bufmode 'line)]
                 [(or (eq? keyword 'datum) (eq? keyword 'flush))
                  (tuple-set! v port.wr-flush? #t)
                  (tuple-set! v port.bufmode 'datum)]
                 [(eq? keyword 'block) (tuple-set! v port.bufmode 'block)]
                 [else (assertion-violation 'io/make-port "bad attributes" rest)]))
            rest)
        (if (and binary? textual?)
            (assertion-violation 'io/make-port "cannot be both binary and textual" rest))
        
        (tuple-set! v
                    port.type
                    (cond 
                        [(and binary? input? output?) type:binary-input/output]
                        [(and binary? input?) type:binary-input]
                        [(and binary? output?) type:binary-output]
                        [(and textual? input? output?) type:textual-input/output]
                        [(and textual? input?) type:textual-input]
                        [(and textual? output?) type:textual-output]
                        [input? type:textual-input]
                        [output? type:textual-output]
                        [else (assertion-violation 'io/make-port "must be input or output" rest)]))
        
        (tuple-set! v port.mainbuf (make-bytevector port.mainbuf-size))
        (tuple-set! v port.mainptr 0)
        (tuple-set! v port.mainlim 0)
        (tuple-set! v port.mainpos 0)
        (tuple-set! v port.transcoder (if binary? 0 (native-transcoder)))
        (tuple-set! v port.state (if binary? 'binary 'textual))
        (tuple-set! v port.iodata iodata)
        (tuple-set! v port.ioproc ioproc)
        (tuple-set! v port.auxbuf (make-bytevector 4))
        (tuple-set! v port.auxlim 0)
        (tuple-set! v port.linesread 0)
        (tuple-set! v port.linestart 0)
        (tuple-set! v port.wasreturn #f)
        (tuple-set! v port.readmode (if (not binary?) (default-read-mode) readmode:binary))
        (tuple-set! v port.setposn set-position?)
        (tuple-set! v port.alist '())
        (tuple-set! v port.r7rstype (tuple-ref v port.type))
        (tuple-set! v 0 'type:port)
        v))

(define (default-read-mode) readmode:r7rs)

(define (io/input-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (tuple-ref p port.type))))
         (fx= type:input (fxlogand direction type:input)))))

(define (io/output-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (tuple-ref p port.type))))
         (fx= type:output (fxlogand direction type:output)))))

(define (io/r7rs-input-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (tuple-ref p port.r7rstype))))
         (fx= type:input (fxlogand direction type:input)))))

(define (io/r7rs-output-port? p)
  (and (port? p)
       (let ((direction (fxlogand type-mask:direction
                                  (tuple-ref p port.r7rstype))))
         (fx= type:output (fxlogand direction type:output)))))

(define (io/open-port? p)
  (or (io/input-port? p) (io/output-port? p)))

(define (io/buffer-mode p)
  (if (not (port? p))
      (assertion-violation 'io/buffer-mode "not a port" p))
  (tuple-ref p port.bufmode))

(define (port? p)
    (and (tuple? p)
         (eq? (tuple-ref p 0) 'type:port)))

(define default-transcoder (make-parameter 'codec:latin-1))


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