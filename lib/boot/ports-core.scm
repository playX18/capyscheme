;;; Implementation of Scheme ports, based on R7RS (section 6.13) and R6RS standard libraries.
;;; This file contains the port representation, the
;;; port-handler dispatch protocol, the byte buffer machinery, state
;;; handling, and position operations.
;;;
;;; A port is a tuple with the following fields:
;;;
;;;   0  tag            '<port>
;;;   1  name           string               human-readable name
;;;   2  direction      'input | 'output | 'input-output
;;;   3  transcoder     <transcoder> | #f    #f means binary port
;;;   4  buffer-mode    'none | 'line | 'block
;;;   5  flush-datum?   boolean              flush after each datum (display/write)
;;;   6  state          'open | 'eof | 'closed
;;;   7  handler        <port-handler>       underlying I/O procedures
;;;   8  in-buf         bytevector           input buffer
;;;   9  in-ptr         fixnum               next unread byte
;;;   10 in-lim         fixnum               end of buffered input
;;;   11 out-buf        bytevector           output buffer
;;;   12 out-lim        fixnum               end of buffered output
;;;   13 char-pos       integer              characters consumed / written
;;;   14 lines-read     integer              line endings consumed / written
;;;   15 line-start     integer              character position after last line ending
;;;   16 codec-state    #f | (vector 'utf16 endianness bom-written?)
;;;   17 anchors        alist                (char-pos . byte-pos), most recent first
;;;   18 reader         #f | datum reader    cached reader (boot/reader.scm)
;;;   19 fd             fixnum | #f          file descriptor, if fd-backed
;;;   20 custom?        boolean              true for user-supplied custom ports
;;;   21 codec          #f | 0 latin-1 | 1 utf-8 | 2 utf-16   (cached from transcoder)
;;;   22 eol            eol style symbol     (cached from transcoder)
;;;   23 errmode        error handling mode  (cached from transcoder)
;;;   24 anchor-next    fixnum               char-pos at which to record the next anchor
;;;   25 memory         #f | 'in | 'out | 'sout
;;;   26 hw            fixnum                memory output: high-water mark
;;;   27 pending-char   #f | fixnum          decoded replacement char awaiting delivery
;;;
;;; Memory ports ('in / 'out / 'sout) have no handler traffic: an input
;;; memory port's in-buf *is* the source bytevector, and an output memory
;;; port's out-buf *is* the growing result.  'sout is a string output port.
;;;
;;; Position model:
;;;
;;;   Binary ports: the position is a non-negative exact integer byte
;;;   offset, computed from the handler's get-position (the underlying
;;;   source position) adjusted for buffered bytes.
;;;
;;;   Textual ports: port-position returns the number of characters
;;;   consumed or written (an exact integer which is acceptable as an R6RS
;;;   implementation-dependent position value).  set-port-position! on
;;;   a built-in textual port maps character positions to byte offsets
;;;   via recorded anchors; on a custom textual port it delegates to
;;;   the user-supplied set-position!.
(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (late (boot transcoder) %ports/transcoder-codec-raw %ports/transcoder-eol-raw %ports/transcoder-errmode-raw) (late (boot port-api) current-output-port) (late (boot textual-ports) get-char lookahead-char) (late (boot binary-ports) lookahead-u8))
(|#%provide| all-defined)

(define %port-tag '<port>)

(define (%make-port-tuple name direction transcoder buffer-mode flush-datum? state
          handler in-buf in-ptr in-lim out-buf out-lim char-pos lines-read
          line-start codec-state anchors reader fd custom? codec eol errmode
          anchor-next memory hw pending-char)
  (tuple %port-tag name direction transcoder buffer-mode flush-datum? state
    handler in-buf in-ptr in-lim out-buf out-lim char-pos lines-read
    line-start codec-state anchors reader fd custom? codec eol errmode
    anchor-next memory hw pending-char))

(define (%port? x)
  (and (tuple? x)
    (> (tuple-size x) 0)
    (eq? (tuple-ref x 0) %port-tag)))

(define (%port-name p) (tuple-ref p 1))
(define (%port-direction p) (tuple-ref p 2))
(define (%port-transcoder p) (tuple-ref p 3))
(define (%port-buffer-mode p) (tuple-ref p 4))
(define (%port-flush-datum? p) (tuple-ref p 5))
(define (%port-state p) (tuple-ref p 6))
(define (%port-handler p) (tuple-ref p 7))
(define (%port-in-buf p) (tuple-ref p 8))
(define (%port-in-ptr p) (tuple-ref p 9))
(define (%port-in-lim p) (tuple-ref p 10))
(define (%port-out-buf p) (tuple-ref p 11))
(define (%port-out-lim p) (tuple-ref p 12))
(define (%port-char-pos p) (tuple-ref p 13))
(define (%port-lines-read p) (tuple-ref p 14))
(define (%port-line-start p) (tuple-ref p 15))
(define (%port-codec-state p) (tuple-ref p 16))
(define (%port-anchors p) (tuple-ref p 17))
(define (%port-reader p) (tuple-ref p 18))
(define (%port-fd p) (tuple-ref p 19))
(define (%port-custom? p) (tuple-ref p 20))
(define (%port-codec p) (tuple-ref p 21))
(define (%port-eol p) (tuple-ref p 22))
(define (%port-errmode p) (tuple-ref p 23))
(define (%port-anchor-next p) (tuple-ref p 24))
(define (%port-memory p) (tuple-ref p 25))
(define (%port-hw p) (tuple-ref p 26))
(define (%port-pending-char p) (tuple-ref p 27))

(define (%port-transcoder-set! p v) (tuple-set! p 3 v))
(define (%port-buffer-mode-set! p v) (tuple-set! p 4 v))
(define (%port-flush-datum?-set! p v) (tuple-set! p 5 v))
(define (%port-state-set! p v) (tuple-set! p 6 v))
(define (%port-in-buf-set! p v) (tuple-set! p 8 v))
(define (%port-in-ptr-set! p v) (tuple-set! p 9 v))
(define (%port-in-lim-set! p v) (tuple-set! p 10 v))
(define (%port-out-buf-set! p v) (tuple-set! p 11 v))
(define (%port-out-lim-set! p v) (tuple-set! p 12 v))
(define (%port-char-pos-set! p v) (tuple-set! p 13 v))
(define (%port-lines-read-set! p v) (tuple-set! p 14 v))
(define (%port-line-start-set! p v) (tuple-set! p 15 v))
(define (%port-codec-state-set! p v) (tuple-set! p 16 v))
(define (%port-anchors-set! p v) (tuple-set! p 17 v))
(define (%port-reader-set! p v) (tuple-set! p 18 v))
(define (%port-fd-set! p v) (tuple-set! p 19 v))
(define (%port-anchor-next-set! p v) (tuple-set! p 24 v))
(define (%port-memory-set! p v) (tuple-set! p 25 v))
(define (%port-hw-set! p v) (tuple-set! p 26 v))
(define (%port-pending-char-set! p v) (tuple-set! p 27 v))

;;; The port-handler protocol mirrors the R6RS custom-port procedures:
;;;
;;;   read!        (bytevector start count) -> fixnum bytes read (0 = EOF)
;;;   write!       (bytevector start count) -> fixnum bytes written
;;;   close        () -> unspecified
;;;   ready?       () -> boolean
;;;   get-position () -> fixnum | #f       (underlying source position)
;;;   set-position! (fixnum) -> unspecified
;;;
;;; Any procedure may be #f. Reading requires read!, writing requires
;;; write!, closing uses close (or a no-op), and position support is
;;; detected by the presence of get-position / set-position!.

(define %handler-tag '<port-handler>)

(define (%make-handler read! write! close ready? get-position set-position! data)
  (tuple %handler-tag read! write! close ready? get-position set-position! data))

(define (%handler? x)
  (and (tuple? x) (> (tuple-size x) 0) (eq? (tuple-ref x 0) %handler-tag)))
(define (%handler-read! h) (tuple-ref h 1))
(define (%handler-write! h) (tuple-ref h 2))
(define (%handler-close h) (tuple-ref h 3))
(define (%handler-ready? h) (tuple-ref h 4))
(define (%handler-get-position h) (tuple-ref h 5))
(define (%handler-set-position! h) (tuple-ref h 6))
(define (%handler-data h) (tuple-ref h 7))

;;; Default buffer sizes.

(define %ports/buffer-size 4096)
(define %ports/file-buffer-size 16384)

;;; Smallest memory output buffer; it doubles on demand.
(define %ports/memory-initial-size 64)

;;; Shared zero-length buffer for the unused side of a port.
(define %ports/empty-buffer (make-bytevector 0 0))

;;; A position that no port reaches: used to disable anchor recording.
(define %ports/never 1099511627776)

(define (%ports/codec-code codec)
  (case codec
    ((latin-1) 0)
    ((utf-8) 1)
    ((utf-16) 2)
    (else #f)))

;;; Allocates a fresh port in the open state.  Options are the symbols
;;; 'flush 'datum 'custom, (fd n), (buffer-size n), and (memory kind).
(define (%ports/make-port name direction transcoder buffer-mode handler . opts)
  (define flush-datum? #f)
  (define fd #f)
  (define custom? #f)
  (define memory #f)
  (define buffer-size #f)
  (for-each
    (lambda (opt)
      (cond
        ((eq? opt 'flush) (set! flush-datum? #t))
        ((eq? opt 'datum) (set! flush-datum? #t))
        ((eq? opt 'custom) (set! custom? #t))
        ((and (pair? opt) (eq? (car opt) 'fd))
         (set! fd (cadr opt)))
        ((and (pair? opt) (eq? (car opt) 'buffer-size))
         (set! buffer-size (cadr opt)))
        ((and (pair? opt) (eq? (car opt) 'memory))
         (set! memory (cadr opt)))
        (else
          (assertion-violation '%ports/make-port "invalid port attribute" opt))))
    opts)
  (let* ([input? (or (eq? direction 'input) (eq? direction 'input-output))]
         [output? (or (eq? direction 'output) (eq? direction 'input-output))]
         [size (or buffer-size %ports/buffer-size)]
         [codec (and transcoder (%ports/codec-code (%ports/transcoder-codec-raw transcoder)))]
         [p (%make-port-tuple
              name
              direction
              transcoder
              buffer-mode
              flush-datum?
              'open
              handler
              (if (and input? (not memory)) (make-bytevector size 0) %ports/empty-buffer)
              0
              0
              (cond (memory %ports/empty-buffer)
                (output? (make-bytevector size 0))
                (else %ports/empty-buffer))
              0
              0
              0
              0
              (if (eqv? codec 2) (vector 'utf16 #f #f) #f)
              '()
              #f
              fd
              custom?
              codec
              (if transcoder (%ports/transcoder-eol-raw transcoder) 'none)
              (if transcoder (%ports/transcoder-errmode-raw transcoder) 'replace)
              (if (and transcoder input? (not custom?)) 4096 %ports/never)
              memory
              0
              #f)])
    (when (and fd output?)
      (%ports/register-output! p))
    p))

;;; Convenience accessors used across the port files.

(define (%ports/input-port? p)
  (let ([d (%port-direction p)])
    (or (eq? d 'input) (eq? d 'input-output))))
(define (%ports/output-port? p)
  (let ([d (%port-direction p)])
    (or (eq? d 'output) (eq? d 'input-output))))
(define (%ports/input-output? p)
  (eq? (%port-direction p) 'input-output))

(define (%ports/open? p)
  (not (eq? (%port-state p) 'closed)))

(define (%ports/textual? p)
  (and (%port-transcoder p) #t))

(define (%ports/binary? p)
  (not (%port-transcoder p)))

;;; Predicates combining kind, direction, and open state.  These are the
;;; guards of every public read/write operation.
(define (%ports/open-input? p)
  (and (%port? p)
    (not (eq? (%port-state p) 'closed))
    (%ports/input-port? p)))

(define (%ports/open-output? p)
  (and (%port? p)
    (not (eq? (%port-state p) 'closed))
    (%ports/output-port? p)))

(define (%ports/textual-input? p)
  (and (%ports/open-input? p) (%port-transcoder p) #t))

(define (%ports/binary-input? p)
  (and (%ports/open-input? p) (not (%port-transcoder p))))

(define (%ports/textual-output? p)
  (and (%ports/open-output? p) (%port-transcoder p) #t))

(define (%ports/binary-output? p)
  (and (%ports/open-output? p) (not (%port-transcoder p))))

;;; ---------------------------------------------------------------
;;; Registry of fd-backed output ports, flushed at exit.
;;; ---------------------------------------------------------------

(define %ports/open-outputs '())

(define (%ports/register-output! p)
  (set! %ports/open-outputs (cons p %ports/open-outputs)))

(define (%ports/unregister-output! p)
  (set! %ports/open-outputs
    (let loop ([l %ports/open-outputs])
      (cond ((null? l) '())
        ((eq? (car l) p) (cdr l))
        (else (cons (car l) (loop (cdr l))))))))

;;; Flushes every registered open output port, ignoring failures (the
;;; process is going away; a broken pipe must not mask the exit).
(define (%ports/flush-all!)
  (for-each
    (lambda (p)
      (when (%ports/open? p)
        (call/cc
          (lambda (k)
            (with-exception-handler
              (lambda (e) (k #f))
              (lambda () (%ports/flush-buffer p)))))))
    %ports/open-outputs))

;;; ---------------------------------------------------------------
;;; Byte-level machinery.
;;; ---------------------------------------------------------------

;;; The number of bytes currently held in the input buffer.
(define (%ports/buffered-input p)
  (- (%port-in-lim p) (%port-in-ptr p)))

;;; The underlying source position, from the handler if available,
;;; else #f.
(define (%ports/underlying-position p)
  (let ([memory (%port-memory p)])
    (cond ((not memory)
           (let ([g (%handler-get-position (%port-handler p))])
             (and g (g))))
      ((eq? memory 'in) (%port-in-lim p))
      (else 0))))

;;; The current position in bytes:
;;;   input:   underlying position - unread buffered bytes
;;;   output:  underlying position + buffered output bytes
;;; For output-only ports the input buffer is always empty and for
;;; input-only ports the output buffer is always empty, so the two
;;; adjustments never overlap.  On input/output ports at most one side
;;; is buffered at a time (output is write-through).
(define (%ports/byte-position p)
  (let* ([g (%ports/underlying-position p)]
         [unread (%ports/buffered-input p)]
         [out (%port-out-lim p)])
    (if g
      (+ (- g unread) out)
      (+ (- 0 unread) out))))

;;; Advances the input pointer by n already-buffered bytes.

(define (%ports/advance! p n)
  (%port-in-ptr-set! p (+ (%port-in-ptr p) n)))

;;; Fills the input buffer from the handler.  Returns #t if new data
;;; was read, #f at end of file.  Consumed bytes are compacted to the
;;; front of the buffer.  On input/output ports the read-ahead is
;;; limited so that switching to output can be corrected with a cheap
;;; seek.

(define (%ports/refill! p)
  (if (%port-memory p)
    (begin (%port-state-set! p 'eof) #f)
    (let* ([buf (%port-in-buf p)]
           [ptr (%port-in-ptr p)]
           [lim (%port-in-lim p)]
           [size (bytevector-length buf)]
           [read! (%handler-read! (%port-handler p))])
      (when (> ptr 0)
        (r6rs:bytevector-copy! buf ptr buf 0 (- lim ptr))
        (%port-in-ptr-set! p 0)
        (%port-in-lim-set! p (- lim ptr)))
      (let* ([start (%port-in-lim p)]
             [count (cond ((%ports/input-output? p)
                           ;; read only enough for one character so that
                           ;; switching to output needs only a small seek
                           (if (%ports/textual? p) 4 1))
                      (else (- size start)))]
             [n (read! buf start count)])
        (cond ((or (eq? n 'eof) (and (fixnum? n) (= n 0)))
               (%port-state-set! p 'eof)
               #f)
          ((and (fixnum? n) (> n 0) (<= n count))
            (%port-in-lim-set! p (+ start n))
            #t)
          (else
            (%port-state-set! p 'eof)
            (assertion-violation 'port-input
              "port handler returned an invalid read count"
              n
              p)))))))

;;; Makes at least n bytes available in the input buffer, reading from
;;; the handler as needed.  Returns #t on success and #f if the stream
;;; ended first.  Never reads more than the handler's one call per
;;; iteration, so a terminal is only waited on for bytes that are needed.

(define (%ports/ensure-input p n)
  (let loop ()
    (cond ((>= (- (%port-in-lim p) (%port-in-ptr p)) n) #t)
      ((eq? (%port-state p) 'eof) #f)
      ((%ports/refill! p) (loop))
      (else #f))))

;;; Returns the next byte from the port without consuming it, or the
;;; eof object.

(define (%ports/peek-byte p)
  (when (%ports/input-output? p)
    (%ports/flush-buffer p))
  (let ([ptr (%port-in-ptr p)])
    (if (< ptr (%port-in-lim p))
      (bytevector-u8-ref (%port-in-buf p) ptr)
      (if (%ports/ensure-input p 1)
        (bytevector-u8-ref (%port-in-buf p) (%port-in-ptr p))
        (eof-object)))))

;;; Returns the next byte from the port, or the eof object.

(define (%ports/get-byte p)
  (when (%ports/input-output? p)
    (%ports/flush-buffer p))
  (let ([ptr (%port-in-ptr p)])
    (if (< ptr (%port-in-lim p))
      (let ([b (bytevector-u8-ref (%port-in-buf p) ptr)])
        (%port-in-ptr-set! p (+ ptr 1))
        b)
      (if (%ports/ensure-input p 1)
        (%ports/get-byte p)
        (eof-object)))))

;;; Flushes the output buffer to the handler.  Memory ports accumulate
;;; in place and have nothing to flush.  The buffer always describes
;;; exactly the bytes not yet written, so a failed write can be retried
;;; (or the port closed) without losing or duplicating data.

(define (%ports/flush-buffer p)
  (unless (%port-memory p)
    (let ([lim (%port-out-lim p)])
      (when (> lim 0)
        (let ([write! (%handler-write! (%port-handler p))]
              [buf (%port-out-buf p)])
          (let loop ([count lim])
            (let ([n (write! buf (- lim count) count)])
              (cond ((and (fixnum? n) (> n 0) (<= n count))
                     (if (= n count)
                       (%port-out-lim-set! p 0)
                       (begin
                         ;; partial write: keep only the unwritten tail
                         (r6rs:bytevector-copy! buf (+ (- lim count) n) buf 0 (- count n))
                         (%port-out-lim-set! p (- count n))
                         (set! lim (- count n))
                         (loop (- count n)))))
                (else
                  (raise-i/o-write-error
                    'flush-output-port
                    "write failed"
                    p)))))))))
  (unspecified))

;;; Makes room for n more bytes at the end of the output buffer (n must
;;; not exceed the buffer size for non-memory ports).  Memory ports grow
;;; by doubling; others flush.

(define (%ports/reserve! p n)
  (let* ([buf (%port-out-buf p)]
         [lim (%port-out-lim p)]
         [cap (bytevector-length buf)])
    (when (> (+ lim n) cap)
      (if (%port-memory p)
        (%ports/grow-memory! p (+ lim n))
        (%ports/flush-buffer p))))
  (unspecified))

;;; Grows a memory output buffer to hold at least need bytes.

(define (%ports/grow-memory! p need)
  (let* ([buf (%port-out-buf p)]
         [cap (bytevector-length buf)])
    (when (> need cap)
      (let ([new (make-bytevector (max need (* 2 cap) %ports/memory-initial-size) 0)])
        (r6rs:bytevector-copy! buf 0 new 0 (%port-out-lim p))
        (%port-out-buf-set! p new)))))

;;; The current extent of the data in a memory output port.
(define (%ports/memory-size p)
  (max (%port-hw p) (%port-out-lim p)))

;;; Before writing on an input/output port, move the underlying
;;; position back to the current read position (there may be buffered
;;; input) and discard that input.
(define (%ports/prepare-write! p)
  (let ([unread (%ports/buffered-input p)])
    (when (> unread 0)
      (let* ([g (%ports/underlying-position p)]
             [target (if g (- g unread) 0)])
        (%ports/seek-bytes! p target)))))

;;; Applies the buffer mode after bytes were appended; newline? tells
;;; whether the data just written ended a line.
(define (%ports/after-write! p newline?)
  (let ([mode (%port-buffer-mode p)])
    (cond ((eq? mode 'block) #f)
      ((eq? mode 'none) (%ports/flush-buffer p))
      ((and newline? (eq? mode 'line)) (%ports/flush-buffer p))
      (else #f))))

;;; Appends a byte to the output buffer, flushing as needed.
(define (%ports/put-byte p b)
  (when (%ports/input-output? p)
    (%ports/prepare-write! p))
  (%ports/reserve! p 1)
  (let ([lim (%port-out-lim p)])
    (bytevector-u8-set! (%port-out-buf p) lim b)
    (%port-out-lim-set! p (+ lim 1)))
  (%ports/after-write! p (eq? b 10))
  (unspecified))

;;; Appends count bytes of bv starting at start.
(define (%ports/put-bytes p bv start count)
  (when (%ports/input-output? p)
    (%ports/prepare-write! p))
  (let ([cap (bytevector-length (%port-out-buf p))])
    (cond
      ((and (not (%port-memory p)) (>= count cap))
       ;; larger than the buffer: write straight through
       (%ports/flush-buffer p)
       (let ([write! (%handler-write! (%port-handler p))])
         (let loop ([start start] [count count])
           (when (> count 0)
             (let ([n (write! bv start count)])
               (if (and (fixnum? n) (> n 0) (<= n count))
                 (loop (+ start n) (- count n))
                 (raise-i/o-write-error 'put-bytevector "write failed" p)))))))
      (else
        (let loop ([start start] [count count])
          (when (> count 0)
            (if (%port-memory p)
              (%ports/reserve! p count)
              (when (> (+ (%port-out-lim p) 1) (bytevector-length (%port-out-buf p)))
                (%ports/flush-buffer p)))
            (let* ([lim (%port-out-lim p)]
                   [room (- (bytevector-length (%port-out-buf p)) lim)]
                   [n (min room count)])
              (r6rs:bytevector-copy! bv start (%port-out-buf p) lim n)
              (%port-out-lim-set! p (+ lim n))
              (loop (+ start n) (- count n))))))))
  (when (> count 0)
    (%ports/after-write! p #f))
  (unspecified))

;;; Sets the underlying position to posn (a byte offset), discarding
;;; all buffered input and output.
(define (%ports/seek-bytes! p posn)
  (let ([memory (%port-memory p)])
    (cond
      ((eq? memory 'in)
       (%port-in-ptr-set! p posn)
       (%port-pending-char-set! p #f)
       (when (eq? (%port-state p) 'eof)
         (%port-state-set! p 'open)))
      ((or (eq? memory 'out) (eq? memory 'sout))
       (%port-hw-set! p (%ports/memory-size p))
       (%ports/grow-memory! p posn)
       (%port-out-lim-set! p posn)
       (%port-pending-char-set! p #f))
      (else
        (if (%ports/output-port? p)
          (%ports/flush-buffer p))
        (let ([sp (%handler-set-position! (%port-handler p))])
          (if (not sp)
            (assertion-violation 'set-port-position!
              "port does not support set-port-position!"
              p))
          (sp posn)
          (%port-in-ptr-set! p 0)
          (%port-in-lim-set! p 0)
          (%port-pending-char-set! p #f)
          (when (eq? (%port-state p) 'eof)
            (%port-state-set! p 'open))))))
  (unspecified))

;;; Resets the per-port codec and position-tracking state after a
;;; byte-level seek.  Endianness of a UTF-16 codec is preserved.
(define (%ports/reset-textual-state! p)
  (%port-char-pos-set! p 0)
  (%port-lines-read-set! p 0)
  (%port-line-start-set! p 0)
  (%port-anchors-set! p '())
  (%port-anchor-next-set! p 4096)
  (%port-reader-set! p #f)
  (%port-pending-char-set! p #f)
  (unspecified))

;;; Records an anchor (current character position -> current byte
;;; position) used by set-port-position! on built-in textual ports.
;;; Anchors are spaced 4096 characters apart so the list stays small.
(define (%ports/record-anchor! p)
  (let ([pos (%port-char-pos p)])
    (if (%port-pending-char p)
      (%port-anchor-next-set! p (+ pos 1))
      (begin
        (%port-anchors-set! p
          (cons (cons pos (%ports/byte-position p))
            (%port-anchors p)))
        (%port-anchor-next-set! p (+ pos 4096)))))
  (unspecified))

;;; Removes any anchor at or beyond the given character position.
(define (%ports/truncate-anchors! p pos)
  (let loop ([anchors (%port-anchors p)] [keep '()])
    (if (null? anchors)
      (%port-anchors-set! p (reverse keep))
      (let ([a (car anchors)])
        (if (< (car a) pos)
          (loop (cdr anchors) (cons a keep))
          (loop (cdr anchors) keep)))))
  (%port-anchor-next-set! p (+ pos 4096))
  (unspecified))

;;; ---------------------------------------------------------------
;;; Public predicates and simple operations.
;;; ---------------------------------------------------------------
(define (port? x)
  (%port? x))

(define (input-port? x)
  (and (%port? x) (%ports/input-port? x)))

(define (output-port? x)
  (and (%port? x) (%ports/output-port? x)))

(define (textual-port? x)
  (and (%port? x) (%ports/textual? x)))

(define (binary-port? x)
  (and (%port? x) (%ports/binary? x)))

(define (input-port-open? p)
  (and (input-port? p) (%ports/open? p)))

(define (output-port-open? p)
  (and (output-port? p) (%ports/open? p)))

(define (port-name p)
  (if (port? p)
    (%port-name p)
    (assertion-violation 'port-name "not a port" p)))

(define (port-eof? p)
  (if (and (port? p) (%ports/input-port? p))
    (eof-object?
      (if (%ports/textual? p) (lookahead-char p) (lookahead-u8 p)))
    (assertion-violation 'port-eof? "not an input port" p)))

(define (port-lines-read p)
  (if (and (port? p) (%ports/input-port? p))
    (%port-lines-read p)
    (assertion-violation 'port-lines-read "not an input port" p)))

(define (port-line-start p)
  (if (and (port? p) (%ports/input-port? p))
    (%port-line-start p)
    (assertion-violation 'port-line-start "not an input port" p)))

(define (port-fd p)
  (if (port? p)
    (%port-fd p)
    (assertion-violation 'port-fd "not a port" p)))

(define port-fileno port-fd)

(define (port-closed? p)
  (and (port? p) (eq? (%port-state p) 'closed)))

;;; Reader cache used by boot/reader.scm (get-port-reader).

(define (%ports/port-reader p)
  (if (port? p)
    (%port-reader p)
    (assertion-violation 'port-reader "not a port" p)))

(define (%ports/port-reader-set! p r)
  (if (port? p)
    (%port-reader-set! p r)
    (assertion-violation 'port-reader-set! "not a port" p)))

;;; Close the port: flush pending output, invoke the handler's close
;;; procedure once, and mark the port closed.  The port is marked
;;; closed first, and the handler is closed even if the final flush
;;; fails, so a broken output stream cannot leak its descriptor.
;;; Closing an already closed port has no effect (R6RS 8.2.6).

(define (close-port p)
  (if (port? p)
    (when (not (eq? (%port-state p) 'closed))
      (%port-state-set! p 'closed)
      (when (%port-fd p)
        (%ports/unregister-output! p))
      (dynamic-wind
        (lambda () #f)
        (lambda ()
          (when (%ports/output-port? p)
            (%ports/flush-buffer p)))
        (lambda ()
          (let ([c (%handler-close (%port-handler p))])
            (when c (c)))
          (%ports/release-buffers! p)))
      (unspecified))
    (assertion-violation 'close-port "not a port" p)))

;;; Drops references to the buffers of a closed port so they can be
;;; collected even if the port object itself stays reachable.  Memory
;;; output ports keep theirs: the result stays extractable after close.
(define (%ports/release-buffers! p)
  (unless (%port-memory p)
    (%port-in-buf-set! p %ports/empty-buffer)
    (%port-out-buf-set! p %ports/empty-buffer))
  (%port-in-ptr-set! p 0)
  (%port-in-lim-set! p 0)
  (unless (%port-memory p)
    (%port-out-lim-set! p 0)))

(define (close-input-port p)
  (cond ((input-port? p)
         (close-port p))
    (else
      (assertion-violation 'close-input-port "not an input port" p))))

(define (close-output-port p)
  (cond ((output-port? p)
         (close-port p))
    (else
      (assertion-violation 'close-output-port "not an output port" p))))

(define (call-with-port p proc)
  (call-with-values
    (lambda () (proc p))
    (lambda results
      (if (%ports/open? p)
        (close-port p))
      (apply values results))))

;;; Reads are possible only on open input ports; this is the shared
;;; guard for the binary and textual input procedures.

(define (%ports/check-input p who)
  (if (%ports/open-input? p)
    (unspecified)
    (assertion-violation who "not an open input port" p)))

;;; Output is possible on open output ports.
(define (%ports/check-output p who)
  (if (%ports/open-output? p)
    (unspecified)
    (assertion-violation who "not an open output port" p)))

;;; Flushes the output buffer if the port was created with the flush
;;; attribute (used after writing a datum, e.g. by print.scm).
(define (%ports/discretionary-flush p)
  (if (and (port? p) (%ports/output-port? p))
    (when (and (%port-flush-datum? p) (%ports/open? p))
      (%ports/flush-buffer p))
    (assertion-violation 'discretionary-flush "not an output port" p)))

;;; Flushes the output buffer of any open output port.

(define (flush-output-port . rest)
  (define p (if (null? rest) (current-output-port) (car rest)))
  (if (and (port? p) (%ports/output-port? p))
    (if (%ports/open? p)
      (%ports/flush-buffer p)
      (unspecified))
    (assertion-violation 'flush-output-port "not an output port" p)))

(define (output-port-buffer-mode p)
  (if (and (port? p) (%ports/output-port? p))
    (%port-buffer-mode p)
    (assertion-violation 'output-port-buffer-mode "not an output port" p)))

;;; ---------------------------------------------------------------
;;; Position operations.
;;; ---------------------------------------------------------------

(define (port-has-port-position? p)
  (if (port? p)
    (if (%ports/textual? p)
      ;; built-in textual ports track character positions; custom
      ;; textual ports only support port-position when the user
      ;; supplied a get-position procedure
      (if (%port-custom? p)
        (and (%ports/underlying-position p) #t)
        #t)
      (and (%ports/underlying-position p) #t))
    (assertion-violation 'port-has-port-position? "not a port" p)))

(define (port-has-set-port-position!? p)
  (if (port? p)
    (and (%handler-set-position! (%port-handler p)) #t)
    (assertion-violation 'port-has-set-port-position!? "not a port" p)))

(define (port-position p)
  (if (not (port? p))
    (assertion-violation 'port-position "not a port" p))
  (cond ((not (%ports/open? p))
         (assertion-violation 'port-position "port is closed" p))
    ((%ports/textual? p)
      (%port-char-pos p))
    (else
      (if (not (%ports/underlying-position p))
        (assertion-violation 'port-position
          "port does not support port-position"
          p))
      (%ports/byte-position p))))

;;; Binary ports: pos is a byte offset; seek directly.

(define (%ports/set-position-binary! p pos)
  (if (not (and (integer? pos) (exact? pos) (>= pos 0)))
    (raise-i/o-invalid-position-error
      'set-port-position! "invalid position" p pos))
  (%ports/seek-bytes! p pos)
  (unspecified))

;;; Textual ports, built-in (bytevector/string/file): pos is a
;;; character count.  Seek to the nearest recorded anchor at or before
;;; pos, then read forward to the exact character.  Position 0 resets
;;; the whole stream.

(define (%ports/set-position-textual-builtin! p pos)
  (if (not (and (integer? pos) (exact? pos) (>= pos 0)))
    (raise-i/o-invalid-position-error
      'set-port-position! "invalid position" p pos))
  (cond
    ((and (eqv? (%port-codec p) 0) (eq? 'none (%port-eol p)))
     ;; one byte per character
     (%ports/seek-bytes! p pos)
     (%ports/reset-textual-state! p)
     (%port-char-pos-set! p pos)
     (%port-anchor-next-set! p (+ pos 4096))
     (unspecified))
    ((= pos 0)
     (%ports/seek-bytes! p 0)
     (%ports/reset-textual-state! p)
     (if (and (%port-codec-state p) (vector? (%port-codec-state p)))
       (vector-set! (%port-codec-state p) 1 #f))
     (unspecified))
    ((eq? (%port-direction p) 'output)
     ;; character positions of an output port are not seekable except
     ;; to where the port already is
     (if (= pos (%port-char-pos p))
       (unspecified)
       (raise-i/o-invalid-position-error
         'set-port-position! "invalid position" p pos)))
    (else
      (let ([anchor
              (let loop ([anchors (%port-anchors p)])
                (cond ((null? anchors) '(0 . 0))
                  ((<= (caar anchors) pos) (car anchors))
                  (else (loop (cdr anchors)))))])
        (%ports/seek-bytes! p (cdr anchor))
        (%ports/truncate-anchors! p (car anchor))
        (%port-char-pos-set! p (car anchor))
        (%port-pending-char-set! p #f)
        (let loop ([n (- pos (car anchor))])
          (if (= n 0)
            (unspecified)
            (let ([c (get-char p)])
              (if (eof-object? c)
                (raise-i/o-invalid-position-error
                  'set-port-position! "position out of range" p pos)
                (loop (- n 1))))))))))

;;; Textual ports, custom: delegate to the user's set-position! with
;;; the character position.

(define (%ports/set-position-textual-custom! p pos)
  (if (not (and (integer? pos) (exact? pos) (>= pos 0)))
    (raise-i/o-invalid-position-error
      'set-port-position! "invalid position" p pos))
  (if (%ports/output-port? p)
    (%ports/flush-buffer p))
  (let ([sp (%handler-set-position! (%port-handler p))])
    (if (not sp)
      (assertion-violation 'set-port-position!
        "port does not support set-port-position!"
        p))
    (sp pos)
    (%port-in-ptr-set! p 0)
    (%port-in-lim-set! p 0)
    (%port-pending-char-set! p #f)
    (%port-char-pos-set! p pos)
    (when (eq? (%port-state p) 'eof)
      (%port-state-set! p 'open)))
  (unspecified))

(define (set-port-position! p pos)
  (if (not (port? p))
    (assertion-violation 'set-port-position! "not a port" p))
  (cond ((not (%ports/open? p))
         (unspecified))
    ((not (%ports/textual? p))
      (%ports/set-position-binary! p pos))
    ((%port-custom? p)
      (%ports/set-position-textual-custom! p pos))
    (else
      (%ports/set-position-textual-builtin! p pos)))
  (unspecified))

;;; eof
