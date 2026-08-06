;;; Implementation of Scheme ports, designed directly from
;;; the R6RS standard libraries (rnrs io ports, 8.2) and R7RS-small
;;; section 6.13.  This file contains the port record, the port-handler
;;; dispatch protocol, the byte buffer machinery, state handling, and
;;; position operations. 

;;; A port is a record with these fields:
;;;
;;;   name          string               human-readable name
;;;   direction     'input | 'output | 'input-output
;;;   transcoder    <transcoder> | #f    #f means binary port
;;;   buffer-mode   'none | 'line | 'block
;;;   flush-datum?  boolean              flush after each datum (display/write)
;;;   state         'open | 'eof | 'closed
;;;   handler       <port-handler>       underlying I/O procedures
;;;   in-buf        bytevector           input buffer
;;;   in-ptr        fixnum               next unread byte
;;;   in-lim        fixnum               end of buffered input
;;;   out-buf       bytevector           output buffer
;;;   out-lim       fixnum               end of buffered output
;;;   pending       bytevector           pushed-back bytes (lookahead)
;;;   pending-len   fixnum               number of pushed-back bytes
;;;   char-pos      integer              characters consumed / written
;;;   lines-read    integer              line endings consumed / written
;;;   line-start    integer              character position after last line ending
;;;   codec-state   #f | (vector 'utf16 endianness bom-written?)
;;;   anchors       alist                (char-pos . byte-pos), most recent first
;;;   reader        #f | datum reader    cached reader (boot/reader.scm)
;;;   fd            fixnum | #f          file descriptor, if fd-backed
;;;   custom?       boolean              true for user-supplied custom ports
;;;
;;; Position model:
;;;
;;;   Binary ports: the position is a non-negative exact integer byte
;;;   offset, computed from the handler's get-position (the underlying
;;;   source position) adjusted for buffered and pushed-back bytes.
;;;
;;;   Textual ports: port-position returns the number of characters
;;;   consumed or written (an exact integer; acceptable as an R6RS
;;;   implementation-dependent position value).  set-port-position! on
;;;   a built-in textual port maps character positions to byte offsets
;;;   via recorded anchors; on a custom textual port it delegates to
;;;   the user-supplied set-position!.

(define <port>
  (let* ([rtd (make-record-type-descriptor '<port> #f #f #f #f '#(
            (immutable name)
            (immutable direction)
            (mutable transcoder)
            (mutable buffer-mode)
            (mutable flush-datum?)
            (mutable state)
            (immutable handler)
            (mutable in-buf)
            (mutable in-ptr)
            (mutable in-lim)
            (mutable out-buf)
            (mutable out-lim)
            (mutable pending)
            (mutable pending-len)
            (mutable char-pos)
            (mutable lines-read)
            (mutable line-start)
            (mutable codec-state)
            (mutable anchors)
            (mutable reader)
            (mutable fd)
            (immutable custom?)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '<port> rtd rcd)))

(define %port? (record-predicate (record-type-rtd <port>)))
(define %make-port (record-constructor (record-type-rcd <port>)))
(define %port-name (record-accessor (record-type-rtd <port>) 0))
(define %port-direction (record-accessor (record-type-rtd <port>) 1))
(define %port-transcoder (record-accessor (record-type-rtd <port>) 2))
(define %port-buffer-mode (record-accessor (record-type-rtd <port>) 3))
(define %port-flush-datum? (record-accessor (record-type-rtd <port>) 4))
(define %port-state (record-accessor (record-type-rtd <port>) 5))
(define %port-handler (record-accessor (record-type-rtd <port>) 6))
(define %port-in-buf (record-accessor (record-type-rtd <port>) 7))
(define %port-in-ptr (record-accessor (record-type-rtd <port>) 8))
(define %port-in-lim (record-accessor (record-type-rtd <port>) 9))
(define %port-out-buf (record-accessor (record-type-rtd <port>) 10))
(define %port-out-lim (record-accessor (record-type-rtd <port>) 11))
(define %port-pending (record-accessor (record-type-rtd <port>) 12))
(define %port-pending-len (record-accessor (record-type-rtd <port>) 13))
(define %port-char-pos (record-accessor (record-type-rtd <port>) 14))
(define %port-lines-read (record-accessor (record-type-rtd <port>) 15))
(define %port-line-start (record-accessor (record-type-rtd <port>) 16))
(define %port-codec-state (record-accessor (record-type-rtd <port>) 17))
(define %port-anchors (record-accessor (record-type-rtd <port>) 18))
(define %port-reader (record-accessor (record-type-rtd <port>) 19))
(define %port-fd (record-accessor (record-type-rtd <port>) 20))
(define %port-custom? (record-accessor (record-type-rtd <port>) 21))

(define %port-transcoder-set! (record-mutator (record-type-rtd <port>) 2))
(define %port-buffer-mode-set! (record-mutator (record-type-rtd <port>) 3))
(define %port-flush-datum?-set! (record-mutator (record-type-rtd <port>) 4))
(define %port-state-set! (record-mutator (record-type-rtd <port>) 5))
(define %port-in-buf-set! (record-mutator (record-type-rtd <port>) 7))
(define %port-in-ptr-set! (record-mutator (record-type-rtd <port>) 8))
(define %port-in-lim-set! (record-mutator (record-type-rtd <port>) 9))
(define %port-out-buf-set! (record-mutator (record-type-rtd <port>) 10))
(define %port-out-lim-set! (record-mutator (record-type-rtd <port>) 11))
(define %port-pending-set! (record-mutator (record-type-rtd <port>) 12))
(define %port-pending-len-set! (record-mutator (record-type-rtd <port>) 13))
(define %port-char-pos-set! (record-mutator (record-type-rtd <port>) 14))
(define %port-lines-read-set! (record-mutator (record-type-rtd <port>) 15))
(define %port-line-start-set! (record-mutator (record-type-rtd <port>) 16))
(define %port-codec-state-set! (record-mutator (record-type-rtd <port>) 17))
(define %port-anchors-set! (record-mutator (record-type-rtd <port>) 18))
(define %port-reader-set! (record-mutator (record-type-rtd <port>) 19))
(define %port-fd-set! (record-mutator (record-type-rtd <port>) 20))

;;; The port-handler protocol mirrors the R6RS custom-port procedures:
;;;
;;;   read!        (bytevector start count) -> fixnum bytes read (0 = EOF)
;;;   write!       (bytevector start count) -> fixnum bytes written
;;;   close        () -> unspecified
;;;   ready?       () -> boolean
;;;   get-position () -> fixnum | #f       (underlying source position)
;;;   set-position! (fixnum) -> unspecified
;;;
;;; Any procedure may be #f; reading requires read!, writing requires
;;; write!, closing uses close (or a no-op), and position support is
;;; detected by the presence of get-position / set-position!.

(define <port-handler>
  (let* ([rtd (make-record-type-descriptor '<port-handler> #f #f #f #f '#(
            (immutable read!)
            (immutable write!)
            (immutable close)
            (immutable ready?)
            (immutable get-position)
            (immutable set-position!)
            (immutable data)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '<port-handler> rtd rcd)))

(define %handler? (record-predicate (record-type-rtd <port-handler>)))
(define %make-handler (record-constructor (record-type-rcd <port-handler>)))
(define %handler-read! (record-accessor (record-type-rtd <port-handler>) 0))
(define %handler-write! (record-accessor (record-type-rtd <port-handler>) 1))
(define %handler-close (record-accessor (record-type-rtd <port-handler>) 2))
(define %handler-ready? (record-accessor (record-type-rtd <port-handler>) 3))
(define %handler-get-position (record-accessor (record-type-rtd <port-handler>) 4))
(define %handler-set-position! (record-accessor (record-type-rtd <port-handler>) 5))
(define %handler-data (record-accessor (record-type-rtd <port-handler>) 6))

;;; Default buffer size for byte buffering.

(define %ports/buffer-size 4096)

;;; Allocates a fresh port in the open state.

(define (%ports/make-port name direction transcoder buffer-mode handler . opts)
  (define flush-datum? #f)
  (define fd #f)
  (define custom? #f)
  (for-each
    (lambda (opt)
      (case opt
        ((flush) (set! flush-datum? #t))
        ((datum) (set! flush-datum? #t))
        ((custom) (set! custom? #t))
        (else
          (assertion-violation '%ports/make-port "invalid port attribute" opt))))
    opts)
  (let ([p (%make-port
             name
             direction
             transcoder
             buffer-mode
             flush-datum?
             'open
             handler
             (make-bytevector %ports/buffer-size 0)
             0
             0
             (make-bytevector %ports/buffer-size 0)
             0
             (make-bytevector 4 0)
             0
             0
             0
             0
             (if (and transcoder (eq? 'utf-16 (%ports/transcoder-codec-raw transcoder)))
               (vector 'utf16 #f #f)
               #f)
             '()
             #f
             fd
             custom?)])
    p))

;;; Convenience accessors used across the port files.

(define (%ports/input-port? p)
  (and (memq (%port-direction p) '(input input-output)) #t))
(define (%ports/output-port? p)
  (and (memq (%port-direction p) '(output input-output)) #t))
(define (%ports/input-output? p)
  (and (eq? (%port-direction p) 'input-output) #t))

(define (%ports/open? p)
  (not (eq? (%port-state p) 'closed)))

(define (%ports/textual? p)
  (and (%port-transcoder p) #t))

(define (%ports/binary? p)
  (not (%port-transcoder p)))

;;; ---------------------------------------------------------------
;;; Byte-level machinery.
;;; ---------------------------------------------------------------

;;; The number of bytes currently held in the input buffer (including
;;; pushed-back bytes is NOT counted here; see pending below).

(define (%ports/buffered-input p)
  (- (%port-in-lim p) (%port-in-ptr p)))

;;; The underlying source position, from the handler if available,
;;; else #f.

(define (%ports/underlying-position p)
  (let ([g (%handler-get-position (%port-handler p))])
    (and g (g))))

;;; The current position in bytes:
;;;   input:   underlying position - unread buffered bytes - pushed-back bytes
;;;   output:  underlying position + buffered output bytes
;;; For output-only ports the input buffer is always empty and for
;;; input-only ports the output buffer is always empty, so the two
;;; adjustments never overlap.  On input/output ports at most one side
;;; is buffered at a time (output is write-through).

(define (%ports/byte-position p)
  (let* ([g (%ports/underlying-position p)]
         [unread (+ (%ports/buffered-input p) (%port-pending-len p))]
         [out (%port-out-lim p)]
         [pos (if g
                (+ (- g unread) out)
                (+ (- 0 unread) out))])
    pos))

;;; Pushes back n bytes from scratch (bytevector) into the pending
;;; buffer, preserving stream order.  Called by the textual layer when
;;; a lookahead must be undone.  n <= 4.

(define (%ports/push-back! p scratch n)
  (when (> n 0)
    (let* ([pending (%port-pending p)]
           [old (%port-pending-len p)]
           [new (+ old n)])
      (when (> new (bytevector-length pending))
        (let ([bigger (make-bytevector (* 2 new) 0)])
          (r6rs:bytevector-copy! pending 0 bigger n old)
          (%port-pending-set! p bigger)
          (set! pending bigger)))
      (when (> old 0)
        (r6rs:bytevector-copy! pending 0 pending n old))
      (r6rs:bytevector-copy! scratch 0 pending 0 n)
      (%port-pending-len-set! p new)))
  (unspecified))

;;; Pushes back one byte into the pending buffer.

(define (%ports/push-back-one! p b)
  (let* ([pending (%port-pending p)]
         [old (%port-pending-len p)]
         [new (+ old 1)])
    (when (> new (bytevector-length pending))
      (let ([bigger (make-bytevector (* 2 new) 0)])
        (r6rs:bytevector-copy! pending 0 bigger 1 old)
        (%port-pending-set! p bigger)
        (set! pending bigger)))
    (when (> old 0)
      (r6rs:bytevector-copy! pending 0 pending 1 old))
    (bytevector-u8-set! pending 0 b)
    (%port-pending-len-set! p new))
  (unspecified))

;;; Removes one byte from the front of the pending buffer, or #f if
;;; the pending buffer is empty.

(define (%ports/pop-pending! p)
  (let ([len (%port-pending-len p)])
    (if (= len 0)
      #f
      (let* ([pending (%port-pending p)]
             [b (bytevector-u8-ref pending 0)])
        (r6rs:bytevector-copy! pending 1 pending 0 (- len 1))
        (%port-pending-len-set! p (- len 1))
        b))))

;;; Fills the input buffer from the handler.  Returns #t if new data
;;; was read, #f at end of file.  Consumed bytes are compacted to the
;;; front of the buffer.  On input/output ports the read-ahead is
;;; limited to a single byte so that switching to output can be
;;; corrected with a cheap seek.

(define (%ports/refill! p)
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
            p))))))

;;; Returns the next byte from the port, or the eof object.

(define (%ports/get-byte p)
  (if (%ports/input-output? p)
    (%ports/flush-buffer p))
  (let loop ()
    (let ([b (%ports/pop-pending! p)])
      (if b
        b
        (let ([ptr (%port-in-ptr p)]
              [lim (%port-in-lim p)])
          (cond ((< ptr lim)
                 (let ([b (bytevector-u8-ref (%port-in-buf p) ptr)])
                   (%port-in-ptr-set! p (+ ptr 1))
                   b))
            ((eq? (%port-state p) 'eof)
              (eof-object))
            (else
              (if (%ports/refill! p) (loop) (eof-object)))))))))

;;; Flushes the output buffer to the handler.

(define (%ports/flush-buffer p)
  (let* ([write! (%handler-write! (%port-handler p))]
         [buf (%port-out-buf p)]
         [lim (%port-out-lim p)])
    (if (> lim 0)
      (let loop ([start 0] [count lim])
        (if (= count 0)
          (begin
            (%port-out-lim-set! p 0)
            (unspecified))
          (let ([n (write! buf start count)])
            (cond ((and (fixnum? n) (> n 0) (<= n count))
                   (loop (+ start n) (- count n)))
              (else
                (raise-i/o-write-error
                  'flush-output-port
                  "write failed"
                  p))))))
      (unspecified))))

;;; Appends a byte to the output buffer, flushing as needed.

(define (%ports/put-byte p b)
  (if (%ports/input-output? p)
    ;; Before writing on an input/output port, move the underlying
    ;; position back to the current read position (there may be
    ;; buffered or pushed-back input) and discard that input.
    (let ([unread (+ (%ports/buffered-input p) (%port-pending-len p))])
      (when (> unread 0)
        (let* ([g (%ports/underlying-position p)]
               [target (if g (- g unread) 0)])
          (%ports/seek-bytes! p target))
        (%port-in-ptr-set! p 0)
        (%port-in-lim-set! p 0)
        (%port-pending-len-set! p 0))))
  (let* ([buf (%port-out-buf p)]
         [size (bytevector-length buf)])
    (when (>= (%port-out-lim p) size)
      (%ports/flush-buffer p))
    (let ([lim (%port-out-lim p)])
      (bytevector-u8-set! buf lim b)
      (%port-out-lim-set! p (+ lim 1))
      (when (or (eq? (%port-buffer-mode p) 'none)
            (and (eq? (%port-buffer-mode p) 'line) (= b 10)))
        (%ports/flush-buffer p))))
  (unspecified))

;;; Sets the underlying position to posn (a byte offset), discarding
;;; all buffered input and output.

(define (%ports/seek-bytes! p posn)
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
    (%port-pending-len-set! p 0)
    (when (eq? (%port-state p) 'eof)
      (%port-state-set! p 'open)))
  (unspecified))

;;; Resets the per-port codec and position-tracking state after a
;;; byte-level seek.  Endianness of a UTF-16 codec is preserved.

(define (%ports/reset-textual-state! p)
  (%port-char-pos-set! p 0)
  (%port-lines-read-set! p 0)
  (%port-line-start-set! p 0)
  (%port-anchors-set! p '())
  (%port-reader-set! p #f)
  (unspecified))

;;; Records an anchor (current character position -> current byte
;;; position) used by set-port-position! on built-in textual ports.

(define (%ports/record-anchor! p)
  (%port-anchors-set! p
    (cons (cons (%port-char-pos p) (%ports/byte-position p))
      (%port-anchors p)))
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
  (unspecified))

;;; ---------------------------------------------------------------
;;; Public predicates and simple operations.
;;; ---------------------------------------------------------------

(define (port? x)
  (%port? x))

(define (input-port? x)
  (and (port? x) (%ports/input-port? x)))

(define (output-port? x)
  (and (port? x) (%ports/output-port? x)))

(define (textual-port? x)
  (and (port? x) (%ports/textual? x)))

(define (binary-port? x)
  (and (port? x) (%ports/binary? x)))

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
;;; procedure once, and mark the port closed.  Closing an already
;;; closed port has no effect (R6RS 8.2.6).

(define (close-port p)
  (if (port? p)
    (when (not (eq? (%port-state p) 'closed))
      (if (%ports/output-port? p)
        (%ports/flush-buffer p))
      (let ([c (%handler-close (%port-handler p))])
        (when c (c)))
      (%port-state-set! p 'closed)
      (unspecified))
    (assertion-violation 'close-port "not a port" p)))

(define (close-input-port p)
  (cond ((input-port? p)
         (if (%ports/input-output? p)
           (close-port p)
           (close-port p)))
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
  (if (and (port? p) (%ports/input-port? p) (not (eq? (%port-state p) 'closed)))
    (unspecified)
    (assertion-violation who "not an open input port" p)))

;;; Output is possible on open output ports.

(define (%ports/check-output p who)
  (if (and (port? p) (%ports/output-port? p) (not (eq? (%port-state p) 'closed)))
    (unspecified)
    (assertion-violation who "not an open output port" p)))

;;; Flushes the output buffer if the port was created with the flush
;;; attribute (used after writing a datum, e.g. by print.scm).

(define (%ports/discretionary-flush p)
  (if (and (port? p) (%ports/output-port? p) (%ports/open? p))
    (when (%port-flush-datum? p)
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
  (let ([codec (%ports/transcoder-codec-raw (%port-transcoder p))])
    (cond ((and (eq? codec 'latin-1)
             (eq? 'none (%ports/transcoder-eol-raw (%port-transcoder p))))
           ;; one byte per character
           (%ports/seek-bytes! p pos)
           (%ports/reset-textual-state! p)
           (%port-char-pos-set! p pos)
           (unspecified))
      ((= pos 0)
        (%ports/seek-bytes! p 0)
        (%ports/reset-textual-state! p)
        (if (and (%port-codec-state p) (vector? (%port-codec-state p)))
          (vector-set! (%port-codec-state p) 1 #f))
        (unspecified))
      (else
        (let ([anchor
                (let loop ([anchors (%port-anchors p)])
                  (cond ((null? anchors) '(0 . 0))
                    ((<= (caar anchors) pos) (car anchors))
                    (else (loop (cdr anchors)))))])
          (%ports/seek-bytes! p (cdr anchor))
          (%ports/truncate-anchors! p (car anchor))
          (%port-char-pos-set! p (car anchor))
          (let loop ([n (- pos (car anchor))])
            (if (= n 0)
              (unspecified)
              (let ([c (get-char p)])
                (if (eof-object? c)
                  (raise-i/o-invalid-position-error
                    'set-port-position! "position out of range" p pos)
                  (loop (- n 1)))))))))))

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
    (%port-pending-len-set! p 0)
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
