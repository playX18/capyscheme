;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Private procedures (called only from code within this file)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Works only on input ports.
; The main invariants may not hold here.
; In particular, the port may be in the textual state
; but have a nonempty auxbuf.

(define (io/fill-buffer! p)
  (let ((r (((tuple-ref p port.ioproc) 'read)
            (tuple-ref p port.iodata)
            (tuple-ref p port.mainbuf))))
    (cond ((eq? r 'eof)
           (io/set-eof-state! p))
      ((eq? r 'error)
        ; FIXME: should retry before giving up
        (io/set-error-state! p)
        (error "Read error on port " p)
        #t)
      ((and (fixnum? r) (>= r 0))
        (tuple-set! p port.mainpos
          (+ (tuple-ref p port.mainpos)
            (tuple-ref p port.mainptr)))
        (tuple-set! p port.mainptr 0)
        (tuple-set! p port.mainlim r))
      (else
        (io/set-error-state! p)
        (error "io/fill-buffer!: bad value " r " on " p)))
    (io/transcode-port! p)))

; The main buffer has just been filled, but the state has not been changed.
; If the port was in the textual state, it should enter the auxstart state.
; If the port was in the auxstart state, it should remain in that state.
; If the port was in the auxend state, it should enter the auxstart state.
; So the main job here is to convert the port to the auxstart state.

(define (io/transcode-port! p)
  (let* ((type (tuple-ref p port.type))
         (state (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainptr (tuple-ref p port.mainptr))
         (mainlim (tuple-ref p port.mainlim))
         (auxbuf (tuple-ref p port.auxbuf))
         (auxptr (tuple-ref p port.auxptr))
         (auxlim (tuple-ref p port.auxlim)))
    (assert (= mainptr 0))
    (cond ((= type type:binary-input) #t)
      ((= type type:textual-input)
        (case state
          ((closed error eof)
            (io/reset-buffers! p))
          ((textual auxstart auxend)
            (assert (< auxlim (bytevector-length auxbuf)))
            (bytevector-set! auxbuf auxlim (bytevector-ref mainbuf 0))
            (tuple-set! p port.auxlim (+ auxlim 1))
            (bytevector-set! mainbuf 0 port.sentinel)
            (tuple-set! p port.state 'auxstart))
          (else
            (error 'io/transcode-port! "internal error" p))))
      (else
        (error 'io/transcode-port! "internal error" p)))))

; Works only on output ports.

(define (io/flush-buffer p)
  (let ((wr-ptr (tuple-ref p port.mainlim)))
    (if (> wr-ptr 0)
      (let ((r (((tuple-ref p port.ioproc) 'write)
                (tuple-ref p port.iodata)
                (tuple-ref p port.mainbuf)
                wr-ptr)))

        (cond ((eq? r 'ok)
               (tuple-set! p port.mainpos
                 (+ (tuple-ref p port.mainpos) wr-ptr))
               (tuple-set! p port.mainlim 0))
          ((eq? r 'error)
            (io/set-error-state! p)
            (error "Write error on port " p)
            #t)
          (else
            (io/set-error-state! p)
            (error "io/flush-buffer: bad value " r " on " p)
            #t))))))

; Converts port to a clean error state.

(define (io/set-error-state! p)
  (tuple-set! p port.state 'error)
  (io/reset-buffers! p))

; Converts port to a clean eof state.

(define (io/set-eof-state! p)
  (tuple-set! p port.state 'eof)
  (io/reset-buffers! p))

; Converts port to a clean closed state.
; FIXME:  Should this reduce the size of mainbuf?

(define (io/set-closed-state! p)
  (tuple-set! p port.type type:closed)
  (tuple-set! p port.state 'closed)
  (io/reset-buffers! p))

(define (io/port-closed? p)
  (eq? (tuple-ref p port.state) 'closed))

; Resets buffers to an empty state.

(define (io/reset-buffers! p)
  (tuple-set! p
    port.mainpos
    (+ (tuple-ref p port.mainpos)
      (tuple-ref p port.mainptr)))
  (tuple-set! p port.mainptr 0)
  (tuple-set! p port.mainlim 0)
  (tuple-set! p port.auxptr 0)
  (tuple-set! p port.auxlim 0)
  (bytevector-set! (tuple-ref p port.mainbuf) 0 port.sentinel)
  (case (tuple-ref p port.state)
    ((auxstart auxend)
      (tuple-set! p port.state 'textual))))

; Shallow-clones a port without closing it.
(define (io/clone-port p)
  (let* ((n (tuple-size p))
         (newport (make-tuple n)))
    (define (copy-loop i)
      (if (= i n)
        newport
        (begin
          (tuple-set! newport i (tuple-ref p i))
          (copy-loop (+ i 1)))))
    (copy-loop 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End-of-line processing.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Whenever io/get-char is about to return one of the four
; end-of-line characters (#\linefeed, #\return, #\x85, #x2028),
; it should perform a tail call to this procedure instead, with
; the scalar value of the specific end-of-line character as the
; last argument.
;
; That call should occur *after* the port's state has been
; updated to consume the character (unless lookahead?
; is true), but before the linesread, linestart, and wasreturn
; fields have been updated.  Updating those fields is the
; responsibility of these procedures.

(define (io/return-eol p lookahead? sv)
  (case sv
    ((13)
      (io/return-cr p lookahead?))
    ((10 #x85 #x2028)
      (if (tuple-ref p port.wasreturn)
        (begin (tuple-set! p port.wasreturn #f)
          (io/return-char-following-cr p lookahead? sv))
        (let* ((pos (tuple-ref p port.mainpos))
               (ptr (tuple-ref p port.mainptr))
               (line (tuple-ref p port.linesread))
               (transcoder (tuple-ref p port.transcoder))
               (eolstyle (logand transcoder transcoder-mask:eolstyle)))
          (cond ((or (= sv 10)
                   (not (= eolstyle eolstyle:none)))
                 (if (not lookahead?)
                   (begin (tuple-set! p port.linesread (+ line 1))
                     (tuple-set! p port.linestart (+ pos ptr))))
                 #\newline)
            (else
              (integer->char sv))))))
    (else
      (assertion-violation 'io/return-eol "internal error" p lookahead? sv))))

; Unless the eolstyle is none, a #\linefeed or #\x85 following
; a #\return should be ignored.
;
; When a #\return is consumed, the port.wasreturn field is set
; true and the port.linestart field is set to the character
; position following the #\return.

(define (io/return-cr p lookahead?)
  (let* ((transcoder (tuple-ref p port.transcoder))
         (eolstyle (logand transcoder transcoder-mask:eolstyle)))
    (cond (lookahead?
           (if (= eolstyle eolstyle:none)
             #\return
             #\linefeed))
      ((= eolstyle eolstyle:none)
        #\return)
      (else
        (let* ((mainptr (tuple-ref p port.mainptr))
               (mainpos (tuple-ref p port.mainpos))
               (pos (+ mainpos mainptr))
               (linesread (tuple-ref p port.linesread)))
          (tuple-set! p port.linesread (+ linesread 1))
          (tuple-set! p port.linestart pos)
          (tuple-set! p port.wasreturn #t))
        #\linefeed))))

(define (io/return-char-following-cr p lookahead? sv)
  (assert (not (tuple-ref p port.wasreturn)))
  (let* ((c (integer->char sv))
         (pos (+ (tuple-ref p port.mainpos)
               (tuple-ref p port.mainptr)))
         (linesread (tuple-ref p port.linesread))
         (linestart (tuple-ref p port.linestart))
         (transcoder (tuple-ref p port.transcoder))
         (eolstyle (logand transcoder transcoder-mask:eolstyle)))
    (cond ((or (= sv 10)
             (= sv #x85))
           (if (and (= pos (if lookahead? linestart (+ linestart 1)))
                (not (eq? eolstyle eolstyle:none)))
             ; consume the character while preserving position
             ; and retry the get-char operation
             (begin (if lookahead? (io/get-char p #f))
               (let ((mainpos (tuple-ref p port.mainpos)))
                 (tuple-set! p port.mainpos (- mainpos 1)))
               (tuple-set! p port.linesread linesread)
               (tuple-set! p port.linestart linestart)
               (io/get-char p lookahead?))
             ; treat the character as a normal linefeed
             (io/return-eol p lookahead? 10)))
      ((char=? c #\return)
        (io/return-cr p lookahead?))
      ((= sv #x2028)
        (io/return-eol p lookahead? sv))
      (else
        c))))

; Whenever io/put-char is about to output a #\newline,
; it should perform a tail call to this procedure instead.

(define (io/put-eol p)
  (let* ((buf (tuple-ref p port.mainbuf))
         (mainlim (tuple-ref p port.mainlim))
         (transcoder (tuple-ref p port.transcoder))
         (eolstyle (logand transcoder transcoder-mask:eolstyle)))

    (define (put-byte b)
      (bytevector-set! buf mainlim b)
      (let* ((mainpos (tuple-ref p port.mainpos))
             (linesread (tuple-ref p port.linesread)))
        (tuple-set! p port.mainlim (+ mainlim 1))
        (tuple-set! p port.linesread (+ linesread 1))
        (tuple-set! p port.linestart (+ mainlim mainpos)))
      (unspecified))

    (define (put-bytes2 b0 b1)
      (bytevector-set! buf mainlim b0)
      (bytevector-set! buf (+ mainlim 1) b1)
      (finish 2))
    (define (put-bytes3 b0 b1 b2)
      (bytevector-set! buf mainlim b0)
      (bytevector-set! buf (+ mainlim 1) b1)
      (bytevector-set! buf (+ mainlim 2) b2)
      (finish 3))

    (define (finish count)
      (let* ((mainpos (tuple-ref p port.mainpos))
             (mainpos (+ mainpos (- count 1)))
             (linesread (tuple-ref p port.linesread)))
        (tuple-set! p port.mainlim (+ mainlim count))
        (tuple-set! p port.mainpos mainpos)
        (tuple-set! p port.linesread (+ linesread 1))
        (tuple-set! p port.linestart (+ mainlim mainpos)))
      (unspecified))

    (cond ((< (- (bytevector-length buf) mainlim) 4)
           (io/flush-buffer p)
           (io/put-eol p))

      ((or (eq? eolstyle eolstyle:none)
          (eq? eolstyle eolstyle:lf))
        (put-byte 10))

      ((eq? eolstyle eolstyle:cr)
        (put-byte 13))

      ((eq? eolstyle eolstyle:crlf)
        (put-bytes2 13 10))

      ((eq? codec:latin-1 (logand transcoder transcoder-mask:codec))
        (cond ((eq? eolstyle eolstyle:nel)
               (put-byte #x85))
          ((eq? eolstyle eolstyle:crnel)
            (put-bytes2 13 #x85))
          (else
            (assertion-violation 'put-char "internal error" p))))

      ((eq? codec:utf-8 (logand transcoder transcoder-mask:codec))
        (cond ((eq? eolstyle eolstyle:nel)
               (put-bytes2 #xc2 #x85))
          ((eq? eolstyle eolstyle:crnel)
            (put-bytes3 13 #xc2 #x85))
          ((eq? eolstyle eolstyle:ls)
            (put-bytes3 #xe2 #x80 #xa8))
          (else
            (assertion-violation 'put-char "internal error" p))))

      (else
        (assertion-violation 'put-char "internal error" p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; On-the-fly transcoding of UTF-8.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; On-the-fly transcoding of a non-Ascii UTF-8 character.
; The first argument is known to be a textual input port
; whose transcoder uses the UTF-8 codec.
; The second argument is true if this is a lookahead operation.
; The third argument is the first code unit of the character.
; The last three arguments are for the active buffer, either
; mainbuf (if the state is textual or auxend)
; or auxbuf (if the state is auxstart).
;
; If the active buffer does not contain enough bytes, then
; the port must be forced into the auxstart state, and bytes
; must be transferred from mainbuf to auxbuf until the auxbuf
; contains a complete character.

(define (io/get-char-utf-8 p lookahead? unit buf ptr lim)

  (define (decoding-error units)
    (for-each (lambda (x) (io/consume-byte! p)) units)
    (let* ((transcoder (tuple-ref p port.transcoder))
           (errmode (logand transcoder-mask:errmode transcoder)))
      (cond ((= errmode errmode:replace)
             (integer->char #xfffd))
        ((= errmode errmode:ignore)
          (io/get-char p lookahead?))
        (else
          (let* ((line (+ 1 (port-lines-read p)))
                 (msg (string-append "utf-8 decoding error in line "
                       (number->string line))))
            (raise-i/o-decoding-error
              'get-char
              "utf-8 decoding error"
              p))))))

  ; Forces at least one more byte into the active buffer,
  ; and retries.

  (define (read-more-bytes)
    (let* ((state (tuple-ref p port.state))
           (mainbuf (tuple-ref p port.mainbuf))
           (mainptr (tuple-ref p port.mainptr))
           (mainlim (tuple-ref p port.mainlim))
           (auxbuf (tuple-ref p port.auxbuf))
           (auxptr (tuple-ref p port.auxptr))
           (auxlim (tuple-ref p port.auxlim))
           (m (- mainlim mainptr))
           (n (- auxlim auxptr)))
      (case state

        ((auxend)
          (assert (eq? buf mainbuf))
          (assert (< 0 n))
          (r6rs:bytevector-copy! mainbuf mainptr mainbuf 0 m)
          (r6rs:bytevector-copy! auxbuf auxptr mainbuf m n)
          (bytevector-set! mainbuf (+ m n) port.sentinel)

          (tuple-set! p
            port.mainpos
            (+ (tuple-ref p port.mainpos) mainptr))
          (tuple-set! p port.mainptr 0)
          (tuple-set! p port.mainlim (+ m n))
          (tuple-set! p port.auxptr 0)
          (tuple-set! p port.auxlim 0)
          (tuple-set! p port.state 'textual)
          (io/get-char p lookahead?))

        ((textual)
          (assert (eq? buf mainbuf))
          (assert (= 0 auxlim))
          (assert (< m 4))
          (r6rs:bytevector-copy! mainbuf mainptr auxbuf 0 m)
          (tuple-set! p
            port.mainpos
            (+ (tuple-ref p port.mainpos) mainptr))
          (tuple-set! p port.mainptr 0)
          (tuple-set! p port.mainlim 0)
          (tuple-set! p port.auxptr 0)
          (tuple-set! p port.auxlim m)
          (io/fill-buffer! p)
          (io/get-char p lookahead?))

        ((auxstart)
          (assert (eq? buf auxbuf))
          (assert (= 0 auxptr))
          (assert (< n 4))
          (if (>= m 2)
            (begin

              ; Copy one byte from mainbuf to auxbuf,
              ; and move mainbuf down by 1.
              ; FIXME:  This is grossly inefficient, but works for now.

              (bytevector-set! auxbuf auxlim (bytevector-ref mainbuf 1))
              (r6rs:bytevector-copy! mainbuf 2 mainbuf 1 (- m 2))
              (tuple-set! p port.mainlim (- mainlim 1))
              (tuple-set! p port.auxlim (+ auxlim 1))
              (io/get-char p lookahead?))

            (begin
              (io/fill-buffer! p)
              (io/get-char p lookahead?))))

        (else
          ; state is closed, error, eof, or binary
          (error 'io/get-char-utf-8 "internal error" state)))))

  (define (finish k sv)
    (if (not lookahead?)
      (let ((mainbuf (tuple-ref p port.mainbuf))
            (mainptr (tuple-ref p port.mainptr))
            (mainpos (tuple-ref p port.mainpos)))
        (if (eq? mainbuf buf)
          (begin (tuple-set! p port.mainpos (+ mainpos (- 1 k)))
            (tuple-set! p port.mainptr (+ k mainptr)))
          (begin (tuple-set! p port.mainpos (+ mainpos 1))
            (io/consume-byte-from-auxbuf! p)
            (io/consume-byte-from-auxbuf! p)
            (if (> k 2) (io/consume-byte-from-auxbuf! p))
            (if (> k 3) (io/consume-byte-from-auxbuf! p))))))
    (case sv
      ((#x85 #x2028)
        (io/return-eol p lookahead? sv))
      (else
        (integer->char sv))))

  (define (decode2) ; decodes 2 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1))))
      (if (<= #x80 unit2 #xbf)
        (finish 2
          (logior
            (ash (logand #b00011111 unit) 6)
            (logand #b00111111 (bytevector-ref buf (+ ptr 1)))))
        (decoding-error (list unit unit2)))))

  (define (decode3) ; decodes 3 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1)))
          (unit3 (bytevector-ref buf (+ ptr 2))))
      (cond ((or (and (= unit #xe0)
                  (< unit2 #xa0))
               (not (<= #x80 unit2 #xbf)))
             (decoding-error (list unit unit2)))
        ((not (<= #x80 unit3 #xbf))
          (decoding-error (list unit unit2 unit3)))
        (else
          (finish 3
            (logior
              (ash (logand #b00001111 unit) 12)
              (logior
                (ash (logand #x3f unit2) 6)
                (logand #x3f unit3))))))))

  (define (decode4) ; decodes 4 bytes
    (let ((unit2 (bytevector-ref buf (+ ptr 1)))
          (unit3 (bytevector-ref buf (+ ptr 2)))
          (unit4 (bytevector-ref buf (+ ptr 3))))
      (cond ((or (and (= unit #xf0)
                  (< unit2 #x90))
               (and (= unit #xf4)
                 (> unit2 #x8f))
               (not (<= #x80 unit2 #xbf)))
             (decoding-error (list unit unit2)))
        ((not (<= #x80 unit3 #xbf))
          (decoding-error (list unit unit2 unit3)))
        ((not (<= #x80 unit4 #xbf))
          (decoding-error (list unit unit2 unit3 unit4)))
        (else
          (finish 4
            (logior
              (logior
                (ash (logand #b00000111 unit) 18)
                (ash (logand #x3f unit2) 12))
              (logior
                (ash (logand #x3f unit3) 6)
                (logand #x3f unit4))))))))

  (define n (- lim ptr))

  (cond ((< n 2)
         (read-more-bytes))
    ((<= unit #xc1)
      (decoding-error (list unit)))
    ((<= unit #xdf)
      (decode2))
    ((< n 3)
      (read-more-bytes))
    ((<= unit #xef)
      (decode3))
    ((< n 4)
      (read-more-bytes))
    ((<= unit #xf4)
      (decode4))
    (else
      (decoding-error (list unit)))))

; The special case of get-char and lookahead-char on a textual port
; that's in the auxstart state, where it reads from auxbuf instead
; of mainbuf.
; The port is known to be a textual input port in the auxstart state.

(define (io/get-char-auxstart p lookahead?)

  (assert (eq? 'auxstart (tuple-ref p port.state)))

  (let ((buf (tuple-ref p port.auxbuf))
        (ptr (tuple-ref p port.auxptr))
        (lim (tuple-ref p port.auxlim)))

    (cond ((< ptr lim)
           (let ((unit (bytevector-ref buf ptr)))
             (cond ((<= unit #x7f)
                    (cond ((> unit 13)
                           ; not #\linefeed, #\return, #\nel, or #\x2028
                           (if (not lookahead?)
                             (let ((pos (tuple-ref p port.mainpos)))
                               (tuple-set! p port.mainpos (+ pos 1))
                               (io/consume-byte-from-auxbuf! p)))
                           (integer->char unit))
                      ((or (= unit 10) ; #\linefeed
                          (= unit 13)) ; #\return
                        (let ((pos (tuple-ref p port.mainpos)))
                          (if (not lookahead?)
                            (begin
                              (tuple-set! p port.mainpos (+ pos 1))
                              (io/consume-byte-from-auxbuf! p)))
                          (io/return-eol p lookahead? unit)))
                      (else
                        (if (not lookahead?)
                          (let ((pos (tuple-ref p port.mainpos)))
                            (tuple-set! p port.mainpos (+ pos 1))
                            (io/consume-byte-from-auxbuf! p)))
                        (integer->char unit))))
               ((let ((codec (logand
                              transcoder-mask:codec
                              (tuple-ref p port.transcoder))))
                   (= codec codec:latin-1))
                 ; Latin-1
                 (if (not lookahead?)
                   (let ((pos (tuple-ref p port.mainpos)))
                     (tuple-set! p port.mainpos (+ pos 1))
                     (io/consume-byte-from-auxbuf! p)))
                 (if (= unit #x85)
                   (io/return-eol p lookahead? unit)
                   (integer->char unit)))
               (else
                 (io/get-char-utf-8 p lookahead? unit buf ptr lim)))))
      (else
        ; In the auxstart state, auxbuf should always be nonempty.
        (error 'io/get-char-auxstart "internal error" p)
        (eof-object)))))

; Given an input port in auxstart state, consumes a byte from its auxbuf.
; If that empties auxbuf, then the port enters a textual or auxend state.

(define (io/consume-byte-from-auxbuf! p)

  (define (leave-auxstart-state!)
    (let ((mainbuf (tuple-ref p port.mainbuf))
          (mainptr (tuple-ref p port.mainptr))
          (mainlim (tuple-ref p port.mainlim))
          (mainpos (tuple-ref p port.mainpos)))
      (assert (= 0 mainptr))
      (assert (< 0 mainlim))
      (tuple-set! p port.mainpos (- mainpos 1))
      (tuple-set! p port.mainptr 1)
      (if (< mainlim (bytevector-length mainbuf))
        (begin (bytevector-set! mainbuf mainlim port.sentinel)
          (tuple-set! p port.state 'textual))
        (begin (bytevector-set! (tuple-ref p port.auxbuf)
                0
                (bytevector-ref mainbuf (- mainlim 1)))
          (bytevector-set! mainbuf (- mainlim 1) port.sentinel)
          (tuple-set! p port.mainlim (- mainlim 1))
          (tuple-set! p port.auxptr 0)
          (tuple-set! p port.auxlim 1)
          (tuple-set! p port.state 'auxend)))))

  (assert (eq? 'auxstart (tuple-ref p port.state)))

  (let* ((ptr (tuple-ref p port.auxptr))
         (lim (tuple-ref p port.auxlim))
         (ptr+1 (+ ptr 1)))

    (cond ((= ptr+1 lim)
           (tuple-set! p port.auxptr 0)
           (tuple-set! p port.auxlim 0)
           (leave-auxstart-state!))
      (else
        (tuple-set! p port.auxptr ptr+1)))))

; Given an input textual port, consumes a byte from its buffers.
; This may cause a change of state.
; This procedure is called only during error handling
; and #\return handling, so it can be fairly slow.

(define (io/consume-byte! p)
  (let ((state (tuple-ref p port.state))
        (mainbuf (tuple-ref p port.mainbuf))
        (mainptr (tuple-ref p port.mainptr))
        (mainlim (tuple-ref p port.mainlim))
        (auxbuf (tuple-ref p port.auxbuf))
        (auxptr (tuple-ref p port.auxptr))
        (auxlim (tuple-ref p port.auxlim)))
    (case state
      ((auxstart)
        (io/consume-byte-from-auxbuf! p))
      ((textual auxend)
        (cond ((< mainptr mainlim)
               (tuple-set! p port.mainpos
                 (- (tuple-ref p port.mainpos) 1))
               (tuple-set! p port.mainptr (+ mainptr 1)))
          ((eq? state 'auxend)
            (assert (< auxptr auxlim))
            (r6rs:bytevector-copy! auxbuf auxptr mainbuf 0 (- auxlim auxptr))
            (bytevector-set! mainbuf (- auxlim auxptr) port.sentinel)
            (tuple-set! p
              port.mainpos
              (+ (tuple-ref p port.mainpos) mainptr))
            (tuple-set! p port.mainptr 0)
            (tuple-set! p port.mainlim (- auxlim auxptr))
            (tuple-set! p port.auxptr 0)
            (tuple-set! p port.auxlim 0)
            (tuple-set! p port.state 'textual)
            (io/consume-byte! p))
          (else
            (io/reset-buffers! p))))
      (else
        ; must be closed, error, eof
        (unspecified)))))

(define (io/port-fd p)
  (define ioproc (tuple-ref p port.ioproc))
  (define iodata (tuple-ref p port.iodata))
  ((ioproc 'fd) iodata))
