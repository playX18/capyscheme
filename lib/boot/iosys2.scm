;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Basic operations.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The general case for get-u8 and lookahead-u8.

(define (io/get-u8 p lookahead?)
  (assert (port? p))

  (let ((type (tuple-ref p port.type))
        (buf (tuple-ref p port.mainbuf))
        (ptr (tuple-ref p port.mainptr))
        (lim (tuple-ref p port.mainlim)))

    (cond ((not (eq? type type:binary-input))
           (if (eq? type type:binary-input/output)
             (io/get-u8-input/output p lookahead?)
             (assertion-violation 'get-u8
               "not an open binary input port"
               p)))
      ((< ptr lim)
        (let ((byte (bytevector-ref buf ptr)))
          (if (not lookahead?)
            (tuple-set! p port.mainptr (+ ptr 1)))
          byte))
      ((eq? (tuple-ref p port.state) 'eof)
        (eof-object))
      (else
        ; The error state will be handled by io/fill-buffer!.
        (io/fill-buffer! p)
        (io/get-u8 p lookahead?)))))

; The general case for get-char and lookahead-char.
; The transcoder will be for Latin-1 or UTF-8.

(define (io/get-char p lookahead?)
  (assert (port? p))
  (let ((type (tuple-ref p port.type))
        (buf (tuple-ref p port.mainbuf))
        (ptr (tuple-ref p port.mainptr))
        (lim (tuple-ref p port.mainlim)))

    (cond ((not (eq? type type:textual-input))
           (if (eq? type type:textual-input/output)
             (io/get-char-input/output p lookahead?)
             (assertion-violation 'get-char
               "argument not a textual input port"
               p)))
      ((< ptr lim)
        (let ((unit (bytevector-ref buf ptr)))
          (cond ((<= unit #x7f)
                 (cond ((> unit 13)
                        ; not #\linefeed, #\return, #\nel, or #\x2028
                        (if (not lookahead?)
                          (tuple-set! p port.mainptr (+ ptr 1)))
                        (integer->char unit))
                   ((or (= unit 10) ; #\linefeed
                       (= unit 13)) ; #\return
                     (if (not lookahead?)
                       (tuple-set! p port.mainptr (+ ptr 1)))
                     (io/return-eol p lookahead? unit))
                   (else
                     (if (not lookahead?)
                       (tuple-set! p port.mainptr (+ ptr 1)))
                     (integer->char unit))))
            ((and (= unit port.sentinel)
                (= ptr 0)
                (eq? (tuple-ref p port.state) 'auxstart))
              (io/get-char-auxstart p lookahead?))

            (else
              (let ((state (tuple-ref p port.state)))
                (case state
                  ((eof) (eof-object))
                  ((error) (error 'get-char "permanent read error" p))
                  ((textual auxend)
                    (let ((codec (logand
                                  transcoder-mask:codec
                                  (tuple-ref p port.transcoder))))
                      (cond ((= codec codec:latin-1)
                             ; Latin-1
                             (if (not lookahead?)
                               (tuple-set!
                                 p
                                 port.mainptr
                                 (+ ptr 1)))
                             (if (= unit #x85)
                               (io/return-eol p lookahead? unit)
                               (integer->char unit)))
                        ((= codec codec:utf-8)
                          (io/get-char-utf-8
                            p
                            lookahead?
                            unit
                            buf
                            ptr
                            lim))
                        (else
                          (error 'io/get-char
                            "unimplemented codec"
                            codec
                            p)
                          (eof-object)))))
                  (else
                    (error 'io/get-char "internal error" state p))))))))
      ((eq? (tuple-ref p port.state) 'eof)
        (io/reset-buffers! p) ; FIXME: probably redundant
        (eof-object))
      ((eq? (tuple-ref p port.state) 'error)
        (io/reset-buffers! p) ; FIXME: probably redundant
        (error 'get-char "permanent read error" p))
      ((eq? (tuple-ref p port.state) 'auxend)
        (let* ((auxbuf (tuple-ref p port.auxbuf))
               (auxptr (tuple-ref p port.auxptr))
               (auxlim (tuple-ref p port.auxlim))
               (n (- auxlim auxptr))
               (mainbuf (tuple-ref p port.mainbuf)))
          (assert (< auxptr auxlim))
          (r6rs:bytevector-copy! auxbuf auxptr mainbuf 0 n)
          (bytevector-set! mainbuf n port.sentinel)
          (tuple-set! p
            port.mainpos
            (+ (tuple-ref p port.mainpos) ptr))
          (tuple-set! p port.mainptr 0)
          (tuple-set! p port.mainlim n)
          (tuple-set! p port.auxptr 0)
          (tuple-set! p port.auxlim 0)
          (tuple-set! p port.state 'textual)
          (io/get-char p lookahead?)))
      (else
        (io/reset-buffers! p)
        (io/fill-buffer! p)
        (io/get-char p lookahead?)))))

(define (io/put-u8 p byte)
  (assert (port? p))
  (let ((type (tuple-ref p port.type))
        (buf (tuple-ref p port.mainbuf))
        (lim (tuple-ref p port.mainlim)))
    (cond ((eq? type type:binary-output)
           (cond ((< lim (bytevector-length buf))
                  (bytevector-set! buf lim byte)
                  (tuple-set! p port.mainlim (+ lim 1))
                  (unspecified))
             (else
               (io/flush-buffer p)
               (io/put-u8 p byte))))
      ((eq? type type:binary-input/output)

        (io/put-u8-input/output p byte))
      (else
        (error 'put-u8 "not a binary output port" p)
        #t))))

(define (io/put-char p c)
  (unless (char? c)
    (error 'put-char "not a character" c))
  (if (port? p)
    (let ((type (tuple-ref p port.type))
          (buf (tuple-ref p port.mainbuf))
          (lim (tuple-ref p port.mainlim)))
      (cond ((eq? type type:textual-output)
             (let ((sv (char->integer c))
                   (n (bytevector-length buf)))
               (cond ((>= lim n)
                      (io/flush-buffer p)
                      (io/put-char p c))
                 ((= sv 10)
                   (io/put-eol p))
                 ((<= sv #x7f)
                   (bytevector-set! buf lim sv)
                   (tuple-set! p port.mainlim (+ lim 1))
                   (unspecified))
                 ((and (<= sv #xff)
                     (= codec:latin-1
                       (logand
                         transcoder-mask:codec
                         (tuple-ref p port.transcoder))))
                   (bytevector-set! buf lim sv)
                   (tuple-set! p port.mainlim (+ lim 1))
                   (unspecified))
                 ((not (= codec:utf-8
                        (logand
                          transcoder-mask:codec
                          (tuple-ref p port.transcoder))))
                   (let* ((t (tuple-ref p port.transcoder))
                          (mode (logand transcoder-mask:errmode t)))
                     (cond ((= mode errmode:ignore)
                            (unspecified))
                       ((= mode errmode:replace)
                         (io/put-char p #\?))
                       ((= mode errmode:raise)
                         (raise-i/o-encoding-error
                           'put-char
                           "encoding error"
                           p
                           c))
                       ;(raise-r6rs-exception
                       ; (make-i/o-encoding-error p c)
                       ; 'put-char "encoding error" (list p c)))
                       (else
                         (assertion-violation 'put-char
                           "internal error"
                           p
                           c)))))
                 ((>= lim (- n 4))
                   (io/flush-buffer p)
                   (io/put-char p c))
                 ((<= sv #x07ff)
                   (let ((u0 (logior (logand (ash sv -6)
                                      #x1f)
                              #xc0))
                         (u1 (logior #b10000000
                              (logand sv #b00111111)))
                         (pos (tuple-ref p port.mainpos)))
                     (bytevector-set! buf lim u0)
                     (bytevector-set! buf (+ lim 1) u1)
                     (tuple-set! p port.mainpos (- pos 1))
                     (tuple-set! p port.mainlim (+ lim 2))))
                 ((<= sv #xffff)
                   (let ((u0 (logior #b11100000
                              (ash sv -12)))
                         (u1 (logior #b10000000
                              (logand (ash sv -6)
                                #b00111111)))
                         (u2 (logior #b10000000
                              (logand sv #b00111111)))
                         (pos (tuple-ref p port.mainpos)))
                     (bytevector-set! buf lim u0)
                     (bytevector-set! buf (+ lim 1) u1)
                     (bytevector-set! buf (+ lim 2) u2)
                     (tuple-set! p port.mainpos (- pos 2))
                     (tuple-set! p port.mainlim (+ lim 3))))
                 (else
                   (let ((u0 (logior #b11110000
                              (ash sv -18)))
                         (u1 (logior #b10000000
                              (logand (ash sv -12)
                                #b00111111)))
                         (u2 (logior #b10000000
                              (logand (ash sv -6)
                                #b00111111)))
                         (u3 (logior #b10000000
                              (logand sv #b00111111)))
                         (pos (tuple-ref p port.mainpos)))
                     (bytevector-set! buf lim u0)
                     (bytevector-set! buf (+ lim 1) u1)
                     (bytevector-set! buf (+ lim 2) u2)
                     (bytevector-set! buf (+ lim 3) u3)
                     (tuple-set! p port.mainpos (- pos 3))
                     (tuple-set! p port.mainlim (+ lim 4)))))))
        ((eq? type type:textual-input/output)
          (io/put-char-input/output p c))
        (else
          (error 'put-char "not an output port" p)
          #t)))
    (begin (error "put-char: not an output port: " p)
      #t)))

; Operations on input/output ports, which are peculiar.
; Currently, the only input/output ports are
;
;     bytevector input/output ports
;     custom input/output ports
;     transcoded input/output ports
;
; These operations are invoked only when p is known to be an
; input/output port of the correct type (binary/textual).

(define (io/get-u8-input/output p lookahead?)
  (let* ((state (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainlim (tuple-ref p port.mainlim))
         (iodata (tuple-ref p port.iodata))
         (ioproc (tuple-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
      ((eq? state 'error)
        (error 'get-u8 "permanent read error on port " p)
        (eof-object))
      ((eq? state 'closed)
        (error 'get-u8 "read attempted on closed port " p))
      ((> mainlim 0)
        (let ((r (bytevector-ref mainbuf 0)))
          (if (not lookahead?)
            (let ((mainpos (tuple-ref p port.mainpos)))
              (tuple-set! p port.mainpos (+ mainpos 1))
              (tuple-set! p port.mainlim 0)))
          r))
      (else
        (let ((n ((ioproc 'read) iodata mainbuf)))
          (cond ((eq? n 1)
                 (let ((r (bytevector-ref mainbuf 0)))
                   (if (not lookahead?)
                     (let ((mainpos (tuple-ref p port.mainpos)))
                       (tuple-set! p port.mainpos (+ mainpos 1))
                       (tuple-set! p port.mainlim 0))
                     (tuple-set! p port.mainlim 1))
                   r))
            ((or (eq? n 0) (eq? n 'eof))
              (io/set-eof-state! p)
              (eof-object))
            (else
              (io/set-error-state! p)
              (io/get-u8-input/output p lookahead?))))))))

(define (io/get-char-input/output p lookahead?)
  (let* ((state (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainlim (tuple-ref p port.mainlim))
         (iodata (tuple-ref p port.iodata))
         (ioproc (tuple-ref p port.ioproc)))
    (cond ((eq? state 'eof)
           (eof-object))
      ((eq? state 'error)
        (error 'get-char "Read error on port " p)
        (eof-object))
      ((eq? state 'closed)
        (error 'get-char "Read attempted on closed port " p))
      ((> mainlim 0)
        (let* ((bv (make-bytevector mainlim))
               (s (begin (r6rs:bytevector-copy! mainbuf 0 bv 0 mainlim)
                   (utf8->string bv))))
          (if (not lookahead?)
            (let ((mainpos (tuple-ref p port.mainpos)))
              (tuple-set! p port.mainpos (+ mainpos 1))
              (tuple-set! p port.mainlim 0)))
          (string-ref s 0)))
      (else
        (let ((n ((ioproc 'read) iodata mainbuf)))
          (cond ((and (fixnum? n) (> n 0))
                 (let* ((bv (make-bytevector n))
                        (s (begin (r6rs:bytevector-copy! mainbuf 0 bv 0 n)
                            (utf8->string bv))))
                   (if (not lookahead?)
                     (let ((mainpos (tuple-ref p port.mainpos)))
                       (tuple-set! p port.mainpos (+ mainpos 1))
                       (tuple-set! p port.mainlim 0))
                     (tuple-set! p port.mainlim n))
                   (string-ref s 0)))
            ((or (eq? n 0) (eq? n 'eof))
              (io/set-eof-state! p)
              (eof-object))
            (else
              (io/set-error-state! p)
              (io/get-char-input/output p lookahead?))))))))

(define (io/put-u8-input/output p byte)
  (let* ((state (tuple-ref p port.state))
         (mainbuf (tuple-ref p port.mainbuf))
         (mainpos (tuple-ref p port.mainpos))
         (mainlim (tuple-ref p port.mainlim))
         (iodata (tuple-ref p port.iodata))
         (ioproc (tuple-ref p port.ioproc))
         (buf mainbuf))
    (cond ((eq? state 'error)
           (error 'put-u8 "permanent write error on port " p)
           (eof-object))
      ((eq? state 'closed)
        (error 'put-u8 "write attempted on closed port " p))
      ((> mainlim 0)
        (if (tuple-ref p port.setposn)
          (begin (io/set-port-position! p mainpos)
            (io/put-u8-input/output p byte))
          (begin (io/set-error-state! p)
            (error 'put-u8
              "input/output port without set-port-position!"
              p))))
      (else
        (bytevector-set! buf 0 byte)
        (let ((r ((ioproc 'write) iodata buf 1)))
          (cond ((eq? r 'ok)
                 (tuple-set! p port.mainpos (+ mainpos 1))
                 (unspecified))
            (else
              (io/set-error-state! p)
              (io/put-u8-input/output p byte))))))))

(define (io/put-char-input/output p c)
  (let* ((state (tuple-ref p port.state))
         (mainpos (tuple-ref p port.mainpos))
         (mainlim (tuple-ref p port.mainlim))
         (iodata (tuple-ref p port.iodata))
         (ioproc (tuple-ref p port.ioproc))
         (buf (string->utf8 (string c))))
    (cond ((eq? state 'error)
           (error 'put-char "permanent write error on port " p)
           (eof-object))
      ((eq? state 'closed)
        (error 'put-char "write attempted on closed port " p))
      ((> mainlim 0)

        ; Must correct for buffered lookahead character.

        (if (tuple-ref p port.setposn)
          (begin (io/set-port-position! p mainpos)
            (io/put-char-input/output p c))
          (begin (io/set-error-state! p)
            (error 'put-char
              "input/output port without set-port-position!"
              p))))
      (else
        (let* ((n0 (bytevector-length buf))
               (r ((ioproc 'write) iodata buf n0)))
          (cond ((eq? r 'ok)
                 (tuple-set! p port.mainpos (+ mainpos 1))
                 (unspecified))
            (else
              (io/set-error-state! p)
              (io/put-char-input/output p c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bulk i/o.
;;;
;;; Most of these handle a common case by returning the same
;;; value as a corresponding R6RS library procedure, but may
;;; fail on complex or unusual cases by returning #f.
;;;
;;; These should be majorly bummed, else there's no point.
;;;
;;; FIXME: could add a few more, such as
;;;     get-bytevector-n!
;;;     get-string-n!
;;;     put-bytevector
;;;
;;; Note, however, that io/put-string-maybe didn't help as much
;;; as io/get-line-maybe, and probably wasn't worth the effort
;;; and code size.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Handles the common case in which the line is all-Ascii,
; terminated by a linefeed, and lies entirely within the buffer.

(define (io/get-line-maybe p)
  (and (port? p)
    (let ((type (tuple-ref p port.type))
          (buf (tuple-ref p port.mainbuf))
          (ptr (tuple-ref p port.mainptr)))
      (define (loop i)
        (let ((unit (bytevector-ref buf i))) ; FIXME: should be trusted
          (cond ((and (< 13 unit) ; 13 = #\return
                   (< unit 128))
                 (loop (+ i 1)))
            ((= 10 unit) ; 10 = #\linefeed
              (let* ((n (- i ptr))
                     (s (make-string n)))
                (loop2 ptr i s 0)))
            (else #f))))
      (define (loop2 j k s i)
        (cond ((< j k)
               (string-set! s i (.integer->char:trusted
                                 (bytevector-ref buf j)))
               (loop2 (+ j 1) k s (+ i 1)))
          (else
            (tuple-set! p port.mainptr (+ k 1))
            s)))
      (and (eq? type type:textual-input)
        (not (tuple-ref p port.wasreturn))
        (loop ptr)))))

; Handles the common case in which the string is all-Ascii
; and can be buffered without flushing.

(define (io/put-string-maybe p s start count)
  (and (port? p)
    (string? s)
    (fixnum? start)
    (fixnum? count)
    (<= 0 start)
    (let ((k (+ start count))
          (n (string-length s))
          (type (tuple-ref p port.type))
          (buf (tuple-ref p port.mainbuf))
          (lim (tuple-ref p port.mainlim)))
      (define (loop i j)
        (cond ((< i k)
               (let* ((c (string-ref s i))
                      (sv (char->integer c)))
                 (if (and (< 10 sv) ; 10 = #\newline
                      (< sv 128))
                   (begin (bytevector-set! buf j sv) ; FIXME
                     (loop (+ i 1) (+ j 1)))
                   #f)))
          (else
            (tuple-set! p port.mainlim j)
            #t)))
      (and (< start n)
        (<= k n)
        (eq? type type:textual-output)
        (<= (+ lim count) (bytevector-length buf))
        (loop start lim)))))

; eof
