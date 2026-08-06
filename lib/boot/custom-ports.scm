;;; R6RS custom-port constructors (rnrs io ports, 8.2.7, 8.2.10, 8.2.13).  
;;;The user-supplied procedures are dropped directly into a port-handler; binary ports
;;; use them as-is and textual ports adapt them to the byte-oriented
;;; handler protocol with a UTF-8 transcoder.  

;;; Binary input read-method: calls the user's read! once and maps the
;;; result to a byte count (0 = EOF).
(define (%custom-port/binary-read-method read!)
  (lambda (buffer start count)
    (let ([n (read! buffer start count)])
      (cond ((not (fixnum? n)) 0)
        ((> n 0) n)
        (else 0)))))

;;; Binary output write-method: calls the user's write! repeatedly
;;; until all bytes are written.
(define (%custom-port/binary-write-method write!)
  (lambda (buffer start count)
    (let loop ([i 0] [n count])
      (if (= n 0)
        count
        (let ([k (write! buffer (+ start i) n)])
          (cond ((not (fixnum? k)) 0)
            ((= k 0) 0) ; no progress
            ((<= k n) (loop (+ i k) (- n k)))
            (else 0)))))))

(define (%custom-port/binary-handler read! write! get-position set-position! close)
  (%make-handler
    (and read! (%custom-port/binary-read-method read!))
    (and write! (%custom-port/binary-write-method write!))
    (if close (lambda () (close)) #f)
    (lambda () #t)
    get-position
    set-position!
    #f))

(define (make-custom-binary-input-port id read! get-position set-position! close)
  (if (and (string? id)
       (procedure? read!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'input
      #f
      'block
      (%custom-port/binary-handler read! #f get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-binary-input-port
      "illegal argument(s)" id read! get-position set-position! close)))

(define (make-custom-binary-output-port id write! get-position set-position! close)
  (if (and (string? id)
       (procedure? write!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'output
      #f
      'block
      (%custom-port/binary-handler #f write! get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-binary-output-port
      "illegal argument(s)" id write! get-position set-position! close)))

(define (make-custom-binary-input/output-port id read! write! get-position set-position! close)
  (if (and (string? id)
       (procedure? read!)
       (procedure? write!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'input-output
      #f
      'block
      (%custom-port/binary-handler read! write! get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-binary-input/output-port
      "illegal argument(s)" id read! write! get-position set-position! close)))


;;; Textual custom ports.
;;;
;;; The user's read! / write! operate on strings.  The handler adapts
;;; the byte-oriented protocol: input converts the user's characters
;;; to UTF-8 bytes, output decodes the buffered UTF-8 bytes to a
;;; string for the user.

;;; Textual input read-method: asks the user for up to count/4
;;; characters (UTF-8 uses at most 4 bytes per character) and encodes
;;; them to UTF-8 bytes.
(define (%custom-port/textual-read-method read!)
  (lambda (buffer start count)
    (let* ([nchars (quotient count 4)]
           [s (make-string nchars #\space)]
           [n (read! s 0 nchars)])
      (cond ((not (fixnum? n)) 0)
        ((> n 0)
          (let* ([s (if (= n nchars) s (substring s 0 n))]
                 [bv (string->utf8 s)]
                 [k (bytevector-length bv)])
            (r6rs:bytevector-copy! bv 0 buffer start k)
            k))
        (else 0)))))

;;; Textual output write-method: decodes the buffered UTF-8 bytes to a
;;; string and hands it to the user's write!.

(define (%custom-port/textual-write-method write!)
  (lambda (buffer start count)
    (let* ([bv (bytevector-copy buffer start (+ start count))]
           [s (utf8->string bv)]
           [n (string-length s)])
      (let loop ([i 0] [m n])
        (if (= m 0)
          count
          (let ([k (write! s i m)])
            (cond ((not (fixnum? k)) 0)
              ((= k 0) 0)
              ((<= k m) (loop (+ i k) (- m k)))
              (else 0))))))))

(define (%custom-port/textual-handler read! write! get-position set-position! close)
  (%make-handler
    (and read! (%custom-port/textual-read-method read!))
    (and write! (%custom-port/textual-write-method write!))
    (if close (lambda () (close)) #f)
    (lambda () #t)
    get-position
    set-position!
    #f))

(define (make-custom-textual-input-port id read! get-position set-position! close)
  (if (and (string? id)
       (procedure? read!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'input
      (make-transcoder (utf-8-codec) 'none 'ignore)
      'block
      (%custom-port/textual-handler read! #f get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-textual-input-port
      "illegal argument(s)" id read! get-position set-position! close)))

(define (make-custom-textual-output-port id write! get-position set-position! close)
  (if (and (string? id)
       (procedure? write!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'output
      (make-transcoder (utf-8-codec) 'none 'replace)
      'block
      (%custom-port/textual-handler #f write! get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-textual-output-port
      "illegal argument(s)" id write! get-position set-position! close)))

(define (make-custom-textual-input/output-port id read! write! get-position set-position! close)
  (if (and (string? id)
       (procedure? read!)
       (procedure? write!)
       (or (procedure? get-position) (eq? get-position #f))
       (or (procedure? set-position!) (eq? set-position! #f))
       (or (procedure? close) (eq? close #f)))
    (%ports/make-port
      id
      'input-output
      (make-transcoder (utf-8-codec) 'none 'ignore)
      'block
      (%custom-port/textual-handler read! write! get-position set-position! close)
      'custom)
    (assertion-violation 'make-custom-textual-input/output-port
      "illegal argument(s)" id read! write! get-position set-position! close)))

;;; eof
