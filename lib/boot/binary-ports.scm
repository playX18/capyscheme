;;; R6RS binary input and output operations (rnrs io ports, 8.2.8 and 8.2.11) and bytevector ports.

(define (%ports/binary-in-guard p who)
  (unless (%ports/binary-input? p)
    (assertion-violation who "not an open binary input port" p)))

(define (get-u8 p)
  (if (%ports/binary-input? p)
    (%ports/get-byte p)
    (assertion-violation 'get-u8 "not an open binary input port" p)))

(define (lookahead-u8 p)
  (if (%ports/binary-input? p)
    (%ports/peek-byte p)
    (assertion-violation 'lookahead-u8 "not an open binary input port" p)))

(define (put-u8 p b)
  (if (and (%ports/binary-output? p)
       (fixnum? b)
       (<= 0 b 255))
    (%ports/put-byte p b)
    (assertion-violation 'put-u8 "not an open binary output port or invalid octet" p b)))

;;; Copies up to count buffered bytes (refilling as needed) into bv at
;;; start.  Returns the number of bytes copied -- fewer than count only at
;;; end of file.
(define (%ports/read-bytes! p bv start count)
  (when (%ports/input-output? p)
    (%ports/flush-buffer p))
  (let loop ([start start] [count count] [total 0])
    (if (= count 0)
      total
      (let ([avail (- (%port-in-lim p) (%port-in-ptr p))])
        (if (> avail 0)
          (let ([n (min avail count)])
            (r6rs:bytevector-copy! (%port-in-buf p) (%port-in-ptr p) bv start n)
            (%ports/advance! p n)
            (loop (+ start n) (- count n) (+ total n)))
          (if (%ports/ensure-input p 1)
            (loop start count total)
            total))))))

(define (get-bytevector-n! p bv start count)
  (if (and (%ports/binary-input? p)
       (bytevector? bv)
       (fixnum? start)
       (fixnum? count)
       (<= 0 start)
       (<= 0 count)
       (<= (+ start count) (bytevector-length bv)))
    (if (= count 0)
      0
      (let ([n (%ports/read-bytes! p bv start count)])
        (if (= n 0) (eof-object) n)))
    (assertion-violation 'get-bytevector-n! "illegal arguments" p bv start count)))

(define (get-bytevector-n p count)
  (if (and (%ports/binary-input? p)
       (fixnum? count)
       (<= 0 count))
    (if (= count 0)
      (make-bytevector 0 0)
      ;; do not trust count for the allocation: a huge request on a
      ;; short stream must not allocate that much up front
      (let ([bv (make-bytevector (min count %ports/file-buffer-size) 0)])
        (let loop ([bv bv] [have 0])
          (let ([n (%ports/read-bytes! p bv have (- (bytevector-length bv) have))])
            (let ([have (+ have n)])
              (cond ((< have (bytevector-length bv))
                     ;; end of file
                     (cond ((= have 0) (eof-object))
                       (else (bytevector-copy bv 0 have))))
                ((= have count) bv)
                (else
                  (let ([bigger (make-bytevector (min count (* 2 have)) 0)])
                    (r6rs:bytevector-copy! bv 0 bigger 0 have)
                    (loop bigger have)))))))))
    (assertion-violation 'get-bytevector-n "illegal arguments" p count)))

(define (get-bytevector-some p)
  (if (%ports/binary-input? p)
    (let loop ()
      (when (%ports/input-output? p)
        (%ports/flush-buffer p))
      (let ([n (%ports/buffered-input p)])
        (cond ((> n 0)
               (let ([bv (make-bytevector n 0)])
                 (r6rs:bytevector-copy!
                   (%port-in-buf p)
                   (%port-in-ptr p)
                   bv
                   0
                   n)
                 (%ports/advance! p n)
                 bv))
          ((eq? (%port-state p) 'eof)
            (eof-object))
          (else
            (if (%ports/refill! p) (loop) (eof-object))))))
    (assertion-violation 'get-bytevector-some "not an open binary input port" p)))

(define (get-bytevector-all p)
  (if (%ports/binary-input? p)
    (let loop ([bv (make-bytevector %ports/file-buffer-size 0)] [have 0])
      (let* ([n (%ports/read-bytes! p bv have (- (bytevector-length bv) have))]
             [have (+ have n)])
        (cond ((< have (bytevector-length bv))
               (if (= have 0)
                 (eof-object)
                 (bytevector-copy bv 0 have)))
          (else
            (let ([bigger (make-bytevector (* 2 have) 0)])
              (r6rs:bytevector-copy! bv 0 bigger 0 have)
              (loop bigger have))))))
    (assertion-violation 'get-bytevector-all "not an open binary input port" p)))

(define (put-bytevector p bv . rest)
  (define (put p bv start count)
    (if (and (%ports/binary-output? p)
         (bytevector? bv)
         (fixnum? start)
         (fixnum? count)
         (<= 0 start)
         (<= 0 count)
         (<= (+ start count) (bytevector-length bv)))
      (%ports/put-bytes p bv start count)
      (assertion-violation 'put-bytevector "illegal arguments" p bv start count)))
  (cond ((null? rest)
         (put p bv 0 (bytevector-length bv)))
    ((null? (cdr rest))
      (put p bv (car rest) (- (bytevector-length bv) (car rest))))
    ((null? (cddr rest))
      (put p bv (car rest) (cadr rest)))
    (else
      (assertion-violation 'put-bytevector "too many arguments" p bv rest))))

;;; ---------------------------------------------------------------
;;; Bytevector ports.
;;;
;;; Input: the port's input buffer is the source bytevector itself (no
;;; copy -- the source must not be modified while the port is in use).
;;; Output: the port's output buffer is the growing result (see the
;;; memory field in ports-core.scm), so bytes are written exactly once.
;;; Input/output bytevector ports keep the generic handler design:
;;;   (vector 'bytevector-input/output-port bv i limit)
;;; where i is the current position and limit is the current size of
;;; the data.  i may exceed limit after set-port-position!, matching
;;; the R6RS lseek-like semantics.
;;; ---------------------------------------------------------------

(define (%ports/make-bytevector-handler data)
  (define (read! buf start count)
    (let* ([bv (vector-ref data 1)]
           [i (vector-ref data 2)]
           [limit (vector-ref data 3)]
           [n (max 0 (min count (- limit i)))])
      (if (= n 0)
        0
        (begin
          (r6rs:bytevector-copy! bv i buf start n)
          (vector-set! data 2 (+ i n))
          n))))
  (define (write! buf start count)
    (let* ([bv (vector-ref data 1)]
           [i (vector-ref data 2)]
           [limit (vector-ref data 3)]
           [need (+ i count)])
      (when (> need (bytevector-length bv))
        (let ([new (make-bytevector (max (* 2 need) 32) 0)])
          (r6rs:bytevector-copy! bv 0 new 0 limit)
          (vector-set! data 1 new)
          (set! bv new)))
      (r6rs:bytevector-copy! buf start bv i count)
      (vector-set! data 2 need)
      (when (> need limit)
        (vector-set! data 3 need))
      count))
  (define (get-position) (vector-ref data 2))
  (define (set-position! pos)
    (if (and (fixnum? pos) (>= pos 0))
      (vector-set! data 2 pos)
      (assertion-violation 'set-port-position! "invalid position" pos)))
  (%make-handler
    read!
    write!
    (lambda () (unspecified))
    (lambda () #t)
    get-position
    set-position!
    data))


(define %ports/memory-handler
  (%make-handler
    (lambda (buf start count) 0)
    (lambda (buf start count) count)
    #f
    (lambda () #t)
    (lambda () 0)
    (lambda (pos) (unspecified))
    #f))

;;; Creates a memory input port over bv (no copy).
(define (%ports/make-memory-input-port bv transcoder)
  (let ([p (%ports/make-port "*bytevector*" 'input transcoder 'block
             %ports/memory-handler
             '(memory in))])
    (%port-in-buf-set! p bv)
    (%port-in-lim-set! p (bytevector-length bv))
    p))

;;; Creates an empty memory output port -- kind is 'out or 'sout.
(define (%ports/make-memory-output-port transcoder kind)
  (let ([p (%ports/make-port "*bytevector*" 'output transcoder 'block
             %ports/memory-handler
             (list 'memory kind))])
    (%port-out-buf-set! p (make-bytevector %ports/memory-initial-size 0))
    p))

(define (%ports/legacy-bytevector-data p)
  (let ([d (%handler-data (%port-handler p))])
    (and (vector? d) (> (vector-length d) 0) d)))

(define (bytevector-input-port? p)
  (and (port? p)
    (or (eq? (%port-memory p) 'in)
      (let ([d (%ports/legacy-bytevector-data p)])
        (and d (memq (vector-ref d 0) '(bytevector-input/output-port)) #t)))))

(define (bytevector-output-port? p)
  (and (port? p)
    (or (eq? (%port-memory p) 'out)
      (eq? (%port-memory p) 'sout)
      (let ([d (%ports/legacy-bytevector-data p)])
        (and d (memq (vector-ref d 0) '(bytevector-input/output-port)) #t)))))

(define (open-input-bytevector bv)
  (if (not (bytevector? bv))
    (assertion-violation 'open-input-bytevector "illegal argument" bv))
  (%ports/make-memory-input-port bv #f))

(define (open-output-bytevector)
  (%ports/make-memory-output-port #f 'out))

(define (open-input/output-bytevector bv)
  (if (not (bytevector? bv))
    (assertion-violation 'open-input/output-bytevector "illegal argument" bv))
  (let ([data (vector 'bytevector-input/output-port (bytevector-copy bv) 0 (bytevector-length bv))])
    (%ports/make-port
      "*bytevector*"
      'input-output
      #f
      'block
      (%ports/make-bytevector-handler data))))

;;; Copies the bytes accumulated by a bytevector output port.

(define (get-output-bytevector p)
  (if (not (bytevector-output-port? p))
    (assertion-violation 'get-output-bytevector "illegal argument" p))
  (cond ((%port-memory p)
         (bytevector-copy (%port-out-buf p) 0 (%ports/memory-size p)))
    (else
      (flush-output-port p)
      (let* ([data (%handler-data (%port-handler p))]
             [bv (vector-ref data 1)]
             [limit (vector-ref data 3)])
        (bytevector-copy bv 0 limit)))))

(define (reset-output-bytevector p)
  (if (not (bytevector-output-port? p))
    (assertion-violation 'reset-output-bytevector "illegal argument" p))
  (cond ((%port-memory p)
         (%port-out-buf-set! p (make-bytevector %ports/memory-initial-size 0))
         (%port-out-lim-set! p 0)
         (%port-hw-set! p 0))
    (else
      (flush-output-port p)
      (let ([data (%handler-data (%port-handler p))])
        (vector-set! data 1 (make-bytevector 32 0))
        (vector-set! data 2 0)
        (vector-set! data 3 0))))
  (unspecified))

;;; eof
