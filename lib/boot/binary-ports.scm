;;; R6RS binary input and output operations (rnrs io ports, 8.2.8 and 8.2.11) and bytevector ports.


(define (get-u8 p)
  (if (and (port? p) (%ports/input-port? p) (%ports/binary? p))
    (%ports/get-byte p)
    (assertion-violation 'get-u8 "not an open binary input port" p)))

(define (lookahead-u8 p)
  (if (and (port? p) (%ports/input-port? p) (%ports/binary? p))
    (let ([b (%ports/get-byte p)])
      (if (eof-object? b)
        b
        (begin
          (%ports/push-back-one! p b)
          b)))
    (assertion-violation 'lookahead-u8 "not an open binary input port" p)))

(define (put-u8 p b)
  (if (and (port? p)
       (%ports/output-port? p)
       (%ports/binary? p)
       (fixnum? b)
       (<= 0 b 255))
    (%ports/put-byte p b)
    (assertion-violation 'put-u8 "not an open binary output port or invalid octet" p b)))

(define (get-bytevector-n! p bv start count)
  (if (and (port? p)
       (%ports/input-port? p)
       (%ports/binary? p)
       (bytevector? bv)
       (fixnum? start)
       (fixnum? count)
       (<= 0 start)
       (<= 0 count)
       (<= (+ start count) (bytevector-length bv)))
    (if (= count 0)
      0
      (let loop ([i start] [n (+ start count)])
        (cond ((= i n)
               (- i start))
          (else
            (let ([b (get-u8 p)])
              (cond ((eof-object? b)
                     (if (= i start) (eof-object) (- i start)))
                (else
                  (bytevector-u8-set! bv i b)
                  (loop (+ i 1) n))))))))
    (assertion-violation 'get-bytevector-n! "illegal arguments" p bv start count)))

(define (get-bytevector-n p count)
  (if (and (port? p)
       (%ports/input-port? p)
       (%ports/binary? p)
       (fixnum? count)
       (<= 0 count))
    (let* ([bv (make-bytevector count 0)]
           [n (get-bytevector-n! p bv 0 count)])
      (cond ((eof-object? n) (eof-object))
        ((= n count) bv)
        (else
          (let ([bv2 (make-bytevector n 0)])
            (r6rs:bytevector-copy! bv 0 bv2 0 n)
            bv2))))
    (assertion-violation 'get-bytevector-n "illegal arguments" p count)))

(define (get-bytevector-some p)
  (if (and (port? p) (%ports/input-port? p) (%ports/binary? p))
    (let loop ()
      (let ([n (%ports/buffered-input p)])
        (cond ((> n 0)
               (let ([bv (make-bytevector n 0)])
                 (r6rs:bytevector-copy!
                   (%port-in-buf p)
                   (%port-in-ptr p)
                   bv
                   0
                   n)
                 (%port-in-ptr-set! p (+ (%port-in-ptr p) n))
                 bv))
          ((eq? (%port-state p) 'eof)
            (eof-object))
          (else
            (if (%ports/refill! p) (loop) (eof-object))))))
    (assertion-violation 'get-bytevector-some "not an open binary input port" p)))

(define (get-bytevector-all p)
  (if (and (port? p) (%ports/input-port? p) (%ports/binary? p))
    (let* ([out (open-output-bytevector)])
      (let loop ()
        (let ([b (get-u8 p)])
          (if (eof-object? b)
            (let ([bv (get-output-bytevector out)])
              (close-port out)
              (if (= 0 (bytevector-length bv)) (eof-object) bv))
            (begin
              (%ports/put-byte out b)
              (loop))))))
    (assertion-violation 'get-bytevector-all "not an open binary input port" p)))

(define (put-bytevector p bv . rest)
  (define (put p bv start count)
    (if (and (port? p)
         (%ports/output-port? p)
         (%ports/binary? p)
         (bytevector? bv)
         (fixnum? start)
         (fixnum? count)
         (<= 0 start)
         (<= 0 count)
         (<= (+ start count) (bytevector-length bv)))
      (let loop ([i start] [n (+ start count)])
        (if (< i n)
          (begin
            (%ports/put-byte p (bytevector-u8-ref bv i))
            (loop (+ i 1) n))
          (unspecified)))
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
;;; The handler data is a vector
;;;   (vector 'bytevector-input-port | 'bytevector-output-port |
;;;           'bytevector-input/output-port bv i limit)
;;; where i is the current position and limit is the current size of
;;; the output.  i may exceed limit after set-port-position!, matching
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

(define (bytevector-input-port? p)
  (and (port? p)
    (let ([d (%handler-data (%port-handler p))])
      (and (vector? d)
        (> (vector-length d) 0)
        (memq (vector-ref d 0) '(bytevector-input-port bytevector-input/output-port))))))

(define (bytevector-output-port? p)
  (and (port? p)
    (let ([d (%handler-data (%port-handler p))])
      (and (vector? d)
        (> (vector-length d) 0)
        (memq (vector-ref d 0) '(bytevector-output-port bytevector-input/output-port))))))

(define (open-input-bytevector bv)
  (if (not (bytevector? bv))
    (assertion-violation 'open-input-bytevector "illegal argument" bv))
  (let ([data (vector 'bytevector-input-port (bytevector-copy bv) 0 (bytevector-length bv))])
    (%ports/make-port
      "*bytevector*"
      'input
      #f
      'block
      (%ports/make-bytevector-handler data))))

(define (open-output-bytevector)
  (let ([data (vector 'bytevector-output-port (make-bytevector 32 0) 0 0)])
    (%ports/make-port
      "*bytevector*"
      'output
      #f
      'block
      (%ports/make-bytevector-handler data))))

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

(define (get-output-bytevector p)
  (if (not (bytevector-output-port? p))
    (assertion-violation 'get-output-bytevector "illegal argument" p))
  (flush-output-port p)
  (let* ([data (%handler-data (%port-handler p))]
         [bv (vector-ref data 1)]
         [limit (vector-ref data 3)]
         [r (make-bytevector limit 0)])
    (r6rs:bytevector-copy! bv 0 r 0 limit)
    r))

(define (reset-output-bytevector p)
  (if (not (bytevector-output-port? p))
    (assertion-violation 'reset-output-bytevector "illegal argument" p))
  (flush-output-port p)
  (let ([data (%handler-data (%port-handler p))])
    (vector-set! data 1 (make-bytevector 32 0))
    (vector-set! data 2 0)
    (vector-set! data 3 0)
    (unspecified)))

;;; eof
