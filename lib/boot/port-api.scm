;;; Public API for Scheme ports:
;;; current ports, file options and buffer modes, the R6RS open-* and
;;; R7RS read-*/write-* procedures, string/bytevector conversion, the
;;; R5RS-style simple I/O procedures, and the kept extensions.
;;; Designed directly from R6RS and R7RS-small; not derived from
;;; Larceny or Ypsilon code.

;;; ---------------------------------------------------------------
;;; File options and buffer modes (R6RS 8.2.2, 8.2.3).
;;; ---------------------------------------------------------------

(define *file-option-symbols* '(no-create no-fail no-truncate))

(define no-create 'no-create)
(define no-fail 'no-fail)
(define no-truncate 'no-truncate)

(define *file-options-enumeration-set* #f)

(define (file-options-enumeration-set)
  (unless *file-options-enumeration-set*
    (set! *file-options-enumeration-set*
      (make-enumeration *file-option-symbols*)))
  *file-options-enumeration-set*)

(define (make-file-options-set syms)
  ((enum-set-constructor (file-options-enumeration-set)) syms))

(define (file-options->list options)
  (enum-set->list options))

(define (file-options . symbols)
  (make-file-options-set
    (filter (lambda (sym) (memq sym *file-option-symbols*))
      symbols)))

(define none 'none)
(define line 'line)
(define block 'block)

(define (buffer-mode mode)
  (case mode
    ((none line block) mode)
    (else
      (assertion-violation 'buffer-mode "invalid buffer mode" mode))))

(define (buffer-mode? mode)
  (case mode
    ((none line block) #t)
    (else #f)))


(define current-input-port
  (make-parameter #f (lambda (x) (input-port? x))))

(define current-output-port
  (make-parameter #f (lambda (x) (output-port? x))))


(define (current-error-port)
  (console-error-port))

(define (console-input-port)
  ((console-input-port-factory)))

(define (console-output-port)
  ((console-output-port-factory)))

(define (console-error-port)
  ((console-error-port-factory)))

(define console-input-port-factory
  (make-parameter console/console-input-port procedure?))

(define console-output-port-factory
  (make-parameter console/console-output-port procedure?))

(define console-error-port-factory
  (make-parameter console/console-error-port procedure?))

(define (initialize-io-system)
  (console/initialize)
  (current-input-port (console-input-port))
  (current-output-port (console-output-port))
  (unspecified))

(define (shutdown-io-system)
  (unspecified))

;;; ---------------------------------------------------------------
;;; Standard ports (R6RS 8.2.7, 8.2.10): fresh binary ports.
;;; ---------------------------------------------------------------

(define (standard-input-port)
  (let* ([fd (osdep/open-console 'input)]
         [p (apply %ports/make-port
              "*console-input*" 'input #f 'block
              (%console-handler fd)
              (list (list 'fd fd)))])
    p))

(define (standard-output-port)
  (let* ([fd (osdep/open-console 'output)]
         [p (apply %ports/make-port
              "*console-output*" 'output #f 'block
              (%console-handler fd)
              (list 'flush (list 'fd fd)))])
    p))

(define (standard-error-port)
  (let* ([fd (osdep/open-console 'error)]
         [p (apply %ports/make-port
              "*error-output*" 'output #f 'block
              (%console-handler fd)
              (list 'flush (list 'fd fd)))])
    p))


(define (open-bytevector-input-port bv . rest)
  (if (and (bytevector? bv) (or (null? rest) (null? (cdr rest))))
    (let ([p (open-input-bytevector bv)])
      (if (null? rest) p (transcoded-port p (car rest))))
    (assertion-violation 'open-bytevector-input-port "illegal argument(s)" bv rest)))

(define (open-bytevector-output-port . rest)
  (if (or (null? rest) (null? (cdr rest)))
    (if (null? rest)
      (let ([p (open-output-bytevector)])
        (values p
          (lambda ()
            (let ([bv (get-output-bytevector p)])
              (reset-output-bytevector p)
              bv))))
      (let ([p (transcoded-port (open-output-bytevector) (car rest))])
        (values p
          (lambda ()
            (let ([bv (get-output-bytevector p)])
              (reset-output-bytevector p)
              bv)))))
    (assertion-violation 'open-bytevector-output-port "too many arguments" rest)))

(define (call-with-bytevector-output-port proc . rest)
  (if (procedure? proc)
    (call-with-port
      (if (null? rest)
        (open-output-bytevector)
        (transcoded-port (open-output-bytevector) (car rest)))
      (lambda (p)
        (proc p)
        (get-output-bytevector p)))
    (assertion-violation 'call-with-bytevector-output-port "not a procedure" proc)))

(define (open-string-input-port s)
  (open-input-string s))

(define (bytevector->string bv t)
  (if (and (bytevector? bv) (%transcoder? t))
    (let ([p (transcoded-port (open-input-bytevector bv) t)])
      (let ([s (get-string-all p)])
        (close-port p)
        (if (eof-object? s) "" s)))
    (assertion-violation 'bytevector->string "illegal argument(s)" bv t)))

(define (string->bytevector s t)
  (if (and (string? s) (%transcoder? t))
    (let ([codec (transcoder-codec t)]
          [eol (transcoder-eol-style t)]
          [errmode (transcoder-error-handling-mode t)])
      (cond ((eq? eol 'none)
             (case codec
               ((utf-8) (string->utf8 s))
               ((utf-16) (string->utf16 s))
               ((latin-1)
                 (let ([bv (make-bytevector (string-length s) 0)])
                   (let loop ([i 0] [n (string-length s)])
                     (if (= i n)
                       bv
                       (let ([sv (char->integer (string-ref s i))])
                         (cond ((<= sv #xff)
                                (bytevector-u8-set! bv i sv)
                                (loop (+ i 1) n))
                           ((eq? errmode 'replace)
                             (bytevector-u8-set! bv i 63)
                             (loop (+ i 1) n))
                           ((eq? errmode 'ignore)
                             (loop (+ i 1) n))
                           (else
                             (raise-i/o-encoding-error
                               'string->bytevector "encoding error" #f (string-ref s i)))))))))
               (else
                 (assertion-violation 'string->bytevector "unknown codec" codec t))))
        (else
          ;; eol style other than none: encode character by character
          ;; so line endings are translated
          (let ([p (transcoded-port (open-output-bytevector) t)])
            (put-string p s)
            (flush-output-port p)
            (get-output-bytevector p)))))
    (assertion-violation 'string->bytevector "illegal argument(s)" s t)))

(define (read-char . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (get-char p)))

(define (peek-char . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (lookahead-char p)))

(define (write-char c . rest)
  (let ([p (if (null? rest) (current-output-port) (car rest))])
    (put-char p c)))

(define (read-string k . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (get-string-n p k)))

(define (read-u8 . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (get-u8 p)))

(define (peek-u8 . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (lookahead-u8 p)))

(define (u8-ready? . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (if (and (port? p) (%ports/input-port? p) (%ports/binary? p))
      (or (eq? (%port-state p) 'eof)
        (> (%ports/buffered-input p) 0)
        (> (%port-pending-len p) 0)
        (let ([r (%handler-ready? (%port-handler p))])
          (and r (r))))
      (assertion-violation 'u8-ready? "not a binary input port" p))))

(define (char-ready? . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (if (and (port? p) (%ports/input-port? p) (%ports/textual? p))
      (or (eq? (%port-state p) 'eof)
        (> (%ports/buffered-input p) 0)
        (> (%port-pending-len p) 0)
        (let ([r (%handler-ready? (%port-handler p))])
          (and r (r))))
      (assertion-violation 'char-ready? "not a textual input port" p))))

(define (write-u8 n . rest)
  (let ([p (if (null? rest) (current-output-port) (car rest))])
    (put-u8 p n)))

(define (read-bytevector k . rest)
  (let ([p (if (null? rest) (current-input-port) (car rest))])
    (get-bytevector-n p k)))

(define (read-bytevector! bv . rest)
  (let* ([p (if (null? rest) (current-input-port) (car rest))]
         [start (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest))]
         [end (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
               (bytevector-length bv)
               (caddr rest))])
    (get-bytevector-n! p bv start (- end start))))

(define (write-bytevector bv . rest)
  (let* ([p (if (null? rest) (current-output-port) (car rest))]
         [start (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest))]
         [end (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
               (bytevector-length bv)
               (caddr rest))])
    (put-bytevector p bv start (- end start))))

(define (write-string s . rest)
  (let* ([p (if (null? rest) (current-output-port) (car rest))]
         [start (if (or (null? rest) (null? (cdr rest))) 0 (cadr rest))]
         [end (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
               (string-length s)
               (caddr rest))])
    (put-string p s start (- end start))))

(define (write-bytevector-like bvl p)
  (put-bytevector p bvl))

(define (open-input-file filename)
  (open-file-input-port
    filename (file-options) 'block (native-transcoder)))

(define (open-output-file filename)
  (open-file-output-port
    filename (file-options) 'block (native-transcoder)))

(define (call-with-input-file filename proc)
  (call-with-port (open-input-file filename) proc))

(define (call-with-output-file filename proc)
  (call-with-port (open-output-file filename) proc))

(define (with-input-from-port port thunk)
  (define old (current-input-port))
  (dynamic-wind
    (lambda () (current-input-port port))
    thunk
    (lambda () (current-input-port old))))

(define (with-output-to-port port thunk)
  (define old (current-output-port))
  (dynamic-wind
    (lambda () (current-output-port port))
    thunk
    (lambda () (current-output-port old))))

(define (with-input-from-file filename thunk)
  (call-with-input-file filename
    (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-file filename thunk)
  (call-with-output-file filename
    (lambda (p) (with-output-to-port p thunk))))

(define (call-with-input-string s proc)
  (call-with-port (open-input-string s) proc))

(define (call-with-output-string proc)
  (call-with-port
    (open-output-string)
    (lambda (p) (proc p) (get-output-string p))))

(define (call-with-output-bytevector proc)
  (call-with-port
    (open-output-bytevector)
    (lambda (p) (proc p) (get-output-bytevector p))))

(define (with-input-from-string s thunk)
  (call-with-input-string s
    (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (p) (with-output-to-port p thunk))))

(define (open-raw-latin-1-input-file filename)
  (open-file-input-port
    filename
    (file-options)
    'block
    (make-transcoder (latin-1-codec) 'none 'ignore)))

(define (open-raw-latin-1-output-file filename)
  (open-file-output-port
    filename
    (file-options)
    'block
    (make-transcoder (latin-1-codec) 'none 'ignore)))

(define (call-with-raw-latin-1-input-file file proc)
  (call-with-port (open-raw-latin-1-input-file file) proc))

(define (call-with-raw-latin-1-output-file file proc)
  (call-with-port (open-raw-latin-1-output-file file) proc))

(define (with-input-from-raw-latin-1-file filename thunk)
  (call-with-raw-latin-1-input-file filename
    (lambda (p) (with-input-from-port p thunk))))

(define (with-output-to-raw-latin-1-file filename thunk)
  (call-with-raw-latin-1-output-file filename
    (lambda (p) (with-output-to-port p thunk))))

(define open-binary-input-file open-raw-latin-1-input-file)
(define open-binary-output-file open-raw-latin-1-output-file)
(define call-with-binary-input-file call-with-raw-latin-1-input-file)
(define call-with-binary-output-file call-with-raw-latin-1-output-file)
(define with-input-from-binary-file with-input-from-raw-latin-1-file)
(define with-output-to-binary-file with-output-to-raw-latin-1-file)

(define (open-text-input-file filename)
  (open-file-input-port
    filename (file-options) 'block (native-transcoder)))

(define (open-text-output-file filename)
  (open-file-output-port
    filename (file-options) 'block (native-transcoder)))

(define (call-with-text-input-file filename proc)
  (call-with-port (open-text-input-file filename) proc))

(define (call-with-text-output-file filename proc)
  (call-with-port (open-text-output-file filename) proc))

(define (close-open-ports) (unspecified))
(define (close-open-files) (unspecified))


(define (file-newer? f1 f2)
  (let ([t1 (file-modification-time f1)]
        [t2 (file-modification-time f2)])
    (let loop ([i 0])
      (cond ((= i (vector-length t1)) #f)
        ((= (vector-ref t1 i) (vector-ref t2 i)) (loop (+ i 1)))
        (else (> (vector-ref t1 i) (vector-ref t2 i)))))))

(define (port-transcoder p)
  (if (port? p)
    (%port-transcoder p)
    (assertion-violation 'port-transcoder "not a port" p)))

(define (put-datum p x)
  (write x p))

;;; eof
