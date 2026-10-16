;;; CapyScheme console ports.
;;;
;;; Console (terminal) ports used by the REPL and standard streams.  
;;; Console ports are UTF-8 transcoded ports over the standard file descriptors 
;;; (or /dev/tty once the standard descriptors have been used). 

;;; The console handler tracks a byte count so port-position works
;;; even though a terminal cannot be seeked.

(define (%console-handler fd)
  (define total 0)
  (define (read! buffer start count0)
    (let ([n (osdep/read-file fd buffer count0 start)])
      (cond ((not (fixnum? n))
             (raise-i/o-read-error 'get-char "read failed" #f))
        ((< n 0)
          (raise-i/o-read-error 'get-char "read failed" #f))
        (else
          (set! total (+ total n))
          n))))
  (define (write! buffer start count0)
    (let ([k (osdep/write-file4 fd buffer count0 start)])
      (cond ((not (fixnum? k))
             (raise-i/o-write-error 'flush-output-port "write failed" #f))
        ((<= k 0)
          (raise-i/o-write-error 'flush-output-port "write failed" #f))
        (else
          (set! total (+ total k))
          k))))
  (define (close)
    (osdep/close-console fd)
    (unspecified))
  (define (ready?)
    (osdep/char-ready-console? fd))
  (define (get-position) total)
  (%make-handler read! write! close ready? get-position #f #f))

;;; Console output policy: a terminal is flushed after every datum so
;;; prompts and progress appear immediately (and at each newline).
;;; Standard error is likewise always flushed per datum. Standard
;;; output redirected to a file or pipe is block buffered and flushed at
;;; exit (see %ports/flush-all!).

(define (console/open-console-port io-mode name)
  (let* ([fd (osdep/open-console io-mode)]
         [tty? (osdep/isatty? fd)]
         [opts (cond ((eq? io-mode 'input) (list (list 'fd fd)))
                 ((or tty? (eq? io-mode 'error)) (list (list 'fd fd) 'flush))
                 (else (list (list 'fd fd))))]
         [p (apply %ports/make-port
              name
              (case io-mode ((input) 'input) (else 'output))
              #f
              (cond ((eq? io-mode 'input) 'block)
                ((or tty? (eq? io-mode 'error)) 'line)
                (else 'block))
              (%console-handler fd)
              opts)])
    (transcoded-port p (console-transcoder))))

(define (console-transcoder)
  (default-transcoder))

(define *current-console-input* #f)
(define *current-console-output* #f)
(define *current-console-error* #f)

(define (console/console-input-port)
  (if (or (not *current-console-input*)
       (not (%ports/open? *current-console-input*))
       (eq? (%port-state *current-console-input*) 'eof))
    (set! *current-console-input*
      (console/open-console-port 'input "*console-input*")))
  *current-console-input*)

(define (console/console-output-port)
  (if (or (not *current-console-output*)
       (not (%ports/open? *current-console-output*)))
    (set! *current-console-output*
      (console/open-console-port 'output "*console-output*")))
  *current-console-output*)

(define (console/console-error-port)
  (if (or (not *current-console-error*)
       (not (%ports/open? *current-console-error*)))
    (set! *current-console-error*
      (console/open-console-port 'error "*error-output*")))
  *current-console-error*)

(define (console/initialize)
  (osdep/initialize-console)
  (set! *current-console-input* (console/open-console-port 'input "*console-input*"))
  (set! *current-console-output* (console/open-console-port 'output "*console-output*"))
  (set! *current-console-error* (console/open-console-port 'error "*error-output*"))
  (unspecified))

;;; eof
