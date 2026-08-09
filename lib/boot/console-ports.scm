;;; CapyScheme console ports.
;;;
;;; Console (terminal) ports used by the REPL and standard streams.  
;;; Console ports are UTF-8 transcoded ports over the standard file descriptors 
;;; (or /dev/tty once the standard descriptors have been used). 

;;; The console handler tracks a byte count so port-position works
;;; even though a terminal cannot be seeked.

(define (%console-handler fd)
  (define count 0)
  (define (read! buffer start count0)
    (let ([n (osdep/read-file fd buffer count0)])
      (cond ((not (fixnum? n)) 0)
        ((< n 0) 0)
        (else
          (set! count (+ count n))
          n))))
  (define (write! buffer start count0)
    (let ([k (osdep/write-file4 fd buffer count0 start)])
      (cond ((not (fixnum? k)) 0)
        ((<= k 0) 0)
        (else
          (set! count (+ count k))
          k))))
  (define (close)
    (osdep/close-console fd)
    (unspecified))
  (define (ready?)
    (osdep/char-ready-console? fd))
  (define (get-position) count)
  (%make-handler read! write! close ready? get-position #f #f))

(define (console/open-console-port io-mode name)
  (let* ([fd (osdep/open-console io-mode)]
         [opts (if (eq? io-mode 'input)
                 (list (list 'fd fd))
                 (list (list 'fd fd) 'flush))]
         [p (apply %ports/make-port
              name
              (case io-mode ((input) 'input) (else 'output))
              #f
              (if (eq? io-mode 'input) 'block 'line)
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
