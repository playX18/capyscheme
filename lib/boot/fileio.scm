; Copyright 1998 Lars T Hansen.
;
; $Id$
;
; File I/O.


(define (issue-deprecated-warnings?) #f)


(define *files-open* #f)

(define (file-io/initialize)
  (set! *files-open* #f)
  #t)

(define (file-io/finalize)
  (if *files-open*
      (file-io/close-open-files))
  #t)

(define (file-io/remember p)
  (if (io/output-port? p)
      (set! *files-open* #t)))

; Actually closes all open ports, because custom output ports use
; the same mechanism to ensure they're flushed and closed on exit.
;
; FIXME:  If we're going to close all open ports, it would be
; better just to keep a list of them and avoid using sro.

(define (file-io/close-open-files)
  #f)

(define (file-io/data fd name)
  (cons fd name))

(define (file-io/fd datum)
  (car datum))

(define (file-io/name datum)
  (cdr datum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-io/ioproc op)
  (case op
    ((read)
     file-io/read)
    ((write)
     file-io/write)
    ((close)
     file-io/close)
    ((ready?)
     file-io/ready?)
    ((name)
     file-io/name)
    ((set-position!)
     file-io/set-position!)
    ((fd)
     file-io/fd)
    (else 
     (error "file-io/ioproc: illegal operation: " op))))

(define (file-io/read data buffer)
  (file-io/read-bytes (file-io/fd data) buffer))

(define (file-io/write data buffer count)
  (file-io/write-bytes (file-io/fd data) buffer count 0))

(define (file-io/close data)
  (file-io/close-file data))

(define (file-io/ready? data) #t)

; Parameters for osdep/lseek-file; the magic numbers are portable
; because they're interpreted by Rts/Sys/osdep-*.c

(define whence:seek-set          0)     ; offset is absolute
(define whence:seek-cur          1)     ; offset is relative to current
(define whence:seek-end          2)     ; offset is relative to end

(define (file-io/port-position-as-binary data)
  (let ((r (osdep/lseek-file (file-io/fd data) 0 whence:seek-cur)))
    (if (>= r 0) r 'error)))

(define (file-io/set-position! data offset)
  (let ((r (osdep/lseek-file (file-io/fd data) offset whence:seek-set)))
    (if (>= r 0) 'ok 'error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: using the alist for this, but not for set-position!, is screwy.

(define (file-io/install-port-position-as-binary! p data)
  (let ((get-position
         (lambda ()
           (file-io/port-position-as-binary data))))
    (io/port-alist-set! p
                        (cons (cons 'port-position-in-bytes get-position)
                              (io/port-alist p)))))

(define (file-io/read-bytes fd buffer)
  (let ((r (osdep/read-file fd buffer (bytevector-length buffer))))
    (cond ((not (fixnum? r)) 'error)
          ((< r 0) 'error)
          ((= r 0) 'eof)
          (else r))))

(define (file-io/write-bytes fd buffer n offset)
  (let ((k (osdep/write-file4 fd buffer n offset)))
    (cond ((not (fixnum? k)) 'error)
          ((<= k 0) 'error)
          ((= k n)  'ok)
          (else (file-io/write-bytes fd buffer (- n k) (+ offset k))))))

(define (file-io/open-file filename . modes)
  (let* ((io-mode (if (memq 'input modes) 'input 'output))
         (tx-mode (if (memq 'binary modes) 'binary 'text))
         (fd      (osdep/open-file filename io-mode tx-mode))) 
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (if (eq? 'binary tx-mode)
                         (io/make-port file-io/ioproc data io-mode tx-mode
                                       'set-position!)
                         (io/make-port file-io/ioproc data io-mode tx-mode))))
          (file-io/install-port-position-as-binary! p data)
          (file-io/remember p)
          p)
        (begin (raise-i/o-filename-error 'open-file "file not found" filename)
               #t))))

; The R6RS says it's supposed to ignore the file options,
; and the buffer mode doesn't appear to have any real semantics
; for files.

(define (file-io/open-file-input-port filename options bufmode transcoder)
  (let* ((fd      (osdep/open-file filename 'input 'binary)))
    (if (>= fd 0)
        (let* ((data (file-io/data fd filename))
               (p    (io/make-port file-io/ioproc data 'input
                                   'binary 'set-position!))
               (p    (if (and transcoder (not (zero? transcoder)))
                         (io/transcoded-port p transcoder)
                         p)))
          (file-io/install-port-position-as-binary! p data)
          (file-io/remember p)
          p)
        (begin (raise-i/o-filename-error 'open-file-input-port
                                     "failed to open file"
                                     filename)
               #t))))

(define (file-io/open-file-output-port filename options bufmode transcoder)
  (let* ((opts (file-options->list options))
         (dont-create (memq 'no-create opts))
         (dont-fail (memq 'no-fail opts))
         (dont-truncate (memq 'no-truncate opts))
         (bufmode (case bufmode
                   ((none) 'none)
                   ((line) 'line)
                   ((datum flush) 'datum)
                   (else 'block)))
         (exists? (file-io/file-exists? filename)))
    (cond ((and exists? (not dont-create) (not dont-fail))
            #t)
           ;(let* ((exec-mode (larceny:execution-mode)))
           ;  (case exec-mode
           ;   ((r5rs) #t)
           ;   ((err5rs)
           ;    (if (issue-deprecated-warnings?)
           ;        (let ((out (current-error-port)))
           ;          (display "WARNING: output file already exists: " out)
           ;          (display filename out)
           ;          (newline out))))
           ;   (else
           ;    (raise-i/o-file-already-exists-error 'open-file-output-port 
           ;     "file already exists"
           ;     filename
           ;     )))))
               ;(raise-r6rs-exception
               ; (make-i/o-file-already-exists-error filename)
               ; 'open-file-output-port
               ; (errmsg 'msg:fileexists)
               ; (list filename opts))))))
          ((and (not exists?) dont-create)
           (raise-i/o-file-does-not-exist-error 
            'open-file-output-port
            "file does not exist"
            filename)))
    (let ((fd (apply osdep/open-file filename 'output 'binary opts)))
      (if (>= fd 0)
          (let* ((data (file-io/data fd filename))
                 (p    (io/make-port file-io/ioproc data 'output
                                     'binary 'set-position! bufmode))
                 (p    (if (and transcoder (not (zero? transcoder)))
                           (io/transcoded-port p transcoder)
                           p)))
            (file-io/install-port-position-as-binary! p data)
            (file-io/remember p)
            p)
          (begin (raise-i/o-filename-error 'open-file-output-port
                                       "failed to open file"
                                       filename)
                 #t)))))

; FIXME:  This should be implemented better.

(define (file-io/open-file-input/output-port filename options bufmode t)
  (let* ((opts (file-options->list options))
         (dont-create (memq 'no-create opts))
         (dont-fail (memq 'no-fail opts))
         (dont-truncate (memq 'no-truncate opts))
         (bufmode (case bufmode
                   ((none) 'none)
                   ((line) 'line)
                   ((datum flush) 'datum)
                   (else 'block)))
         (exists? (file-io/file-exists? filename)))
    (cond ((and exists? (not dont-create) (not dont-fail))
           (let* ((exec-mode (capy:execution-mode)))
             
               (raise-i/o-file-already-exists 
                'open-file-input/output-port 
                "file already exists"
                filename)))
          ((and (not exists?)
                (not dont-create))
           (call-with-port (open-file-output-port filename) values))
          ((not exists?)
           (raise-io/file-does-not-exist-error 
            'open-file-input/output-port
            "file does not exist"
            filename)))
    (let ([fd (osdep/open-file filename 'input+output 'binary opts)])
      (define (read! bv start count)
        (define tmp (make-bytevector count))
        (define nbytes (osdep/read-file fd tmp count))
        (r6rs:bytevector-copy! tmp 0 bv start nbytes)
        nbytes)
      (define (write! bv start count)
        (osdep/write-file4 fd bv count start))
      (define (get-position)
        (osdep/lseek-file fd 0 whence:seek-cur))
      (define (set-position! pos)
        (osdep/lseek-file fd pos whence:seek-set))
      (define (close!)
        (osdep/close-file fd))
      (when (< fd 0)
        (raise-i/o-filename-error 'open-file-input/output-port
                                     "failed to open file"
                                     filename))
      (let ([p (make-custom-binary-input/output-port 
        filename 
        read! write! get-position set-position! close!)])
        (if (and t (not (zero? t)))
          (io/transcoded-port p t)
          p)))))

(define (file-io/close-file data)
  (let ((r (osdep/close-file (file-io/fd data))))
    (if (< r 0) 
        'error
        'ok)))

(define (file-io/file-modification-time filename)
  (osdep/file-modification-time filename))

(define (file-io/file-exists? filename)
  (osdep/file-exists? filename))

(define (file-io/relative-path-string? filename)
  (osdep/relative-path-string? filename))

(define (file-io/absolute-path-string? filename)
  (osdep/absolute-path-string? filename))

(define (file-io/rename-file from to)
  (osdep/rename-file from to))

(define (file-io/delete-file filename)
  (osdep/delete-file filename))

; eof
