;;; R6RS file-port procedures (rnrs io ports, 8.2.7, 8.2.10, 8.2.13)


(define whence:seek-set 0)
(define whence:seek-cur 1)
(define whence:seek-end 2)

;;; A file port handler over a file descriptor.  Position support
;;; depends on whether the descriptor can be seeked (regular files can,
;;; pipes and terminals cannot); get-position returns #f when lseek
;;; fails, which makes port-has-port-position? false.

(define (%file-handler fd)
  (define (read! buffer start count)
    (let ([n (if (= start 0)
               (osdep/read-file fd buffer count)
               (let ([tmp (make-bytevector count 0)])
                 (let ([k (osdep/read-file fd tmp count)])
                   (if (fixnum? k)
                     (begin
                       (r6rs:bytevector-copy! tmp 0 buffer start k)
                       k)
                     k))))])
      (cond ((not (fixnum? n))
             (raise-i/o-read-error 'get-u8 "read failed" #f))
        ((< n 0)
          (raise-i/o-read-error 'get-u8 "read failed" #f))
        (else n))))
  (define (write! buffer start count)
    (let ([k (osdep/write-file4 fd buffer count start)])
      (cond ((not (fixnum? k))
             (raise-i/o-write-error 'flush-output-port "write failed" #f))
        ((<= k 0)
          (raise-i/o-write-error 'flush-output-port "write failed" #f))
        (else k))))
  (define (close)
    (osdep/close-file fd)
    (unspecified))
  (define (ready?) #t)
  (define (get-position)
    (let ([r (osdep/lseek-file fd 0 whence:seek-cur)])
      (if (>= r 0) r #f)))
  (define (set-position! pos)
    (let ([r (osdep/lseek-file fd pos whence:seek-set)])
      (when (< r 0)
        (raise-i/o-invalid-position-error 'set-port-position!
          "invalid position"
          #f
          pos))
      (unspecified)))
  (%make-handler read! write! close ready? get-position set-position! #f))


(define (%file-ports/open filename io-mode opts)
  (let ([fd (apply osdep/open-file filename io-mode 'binary opts)])
    (if (>= fd 0)
      fd
      (raise-i/o-filename-error 'open-file-port "failed to open file" filename))))

(define (open-file-input-port filename . rest)
  (define file-options (if (null? rest) #f (car rest)))
  (define buffer-mode (if (or (null? rest) (null? (cdr rest))) #f (cadr rest)))
  (define transcoder (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
                       #f
                       (caddr rest)))
  (if (not (string? filename))
    (assertion-violation 'open-file-input-port "illegal filename" filename))
  (let* ([fd (%file-ports/open filename 'input '())]
         [p (%ports/make-port filename 'input #f 'block (%file-handler fd)
               (list 'fd fd))])
    (if transcoder
      (transcoded-port p transcoder)
      p)))

(define (open-file-output-port filename . rest)
  (define opts (if (null? rest) '() (file-options->list (car rest))))
  (define bufmode (if (or (null? rest) (null? (cdr rest))) 'block (cadr rest)))
  (define transcoder (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
                       #f
                       (caddr rest)))
  (define exists? (file-exists? filename))
  (cond ((and (not exists?) (memq 'no-create opts))
         (raise-i/o-file-does-not-exist-error
           'open-file-output-port "file does not exist" filename))
    (else
      (let* ([fd (%file-ports/open filename 'output opts)]
             [bmode (case bufmode
                      ((none) 'none)
                      ((line) 'line)
                      ((datum flush) 'block)
                      (else 'block))]
             [p (apply %ports/make-port
                  filename
                  'output
                  #f
                  bmode
                  (%file-handler fd)
                  (if (memq bufmode '(datum flush))
                    (list 'flush (list 'fd fd))
                    (list (list 'fd fd))))])
        (if transcoder
          (transcoded-port p transcoder)
          p)))))

(define (open-file-input/output-port filename . rest)
  (define opts (if (null? rest) '() (file-options->list (car rest))))
  (define bufmode (if (or (null? rest) (null? (cdr rest))) 'block (cadr rest)))
  (define transcoder (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
                       #f
                       (caddr rest)))
  (define exists? (file-exists? filename))
  (cond ((and (not exists?) (memq 'no-create opts))
         (raise-i/o-file-does-not-exist-error
           'open-file-input/output-port "file does not exist" filename))
    (else
      (let* ([fd (%file-ports/open filename 'input+output opts)]
             [p (%ports/make-port filename 'input-output #f 'block (%file-handler fd)
                   (list 'fd fd))])
        (if transcoder
          (transcoded-port p transcoder)
          p)))))

(define (open-binary-fd-input-port name fd buffer-mode)
  (%ports/make-port name 'input #f 'block (%file-handler fd)
    (list 'fd fd)))

(define (open-binary-fd-output-port name fd buffer-mode)
  (%ports/make-port
    name
    'output
    #f
    (case buffer-mode
      ((none) 'none)
      ((line) 'line)
      (else 'block))
    (%file-handler fd)
    (list 'flush (list 'fd fd))))

(define (file-exists? filename)
  (osdep/file-exists? filename))

(define (delete-file filename)
  (if (osdep/delete-file filename)
    (unspecified)
    (raise-i/o-filename-error 'delete-file "file cannot be deleted" filename)))

(define (rename-file from to)
  (if (osdep/rename-file from to)
    (unspecified)
    (raise-i/o-filename-error 'rename-file "file cannot be renamed" from)))

(define (file-modification-time filename)
  (osdep/file-modification-time filename))

(define (relative-path-string? filename)
  (osdep/relative-path-string? filename))

(define (absolute-path-string? filename)
  (osdep/absolute-path-string? filename))

;;; Creates a fresh temporary file whose name is derived from the
;;; template (the mkstemp convention appends a random suffix), and
;;; returns two values: a binary input/output port over the file and
;;; the file name.  The caller is responsible for closing the port and
;;; deleting the file.

(define (make-temporary-file-port template)
  (if (not (string? template))
    (assertion-violation 'make-temporary-file-port "illegal template" template))
  (let* ([fd-and-name (io/mkstemp template)]
         [fd (car fd-and-name)]
         [name (cadr fd-and-name)])
    (values
      (%ports/make-port name 'input-output #f 'block (%file-handler fd)
        (list 'fd fd))
      name)))

;;; eof
