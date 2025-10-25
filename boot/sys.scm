; Copyright 1998 Lars T Hansen
;
; $Id$
;
; Unix operating system hooks: safe interfaces to unsafe primitives.

; Private values and procedures.

; Standard Unix file descriptors

(define unix:stdin  0)
(define unix:stdout 1)
(define unix:stderr 2)

; Parameters for unix:open; the magic numbers are portable.

(define unix:open-read   #x01)        ; open for read
(define unix:open-write  #x02)        ; open for write
(define unix:open-append #x04)        ; position at end (writing)
(define unix:open-create #x08)        ; create if not existing (writing)
(define unix:open-trunc  #x10)        ; truncate if existing (writing)
(define unix:open-binary #x20)        ; binary mode
(define unix:create-mode #o666)       ; default mode for new files

; Parameters for unix:access; the magic numbers are portable.

(define unix:access-exists  #x01)     ; path searchable, file exists
(define unix:access-read    #x02)     ; file readable
(define unix:access-write   #x04)     ; file writable
(define unix:access-execute #x08)     ; file executable

(define (unix:open filename flags mode)
  (syscall:open filename flags mode))

(define (unix:close fd)
  (syscall:close fd))

(define (unix:read fd buffer nbytes)
  (syscall:read fd buffer nbytes))

(define (unix:write fd buffer nbytes offset)
  (syscall:write fd buffer nbytes offset))

(define (unix:lseek fd offset whence)
  (syscall:lseek fd offset whence))

(define (unix:pollinput fd)                ; #t if ready
  (= (syscall:pollinput fd) 1))


; System dependent values.

(define osdep/newline 10)


; Console.
;
; The first time through, we use standard file descriptors inherited from
; the parent process.  Later, we open /dev/tty.

(define *conio-input-firsttime*)
(define *conio-output-firsttime*)
(define *conio-error-firsttime*)

(define (osdep/initialize-console)     ; Must be called in every process.
  (set! *conio-input-firsttime* #t)
  (set! *conio-output-firsttime* #t)
  (set! *conio-error-firsttime* #t)
  #t)

(define (osdep/open-console io-mode)
  (case io-mode
    ((input)
     (if *conio-input-firsttime* 
         (begin (set! *conio-input-firsttime* #f)
                unix:stdin)
         (unix:open "/dev/tty" unix:open-read 0)))
    ((output)
     (if *conio-output-firsttime*
         (begin (set! *conio-output-firsttime* #f)
                unix:stdout)
         (unix:open "/dev/tty" unix:open-write unix:create-mode)))
    ((error)
     (if *conio-error-firsttime*
         (begin (set! *conio-error-firsttime* #f)
                unix:stderr)
         (unix:open "/dev/tty" unix:open-write unix:create-mode)))
    (else
     (error "osdep/open-terminal: invalid mode: " io-mode)
             #t)))

(define (osdep/close-console fd)
  (osdep/close-file fd))

(define (osdep/char-ready-console? fd)
  (if (not (fixnum? fd))
      (error "osdep/char-ready-console?: bad descriptor " fd))
  (unix:pollinput fd))


; File system.
;
; A file name is a string.
; A file descriptor is a fixnum.
; A buffer is a bytevector.
;
; io-mode is a symbol ('input' or 'output').
; tx-mode is a symbol ('text' or 'binary').
;
; The optional arguments recognized by osdep/open-file are the symbols
;     'no-create'
;     'no-truncate'
; These optional arguments are ignored if io-mode is 'input'.

(define (osdep/open-file fn io-mode tx-mode . optargs)
  (if (not (string? fn))
      (error "osdep/open-file: invalid filename " fn))
  (let ((binary-mode (if (eq? tx-mode 'binary) unix:open-binary 0))
        (create-mode (if (and (eq? io-mode 'output)
                              (memq 'no-create optargs))
                         0
                         unix:open-create))
        (truncate-mode (if (and (eq? io-mode 'output)
                                (memq 'no-truncate optargs))
                           0
                           unix:open-trunc)))
    (cond ((eq? io-mode 'input)
           (unix:open fn (+ unix:open-read binary-mode) 0))
          ((eq? io-mode 'output)
           (unix:open fn 
                      (+ unix:open-write create-mode truncate-mode
                         binary-mode)
                      unix:create-mode))
          (else
           (error "osdep/open-file: " io-mode " is not a valid file mode.")
           #t))))

(define (osdep/close-file fd)
  (if (not (fixnum? fd))
      (error "osdep/close-file: invalid file descriptor " fd))
  (unix:close fd))

(define (osdep/read-file fd buffer nbytes)
  (if (not (fixnum? fd))
      (error "osdep/read-file: invalid descriptor " fd))
  (if (not (bytevector? buffer))
      (error "osdep/read-file: invalid buffer " buffer))
  (if (not (and (fixnum? nbytes) (>= nbytes 0)))
      (error "osdep/read-file: invalid byte count " nbytes))
  (unix:read fd buffer nbytes))

(define (osdep/write-file fd buf k)
  
  (osdep/write-file4 fd buf k 0))

(define (osdep/write-file4 fd buf k offset)
  (if (not (fixnum? fd))
      (error 'osdep/write-file "invalid descriptor " fd))
  (if (not (bytevector? buf))
      (error 'osdep/write-file "invalid buffer " buf))
  (if (not (and (fixnum? k) (>= k 0)))
      (error 'osdep/write-file "invalid byte count " k))
  (if (not (and (>= offset 0)
                (<= (+ offset k) (bytevector-length buf))))
      (error 'osdep/write-file "invalid byte count or offset " k "/" offset))
  (unix:write fd buf k offset))

(define (osdep/lseek-file fd offset whence)
  (unix:lseek fd offset whence))

(define (osdep/delete-file fn)
  (if (not (string? fn))
      (error "osdep/delete-file: invalid filename " fn))
  (zero? (syscall:unlink fn)))

(define (osdep/rename-file old new)
  (if (not (string? old))
      (error "osdep/rename-file: bad file name " old))
  (if (not (string? new))
      (error "osdep/rename-file: bad file name " new))
  (zero? (syscall:rename old new)))

(define (osdep/file-modification-time fn)
  (if (not (string? fn))
      (error "osdep/file-modification-time: bad file name " fn))
  (let ((v (make-vector 6)))
    (if (zero? (syscall:mtime fn v))
        v
        #f)))

(define (osdep/file-exists? fn)
  (if (not (string? fn))
      (error "osdep/file-exists?: bad file name " fn))
  (zero? (syscall:access fn unix:access-exists)))

(define (osdep/relative-path-string? path)
  (not (osdep/absolute-path-string? path)))

; FIXME: Why was this file used on Win32?
; As of changeset:5768, this file is no longer used on Win32,
; but we'll continue to support Windows paths...for now.

(define (osdep/absolute-path-string? path)
  (or (char=? #\/ (string-ref path 0))
      (and (> (string-length path) 1)
           (char=? #\: (string-ref path 1)))))


; Other system hooks

(define (osdep/find-init-files)
  (let ((result '())
        (system (string-append (current-larceny-root) "/startup.sch"))
        (home   (getenv "HOME")))
    (if home
      (let ((user (string-append home "/.larceny")))
        (if (file-exists? user)
          (set! result (cons user result)))))
    (if (file-exists? system)
      (set! result (cons system result)))
    result))



; eof
