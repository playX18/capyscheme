
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Transcoders et cetera.
; For representations, see iosys.sch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latin-1-codec) 'latin-1)
(define (utf-8-codec) 'utf-8)
(define (utf-16-codec) 'utf-16)

; The deprecated eol-style syntax is supported only by R6RS modes.

(define (native-eol-style) 'none)   ; FIXME: for backward compatibility

; FIXME:  &i/o-decoding, &i/o-encoding, and their associated
; operations might not be implemented yet.

; The deprecated error-handling-mode syntax is supported only by R6RS modes.

(define (make-transcoder codec . rest)
  (cond ((null? rest)
         (io/make-transcoder codec (native-eol-style) 'replace))
        ((null? (cdr rest))
         (io/make-transcoder codec (car rest) 'replace))
        ((null? (cddr rest))
         (io/make-transcoder codec (car rest) (cadr rest)))
        (else
         (assertion-violation 'make-transcoder
                              (errmsg 'msg:wna)
                              (cons codec rest)))))

; FIXME: let's see how far we get...

(define (native-transcoder)
  ;(make-transcoder (latin-1-codec) 'none 'ignore))
  (default-transcoder))

(define (transcoder-codec t)
  (io/transcoder-codec t))

(define (transcoder-eol-style t)
  (io/transcoder-eol-style t))

(define (transcoder-error-handling-mode t)
  (io/transcoder-error-handling-mode t))

(define (bytevector->string bv t)
    (let ((s (call-with-port
            (transcoded-port (open-input-bytevector bv) t)
            get-string-all)))
    (if (eof-object? s)
        ""
        s)))

(define p (io/make-port (lambda x #f) 'dummy 'input 'textual))

(print (port? p))
(print (io/input-port? p))
(print (io/output-port? p))