(import (srfi 64)
        (capy)
        (core io process))

(test-begin "boot class redefinition from the compile cache")

;; A define-class that redefines a class keeps the class object: classes that

(define base
  (string-append "/tmp/capy-cached-redefinition-"
    (number->string (exact (floor (* 1000000 (current-second)))))))
;; Microseconds, not seconds: parallel runs of the suite start in the same second.
(define source (string-append base ".scm"))
(define output (string-append base ".out"))

(define (write-program)
  (call-with-output-file source
    (lambda (port)
      (for-each
        (lambda (form) (write form port) (newline port))
        '((import (capy) (capy mop))
          (define-class <tm> (<class>) ((count #:init-value 0)))
          (define-class <tp> () (x) #:metaclass <tm>)
          (define-class <tm> (<class>) ((count #:init-value 0) (extra #:init-value 1)))
          (write (list (eq? <tm> (class-of <tp>))
                       (slot-ref <tp> 'count)
                       (slot-ref <tp> 'extra)))
          (newline))))))

(define (run-capy)
  (let ([capy (or (getenv "CAPY") "capy")])
    (system (string-append capy " -L src,../std/src -s " source " > " output " 2>&1"))))

(define (output-text)
  (call-with-input-file output
    (lambda (port)
      (let loop ([chars '()])
        (let ([c (read-char port)])
          (if (eof-object? c)
            (list->string (reverse chars))
            (loop (cons c chars))))))))

(write-program)
(define fresh-status (run-capy))
(define fresh-text (output-text))
(define cached-status (run-capy))
(define cached-text (output-text))

(test-equal "a fresh run keeps the redefined class"
  0 fresh-status)
(test-equal "the fresh run prints the expected values"
  "(#t 0 1)\n" fresh-text)
(test-equal "the cached run keeps the redefined class"
  0 cached-status)
(test-equal "the cached run prints the expected values"
  "(#t 0 1)\n" cached-text)

(delete-file source)
(delete-file output)

(test-end "boot class redefinition from the compile cache")
