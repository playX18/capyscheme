#!r6rs
;; Tests for bidirectional (input/output) port correctness.
;; Run: capy -L . -s tests/bidi-ports-test.sps

(import (rnrs)
        (rnrs mutable-strings (6)))

(define *pass* 0)
(define *fail* 0)
(define *failures* '())

(define-syntax check
  (syntax-rules ()
    [(_ expr expected)
     (let ([got expr]
           [exp expected])
       (if (equal? got exp)
         (set! *pass* (+ *pass* 1))
         (begin
           (set! *fail* (+ *fail* 1))
           (set! *failures* (cons (list 'expr 'got: got 'expected: exp) *failures*)))))]))

(define-syntax check-unspec
  (syntax-rules ()
    [(_ expr)
     (begin expr (set! *pass* (+ *pass* 1)))]))

(define-syntax check-eof
  (syntax-rules ()
    [(_ expr)
     (let ([got expr])
       (if (eof-object? got)
         (set! *pass* (+ *pass* 1))
         (begin
           (set! *fail* (+ *fail* 1))
           (set! *failures* (cons (list 'expr 'got: got 'expected: 'eof) *failures*)))))]))

;; ========================================================================
;; Binary bytevector input/output port
;; ========================================================================
(display ";; Binary bytevector input/output port tests") (newline)

;; Basic read after write
(let ()
  (define bv #vu8(1 2 3 4 5))
  (define p (open-bytevector-input-port bv))
  ;; open-bytevector-input-port gives input-only; we need input/output
  ;; Use the internal opener if available, otherwise test via custom port
  (close-port p))

;; Test bytevector input/output port via make-bytevector
(let ()
  (define bv (make-bytevector 8 0))
  (bytevector-set! bv 0 10)
  (bytevector-set! bv 1 20)
  (bytevector-set! bv 2 30)
  (define p (open-bytevector-input-port bv))
  (check (get-u8 p) 10)
  (check (get-u8 p) 20)
  (check (lookahead-u8 p) 30)
  (close-port p))

;; ========================================================================
;; Custom binary input/output port
;; ========================================================================
(display ";; Custom binary input/output port tests") (newline)

;; Test 1: basic write then read
(let* ([save #f]
       [p (make-custom-binary-input/output-port
           "custom-bin-io"
           (lambda (bv start end)
             (bytevector-u8-set! bv start 42)
             1)
           (lambda (bv start end)
             (set! save (bytevector-u8-ref bv start))
             1)
           #f #f #f)])
  (check (input-port? p) #t)
  (check (output-port? p) #t)
  (check (binary-port? p) #t)
  (check (textual-port? p) #f)
  (check-unspec (put-u8 p 10))
  (check-unspec (flush-output-port p))
  (check save 10)
  (check (get-u8 p) 42)
  (close-port p))

;; Test 2: interleaved read and write
(let* ([read-log '()]
       [write-log '()]
       [p (make-custom-binary-input/output-port
           "interleaved"
           (lambda (bv start end)
             (bytevector-u8-set! bv start 99)
             1)
           (lambda (bv start end)
             (set! write-log (cons (bytevector-u8-ref bv start) write-log))
             1)
           #f #f #f)])
  ;; write a byte
  (check-unspec (put-u8 p 1))
  (check-unspec (flush-output-port p))
  (check write-log '(1))
  ;; read a byte
  (check (get-u8 p) 99)
  ;; write another byte
  (check-unspec (put-u8 p 2))
  (check-unspec (flush-output-port p))
  (check write-log '(2 1))
  ;; read another byte
  (check (get-u8 p) 99)
  (close-port p))

;; Test 3: lookahead then write (must undo lookahead).
;; Requires set-port-position! since lookahead buffers a byte that must be
;; "put back" before the write can proceed.
(let* ([save #f]
       [pos 0]
       [p (make-custom-binary-input/output-port
           "lookahead-then-write"
           (lambda (bv start end)
             (bytevector-u8-set! bv start 55)
             1)
           (lambda (bv start end)
             (set! save (bytevector-u8-ref bv start))
             1)
           (lambda () pos)
           (lambda (newpos) (set! pos newpos))
           #f)])
  (check (lookahead-u8 p) 55)
  (check-unspec (put-u8 p 77))
  (check-unspec (flush-output-port p))
  (check save 77)
  (close-port p))

;; Test 4: write then lookahead (should see written? no - separate)
(let* ([save #f]
       [p (make-custom-binary-input/output-port
           "write-then-lookahead"
           (lambda (bv start end)
             (bytevector-u8-set! bv start 88)
             1)
           (lambda (bv start end)
             (set! save (bytevector-u8-ref bv start))
             1)
           #f #f #f)])
  (check-unspec (put-u8 p 100))
  (check-unspec (flush-output-port p))
  (check save 100)
  (check (lookahead-u8 p) 88)
  (check (get-u8 p) 88)
  (close-port p))

;; ========================================================================
;; Custom textual input/output port
;; ========================================================================
(display ";; Custom textual input/output port tests") (newline)

;; Test 5: basic textual write then read
(let* ([save #f]
       [p (make-custom-textual-input/output-port
           "custom-text-io"
           (lambda (str start end)
             (string-set! str start #\!)
             1)
           (lambda (str start end)
             (set! save (string-ref str start))
             1)
           #f #f #f)])
  (check (input-port? p) #t)
  (check (output-port? p) #t)
  (check (textual-port? p) #t)
  (check (binary-port? p) #f)
  (check-unspec (put-char p #\q))
  (check-unspec (flush-output-port p))
  (check save #\q)
  (check (get-char p) #\!)
  (close-port p))

;; Test 6: textual interleaved read/write
(let* ([save #f]
       [p (make-custom-textual-input/output-port
           "text-interleaved"
           (lambda (str start end)
             (string-set! str start #\X)
             1)
           (lambda (str start end)
             (set! save (string-ref str start))
             1)
           #f #f #f)])
  (check-unspec (put-char p #\a))
  (check-unspec (flush-output-port p))
  (check save #\a)
  (check (get-char p) #\X)
  (check-unspec (put-char p #\b))
  (check-unspec (flush-output-port p))
  (check save #\b)
  (check (get-char p) #\X)
  (close-port p))

;; Test 7: textual lookahead then write (requires set-position!)
(let* ([save #f]
       [pos 0]
       [p (make-custom-textual-input/output-port
           "text-lookahead-write"
           (lambda (str start end)
             (string-set! str start #\Z)
             1)
           (lambda (str start end)
             (set! save (string-ref str start))
             1)
           (lambda () pos)
           (lambda (newpos) (set! pos newpos))
           #f)])
  (check (lookahead-char p) #\Z)
  (check-unspec (put-char p #\w))
  (check-unspec (flush-output-port p))
  (check save #\w)
  (close-port p))

;; Test 8: put-string then get-char on textual i/o port
(let* ([save #f]
       [p (make-custom-textual-input/output-port
           "text-putstring"
           (lambda (str start end)
             (string-set! str start #\R)
             1)
           (lambda (str start end)
             (set! save (string-ref str start))
             1)
           #f #f #f)])
  (check-unspec (put-string p "hi"))
  (check-unspec (flush-output-port p))
  (check save #\i)
  (close-port p))

;; ========================================================================
;; File input/output port (binary)
;; ========================================================================
(display ";; File input/output port tests (binary)") (newline)

(let ([tmp "bidi-tmp1"])
  ;; create file with initial content
  (call-with-port
    (open-file-output-port tmp)
    (lambda (p)
      (put-bytevector p #vu8(1 2 3 4 5))))
  
  (let ([p (open-file-input/output-port tmp (file-options no-fail no-truncate))])
    (check (binary-port? p) #t)
    (check (input-port? p) #t)
    (check (output-port? p) #t)
    (check (port-position p) 0)
    (check (get-u8 p) 1)
    (check (get-u8 p) 2)
    (check (port-position p) 2)
    (check-unspec (put-bytevector p #vu8(10 20 30)))
    (check-unspec (flush-output-port p))
    (check-unspec (set-port-position! p 0))
    (check (get-bytevector-n p 5) #vu8(1 2 10 20 30))
    (close-port p))
  
  (delete-file tmp))

;; Test: file i/o with position seeking
(let ([tmp "bidi-tmp2"])
  (call-with-port
    (open-file-output-port tmp)
    (lambda (p)
      (put-bytevector p #vu8(0 1 2 3 4 5 6 7 8 9))))
  
  (let ([p (open-file-input/output-port tmp (file-options no-fail no-truncate))])
    ;; read 3 bytes
    (check (get-u8 p) 0)
    (check (get-u8 p) 1)
    (check (get-u8 p) 2)
    ;; seek to position 5
    (check-unspec (set-port-position! p 5))
    (check (get-u8 p) 5)
    ;; overwrite position 6-7
    (check-unspec (put-bytevector p #vu8(100 101)))
    (check-unspec (flush-output-port p))
    ;; seek to beginning and verify
    (check-unspec (set-port-position! p 0))
    (check (get-bytevector-n p 10) #vu8(0 1 2 3 4 5 100 101 8 9))
    (close-port p))
  
  (delete-file tmp))

;; ========================================================================
;; File input/output port (textual, latin-1)
;; ========================================================================
(display ";; File input/output port tests (textual latin-1)") (newline)

(let ([tmp "bidi-tmp3"])
  (call-with-port
    (open-file-output-port tmp)
    (lambda (p)
      (put-bytevector p #vu8(65 66 67 68 69))))
  
  (let ([p (open-file-input/output-port
            tmp
            (file-options no-fail no-truncate)
            'none
            (make-transcoder (latin-1-codec)))])
    (check (textual-port? p) #t)
    (check (get-char p) #\A)
    (check (get-char p) #\B)
    (check-unspec (put-string p "XY"))
    (check-unspec (flush-output-port p))
    (check-unspec (set-port-position! p 0))
    (check (get-string-n p 5) "ABXYE")
    (close-port p))
  
  (delete-file tmp))

;; ========================================================================
;; Bytevector output port get-output-bytevector
;; ========================================================================
(display ";; Bytevector output port tests") (newline)

(let-values ([(p get) (open-bytevector-output-port)])
  (check-unspec (put-u8 p 10))
  (check-unspec (put-bytevector p #vu8(11 12 13)))
  (check-unspec (put-bytevector p #vu8(14 15 16 17 18) 4))
  (check-unspec (put-bytevector p #vu8(14 15 16 17 18) 2 1))
  (check (get) #vu8(10 11 12 13 18 16))
  (check (get) #vu8())
  (close-port p))

;; ========================================================================
;; Summary
;; ========================================================================
(newline)
(display ";; ========================================") (newline)
(format #t ";; PASS: ~a  FAIL: ~a~%" *pass* *fail*)
(when (pair? *failures*)
  (display ";; Failures:") (newline)
  (for-each
    (lambda (f)
      (format #t ";;   ~s~%" f))
    (reverse *failures*)))
(if (zero? *fail*)
  (begin
    (display ";; ALL TESTS PASSED") (newline)
    (exit 0))
  (begin
    (display ";; SOME TESTS FAILED") (newline)
    (exit 1)))
