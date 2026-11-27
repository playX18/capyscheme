;;; (capy compress) throughput, and the system gzip on the same file for comparison.
;;;
;;;   stage-2/capy benchmarks/compress.scm [FILE] [RUNS]

(import (rnrs)
        (only (capy) cpu-time)
        (capy compress)
        (capy process))

(define (ends-with? s suffix)
  (let ([n (string-length s)] [k (string-length suffix)])
    (and (>= n k) (string=? (substring s (- n k) n) suffix))))

;; The arguments after this script's path, which may or may not be in (command-line).
(define args
  (let loop ([cl (command-line)])
    (cond [(null? cl) (cdr (command-line))]
          [(ends-with? (car cl) "compress.scm") (cdr cl)]
          [else (loop (cdr cl))])))
(define runs (if (> (length args) 1) (string->number (cadr args)) 5))

(define (read-file path)
  (call-with-port (open-file-input-port path) get-bytevector-all))

(define corpus-path
  (if (or (null? args) (string=? (car args) ""))
    (let ([path "/tmp/capy-compress-bench.txt"])
      (run "sh" (list "-c" (string-append "f=$(find pkgs/std/src -name '*.scm' | sort); "
                                       "for k in 1 2 3 4 5 6; do cat $f; done > " path)))
      path)
    (car args)))

(define data (read-file corpus-path))
(define n (bytevector-length data))

;; The best CPU time of THUNK in microseconds, after at least RUNS runs and one second, so that
;; Tier 2 has compiled the kernel.
(define (best-time thunk)
  (let loop ([k 0] [best #f] [result #f] [start (cpu-time)])
    (if (and (>= k runs) (> (- (cpu-time) start) 1000))
      (values best result)
      (let* ([t0 (cpu-time)] [r (thunk)] [t (max 1 (* 1000 (- (cpu-time) t0)))])
        (loop (+ k 1) (if (or (not best) (< t best)) t best) r start)))))

(define (mb/s bytes micros)
  (/ (round (* 10 (/ bytes micros))) 10.0))

(define (report name micros out-size)
  (display name)
  (display (make-string (max 1 (- 28 (string-length name))) #\space))
  (display (mb/s n micros))
  (display " MB/s")
  (when out-size
    (display "   ratio ")
    (display (/ (round (* 1000 (/ out-size n))) 1000.0)))
  (newline))

(display "input: ") (display corpus-path) (display ", ") (display n) (display " bytes") (newline)

(let-values ([(t _) (best-time (lambda () (crc32 data)))]) (report "crc32" t #f))
(let-values ([(t _) (best-time (lambda () (adler32 data)))]) (report "adler32" t #f))

(for-each
  (lambda (level)
    (let-values ([(t z) (best-time (lambda () (deflate data #:level level)))])
      (report (string-append "deflate level " (number->string level)) t (bytevector-length z))
      (let-values ([(t out) (best-time (lambda () (inflate z)))])
        (unless (equal? out data) (error 'bench "round trip failed" level))
        (report (string-append "inflate (of level " (number->string level) ")") t #f))))
  '(1 6 9))

(define (shell-time command)
  ;; Best user + system time of COMMAND, in microseconds: ten runs per measurement, since
  ;; `time -p` counts in hundredths of a second.
  (let loop ([k 0] [best #f])
    (if (= k runs)
      best
      (let* ([ten (string-append "for i in 0 1 2 3 4 5 6 7 8 9; do " command "; done")]
             [p (spawn "/usr/bin/time" (list "-p" "sh" "-c" ten) #:stdout 'null #:stderr 'string)]
             [_ (process-wait p)]
             [t (div (cpu-seconds (process-stderr-string p)) 10)])
        (loop (+ k 1) (if (or (not best) (< t best)) t best))))))

;; The user + sys seconds of `time -p` output, in microseconds.
(define (cpu-seconds text)
  (let ([port (open-string-input-port text)])
    (let loop ([total 0])
      (let ([word (read port)])
        (cond
          [(eof-object? word) (max 1 (exact (round (* 1000000 total))))]
          [(memq word '(user sys)) (loop (+ total (read port)))]
          [else (loop total)])))))

(for-each
  (lambda (level)
    (let* ([lv (number->string level)]
           [gz (string-append "/tmp/capy-compress-bench." lv ".gz")]
           [t (shell-time (string-append "gzip -c -n -" lv " " corpus-path " > " gz))]
           [size (bytevector-length (read-file gz))])
      (report (string-append "gzip -" lv) t size)
      (report (string-append "gzip -d (of -" lv ")") (shell-time (string-append "gzip -d -c " gz " > /dev/null")) #f)))
  '(1 6 9))
