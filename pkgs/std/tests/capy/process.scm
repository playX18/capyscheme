(import (rnrs) (srfi 64) (capy process) (capy concurrency) (only (capy) current-second executable-path microsecond))

(test-begin "capy process")

(define (read-lines path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([out '()])
        (let ([line (get-line port)])
          (if (eof-object? line) (reverse out) (loop (cons line out))))))))

(test-group "capture and exit codes"
  (test-equal "run/string" "hello world $HOME\n" (run/string "echo" '("hello" "world $HOME")))
  (test-equal "exit status" 3 (process-wait (spawn "sh" '("-c" "exit 3"))))
  (test-assert "success" (process-success? (spawn "true" '())))
  (test-assert "failure" (not (process-success? (spawn "false" '()))))
  (test-equal "status of a killed process" -9
    (let ([p (spawn "sleep" '("30"))])
      (process-kill p 'kill)
      (process-wait p)))
  (test-equal "run captures stderr in the condition"
    '(7 "boom\n" ("sh" "-c" "echo boom >&2; exit 7"))
    (guard (e [(process-failure? e)
                (list (process-failure-status e) (process-failure-stderr e) (process-failure-command e))])
      (run "sh" '("-c" "echo boom >&2; exit 7"))
      'no-error))
  (test-error "missing program" (spawn "capy-no-such-program" '()))
  (test-equal "stderr merged into stdout" "a\nb\n"
    (process-stdout-string
      (let ([p (spawn "sh" '("-c" "echo a; echo b >&2") #:stdout 'string #:stderr 'stdout)])
        (process-wait p)
        p))))

(test-group "environment and directory"
  (test-equal "env" "v1\n"
    (run/string "sh" '("-c" "echo $CAPY_PROCESS_TEST") #:env '(("CAPY_PROCESS_TEST" . "v1"))))
  (test-equal "env removal" "\n"
    (run/string "sh" '("-c" "echo $HOME") #:env '(("HOME" . #f))))
  (test-equal "cwd" "/\n" (run/string "pwd" '() #:cwd "/")))

(test-group "files and pipes"
  (let ([log (string-append "/tmp/capy-process-test-" (number->string (microsecond)) ".log")])
    (test-equal "stdout to a file" '("one")
      (begin
        (process-wait (spawn "echo" '("one") #:stdout log))
        (read-lines log)))
    (test-equal "append" '("one" "two")
      (begin
        (process-wait (spawn "echo" '("two") #:stdout (list 'append log)))
        (read-lines log)))
    (test-equal "stdin from a file" "one\ntwo\n"
      (run/string "cat" '() #:stdin log))
    (delete-file log))
  (test-equal "stdin pipe" "ping\n"
    (let ([p (spawn "cat" '() #:stdin 'pipe #:stdout 'string)])
      (put-bytevector (process-stdin p) (string->utf8 "ping\n"))
      (close-port (process-stdin p))
      (process-wait p)
      (process-stdout-string p)))
  (test-equal "stdout pipe" "pipe\n"
    (let* ([p (spawn "echo" '("pipe") #:stdout 'pipe)]
           [line (get-line (transcoded-port (process-stdout p) (native-transcoder)))])
      (process-wait p)
      (string-append line "\n"))))

(test-group "concurrency"
  (test-equal "output larger than a pipe, stdout and stderr at once"
    '(0 400000 400000)
    (let* ([p (spawn "sh"
                '("-c" "head -c 400000 /dev/zero | tr '\\0' x; head -c 400000 /dev/zero | tr '\\0' y >&2")
                #:stdout 'string #:stderr 'string)]
           [status (process-wait p)])
      (list status (string-length (process-stdout-string p)) (string-length (process-stderr-string p)))))
  (test-equal "wait-any returns the one that exits first"
    '(1 0)
    (let* ([slow (spawn "sleep" '("5"))]
           [fast (spawn "sh" '("-c" "exit 1"))]
           [first (process-wait-any (list slow fast))])
      (process-kill slow 'kill)
      (process-wait slow)
      (list (process-exit-status first) (if (eq? first fast) 0 1))))
  (test-equal "several children, large output, all at once"
    '(0 0 0 0)
    (let* ([ps (map (lambda (n)
                      (spawn "sh" '("-c" "head -c 300000 /dev/zero | tr '\\0' z")
                        #:stdout 'string))
                 '(1 2 3 4))])
      (let loop ([left ps] [sizes '()])
        (if (null? left)
          (map (lambda (s) (if (= s 300000) 0 1)) sizes)
          (let ([done (process-wait-any left)])
            (process-wait done)
            (loop (remq done left) (cons (string-length (process-stdout-string done)) sizes)))))))
  (test-assert "jobs run in parallel"
    (let* ([start (current-second)]
           [processes (run-jobs (map (lambda (n) (lambda () (spawn "sleep" '("1")))) '(1 2 3 4)) #:jobs 4)]
           [elapsed (- (current-second) start)])
      (and (= (length processes) 4) (< elapsed 3.5))))
  (test-equal "the pool never exceeds its size"
    '(2 2)
    (let ([pool (make-process-pool 2)] [peak 0])
      (for-each
        (lambda (n)
          (when (process-pool-full? pool) (process-pool-wait! pool))
          (process-pool-start! pool n (lambda () (spawn "sleep" '("0.2"))))
          (set! peak (max peak (process-pool-count pool))))
        '(1 2 3 4 5))
      (let drain () (unless (zero? (process-pool-count pool)) (process-pool-wait! pool) (drain)))
      (list peak (process-pool-size pool))))
  (test-equal "run-jobs returns the processes in order"
    '(0 1 2)
    (map process-exit-status
      (run-jobs (map (lambda (n) (lambda () (spawn "sh" (list "-c" (string-append "exit " (number->string n))))))
                 '(0 1 2))
        #:jobs 2))))

(test-group "exec"
  (test-equal "exec-process replaces the process"
    "hello\n"
    (process-stdout-string
      (run (executable-path)
        (list "-c" "(import (capy process)) (exec-process \"/bin/echo\" (list \"echo\" \"hello\"))"))))
  (test-equal "exec-process of a missing program raises"
    "raised"
    (process-stdout-string
      (run (executable-path)
        (list "-c" "(import (rnrs) (capy process)) (guard (e [#t (display \"raised\")]) (exec-process \"/no/such/program\" (list \"x\")))")))))

(define (remq x l) (cond [(null? l) '()] [(eq? x (car l)) (cdr l)] [else (cons (car l) (remq x (cdr l)))]))

(test-end)
