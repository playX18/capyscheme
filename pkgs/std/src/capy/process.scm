;;; Child processes.
;;;
;;; A program is run with the arguments it is given, found in the `PATH`, and no shell is
;;; involved. `spawn` starts one and returns a process, `process-wait` waits for it:
;;;
;;;   (spawn "cc" '("-c" "a.c") #:env '(("CFLAGS" . "-O2")) #:cwd "src" #:stdout "build.log")
;;;   (run/string "git" '("rev-parse" "HEAD"))          ; => "4f1c2e9...\n"
;;;
;;; The keyword arguments of `spawn` are
;;;
;;;   #:env     an alist of (name . value): variables set in the environment of the child, on
;;;             top of the one of this process; (name . #f) removes one
;;;   #:cwd     the working directory of the child
;;;   #:stdin   `inherit` (the default), `null`, `pipe` (see `process-stdin`) or the path of a
;;;             file to read
;;;   #:stdout  `inherit` (the default), `null`, `pipe` (see `process-stdout`), `string` or
;;;             `bytes` (the output is collected while the process runs: see
;;;             `process-stdout-string`), the path of a file to write, or `(append PATH)`
;;;   #:stderr  like #:stdout, and `stdout`, which sends it where standard output goes
;;;
;;; The output of a child that is collected, or written to a file, is read as the child writes
;;; it, so a child that writes more than a pipe holds never waits for this process while this
;;; process waits for it, and any number of children can run at once.
;;;
;;; Waiting for several children is `process-wait-any`: no polling, the thread sleeps until one
;;; of them exits. A process pool runs a number of children at the most:
;;;
;;;   (run-jobs (map (lambda (file) (lambda () (spawn "cc" (list "-c" file)))) files) #:jobs 4)
;;;
;;; The exit status of a process is its exit code, or the negative of the number of the signal
;;; that ended it.

(library (capy process)
  (export spawn
    process?
    process-id
    process-command
    process-wait
    process-wait-any
    process-exit-status
    process-success?
    process-kill
    process-stdin
    process-stdout
    process-stderr
    process-stdout-bytes
    process-stdout-string
    process-stderr-bytes
    process-stderr-string
    run
    run/string
    &process-failure
    make-process-failure
    process-failure?
    process-failure-command
    process-failure-status
    process-failure-stderr
    make-process-pool
    process-pool?
    process-pool-size
    process-pool-count
    process-pool-full?
    process-pool-start!
    process-pool-wait!
    run-jobs
    exec-process)
  (import (except (rnrs) define)
    (rename (only (core kw) new-define) (new-define define))
    (only (capy)
      flush-output-port
      open-binary-fd-input-port
      open-binary-fd-output-port
      %process-spawn*
      %process-wait
      %process-kill
      %process-exec)
    (rename (only (capy) term/nonblocking!) (term/nonblocking! set-nonblocking!))
    (capy channel)
    (core thread))

  ;; The `done` channel of a process is closed when it has exited and `status` is set.
  (define-record-type (process %make-process process?)
    (fields id
      command
      (mutable status)
      done
      stdin
      stdout
      stderr
      (mutable out-capture)
      (mutable err-capture)))

  (define (process-exit-status p) (process-status p))

  ;; ----- spawning -----

  (define (stdio-argument who spec what)
    (cond
      [(memq spec '(inherit null pipe)) (symbol->string spec)]
      [(string? spec) (string-append "file:" spec)]
      [(and (pair? spec) (eq? (car spec) 'append) (pair? (cdr spec)) (string? (cadr spec)))
        (string-append "append:" (cadr spec))]
      [else (assertion-violation who (string-append "bad " what " specification") spec)]))

  (define (capture-spec? spec) (memq spec '(string bytes)))

  (define (environment-vector who env)
    (list->vector
      (map (lambda (entry)
            (unless (and (pair? entry) (string? (car entry))
                     (or (string? (cdr entry)) (not (cdr entry))))
              (assertion-violation who "expected (name . value) with a string name" entry))
            (if (cdr entry)
              (string-append (car entry) "=" (cdr entry))
              (car entry)))
        env)))

  (define (start-capture! fd)
    (set-nonblocking! fd #t)
    (let ([port (open-binary-fd-input-port "process output" fd 'block)])
      (go (lambda ()
            (let ([bytes (get-bytevector-all port)])
              (close-port port)
              (if (eof-object? bytes) (make-bytevector 0) bytes))))))

  (define (pipe-input-port fd)
    (set-nonblocking! fd #t)
    (open-binary-fd-input-port "process output" fd 'block))

  (define (spawn program args
            #:env [env '()]
            #:cwd [cwd #f]
            #:stdin [stdin 'inherit]
            #:stdout [stdout 'inherit]
            #:stderr [stderr 'inherit])
    (unless (string? program)
      (assertion-violation 'spawn "expected a program name" program))
    (unless (and (list? args) (for-all string? args))
      (assertion-violation 'spawn "expected a list of strings" args))
    (unless (or (not cwd) (string? cwd))
      (assertion-violation 'spawn "expected a directory" cwd))
    (let* ([merge? (eq? stderr 'stdout)]
           [out-arg (if (capture-spec? stdout) "pipe" (stdio-argument 'spawn stdout "#:stdout"))]
           [err-arg (cond
                      [merge? "merge"]
                      [(capture-spec? stderr) "pipe"]
                      [else (stdio-argument 'spawn stderr "#:stderr")])]
           [result (%process-spawn* (list->vector (cons program args))
                    (environment-vector 'spawn env)
                    cwd
                    (stdio-argument 'spawn stdin "#:stdin")
                    out-arg
                    err-arg)]
           [pid (car result)]
           [in-fd (cadr result)]
           [out-fd (caddr result)]
           [err-fd (cadddr result)]
           [done (make-channel 0)]
           [p (%make-process pid (cons program args) #f done
               (and in-fd (open-binary-fd-output-port "process input" in-fd 'block))
               (and out-fd (not (capture-spec? stdout)) (pipe-input-port out-fd))
               (and err-fd (not (capture-spec? stderr)) (pipe-input-port err-fd))
               (and out-fd (capture-spec? stdout) (start-capture! out-fd))
               (and err-fd (capture-spec? stderr) (start-capture! err-fd)))])
      (go (lambda ()
            (let ([status (%process-wait pid)])
              (process-status-set! p status)
              (channel-close done))))
      p))

  ;; ----- waiting -----

  ;; Waits for the process to exit and returns its exit status.
  (define (process-wait p)
    (channel-recv/ok (process-done p))
    (process-status p))

  (define (process-success? p) (eqv? (process-wait p) 0))

  ;; Waits until one of the processes PROCESSES has exited and returns it (the first of them
  ;; when several have). Its status is in `process-exit-status`.
  (define (process-wait-any processes)
    (when (null? processes)
      (assertion-violation 'process-wait-any "no processes to wait for"))
    (cond
      [(find process-status processes) => (lambda (p) p)]
      [else
        (let-values ([(index value ok?)
                      (channel-select
                        (list->vector
                          (map (lambda (p) (vector 'recv (process-done p) #f)) processes))
                        #f)])
          (list-ref processes index))]))

  (define (signal-number who signal)
    (cond
      [(integer? signal) signal]
      [(assq signal '((hup . 1) (int . 2) (quit . 3) (kill . 9) (term . 15)))
        => cdr]
      [else (assertion-violation who "unknown signal" signal)]))

  ;; Sends the signal SIGNAL (`term` by default, or `hup`, `int`, `quit`, `kill`, or a number)
  ;; to the process, when it has not exited. Returns whether it was sent.
  (define process-kill
    (case-lambda
      [(p) (process-kill p 'term)]
      [(p signal)
        (let ([number (signal-number 'process-kill signal)])
          (and (not (process-status p))
            (%process-kill (process-id p) number)))]))

  ;; ----- output -----

  (define (capture-bytes who p thread)
    (unless thread
      (error who "the output of the process is not collected: spawn it with #:stdout 'string or 'bytes"
        (process-command p)))
    (thread-join thread))

  ;; The bytes the process wrote to standard output; waits for the end of the output. It is
  ;; the output of a process spawned with #:stdout 'string or 'bytes.
  (define (process-stdout-bytes p) (capture-bytes 'process-stdout-bytes p (process-out-capture p)))
  (define (process-stderr-bytes p) (capture-bytes 'process-stderr-bytes p (process-err-capture p)))

  (define (decode bytes)
    (bytevector->string bytes (make-transcoder (utf-8-codec) (eol-style none) (error-handling-mode replace))))

  (define (process-stdout-string p) (decode (process-stdout-bytes p)))
  (define (process-stderr-string p) (decode (process-stderr-bytes p)))

  ;; ----- run -----

  (define-condition-type &process-failure &error
    make-process-failure
    process-failure?
    (command process-failure-command)
    (status process-failure-status)
    (stderr process-failure-stderr))

  (define (command-text command)
    (let loop ([words (cdr command)] [out (car command)])
      (if (null? words) out (loop (cdr words) (string-append out " " (car words))))))

  (define (failure p)
    (let* ([status (process-status p)]
           [stderr (if (process-err-capture p) (process-stderr-string p) "")])
      (condition
        (make-process-failure (process-command p) status stderr)
        (make-who-condition 'run)
        (make-message-condition
          (string-append "command failed: " (command-text (process-command p))
            (if (< status 0)
              (string-append " (killed by signal " (number->string (- status)) ")")
              (string-append " (exit status " (number->string status) ")"))
            (if (string=? stderr "") "" (string-append "\n" stderr)))))))

  ;; Runs the program and waits for it. The output is collected by default (`process-stdout-string`,
  ;; `process-stderr-string`) and standard input is empty. Raises a `&process-failure` condition
  ;; that holds the command, the exit status and what the program wrote to standard error when the
  ;; status is not 0. Returns the process.
  (define (run program args
            #:env [env '()]
            #:cwd [cwd #f]
            #:stdin [stdin 'null]
            #:stdout [stdout 'string]
            #:stderr [stderr 'string])
    (let ([p (spawn program args #:env env #:cwd cwd #:stdin stdin #:stdout stdout #:stderr stderr)])
      (process-wait p)
      (unless (eqv? (process-status p) 0)
        (raise (failure p)))
      p))

  ;; Runs the program like `run` and returns what it wrote to standard output.
  (define (run/string program args
            #:env [env '()]
            #:cwd [cwd #f]
            #:stdin [stdin 'null]
            #:stderr [stderr 'string])
    (process-stdout-string (run program args #:env env #:cwd cwd #:stdin stdin #:stderr stderr)))

  ;; ----- pools -----

  (define-record-type (process-pool %make-process-pool process-pool?)
    (fields size (mutable running)))

  ;; A pool of at most SIZE processes at once.
  (define (make-process-pool size)
    (unless (and (exact-integer? size) (> size 0))
      (assertion-violation 'make-process-pool "expected a positive size" size))
    (%make-process-pool size '()))

  (define (process-pool-count pool) (length (process-pool-running pool)))
  (define (process-pool-full? pool) (>= (process-pool-count pool) (process-pool-size pool)))

  ;; Calls THUNK, which starts a process, and adds the process to the pool under TAG. The pool
  ;; must not be full. Returns the process.
  (define (process-pool-start! pool tag thunk)
    (when (process-pool-full? pool)
      (error 'process-pool-start! "the pool is full"))
    (let ([p (thunk)])
      (process-pool-running-set! pool (append (process-pool-running pool) (list (cons tag p))))
      p))

  ;; Waits until a process of the pool has exited, removes it from the pool and returns its tag
  ;; and the process.
  (define (process-pool-wait! pool)
    (let ([running (process-pool-running pool)])
      (when (null? running)
        (error 'process-pool-wait! "the pool is empty"))
      (let* ([p (process-wait-any (map cdr running))]
             [entry (find (lambda (entry) (eq? (cdr entry) p)) running)])
        (process-wait p)
        (process-pool-running-set! pool (remq entry running))
        (values (car entry) p))))

  (define (remq x list)
    (cond [(null? list) '()] [(eq? x (car list)) (cdr list)] [else (cons (car list) (remq x (cdr list)))]))

  ;; Calls every thunk of THUNKS, which starts a process, keeping at most JOBS processes
  ;; running, and returns the finished processes in the order of the thunks.
  (define (run-jobs thunks #:jobs [jobs 4])
    (let ([pool (make-process-pool jobs)]
          [results (make-vector (length thunks) #f)])
      (define (finish-one!)
        (let-values ([(index p) (process-pool-wait! pool)])
          (vector-set! results index p)))
      (let loop ([thunks thunks] [index 0])
        (cond
          [(null? thunks)
            (let drain ()
              (unless (zero? (process-pool-count pool))
                (finish-one!)
                (drain)))]
          [(process-pool-full? pool) (finish-one!) (loop thunks index)]
          [else
            (process-pool-start! pool index (car thunks))
            (loop (cdr thunks) (+ index 1))]))
      (vector->list results)))

  ;; ----- exec -----

  ;; Replaces this process with PROGRAM, run with the arguments ARGV (which start with the name
  ;; the program is to know itself by). PROGRAM is a path: it is not searched for in the PATH.
  ;; Returns only when the program cannot be run: it raises the I/O error then.
  (define (exec-process program argv)
    (flush-output-port (current-output-port))
    (flush-output-port (current-error-port))
    (%process-exec program (list->vector argv))))
