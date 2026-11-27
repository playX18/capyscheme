;; Looking at running threads: who exists, what each one waits for, which
;; threads are deadlocked, and where a thread is right now.
(library (capy threads)
  (export
    thread-list
    thread-info?
    thread-info-id
    thread-info-parent
    thread-info-name
    thread-info-state
    thread-info-level
    thread-info-base-level
    thread-info-worker
    thread-info-pinned
    thread-info-wait
    thread-info-wait-on
    thread-info-waiting
    thread-info-age
    thread-info-killed?
    find-deadlocks
    dump-threads
    print-thread-tree
    thread-backtrace
    start-thread-trace
    stop-thread-trace!
    call-with-thread-trace
    write-thread-trace)
  (import (rnrs)
    (only (core primitives)
      %thread-snapshot
      %thread-deadlocks
      %thread-dump
      shadow-stack
      current-second)
    (capy concurrency))

  ;; One row of `%thread-snapshot`, as a record. `state` is one of `runnable`,
  ;; `running`, `blocked` and `done`. `wait` is #f or one of `mutex`,
  ;; `condition`, `join`, `sleep`, `io` and `park`; `wait-on` is the id of the
  ;; thread holding the mutex or being joined, the fd for `io`, otherwise #f.
  ;; `waiting` and `age` are in seconds.
  (define-record-type thread-info
    (fields id parent name state level base-level worker pinned
      wait wait-on waiting age killed?))

  (define (row->info row)
    (make-thread-info
      (vector-ref row 0) (vector-ref row 1) (vector-ref row 2)
      (vector-ref row 3) (vector-ref row 4) (vector-ref row 5)
      (vector-ref row 6) (vector-ref row 7) (vector-ref row 8)
      (vector-ref row 9) (vector-ref row 10) (vector-ref row 11)
      (vector-ref row 12)))

  (define (thread-list)
    "Every thread that has not finished, ordered by id, as `thread-info`
    records. The list is a snapshot: threads keep running while it is built."
    (map row->info (%thread-snapshot)))

  (define (find-deadlocks)
    "Lists of thread ids, one per cycle of threads that wait for each other
    through mutexes and joins. Each cycle starts at its smallest id. A
    condition variable wait is never part of a cycle, since any thread may
    signal it."
    (%thread-deadlocks))

  (define dump-threads
    (case-lambda
      [() (dump-threads (current-output-port))]
      [(port)
        (put-string port (%thread-dump))
        (flush-output-port port)]))

  ;; The threads as a forest by spawn parent, indented.
  (define print-thread-tree
    (case-lambda
      [() (print-thread-tree (current-output-port))]
      [(port)
        (let* ([infos (thread-list)]
               [ids (map thread-info-id infos)]
               ;; A thread whose parent has finished is shown as a root.
               [root? (lambda (t)
                        (not (memv (thread-info-parent t) ids)))]
               [children (lambda (t)
                           (filter (lambda (c) (= (thread-info-parent c) (thread-info-id t)))
                             infos))])
          (let walk ([ts (filter root? infos)] [depth 0])
            (for-each
              (lambda (t)
                (put-string port (make-string (* 2 depth) #\space))
                (put-string port
                  (string-append "#" (number->string (thread-info-id t))
                    (if (thread-info-name t)
                      (string-append " " (thread-info-name t))
                      "")
                    " " (symbol->string (thread-info-state t))
                    (if (thread-info-wait t)
                      (string-append " on " (symbol->string (thread-info-wait t)))
                      "")))
                (newline port)
                (walk (children t) (+ depth 1)))
              ts))
          (flush-output-port port))]))

  (define thread-backtrace
    (case-lambda
      [(thread) (thread-backtrace thread 1.0)]
      [(thread timeout)
        "The call stack of `thread`, as a list of `#(source procedure
        arguments)` frames, innermost first, or #f when it does not answer
        within `timeout` seconds (it has finished, or it never reaches an
        interrupt point). The thread is interrupted to read its own stack, so
        a blocked thread wakes briefly and then goes back to waiting."
        (if (eq? thread (current-thread))
          (shadow-stack)
          (let ([reply (make-channel 1)])
            (guard (e [#t #f])
              (thread-interrupt! thread
                (lambda () (channel-try-send reply (shadow-stack))))
              (let-values ([(frames ok timed-out) (channel-recv/timeout reply timeout)])
                (and ok frames)))))]))

  ;; --- Tracing ------------------------------------------------------------
  ;;
  ;; A sampler thread polls `thread-list` and turns each stretch during which a
  ;; thread keeps the same state (and wait) into one span. The result is Chrome
  ;; Trace Event JSON, which Perfetto (ui.perfetto.dev) and chrome://tracing
  ;; open: one row per thread, spans named "running", "blocked on mutex" and so
  ;; on. Spans are as accurate as the sampling interval.

  (define-record-type thread-trace
    (fields lock interval origin
      (mutable stop?)
      (mutable sampler)
      ;; id -> #(key start) for the span now open
      open
      ;; id -> name, kept for the metadata events
      names
      (mutable spans)))

  (define (span-key info)
    (let ([state (thread-info-state info)] [wait (thread-info-wait info)])
      (if (and wait (eq? state 'blocked))
        (string-append (symbol->string state) " on " (symbol->string wait))
        (symbol->string state))))

  (define (trace-sample! tr)
    (let* ([now (- (current-second) (thread-trace-origin tr))]
           [infos (thread-list)]
           [open (thread-trace-open tr)])
      (with-mutex (thread-trace-lock tr)
        (for-each
          (lambda (i)
            (let* ([id (thread-info-id i)]
                   [key (span-key i)]
                   [cur (hashtable-ref open id #f)])
              (hashtable-set! (thread-trace-names tr) id
                (or (thread-info-name i) (string-append "thread " (number->string id))))
              (cond
                [(not cur) (hashtable-set! open id (vector key now))]
                [(not (string=? (vector-ref cur 0) key))
                  (trace-close! tr id cur now)
                  (hashtable-set! open id (vector key now))])))
          infos)
        ;; Threads that have gone: close their spans.
        (let ([live (map thread-info-id infos)])
          (for-each
            (lambda (id)
              (unless (memv id live)
                (trace-close! tr id (hashtable-ref open id #f) now)
                (hashtable-delete! open id)))
            (vector->list (hashtable-keys open)))))))

  (define (trace-close! tr id cur now)
    (thread-trace-spans-set! tr
      (cons (vector id (vector-ref cur 0) (vector-ref cur 1) now)
        (thread-trace-spans tr))))

  (define start-thread-trace
    (case-lambda
      [() (start-thread-trace 0.005)]
      [(interval)
        "Start recording thread states every `interval` seconds (default 5ms).
        Returns a trace for `stop-thread-trace!`."
        (unless (and (real? interval) (positive? interval))
          (assertion-violation 'start-thread-trace "expected a positive number of seconds" interval))
        (let ([tr (make-thread-trace (make-mutex) interval (current-second) #f #f
                    (make-eqv-hashtable) (make-eqv-hashtable) '())])
          (trace-sample! tr)
          (thread-trace-sampler-set! tr
            (go-detached
              (lambda ()
                (set-thread-name! "trace sampler")
                (let loop ()
                  (sleep interval)
                  (unless (thread-trace-stop? tr)
                    (trace-sample! tr)
                    (loop))))))
          tr)]))

  (define (stop-thread-trace! tr)
    "Stop sampling and close every open span. The trace can then be written."
    (unless (thread-trace-stop? tr)
      (thread-trace-stop?-set! tr #t)
      (let ([th (thread-trace-sampler tr)])
        (when th (thread-cancel! th)))
      (let ([now (- (current-second) (thread-trace-origin tr))])
        (with-mutex (thread-trace-lock tr)
          (for-each
            (lambda (id) (trace-close! tr id (hashtable-ref (thread-trace-open tr) id #f) now))
            (vector->list (hashtable-keys (thread-trace-open tr))))
          (hashtable-clear! (thread-trace-open tr))))))

  (define (json-string str)
    (call-with-string-output-port
      (lambda (p)
        (put-char p #\")
        (string-for-each
          (lambda (c)
            (cond
              [(char=? c #\") (put-string p "\\\"")]
              [(char=? c #\\) (put-string p "\\\\")]
              [(char<? c #\space)
                (put-string p "\\u00")
                (put-string p (if (< (char->integer c) 16) "0" ""))
                (put-string p (number->string (char->integer c) 16))]
              [else (put-char p c)]))
          str)
        (put-char p #\"))))

  ;; Microseconds, as the integer the format wants.
  (define (micros seconds)
    (exact (round (* seconds 1000000))))

  (define (write-thread-trace tr port-or-path)
    "Write a stopped trace as Chrome Trace Event JSON, to a port or to the file
    at a path."
    (if (string? port-or-path)
      (call-with-port
        (open-file-output-port port-or-path (file-options no-fail) (buffer-mode block) (native-transcoder))
        (lambda (port) (write-thread-trace tr port)))
      (let ([port port-or-path])
        (unless (thread-trace-stop? tr)
          (assertion-violation 'write-thread-trace "the trace is still running" tr))
        (put-string port "{\"traceEvents\":[")
        (let ([first? #t])
          (define (emit! text)
            (if first? (set! first? #f) (put-string port ","))
            (put-string port text))
          (emit! "{\"ph\":\"M\",\"pid\":1,\"name\":\"process_name\",\"args\":{\"name\":\"capy\"}}")
          (for-each
            (lambda (id)
              (emit!
                (string-append "{\"ph\":\"M\",\"pid\":1,\"tid\":" (number->string id)
                  ",\"name\":\"thread_name\",\"args\":{\"name\":"
                  (json-string (hashtable-ref (thread-trace-names tr) id "?"))
                  "}}")))
            (list-sort < (vector->list (hashtable-keys (thread-trace-names tr)))))
          (for-each
            (lambda (sp)
              (let ([start (vector-ref sp 2)] [end (vector-ref sp 3)])
                (emit!
                  (string-append "{\"ph\":\"X\",\"pid\":1,\"tid\":" (number->string (vector-ref sp 0))
                    ",\"name\":" (json-string (vector-ref sp 1))
                    ",\"ts\":" (number->string (micros start))
                    ",\"dur\":" (number->string (max 1 (micros (- end start))))
                    "}"))))
            (reverse (thread-trace-spans tr))))
        (put-string port "]}")
        (flush-output-port port))))

  (define call-with-thread-trace
    (case-lambda
      [(path thunk) (call-with-thread-trace path 0.005 thunk)]
      [(path interval thunk)
        "Call `thunk` while recording thread states, then write the trace to
        `path` (even if `thunk` raises). Returns the thunk's values."
        (let ([tr (start-thread-trace interval)])
          (dynamic-wind
            (lambda () #f)
            thunk
            (lambda ()
              (stop-thread-trace! tr)
              (write-thread-trace tr path))))]))
  )
