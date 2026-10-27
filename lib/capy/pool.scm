(library (capy pool)
  (export
    make-worker-pool
    pool?
    pool-submit!
    pool-shutdown!
    pool-future?
    pool-future-get
    pool-future-done?
    parallel-map
    fan-out
    fan-out-error
    fan-in
    map-stage
    pipeline)
  (import (rnrs)
    (core thread)
    (capy channel)
    (capy sync))

  ;; A future is filled once, by the worker that runs its job.
  (define-record-type (future make-future* pool-future?)
    (fields
      lock
      cv
      (mutable state)
      (mutable result)))

  (define (make-future)
    (make-future* (make-mutex) (make-condition-variable) 'pending #f))

  (define (future-finish! f state result)
    (with-mutex (future-lock f)
      (future-state-set! f state)
      (future-result-set! f result)
      (condition-variable-broadcast (future-cv f))))

  (define (pool-future-done? f)
    "True once the job behind `f` has finished, successfully or not."
    (with-mutex (future-lock f)
      (not (eq? (future-state f) 'pending))))

  (define (pool-future-get f)
    "Wait for the job behind `f` and return its values. If the job raised,
    the same condition is raised here."
    (let ([state+result
            (with-mutex (future-lock f)
              (let loop ()
                (when (eq? (future-state f) 'pending)
                  (condition-variable-wait (future-cv f) (future-lock f))
                  (loop)))
              (cons (future-state f) (future-result f)))])
      (if (eq? (car state+result) 'ok)
        (apply values (cdr state+result))
        (raise (cdr state+result)))))

  ;; A pool of `n` worker threads taking jobs from one channel. Submitting
  ;; blocks while every worker is busy.
  (define-record-type (pool make-pool* pool?)
    (fields
      jobs
      workers
      lock
      (mutable closed?)))

  (define (make-worker-pool n)
    "A pool with `n` worker threads. Use pool-shutdown! when done."
    (unless (and (integer? n) (> n 0))
      (assertion-violation 'make-worker-pool "expected a positive integer" n))
    (let* ([jobs (make-channel 0)]
           [workers (make-wait-group)])
      (do ([i 0 (+ i 1)]) ((= i n))
        (wait-group-add! workers 1)
        (go (lambda ()
              (let loop ()
                (let-values ([(job ok?) (channel-recv/ok jobs)])
                  (when ok?
                    (job)
                    (loop))))
              (wait-group-done! workers))))
      (make-pool* jobs workers (make-mutex) #f)))

  (define (pool-submit! p thunk)
    "Run `thunk` on a worker and return a future for its values.
    Blocks while every worker is busy: submission itself is the
    backpressure, so the future exists only once a worker took the job."
    (unless (procedure? thunk)
      (assertion-violation 'pool-submit! "expected a thunk" thunk))
    (let ([f (make-future)])
      (when (with-mutex (pool-lock p) (pool-closed? p))
        (assertion-violation 'pool-submit! "pool is shut down"))
      (channel-send (pool-jobs p)
        (lambda ()
          (guard (e [#t (future-finish! f 'error e)])
            (future-finish! f 'ok
              (call-with-values thunk list)))))
      f))

  (define (pool-shutdown! p)
    "Stop accepting jobs, wait for queued and running jobs, then stop the
    workers. Calling it again just waits."
    (with-mutex (pool-lock p)
      (pool-closed?-set! p #t))
    (channel-close (pool-jobs p))
    (wait-group-wait (pool-workers p)))

  (define (parallel-map f lst . maybe-workers)
    "Apply `f` to each element of `lst` concurrently and return the results
    in order. At most `workers` calls run at once (default: one per element).
    If any call raises, the first such condition (in list order) is raised
    after all calls have finished."
    (let* ([n (length lst)]
           [workers (if (null? maybe-workers)
                      (max n 1)
                      (car maybe-workers))]
           [p (make-worker-pool workers)]
           [futures (map (lambda (x) (pool-submit! p (lambda () (f x)))) lst)])
      (let ([results
              (map (lambda (fu)
                     (guard (e [#t (cons 'error e)])
                       (call-with-values
                         (lambda () (pool-future-get fu))
                         (lambda vals (cons 'ok vals)))))
                futures)])
        (pool-shutdown! p)
        (let loop ([rs results] [acc '()])
          (cond
            [(null? rs) (reverse acc)]
            [(eq? (caar rs) 'error) (raise (cdar rs))]
            [else (loop (cdr rs) (cons (apply values (cdar rs)) acc))])))))

  ;; First error raised by a fan-out worker, keyed by its output channel.
  (define fan-out-errors (make-eq-hashtable))
  (define fan-out-errors-lock (make-mutex))

  (define (fan-out-error out)
    "The first condition a `fan-out` (or `map-stage`) worker raised
    while producing `out`, or #f if none did -- or if `out` did not
    come from fan-out. Check it after `out` is closed and drained:
    elements whose procedure raised produce no output value, so
    without this check a failing stage looks like a short input."
    (let ([entry (with-mutex fan-out-errors-lock
                   (hashtable-ref fan-out-errors out #f))])
      (and entry
        (with-mutex (car entry)
          (car (cdr entry))))))

  (define (fan-out in f workers)
    "Start `workers` threads that read from channel `in`, apply `f` and
    send the results to a new channel, which is returned. The output is
    closed once `in` is closed and every worker has finished. Output order is
    not the input order. If `f` raises on an element, that element is
    dropped, later elements are drained without calling `f`, and the
    first condition is kept for `fan-out-error`."
    (let ([out (make-channel 0)]
          [wg (make-wait-group)]
          [err-lock (make-mutex)]
          [err-cell (cons #f '())])
      (with-mutex fan-out-errors-lock
        (hashtable-set! fan-out-errors out (cons err-lock err-cell)))
      (do ([i 0 (+ i 1)]) ((= i workers))
        (go/wg wg
          (lambda ()
            (let loop ()
              (let-values ([(v ok?) (channel-recv/ok in)])
                (when ok?
                  (unless (car err-cell)
                    (let ([r (guard (e [#t
                                         (with-mutex err-lock
                                           (unless (car err-cell)
                                             (set-car! err-cell e)))
                                         no-result])
                               (list (f v)))])
                      (unless (eq? r no-result)
                        (channel-send out (car r)))))
                  (loop)))))))
      (go (lambda ()
            (wait-group-wait wg)
            (channel-close out)))
      out))

  (define no-result (list 'no-result))

  (define (fan-in ins)
    "Merge the channels in list `ins` into one new channel, which is closed
    once all of them are closed."
    (let ([out (make-channel 0)]
          [wg (make-wait-group)])
      (for-each
        (lambda (in)
          (go/wg wg
            (lambda ()
              (let loop ()
                (let-values ([(v ok?) (channel-recv/ok in)])
                  (when ok?
                    (channel-send out v)
                    (loop)))))))
        ins)
      (go (lambda ()
            (wait-group-wait wg)
            (channel-close out)))
      out))

  (define (map-stage f . maybe-workers)
    "A pipeline stage: a procedure from an input channel to an output channel
    that applies `f` to each value, using `workers` threads (default 1, so
    order is kept)."
    (let ([workers (if (null? maybe-workers) 1 (car maybe-workers))])
      (lambda (in) (fan-out in f workers))))

  (define (pipeline input . stages)
    "Feed the channel `input` through each stage in turn and return the last
    channel. A stage is a procedure from an input channel to an output
    channel, such as one made by map-stage."
    (let loop ([ch input] [stages stages])
      (if (null? stages)
        ch
        (loop ((car stages) ch) (cdr stages)))))
  )
