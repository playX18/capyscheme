(library (capy context)
  (export
    background-context
    context?
    with-cancel
    with-timeout
    with-deadline
    with-value
    context-done
    context-err
    context-value)
  (import (rnrs)
    (capy channel)
    (capy select)
    (core thread)
    (only (capy timer) make-timer timer-stop! timer-channel)
    (only (core primitives) current-second))

  ;; Go's context, with a few differences:
  ;; - Deadlines are absolute times in seconds, as given by (current-second).
  ;; - context-err returns #f while the context is live, and otherwise the
  ;;   symbol 'canceled or 'deadline-exceeded.
  ;; - A deadline runs a timer and a watcher thread. The watcher exits as
  ;;   soon as the context is cancelled, and the timer is stopped then.
  ;;
  ;; Cancelable contexts carry a lock, a done channel, an error and the
  ;; children to cancel. Value contexts only add a key/value pair and defer
  ;; everything else to their parent.
  (define-record-type (ctx make-ctx ctx?)
    (fields
      parent
      cancelable?
      lock
      done
      (mutable err)
      children
      key
      val
      (mutable timer)))

  (define (context? obj)
    "True for every context made by this library."
    (ctx? obj))

  (define never-done (make-channel 0))

  (define background-context
    (make-ctx #f #f #f never-done #f #f #f #f #f))

  (define (make-cancelable parent)
    (make-ctx parent #t (make-mutex) (make-channel 0) #f
      (make-eq-hashtable) #f #f #f))

  (define (cancel-ancestor c)
    "The nearest cancelable context at or above `c`, or #f."
    (cond
      [(not c) #f]
      [(ctx-cancelable? c) c]
      [else (cancel-ancestor (ctx-parent c))]))

  ;; Register `child` with `parent`, so that cancelling the parent cancels the
  ;; child. If the parent is already cancelled, the child is cancelled now.
  (define (attach! parent child)
    (let ([err
            (with-mutex (ctx-lock parent)
              (or (ctx-err parent)
                (begin
                  (hashtable-set! (ctx-children parent) child #t)
                  #f)))])
      (when err
        (cancel! child err))))

  (define (detach! parent child)
    (with-mutex (ctx-lock parent)
      (hashtable-delete! (ctx-children parent) child)))

  (define (cancel! c err)
    (let ([kids
            (with-mutex (ctx-lock c)
              (and (not (ctx-err c))
                (begin
                  (ctx-err-set! c err)
                  (channel-close (ctx-done c))
                  (let ([kids (hashtable-keys (ctx-children c))])
                    (hashtable-clear! (ctx-children c))
                    kids))))])
      (when kids
        (let ([t (ctx-timer c)])
          (when t (timer-stop! t)))
        (let ([p (cancel-ancestor (ctx-parent c))])
          (when p (detach! p c)))
        (vector-for-each (lambda (kid) (cancel! kid err)) kids))))

  (define (with-cancel parent)
    "Returns a context that is cancelled when `parent` is, or when the
    returned procedure is called. Returns two values: the context and the
    cancel procedure. Calling the procedure more than once is harmless."
    (let* ([child (make-cancelable parent)]
           [p (cancel-ancestor parent)])
      (when p (attach! p child))
      (values child (lambda () (cancel! child 'canceled)))))

  (define (with-deadline parent deadline)
    "Like with-cancel, but the context is also cancelled with
    'deadline-exceeded once (current-second) reaches `deadline`."
    (unless (real? deadline)
      (assertion-violation 'with-deadline "expected a time in seconds" deadline))
    (let-values ([(child cancel) (with-cancel parent)])
      (let ([remaining (- deadline (current-second))])
        (if (<= remaining 0)
          (cancel! child 'deadline-exceeded)
          (let ([t (make-timer remaining)])
            (ctx-timer-set! child t)
            (go (lambda ()
                  (select
                    ((recv (timer-channel t))
                      (cancel! child 'deadline-exceeded))
                    ((recv (ctx-done child))
                      #f))))
            ;; A cancel that ran before the timer was attached did not stop it.
            (when (ctx-err child) (timer-stop! t)))))
      (values child cancel)))

  (define (with-timeout parent secs)
    "Like with-deadline, with the deadline `secs` seconds from now."
    (with-deadline parent (+ (current-second) secs)))

  (define (with-value parent key val)
    "A context that carries `val` under `key` (compared with equal?) and is
    otherwise the same as `parent`."
    (make-ctx parent #f #f #f #f #f key val #f))

  (define (context-done c)
    "A channel that is closed when `c` is cancelled. It never closes for a
    context with no cancelable ancestor."
    (let ([a (cancel-ancestor c)])
      (if a (ctx-done a) never-done)))

  (define (context-err c)
    "#f while `c` is live, otherwise 'canceled or 'deadline-exceeded."
    (let ([a (cancel-ancestor c)])
      (and a (with-mutex (ctx-lock a) (ctx-err a)))))

  (define (context-value c key)
    "The value bound to `key` by the nearest with-value above `c`, or #f."
    (let loop ([x c])
      (cond
        [(not x) #f]
        [(and (ctx-key x) (equal? (ctx-key x) key)) (ctx-val x)]
        [else (loop (ctx-parent x))])))
  )
