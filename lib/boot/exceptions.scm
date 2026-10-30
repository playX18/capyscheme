(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (late (boot port-api) current-output-port current-error-port) (late (boot print) display newline write) (late (boot format) format))
(|#%provide| all-defined)

;; The parameter used to print uncaught exceptions.
(define current-exception-printer
  (make-parameter
    (lambda (exn . port)
      (define p (if (null? port) (current-error-port) (car port)))
      (print-condition exn p))))

(define exception-handler-key '|exception-handler-key aeee9cb5-b850-45ad-b460-c9868b7f2736|)

(define (call-with-exception-handler proc thunk)
  "Call THUNK with exception handler PROC installed."

  (let ([res (with-continuation-mark
              exception-handler-key
              proc
              (thunk))])
    (unspecified)
    res))

(define (make-nested-exception-handler what old-exn)
  (lambda (exn)
    (if (and (condition? exn) (not (serious-condition? exn)))
      ;; A handler re-raised a non-serious condition (e.g. a warning): let it
      ;; propagate to the remaining handlers instead of printing here.
      exn
      (default-uncaught-exception-handler
        (condition
          (make-non-continuable-violation)
          (make-who-condition (if what what "handler for uncaught exceptions"))
          (make-message-condition "Exception raised while handling exception")
          (make-irritants-condition (list exn old-exn)))))))

;; Marks the handler that `do-raise` is calling, so that a handler wrapper can tell a call
;; made by `do-raise` (which chains on the value it returns) from a call made directly by the
;; runtime for an error raised in Rust code (which has no one to chain on).
(define raise-active-key '|raise-active-key 0f6c2d4e-6a41-4f0b-9a52-3c8f7f1d2b11|)

;; Drops the handlers of SET up to and including SELF.
(define (handlers-after self set)
  (cond
    [(null? set) '()]
    [(eq? (vector-ref (car set) 0) self) (cdr set)]
    [else (handlers-after self (cdr set))]))

(define (do-raise v . maybe-self)
  (define init-v v)
  (define set
    (let ([all (continuation-mark-set->list* (current-continuation-marks) (list exception-handler-key) #f)])
      (if (null? maybe-self) all (handlers-after (car maybe-self) all))))

  (define (call-with-nested-handler thunk)
    (call-with-exception-handler
      (make-nested-exception-handler "excpeption handler" v)
      thunk))
  (let loop ([set set] [v init-v])
    (cond
      [(null? set)
        ;; No handlers remain: the exception is uncaught.  Resumption happens
        ;; only when a handler returns, so even a continuable exception is
        ;; reported by the uncaught handler here (R6RS 7.1).
        (call-with-nested-handler
          (lambda ()
            ((uncaught-exception-handler) v)))]
      [else
        (let* ([handlers (car set)]
               [handler (vector-ref handlers 0)]
               [set (cdr set)]
               [new-v (call-with-nested-handler
                 (lambda ()
                   (with-continuation-mark raise-active-key handler (handler v))))])
          (loop set new-v))])))

(define (raise v . maybe-self)
  (define p uncaught-exception-handler)
  (define y
    (let ([ueh (uncaught-exception-handler)])
      (lambda (exn)
        (define base (if (and (condition? exn) (continuable-exception? exn))
                      (continuable-exception-base exn)
                      exn))
        (if (and (condition? base) (serious-condition? base))
          (ueh base)
          ;; A handler re-raised a non-serious condition: propagate it to the
          ;; remaining handlers.  Resumption happens only when a handler
          ;; returns (R6RS 7.1), never because a handler raised.
          exn))))
  (define (swap)
    (let ([t (p)])
      (p y)
      (set! y t)))
  (dynamic-wind
    swap
    (lambda ()
      (apply do-raise v maybe-self))
    swap))

(define (raise-continuable v)
  ((call/cc
      (lambda (k)
        (raise
          (condition
            (make-continuable-exception k v)))))))

(define (with-exception-handler handler thunk)
  (define (wrapper exn)
    (let ([chained
           (call/cc
        (lambda (esc)
          (call-with-exception-handler
            (lambda (new-exn)
              ;; chain to enclosing handler by returning
              (esc new-exn))
            (lambda ()
              (call-with-values (lambda () (handler (if (continuable-exception? exn)
                                                     (continuable-exception-base exn)
                                                     exn)))
                (if (continuable-exception? exn)
                  (lambda args
                    ((continuable-exception-continuation exn) (lambda () (apply values args))))
                  (lambda args
                    (condition
                      (make-serious-condition)
                      (make-non-continuable-violation)
                      (make-who-condition 'raise)

                      (make-message-condition (format "When handling non-continuable exception, exception handler returned~a"
                                               (if (null? args)
                                                 " (no-values)"
                                                 (apply
                                                   string-append
                                                   ":"
                                                   (let loop ([args args] [n 10])
                                                     (cond
                                                       [(null? args) '()]
                                                       [(zero? n) (list " ...")]
                                                       [else
                                                         (cons (format " ~a" (car args))
                                                           (loop (cdr args) (- n 1)))]))))))
                      (make-irritants-condition args)))))))))])
      (if (eq? (continuation-mark-set-first (current-continuation-marks) raise-active-key #f) wrapper)
        chained
        ;; Called directly by the runtime for an error raised in Rust code: nothing
        ;; chains on the returned value, so continue to the enclosing handlers here.
        (raise chained wrapper))))
  (call-with-exception-handler wrapper thunk))


(define (default-uncaught-exception-handler exn)
  (call-with-exception-handler
    (lambda (e)
      (%return-error e))
    (lambda ()
      (define out (current-output-port))
      ((current-exception-printer) exn out)
      (format out "~%~!")
      (%return-error exn))))

(define uncaught-exception-handler
  (let ([p (make-thread-local-fluid default-uncaught-exception-handler)])
    (lambda args
      (if (null? args)
        (fluid-ref p)
        (let ([old (fluid-ref p)])
          (fluid-set! p (car args))
          old)))))

;; Runtime hooks for Rust, see `install-runtime-hook!`.
(install-runtime-hook! 'raise raise)
(install-runtime-hook! 'raise-continuable raise-continuable)
(install-runtime-hook! 'with-exception-handler with-exception-handler)
