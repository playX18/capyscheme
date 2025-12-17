

(define exception-handler-key '|exception-handler-key aeee9cb5-b850-45ad-b460-c9868b7f2736|)


(define (call-with-exception-handler proc thunk)
  "Call THUNK with exception handler PROC installed."

  (let ([res (with-continuation-mark
              exception-handler-key proc
              (%with-handler proc thunk))])
    (unspecified)
    res))

(define (make-nested-exception-handler what old-exn)
  (lambda (exn)
    (default-uncaught-exception-handler
      (condition
        (make-non-continuable-violation)
        (make-who-condition (if what what "handler for uncaught exceptions"))
        (make-message-condition "Exception raised while handling exception")
        (make-irritants-condition (list exn old-exn))))))

(define (do-raise v)
  (define init-v v)
  (define set (continuation-mark-set->list* (current-continuation-marks) (list exception-handler-key) #f))

  (define (call-with-nested-handler thunk)
    (call-with-exception-handler
      (make-nested-exception-handler "excpeption handler" v)
      thunk))
  (let loop ([set set] [v init-v])
    (cond
      [(null? set)
        (call-with-nested-handler
          (lambda ()
            ((uncaught-exception-handler) v)))]
      [else
        (let* ([handlers (car set)]
               [handler (vector-ref handlers 0)]
               [set (cdr set)]
               [new-v (call-with-nested-handler (lambda () (handler v)))])
          (loop set new-v))])))

(define (raise v)
    (define p uncaught-exception-handler)
    (define y
        (let ([ueh (uncaught-exception-handler)])
            (lambda (exn)
                (define base (if (and (condition? exn) (continuable-exception? exn))
                    (continuable-exception-base exn)
                    exn))
                (if (and (condition? base) (serious-condition? base))
                    (ueh base)
                    ;; not &serious, try to 'continue'
                    (begin
                        ((current-exception-printer) base (current-error-port))
                        (newline (current-error-port))
                        (when (continuable-exception? exn)
                            ((continuable-exception-continuation exn) (lambda () (values)))))))))
    (define (swap)
      (let ([t (p)])
        (p y)
        (set! y t)))
    (dynamic-wind
        swap
        (lambda ()
            (do-raise v))
        swap))

(define (raise-continuable v)
    ((call/cc
        (lambda (k)
            (raise 
              (condition
                (make-continuable-exception k v)))))))

(define (with-exception-handler handler thunk)
    (call-with-exception-handler
        (lambda (exn)
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
                                        (make-irritants-condition args))))))))))
        thunk))

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
