(library (capy future)
  (export
          make-future 
          future-poll-fn 
          future-poll
          future:pending
          pending?
          await
          async 
          waker-wake
          waker-data
          waker-schedule
          make-waker)

  (import (rnrs)
          (capy)
          (capy generator))

  (define-record-type waker 
    (fields 
      data
      schedule))

  (define (waker-wake waker)
    "Wake the task associated with this waker."
    ((waker-schedule waker) (waker-data waker)))

  ;; A future type which is simply a wrapper around a polling function. 
  ;; Ready future returns values, pending future returns future:pending singleton. 
  ;; The polling function takes a waker as argument which can be used to wake
  ;; the task when the future is ready.
  (define-record-type future 
    (fields 
      poll-fn))

  (define (future-poll expr ctx)
    "Poll a future and returns its result."
    (unless (future? expr)
      (error 'poll "not a future" expr))
    ((future-poll-fn expr) ctx))


  ;; a helper to make `await` usable only inside async blocks
  (define-syntax-parameter %task-context 
    (syntax-rules () 
      [(_ . rest) (syntax-error "await used outside of async block")]))


  (define-syntax await 
    (lambda (stx)
      "(await expr) polls the future in expr until it is ready, yielding future:pending in 
      case that it is not ready.
      
      NOTE: This syntax can only be used inside `async` blocks. Using it in nested
      lambdas created inside said blocks is supported, but highly discouraged."
      (syntax-case stx () 
        [(_ expr)          
          #'
          (let ([e expr]) (let loop () 
            (receive vals (future-poll e %task-context)
              (cond 
                [(and (not (null? vals)) (pending? (car vals)))
                  (set! %task-context (yield future:pending))
                  (loop)]
                [else (apply values vals)]))))])))

  (define-syntax async 
    (lambda (stx)
      "
      - (async body ...) creates a future that resolves to the result of running (body ...).
      - (async define (name . args) body ...) creates a function that returns a future when called.
      - (async lambda args body ...) creates a lambda that returns a future when called.
      "
      (syntax-case stx (define) 
        [(_ define (name . args) body ...)
          #'(define (name . args)
              (async body ...))]
        [(_ lambda args body ...)
          #'(lambda args 
              (async body ...))]
        [(_ body ...)
          #'
          (let ([gen 
                  (generator (task-context)
                      (syntax-parameterize ([%task-context 
                        (make-variable-transformer 
                          (lambda (stx)
                            (syntax-case stx () 
                              [(set! _ e) #'(set! task-context e)]
                              [(_ . rest) (syntax-violation "invalid use of %task-context" stx)]
                              [_ #'task-context])))])
                        body ...))])
              (make-future 
                gen))])))

  (define-record-type pending)

  (define future:pending (make-pending))

)