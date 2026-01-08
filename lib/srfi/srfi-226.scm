;; SRFI-226: Control Features
;; Based on public implementation available at: https://github.com/scheme-requests-for-implementation/srfi-226

(library (srfi srfi-226)
  (export call-with-continuation-prompt abort-current-continuation
    call-with-current-continuation
    call-with-composable-continuation
    call-with-non-composable-continuation
    continuation?
    non-composable-continuation?
    call-in-continuation
    call-in
    return-to
    continuation-prompt-available?
    call-with-continuation-barrier
    dynamic-wind
    unwind-protect
    with-continuation-mark
    with-continuation-marks
    call-with-immediate-continuation-mark
    current-continuation-marks
    continuation-marks
    continuation-mark-set?
    continuation-mark-set->list
    continuation-mark-set->list*
    continuation-mark-set->iterator
    continuation-mark-set-first
    make-continuation-prompt-tag
    continuation-prompt-tag?
    default-continuation-prompt-tag
    make-continuation-mark-key
    continuation-mark-key?
    &continuation
    make-continuation-violation
    continuation-violation?
    continuation-violation-prompt-tag
    raise
    raise-continuable
    exception-handler-stack
    current-exception-handler
    with-exception-handler
    guard
    else
    =>
    make-parameter
    parameter?
    parameterize
    parameterization?
    current-parameterization
    call-with-parameterization
    make-thread-parameter
    temporarily
    current-input-port
    current-output-port
    current-error-port
    with-input-from-file
    with-output-to-file
    read-char
    peek-char
    read
    write-char
    newline
    display
    write
    call-in-initial-continuation
    thread
    current-thread
    thread?
    make-thread
    thread-name
    thread-start!
    thread-yield!
    thread-schedule-terminate!
    thread-terminate!
    thread-join!
    thread-interrupt!
    &thread
    make-thread-condition
    thread-condition?
    &uncaught-exception
    make-uncaught-exception-condition
    uncaught-exception-condition?
    uncaught-exception-condition-reason
    &thread-already-terminated
    &thread-timeout
    &thread-abandoned-mutex
    make-thread-abandoned-mutex-condition
    thread-abandoned-mutex-condition?
    make-thread-timeout-condition
    thread-timeout-condition?
    make-thread-already-terminated-condition
    thread-already-terminated-condition?
    delay
    make-promise
    promise?
    force
    run
    (rename (call-with-current-continuation call/cc))
    make-thread-local
    thread-local?
    tlref
    tlset!
    define-fluid
    define-thread-fluid
    fluid-let
    fluid-let*
    define-fluidified
    fluid-parameter)
  (import (core conditions)
    (except (core control)
      with-continuation-mark
      continuation-mark-set->list
      continuation-mark-set->list*
      continuation-mark-set-first
      current-continuation-marks)
    (rnrs records syntactic)
    (rename (except (capy)
             call-with-continuation-mark
             with-continuation-mark
             continuation-mark-set->list
             continuation-mark-set->list*
             continuation-mark-set-first
             current-continuation-marks)
      (call/cc plain-call/cc)
      (call-with-current-continuation plain-call-with-current-continuation)
      (call-in-continuation plain-call-in-continuation))
    (rename (except (rnrs (6))
             call/cc
             call-with-current-continuation
             dynamic-wind
             guard)
      (with-exception-handler rnrs:with-exception-handler)
      (raise-continuable rnrs:raise-continuable)
      (current-input-port rnrs:current-input-port)
      (current-output-port rnrs:current-output-port)
      (current-error-port rnrs:current-error-port)
      (with-input-from-file rnrs:with-input-from-file)
      (with-output-to-file rnrs:with-output-to-file)
      (read-char rnrs:read-char)
      (peek-char rnrs:peek-char)
      (read rnrs:read)
      (write-char rnrs:write-char)
      (newline rnrs:newline)
      (display rnrs:display)
      (write rnrs:write)))

  (define (%continuation=? x y) (eq? x y))

  (define-syntax thunkify
    (syntax-rules ()
      [(thunkify expr)
        (let-values ([val* expr])
          (lambda () (apply values val*)))]))

  (define-syntax with-implicit
    (lambda (stx)
      (syntax-case stx ()
        ((_ (k x* ...) e e* ...)
          #'(with-syntax ((x* (datum->syntax #'k 'x*)) ...)
             e
             e*
             ...))
        (_
          (syntax-violation 'with-implicit "illegal syntax" stx)))))
  (define-syntax define/who
    (lambda (x)
      (define out
        (lambda (k f e)
          (with-syntax ((k k) (f f) (e e))
            (with-implicit (k who)
              #'(define f
                 (let ((who 'f)) e))))))
      (syntax-case x ()
        ((k (f . u*) e e* ...)
          (identifier? #'f)
          (out #'k #'f #'(lambda u* e e* ...)))
        ((k f e)
          (identifier? #'f)
          (out #'k #'f #'e))
        (_
          (syntax-violation 'define/who "invalid syntax" x)))))

  (define-syntax define-syntax/who
    (lambda (x)
      (syntax-case x ()
        [(k name expr)
          (identifier? #'name)
          (with-implicit (k who)
            #'(define-syntax name
               (let ([who 'name])
                expr)))]
        [_
          (syntax-violation 'define-syntax/who "invalid syntax" x)])))

  (define *exit-continuation* (unspecified))
  (define *current-thread* (unspecified))
  (define *lock* #f)

  (define (lock!)
    (assert (not (locked?)))
    (set! *lock* #t))

  (define (unlock!)
    (assert (locked?))
    (set! *lock* #f))

  (define (locked?)
    *lock*)

  (define-record-type %mutex
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable locked?))
    (protocol
      (lambda (p)
        (lambda ()
          (p #f)))))

  (define %mutex-unlock!
    (case-lambda
      [(mtx)
        (assert (%mutex? mtx))
        (%mutex-locked?-set! mtx #f)]
      [(mtx cv)
        (assert (%mutex? mtx))
        (assert (%condition-variable? cv))
        (lock!)
        (%mutex-locked?-set! mtx #f)
        (%condition-variable-waiting-threads-set!
          cv
          (cons (%current-thread)
            (%condition-variable-waiting-threads cv)))
        (let f ()
          (unlock!)
          (%thread-yield!)
          (lock!)
          (when (memq (%current-thread) (%condition-variable-waiting-threads cv))
            (f)))
        (unlock!)]))

  (define %mutex-lock!
    (lambda (mtx)
      (assert (%mutex? mtx))
      (let f ()
        (lock!)
        (if (%mutex-locked? mtx)
          (begin
            (unlock!)
            (%thread-yield!)
            (f))
          (begin
            (%mutex-locked?-set! mtx #t)
            (unlock!))))))

  (define-record-type %condition-variable
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable waiting-threads))
    (protocol
      (lambda (p)
        (lambda ()
          (p '())))))

  (define %condition-variable-broadcast!
    (lambda (cv)
      (assert (%condition-variable? cv))
      (%condition-variable-waiting-threads-set! cv '())))

  (define interrupt-handler
    (lambda ()
      (%set-timer! #t)
      (unless (locked?)
        (%thread-yield!))))

  (define-record-type %thread
    (nongenerative)
    (opaque #t)
    (fields (mutable continuation)
      (mutable terminated?))
    (protocol
      (lambda (p)
        (lambda (thunk)
          (assert (procedure? thunk))
          (p thunk #f)))))

  (define-record-type %primordial-thread
    (parent %thread)
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (protocol
      (lambda (n)
        (lambda ()
          ((n (lambda () (assert #f))))))))

  (define %current-thread
    (lambda ()
      *current-thread*))

  (define current-threads
    (let ([threads '()])
      (case-lambda
        [() threads]
        [(t) (set! threads t)])))

  (define %current-dynamic-environment
    (let ([environment #f])
      (case-lambda
        [() environment]
        [(env) (set! environment env)])))

  (define %run
    (lambda (thunk)
      (lock!)
      (let-values
        ([val*
            (%call-with-current-continuation
              (lambda (k)
                (%call-with-interrupt-handler
                  interrupt-handler
                  (lambda ()
                    (set! *exit-continuation* k)
                    (set! *current-thread* (make-%primordial-thread))
                    (current-threads
                      (list *current-thread*))
                    (unlock!)
                    (thunk)))))])
        (current-threads '())
        (apply values val*))))

  (define %thread-start!
    (lambda (thunk)
      (assert (procedure? thunk))
      (lock!)
      (letrec ([thread
                 (make-%thread
                   (lambda ()
                     (unlock!)
                     (thunk)
                     (%thread-terminate! thread)))])
        (current-threads (append (current-threads) (list thread)))
        (unlock!)
        thread)))

  (define %thread-yield!
    (lambda ()
      (lock!)
      (let* ([t* (current-threads)]
             [ot (car t*)]
             [t* (append (cdr t*) (list ot))]
             [nt (car t*)])
        (if (eq? ot nt)
          (unlock!)
          (let ([env (%current-dynamic-environment)])
            (%call-with-current-continuation
              (lambda (k)
                (%thread-continuation-set! ot k)
                (current-threads t*)
                (abort-to-thread nt)))
            (%current-dynamic-environment env)
            (unlock!))))))

  (define %thread-interrupt!
    (lambda (thread thunk)
      (lock!)
      (if (eq? (%current-thread) thread)
        (begin
          (unlock!)
          (thunk)
          (values))
        (let ([k (%thread-continuation thread)])
          (%thread-continuation-set! thread
            (lambda ()
              (thunk)
              (k)))
          (unlock!)))))

  (define %thread-terminate!
    (lambda (thread)
      (lock!)
      (when (%primordial-thread? thread)
        (*exit-continuation*))
      (%thread-terminated?-set! thread #t)
      (let ([t* (remq thread (current-threads))])
        (current-threads t*)
        (if (eq? (%current-thread) (car t*))
          (unlock!)
          (abort-to-thread (car t*))))))

  (define %thread-join!
    (lambda (thread)
      (do ()
        ((%thread-terminated? thread))
        (%thread-yield!))))

  (define abort-to-thread
    (lambda (thread)
      (set! *current-thread* thread)
      ((%thread-continuation thread))))

  (define (continuation k . args)
    (cond
      [(null? args)
        (procedure-property k 'kont)]
      [else
        (set-procedure-property! k 'kont (car args))
        k]))

  (define-syntax %case-lambda-box
    (syntax-rules ()
      [(%case-lambda-box expr [formals body] ...)
        (let ([proc (case-lambda [formals body] ...)])
          (set-procedure-property! proc 'box expr)
          proc)]))

  (define (%case-lambda-box-ref proc)
    (procedure-property proc 'box))

  (define %call-with-current-continuation
    (lambda (proc)
      (plain-call/cc
        (lambda (k)
          ((plain-call/cc
              (lambda (abort-k)
                (continuation k abort-k)
                (lambda ()
                  (proc k)))))))))

  (define (%call-in-continuation k thunk)
    ((continuation k) thunk))

  (define-condition-type &continuation &violation
    make-continuation-violation
    continuation-violation?
    (prompt-tag continuation-violation-prompt-tag))

  (define continuation-violation
    (lambda (who msg prompt-tag . irr*)
      (let ((c (condition
                (make-continuation-violation prompt-tag)
                (make-message-condition msg)
                (make-irritants-condition irr*))))
        (raise
          (if who
            (condition c (make-who-condition who))
            c)))))

  (define-record-type continuation-mark-key
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields (mutable name))
    (protocol
      (lambda (p)
        (case-lambda
          [() (p #f)]
          [(name)
            (assert (symbol? name))
            (p name)]))))

  ;; Continuation prompt tags

  (define-record-type (%continuation-prompt-tag make-continuation-prompt-tag continuation-prompt-tag?)
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields (mutable name))
    (protocol
      (lambda (p)
        (case-lambda
          [() (p #f)]
          [(name)
            (assert (symbol? name))
            (p name)]))))

  (define default-continuation-prompt-tag
    (let ([prompt-tag (make-continuation-prompt-tag 'default)])
      (lambda ()
        prompt-tag)))

  (define continuation-barrier-tag
    (let ([barrier-tag (make-continuation-prompt-tag 'barrier)])
      (lambda ()
        barrier-tag)))

  (define (assert cond)
    (unless cond
      (continuation-violation 'assertion "assertion failed")))

  (define-record-type continuation-info
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields metacontinuation prompt-tag resume-k non-composable?)
    (protocol
      (lambda (p)
        (lambda (mk tag c non-comp?)
          (assert (continuation-prompt-tag? tag))
          (assert (procedure? c))
          (assert (boolean? non-comp?))
          (p mk tag c non-comp?)))))

  (define make-continuation
    (lambda (mk k marks winders prompt-tag resume-k non-composable?)
      (%case-lambda-box
        (make-continuation-info mk prompt-tag resume-k non-composable?)
        [val* (resume-k (lambda () (apply values val*)))])))

  (define continuation->continuation-info
    (lambda (who k)
      (let ([info (%case-lambda-box-ref k #f)])
        (unless (continuation-info? info)
          (assertion-violation who "not a continuation" k))
        info)))

  (define continuation-metacontinuation
    (lambda (who k)
      (continuation-info-metacontinuation
        (continuation->continuation-info who k))))

  (define continuation-resume-k
    (lambda (who k)
      (continuation-info-resume-k
        (continuation->continuation-info who k))))

  (define continuation-prompt-tag
    (lambda (who k)
      (continuation-info-prompt-tag
        (continuation->continuation-info who k))))

  (define continuation-non-composable?
    (lambda (who k)
      (continuation-info-non-composable?
        (continuation->continuation-info who k))))

  (define continuation?
    (lambda (obj)
      (continuation-info? (%case-lambda-box-ref obj #f))))

  (define non-composable-continuation?
    (lambda (obj)
      (cond
        [(%case-lambda-box-ref obj #f) => continuation-info-non-composable?]
        [else #f])))

  (define-record-type dynamic-environment
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable metacontinuation) (mutable marks) (mutable winders)
      (mutable thread)))

  (define current-metacontinuation
    (case-lambda
      [() (dynamic-environment-metacontinuation
           (%current-dynamic-environment))]
      [(mk) (dynamic-environment-metacontinuation-set!
             (%current-dynamic-environment)
             mk)]))

  (define current-marks
    (case-lambda
      [() (dynamic-environment-marks
           (%current-dynamic-environment))]
      [(marks) (dynamic-environment-marks-set!
                (%current-dynamic-environment)
                marks)]))

  (define current-winders
    (case-lambda
      [() (dynamic-environment-winders
           (%current-dynamic-environment))]
      [(winders) (dynamic-environment-winders-set!
                  (%current-dynamic-environment)
                  winders)]))

  (define make-marks
    (lambda (parameterization handler-stack)
      (list (cons (parameterization-continuation-mark-key) parameterization)
        (cons (handler-stack-continuation-mark-key) handler-stack))))

  (define marks-ref
    (case-lambda
      [(marks key)
        (marks-ref marks key (lambda () (assert #f)))]
      [(marks key fail)
        (marks-ref marks key fail values)]
      [(marks key fail succ)
        (cond
          [(assq key marks) => (lambda (a) (succ (cdr a)))]
          [else (fail)])]))
  (define marks-ref*
    (lambda (marks keys default)
      (let ([n (length keys)])
        (let f ([keys keys] [i 0] [vec #f])
          (define g (lambda (vec) (f (cdr keys) (fx+ i 1) vec)))
          (if (null? keys)
            vec
            (marks-ref
              marks
              (car keys)
              (lambda ()
                (g vec))
              (lambda (val)
                (let ([vec (or vec (make-vector n default))])
                  (vector-set! vec i val)
                  (g vec)))))))))
  (define marks-empty?
    (lambda (marks)
      (null? marks)))

  (define empty-marks?
    (lambda ()
      (marks-empty? (current-marks))))

  (define clear-marks!
    (lambda ()
      (current-marks (make-marks (current-parameterization)
                      (exception-handler-stack)))))

  (define set-mark!
    (lambda (key val)
      (let ([marks (current-marks)])
        (current-marks
          (cond
            [(assq key marks) =>
              (lambda (entry)
                (set-cdr! entry val)
                marks)]
            [else
              (cons (cons key val) marks)])))))

  (define ref-mark
    (lambda (key default)
      (marks-ref (current-marks) key (lambda () default))))

  ;; Winders

  (define-record-type winder
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields height continuation marks pre-thunk post-thunk)
    (protocol
      (lambda (p)
        (lambda (ht k marks pre-thunk post-thunk)
          (assert (fixnum? ht))
          (assert (procedure? k))
          (assert (procedure? pre-thunk))
          (assert (procedure? post-thunk))
          (p ht k marks pre-thunk post-thunk)))))

  (define winders-height
    (lambda (winders)
      (if (null? winders)
        0
        (fx+ (winder-height (car winders)) 1))))

  ;; Mark set frames

  (define-record-type mark-set-frame
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields tag marks)
    (protocol
      (lambda (p)
        (lambda (tag marks)
          (assert (or (not tag)
                   (continuation-prompt-tag? tag)))
          (p tag marks)))))

  ;; Continuation mark sets

  (define-record-type continuation-mark-set
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields frames))

  ;; Metacontinuations

  (define-record-type metacontinuation-frame
    (nongenerative)
    (sealed #t)
    (opaque #f)
    (fields tag continuation handler marks winders) ;TODO: Move handler before contination.
    (protocol
      (lambda (p)
        (lambda (tag k handler marks winders)
          (assert (or (not tag)
                   (continuation-prompt-tag? tag)))
          (assert (procedure? k))
          (assert (or (not handler) (procedure? handler)))
          (p tag k handler marks winders)))))

  (define push-metacontinuation-frame!
    (lambda (frame)
      (assert (metacontinuation-frame? frame))
      (current-metacontinuation
        (cons frame (current-metacontinuation)))))

  (define pop-metacontinuation-frame!
    (lambda ()
      (let ([mk (current-metacontinuation)])
        (and (pair? mk)
          (let ([mf (car mk)])
            (current-metacontinuation (cdr mk))
            (current-marks (metacontinuation-frame-marks mf))
            (current-winders (metacontinuation-frame-winders mf))
            mf)))))

  (define append-metacontinuation!
    (lambda (mk)
      (current-metacontinuation (append mk (current-metacontinuation)))))

  (define take-metacontinuation
    (lambda (who prompt-tag barrier?)
      (let f ([mk (current-metacontinuation)])
        (when (null? mk)
          (assertion-violation who "continuation includes no prompt with the given tag" prompt-tag))
        (let ([frame (car mk)] (mk (cdr mk)))
          (let ([tag (metacontinuation-frame-tag frame)])
            (cond
              [(eq? tag prompt-tag)
                '()]
              [(and barrier? (eq? tag (continuation-barrier-tag)))
                (continuation-violation who "applying the composable continuation would introduce a continuation barrier"
                  prompt-tag)]
              [else
                (cons frame (f mk))]))))))

  (define take-mark-set-frames
    (case-lambda
      [(mk prompt-tag prompt-available?)
        (%call-with-current-continuation
          (lambda (k)
            (let f ([mk mk])
              (cond
                [(null? mk)
                  (unless prompt-available?
                    (k #f))
                  '()]
                [else
                  (let ([frame (car mk)] [mk (cdr mk)])
                    (let ([tag (metacontinuation-frame-tag frame)])
                      (if (eq? tag prompt-tag)
                        '()
                        (let ([marks (metacontinuation-frame-marks frame)])
                          (if (and (not tag) (marks-empty? marks))
                            (f mk)
                            (cons (make-mark-set-frame tag marks) (f mk)))))))]))))]
      [(mk prompt-tag)
        (take-mark-set-frames mk prompt-tag #f)]))

  ;; Trampoline

  (define empty-continuation
    (let ([continuation #f])
      (case-lambda
        [()
          (assert continuation)]
        [(k)
          (assert (procedure? k))
          (set! continuation k)])))

  (define abort-continuation
    (let ([continuation #f])
      (case-lambda
        [()
          (assert continuation)]
        [(k)
          (assert (procedure? k))
          (set! continuation k)])))

  (define abort
    (lambda (thunk)
      (assert (procedure? thunk))
      (%call-in-continuation (empty-continuation) thunk)))

  (define empty-continuation?
    (lambda (k)
      (%continuation=? k (empty-continuation))))

  (define make-initial-metacontinuation
    (lambda (k parameterization handler-stack)
      (list
        (make-metacontinuation-frame
          (default-continuation-prompt-tag)
          k
          (make-default-handler (default-continuation-prompt-tag))
          (make-marks parameterization handler-stack)
          #f))))

  (define make-initial-dynamic-environment
    (lambda (k parameterization handler thread)
      (let ([handler-stack (list handler)])
        (make-dynamic-environment
          (make-initial-metacontinuation k parameterization
            handler-stack)
          (make-marks parameterization (list
                                        (lambda (exc)
                                          (unless (non-serious-condition? exc)
                                            (abort-current-continuation (default-continuation-prompt-tag)
                                              (lambda ()
                                                (raise exc)))))))
          '()
          thread))))

  (define non-serious-condition?
    (lambda (obj)
      (and (condition? obj) (not (serious-condition? obj)))))

  (define run
    (lambda (thunk)
      (%run
        (lambda ()
          (let ([thread (make-primordial-thread)])
            (rnrs:with-exception-handler
              (lambda (con)
                (abort (lambda () (%raise con))))
              (lambda ()
                (trampoline
                  (make-thread-thunk thread (make-parameterization) thunk))))
            (if (thread-end-exception thread)
              (raise (uncaught-exception-condition-reason (thread-end-exception thread)))
              (apply values (thread-end-result thread))))))))

  (define trampoline
    (lambda (thunk)
      (call-with-values
        (lambda ()
          (%call-with-current-continuation
            (lambda (null-k)
              (empty-continuation null-k)
              (abort thunk))))
        (lambda val*
          (cond
            [(pop-metacontinuation-frame!)
              =>
              (lambda (mf)
                (apply (metacontinuation-frame-continuation mf) val*))]
            [else
              (apply values val*)])))))

  (define success-handler
    (lambda val*
      (let* ([thread (current-thread)]
             [mtx (thread-%mutex thread)]
             [end-k (thread-end-continuation thread)])
        (%mutex-lock! mtx)
        (%call-in-continuation
          end-k
          (lambda ()
            (unless (symbol=? (thread-current-state thread) (thread-state terminated))
              (thread-end-result-set! thread val*)))))))

  (define failure-handler
    (lambda (con)
      (let* ([thread (current-thread)]
             [mtx (thread-%mutex thread)]
             [end-k (thread-end-continuation thread)])
        (%mutex-lock! mtx)
        (%call-in-continuation
          end-k
          (lambda ()
            (unless (symbol=? (thread-current-state thread) (thread-state terminated))
              (thread-end-exception-set! thread (make-uncaught-exception-condition con))))))))

  ;; SRFI 18 says that the handler should be called tail-called
  ;; by (some) primitives.  This doesn't make much sense with the
  ;; R[67]RS semantics.  In fact, SRFI 18's `raise' seems to be
  ;; R[67]RS's `raise-continuable'.
  (define %raise
    (lambda (con)
      (let f ([con con])
        (when (null? (exception-handler-stack))
          (abort-current-continuation (default-continuation-prompt-tag)
            (lambda ()
              (raise con))))
        (let ([handler (current-exception-handler)])
          (with-continuation-mark (handler-stack-continuation-mark-key)
            (cdr (exception-handler-stack))
            (begin
              (handler con)
              (f (make-non-continuable-violation))))))))
  ;; Raise-continuable calls the installed handler in tail context.
  (define raise-continuable
    (lambda (con)
      (let ([handler (current-exception-handler)])
        (with-continuation-mark (handler-stack-continuation-mark-key)
          (cdr (exception-handler-stack))
          (handler con)))))

  (define call-in-empty-continuation
    (lambda (thunk)
      (%call-with-current-continuation
        (lambda (k)
          (when (not (empty-continuation? k))
            (push-metacontinuation-frame!
              (make-metacontinuation-frame #f k #f (current-marks) (current-winders)))
            (clear-marks!)
            (current-winders '()))
          (abort thunk)))))

  (define call-in-empty-marks
    (case-lambda
      [(thunk)
        (call-in-empty-marks #f #f thunk)]
      [(tag handler thunk)
        (%call-with-current-continuation
          (lambda (k)
            (when (or tag
                   (not (empty-continuation? k))
                   (not (empty-marks?)))
              (push-metacontinuation-frame!
                (make-metacontinuation-frame tag k handler (current-marks) (current-winders)))
              (clear-marks!)
              (current-winders '()))
            (abort thunk)))]))

  (define abort-to
    (lambda (k marks winders thunk)
      (assert (procedure? k))
      (%call-in-continuation
        k
        (lambda ()
          (current-marks marks)
          (current-winders winders)
          (thunk)))))

  ;; Continuation prompts

  (define call-with-continuation-prompt
    (case-lambda
      [(thunk)
        (call-with-continuation-prompt thunk (default-continuation-prompt-tag))]
      [(thunk prompt-tag)
        (call-with-continuation-prompt thunk prompt-tag #f)]
      [(thunk prompt-tag handler)
        (let ([handler (or handler (make-default-handler prompt-tag))])
          (assert (continuation-prompt-tag? prompt-tag))
          (assert (procedure? handler))
          (call-in-empty-marks prompt-tag handler thunk))]))

  (define make-default-handler
    (lambda (prompt-tag)
      (lambda (thunk)
        (call-with-continuation-prompt thunk prompt-tag))))

  (define/who abort-current-continuation
    (lambda (prompt-tag . arg*)
      (unless (continuation-prompt-tag? prompt-tag)
        (assertion-violation who "not a continuation prompt tag" prompt-tag))
      (unless
        (metacontinuation-contains-prompt?
          (current-metacontinuation)
          prompt-tag)
        (continuation-violation
          who
          "no prompt with the given tag in current continuation"
          prompt-tag))
      (let f ()
        (if (null? (current-winders))
          (let ([mf (car (current-metacontinuation))])
            (if (eq? (metacontinuation-frame-tag mf) prompt-tag)
              (let ([handler (metacontinuation-frame-handler mf)])
                (pop-metacontinuation-frame!)
                (abort-to
                  (metacontinuation-frame-continuation mf)
                  (metacontinuation-frame-marks mf)
                  (metacontinuation-frame-winders mf)
                  (lambda ()
                    (apply handler arg*))))
              (begin
                (pop-metacontinuation-frame!)
                (f))))
          (wind-to
            (current-marks)
            '()
            f
            (lambda ()
              (unless
                (metacontinuation-contains-prompt?
                  (current-metacontinuation)
                  prompt-tag)
                (continuation-violation
                  who
                  "lost prompt with the given tag during abort of the current continuation"
                  prompt-tag))
              (f)))))))

  ;; Continuations

  (define make-composable-continuation
    (lambda (mk k marks winders prompt-tag)
      (make-continuation
        mk
        k
        marks
        winders
        prompt-tag
        (lambda (thunk)
          (call-in-composable-continuation mk k marks winders thunk))
        #f)))

  (define make-non-composable-continuation
    (lambda (mk k marks winders prompt-tag)
      (make-continuation
        mk
        k
        marks
        winders
        prompt-tag
        (lambda (thunk)
          (call-in-non-composable-continuation mk k marks winders prompt-tag thunk))
        #t)))

  (define call-in-composable-continuation
    (lambda (mk k marks winders thunk)
      (call-in-empty-marks
        (lambda ()
          (abort-to-composition (reverse mk) k marks winders thunk #f)))))

  (define call-in-non-composable-continuation
    (lambda (mk k marks winders prompt-tag thunk)
      (let retry ()
        (let-values ([(dest-mf* base-mk)
                       (common-metacontinuation #f mk (current-metacontinuation) prompt-tag)])
          (let f ()
            (if (eq? (current-metacontinuation) base-mk)
              (abort-to-composition dest-mf* k marks winders thunk retry)
              (wind-to
                (current-marks)
                '()
                (lambda ()
                  (pop-metacontinuation-frame!)
                  (f))
                retry)))))))

  (define abort-to-composition
    (lambda (mf* k marks winders thunk maybe-again-thunk)
      (let f ([mf* mf*])
        (if (null? mf*)
          (wind-to
            marks
            winders
            (lambda ()
              (abort-to k marks winders thunk))
            maybe-again-thunk)
          (let ([mf (car mf*)])
            (wind-to
              (metacontinuation-frame-marks mf)
              (metacontinuation-frame-winders mf)
              (lambda ()
                (current-metacontinuation (cons mf (current-metacontinuation)))
                (current-winders '())
                (f (cdr mf*)))
              maybe-again-thunk))))))

  (define/who call-with-non-composable-continuation
    (case-lambda
      [(proc)
        (call-with-non-composable-continuation proc (default-continuation-prompt-tag))]
      [(proc prompt-tag)
        (assert (procedure? proc))
        (assert (continuation-prompt-tag? prompt-tag))
        (%call-with-current-continuation
          (lambda (k)
            (proc (make-non-composable-continuation
                   (take-metacontinuation who prompt-tag #f)
                   k
                   (current-marks)
                   (current-winders)
                   prompt-tag))))]))

  (define/who call-with-current-continuation
    (lambda (proc)
      (call-with-non-composable-continuation proc)))

  (define/who call-with-composable-continuation
    (case-lambda
      [(proc)
        (call-with-composable-continuation proc (default-continuation-prompt-tag))]
      [(proc prompt-tag)
        (assert (procedure? proc))
        (assert (continuation-prompt-tag? prompt-tag))
        (%call-with-current-continuation
          (lambda (k)
            (proc
              (make-composable-continuation
                (take-metacontinuation who prompt-tag #t)
                k
                (current-marks)
                (current-winders)
                prompt-tag))))]))

  (define common-metacontinuation
    (lambda (who dest-mk current-mk tag)
      (let ([base-mk*
              (let f ([current-mk current-mk] [base-mk* '()])
                (when (null? current-mk)
                  (continuation-violation who "current continuation includes no prompt with the given tag" tag))
                (if (eq? (metacontinuation-frame-tag (car current-mk)) tag)
                  (cons current-mk base-mk*)
                  (f (cdr current-mk) (cons current-mk base-mk*))))])
        (let f ([dest-mf* (reverse dest-mk)]
                [base-mk* (cdr base-mk*)]
                [base-mk (car base-mk*)])
          (cond
            [(null? dest-mf*)
              (values '() base-mk)]
            [(null? base-mk*)
              (check-for-barriers dest-mf* tag)
              (values dest-mf* base-mk)]
            [(eq? (car dest-mf*) (caar base-mk*))
              (f (cdr dest-mf*) (cdr base-mk*) (car base-mk*))]
            [else
              (check-for-barriers dest-mf* tag)
              (values dest-mf* base-mk)])))))

  (define check-for-barriers
    (lambda (dest-mf* tag)
      (do ([dest-mf* dest-mf* (cdr dest-mf*)])
        ((null? dest-mf*))
        (when (eq? (metacontinuation-frame-tag (car dest-mf*)) (continuation-barrier-tag))
          (continuation-violation #f "applying the continuation would introduce a continuation barrier" tag)))))

  (define/who call-in-continuation
    (lambda (k proc . args)
      (unless (procedure? proc)
        (assertion-violation who "not a procedure" proc))
      ((continuation-resume-k 'call-in-continuation k) (lambda () (apply proc args)))))

  (define/who call-in
    (lambda (k proc . args)
      (unless (non-composable-continuation? k)
        (assertion-violation who "not a non-composable continuation object" k))
      (unless (procedure? proc)
        (assertion-violation who "not a procedure" proc))
      ((continuation-resume-k 'call-in k) (lambda () (apply proc args)))))

  (define/who return-to
    (lambda (k . args)
      (unless (non-composable-continuation? k)
        (assertion-violation who "not a non-composable continuation object" k))
      ((continuation-resume-k 'return-to k) (lambda () (apply values args)))))

  (define/who continuation-prompt-available?
    (case-lambda
      [(tag)
        (metacontinuation-contains-prompt? (current-metacontinuation) tag)]
      [(tag k)
        (or (and (continuation-non-composable? who k)
             (eq? (continuation-prompt-tag who k) tag))
          (metacontinuation-contains-prompt? (continuation-metacontinuation who k) tag))]))

  (define metacontinuation-contains-prompt?
    (lambda (mk tag)
      (let f ([mk mk])
        (and (not (null? mk))
          (or (eq? (metacontinuation-frame-tag (car mk)) tag)
            (f (cdr mk)))))))

  (define call-with-continuation-barrier
    (lambda (thunk)
      (call-in-empty-marks (continuation-barrier-tag) #f thunk)))

  ;; Dynamic-wind

  (define/who dynamic-wind
    (lambda (pre-thunk thunk post-thunk)
      (unless (procedure? pre-thunk)
        (assertion-violation who "not a procedure" pre-thunk))
      (unless (procedure? thunk)
        (assertion-violation who "not a procedure" thunk))
      (unless (procedure? post-thunk)
        (assertion-violation who "not a procedure" post-thunk))
      (%call-with-current-continuation
        (lambda (k)
          (let* ([winders (current-winders)]
                 [winder (make-winder (winders-height winders)
                          k
                          (current-marks)
                          pre-thunk
                          post-thunk)])
            (pre-thunk)
            (current-winders (cons winder winders))
            (call-with-values thunk
              (lambda val*
                (current-winders winders)
                (post-thunk)
                (apply values val*))))))))

  (define wind-to
    (lambda (marks dest-winders then-thunk maybe-again-thunk)
      (let ([saved-mk (current-metacontinuation)])
        (current-marks marks)
        (let f ([winder* '()] [dest-winders dest-winders])
          (if (and maybe-again-thunk (not (eq? saved-mk (current-metacontinuation))))
            (maybe-again-thunk)
            (let ([winders (current-winders)])
              (cond
                [(winders=? dest-winders winders)
                  (if (null? winder*)
                    (then-thunk)
                    (let ([winders (cons (car winder*) winders)]
                          [winder* (cdr winder*)])
                      (rewind winders
                        (lambda ()
                          (current-winders winders)
                          (f winder* winders)))))]
                [(or (null? dest-winders)
                    (and (not (null? winders))
                      (fx>? (winder-height (car winders))
                        (winder-height (car dest-winders)))))
                  (unwind winders
                    (lambda ()
                      (f winder* dest-winders)))]
                [else
                  (f (cons (car dest-winders) winder*) (cdr dest-winders))])))))))

  (define wind
    (lambda (winders ref then-thunk)
      (let ([winder (car winders)]
            [winders (cdr winders)])
        (let ([winder-thunk (ref winder)])
          (abort-to
            (winder-continuation winder)
            (winder-marks winder)
            winders
            (lambda ()
              (winder-thunk)
              (then-thunk)))))))

  (define unwind
    (lambda (winders thunk)
      (wind winders winder-post-thunk thunk)))

  (define rewind
    (lambda (winders thunk)
      (wind winders winder-pre-thunk thunk)))

  (define winders=?
    (lambda (w1 w2)
      (fx=? (winders-height w1) (winders-height w2))))

  (define-syntax/who unwind-protect
    (lambda (stx)
      (syntax-case stx ()
        [(_ protected-expr cleanup-expr ...)
          #'(dynamic-wind
             (lambda () (values))
             (lambda () (call-with-continuation-barrier (lambda () protected-expr)))
             (lambda () (values) cleanup-expr ...))]
        [_ (syntax-violation who "invalid syntax" stx)])))

  ;; Continuation marks

  (define-syntax/who with-continuation-mark
    (lambda (stx)
      (syntax-case stx ()
        [(_ key-expr val-expr result-expr)
          #'(call-in-continuation-mark
             key-expr
             val-expr
             (lambda ()
              result-expr))]
        [_
          (syntax-violation who "invalid syntax" stx)])))

  (define-syntax/who with-continuation-marks
    (lambda (stx)
      (syntax-case stx ()
        [(_ ([key-expr val-expr] ...) e1 e2 ...)
          #'(call-in-empty-continuation
             (lambda ()
              (set-mark! key-expr val-expr)
              ...
              (letrec* ()
               e1
               e2
               ...)))]
        [_
          (syntax-violation who "invalid syntax" stx)])))

  (define call-in-continuation-mark
    (lambda (key val thunk)
      (call-in-empty-continuation
        (lambda ()
          (set-mark! key val)
          (printf "mark-set done ~%")
          (thunk)
          (printf "thunk done ~%")))))

  (define/who call-with-immediate-continuation-mark
    (case-lambda
      [(key proc)
        (call-with-immediate-continuation-mark key proc #f)]
      [(key proc default)
        (unless (procedure? proc)
          (assertion-violation
            'who
            "not a procedure"
            proc))
        (call-in-empty-continuation
          (lambda ()
            (proc (ref-mark key default))))]))

  (define/who continuation-marks
    (case-lambda
      [(k) (continuation-marks k (default-continuation-prompt-tag))]
      [(k prompt-tag)
        (unless (continuation-prompt-tag? prompt-tag)
          (assertion-violation
            who
            "not a continuation prompt tag"
            prompt-tag))
        (let ([frames (take-mark-set-frames (continuation-metacontinuation who k) prompt-tag
                       (eq? (continuation-prompt-tag who k) prompt-tag))])
          (unless frames
            (assertion-violation who "prompt tag not found in continuation" prompt-tag))
          (make-continuation-mark-set (or frames '())))]))

  (define/who current-continuation-marks
    (case-lambda
      [()
        (current-continuation-marks (default-continuation-prompt-tag))]
      [(prompt-tag)
        (unless (continuation-prompt-tag? prompt-tag)
          (assertion-violation
            who
            "not a continuation prompt tag"
            prompt-tag))
        (let ([frames (or (take-mark-set-frames (current-metacontinuation) prompt-tag)
                       '())])
          (make-continuation-mark-set
            (if (empty-marks?)
              frames
              (cons (make-mark-set-frame #f (current-marks)) frames))))]))

  (define call-with-continuation-mark-set
    (lambda (who set tag proc)
      (unless (continuation-prompt-tag? tag)
        (assertion-violation
          who
          "not a continuation prompt tag"
          tag))
      (let ([set (or set (current-continuation-marks tag))])
        (unless (continuation-mark-set? set)
          (assertion-violation
            who
            "not a continuation mark set"
            set))
        (proc set))))

  (define/who continuation-mark-set->list
    (case-lambda
      [(set key)
        (continuation-mark-set->list set key (default-continuation-prompt-tag))]
      [(set key tag)
        (call-with-continuation-mark-set
          who
          set
          tag
          (lambda (set)
            (let f ([frames (continuation-mark-set-frames set)])
              (if (null? frames)
                '()
                (let ([frame (car frames)] [frames (cdr frames)])
                  (if (eq? (mark-set-frame-tag frame) tag)
                    '()
                    (marks-ref
                      (mark-set-frame-marks frame)
                      key
                      (lambda ()
                        (f frames))
                      (lambda (val)
                        (cons val (f frames))))))))))]))

  (define/who continuation-mark-set->list*
    (case-lambda
      [(set keys)
        (continuation-mark-set->list* set keys #f)]
      [(set keys default)
        (continuation-mark-set->list* set keys default (default-continuation-prompt-tag))]
      [(set keys default tag)
        (unless (list? keys)
          (assertion-violation who "not a key list keys"))
        (let f ([iter (continuation-mark-set->iterator set keys default tag)])
          (let-values ([(vec iter) (iter)])
            (if vec
              (cons vec (f iter))
              '())))]))

  (define/who continuation-mark-set->iterator
    (case-lambda
      [(set keys)
        (continuation-mark-set->iterator set keys #f)]
      [(set keys default)
        (continuation-mark-set->iterator set keys default (default-continuation-prompt-tag))]
      [(set keys default tag)
        (unless (list? keys)
          (assertion-violation who "not a key list" keys))
        (call-with-continuation-mark-set
          who
          set
          tag
          (lambda (set)
            (let make-iterator ([frames (continuation-mark-set-frames set)])
              (lambda ()
                (let f ([frames frames])
                  (if (null? frames)
                    (values #f (make-iterator '()))
                    (let ([frame (car frames)] [frames (cdr frames)])
                      (if (eq? (mark-set-frame-tag frame) tag)
                        (values #f (lambda () (assertion-violation who "attempt to iterator past the end"
                                               set
                                               keys
                                               default
                                               tag)))
                        (let ([val-vec
                                (marks-ref*
                                  (mark-set-frame-marks frame)
                                  keys
                                  default)])
                          (if val-vec
                            (values val-vec (make-iterator frames))
                            (f frames)))))))))))]))

  (define/who continuation-mark-set-first
    (case-lambda
      [(set key)
        (continuation-mark-set-first set key #f)]
      [(set key default)
        (continuation-mark-set-first set key default (default-continuation-prompt-tag))]
      [(set key default tag)
        (call-with-continuation-mark-set
          who
          set
          tag
          (lambda (set)
            (let f ([frames (continuation-mark-set-frames set)])
              (if (null? frames)
                default
                (let ([frame (car frames)])
                  (if (eq? (mark-set-frame-tag frame) tag)
                    default
                    (marks-ref
                      (mark-set-frame-marks frame)
                      key
                      (lambda ()
                        (f (cdr frames)))
                      values)))))))]))

  ;; Parameter objects

  (define-record-type parameterization
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields cells)
    (protocol
      (lambda (p)
        (case-lambda
          [() (p '())]
          [(cells) (p cells)]))))

  (define parameterization-extend
    (lambda (parameterization key+value*)
      (make-parameterization
        (append key+value*
          (parameterization-cells parameterization)))))

  (define parameterization-ref
    (lambda (parameterization key)
      (assert (parameterization? parameterization))
      (assq key (parameterization-cells parameterization))))

  (define parameter-cell
    (lambda (key)
      (parameterization-ref (current-parameterization) key)))

  (define-record-type parameter-key
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable name))
    (protocol
      (lambda (p)
        (case-lambda
          [() (p #f)]
          [(name) (assert (symbol? name)) (p name)]))))

  (define parameterization-continuation-mark-key
    (let ([mark-key (make-continuation-mark-key 'parameterization)])
      (lambda ()
        mark-key)))

  (define current-parameterization
    (lambda ()
      (marks-ref (current-marks) (parameterization-continuation-mark-key))))

  (define/who call-with-parameterization
    (lambda (parameterization thunk)
      (unless (parameterization? parameterization)
        (assertion-violation who "not a paramerization" parameterization))
      (unless (procedure? thunk)
        (assertion-violation who "not a procedure" thunk))
      (with-continuation-mark (parameterization-continuation-mark-key)
        parameterization
        (thunk))))

  (define-record-type parameter-info
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields converter key thread-local?))

  (define/who make-parameter
    (case-lambda
      [(init)
        (make-parameter init values)]
      [(init converter)
        (unless (procedure? converter)
          (assertion-violation who "not a procedure" converter))
        (let ([key (make-parameter-key)]
              [val (converter init)])
          (%case-lambda-box (make-parameter-info converter key #f)
            [()
              (let ([cell (parameter-cell key)])
                (if cell (cdr cell) val))]
            [(init)
              (let ([cell (parameter-cell key)])
                (if cell
                  (set-cdr! cell (converter init))
                  (set! val (converter init))))]))]))

  (define/who make-thread-parameter
    (case-lambda
      [(init)
        (make-thread-parameter init values)]
      [(init converter)
        (unless (procedure? converter)
          (assertion-violation who "not a procedure" converter))
        (let ([key (make-parameter-key)]
              [tl (make-thread-local (converter init) #t)])
          (%case-lambda-box (make-parameter-info converter key #t)
            [()
              (let ([cell (parameter-cell key)])
                (if cell (tlref (cdr cell)) (tlref tl)))]
            [(init)
              (let ([cell (parameter-cell key)])
                (if cell
                  (tlset! (cdr cell) (converter init))
                  (tlset! tl (converter init))))]))]))

  (define parameter?
    (lambda (obj)
      (and (procedure? obj)
        (parameter-info? (%case-lambda-box-ref obj #f)))))

  (define parameter->parameter-info
    (lambda (who param)
      (let ([info (%case-lambda-box-ref param #f)])
        (unless (parameter-info? info)
          (assertion-violation who "not a parameter" param))
        info)))

  (define parameter-key+value
    (lambda (who param init)
      (let ([info (parameter->parameter-info who param)])
        (define val
          ((parameter-info-converter info) init))
        (cons (parameter-info-key info)
          (if (parameter-info-thread-local? info)
            (make-thread-local val #t)
            val)))))

  (define-syntax/who parameterize
    (lambda (stx)
      (syntax-case stx ()
        [(_ ([p v] ...) e1 e2 ...)
          #`(with-continuation-mark
             (parameterization-continuation-mark-key)
             (parameterization-extend
              (current-parameterization)
              (list (parameter-key+value 'parameterize p v) ...))
             (letrec* ()
              e1
              e2
              ...))]
        [_
          (syntax-violation who "invalid syntax" stx)])))

  ;; The temporarily syntax

  (define-syntax/who temporarily
    (lambda (x)
      (syntax-case x ()
        [(_ () b1 b2 ...) #'(let () b1 b2 ...)]
        [(_ ([x e] ...) b1 b2 ...)
          (with-syntax ([(p ...) (generate-temporaries #'(x ...))]
                        [(y ...) (generate-temporaries #'(x ...))])
            #'(let ([p x] ... [y e] ...)
               (let ([swap (lambda ()
                            (let ([t (p)]) (p y) (set! y t))
                            ...)])
                (dynamic-wind swap (lambda () b1 b2 ...) swap))))])))

  ;; Current input/output/error port

  (define/who current-input-port
    (make-parameter
      (rnrs:current-input-port)
      (lambda (port)
        (unless (and (input-port? port)
                 (textual-port? port))
          (assertion-violation who "not a textual input port" port))
        port)))

  (define/who current-output-port
    (make-parameter
      (rnrs:current-output-port)
      (lambda (port)
        (unless (and (output-port? port)
                 (textual-port? port))
          (assertion-violation who "not a textual output port" port))
        port)))

  (define/who current-error-port
    (make-parameter
      (rnrs:current-error-port)
      (lambda (port)
        (unless (and (output-port? port)
                 (textual-port? port))
          (assertion-violation who "not a textual output port" port))
        port)))

  ;; We have to redefine all R6RS procedures that implicitly use the
  ;; current input/output/error port.  All these procedures are in
  ;; (rnrs io simple (6)).

  (define with-input-from-file
    (lambda (filename thunk)
      (rnrs:with-input-from-file
        filename
        (lambda ()
          (parameterize ([current-input-port (rnrs:current-input-port)])
            (thunk))))))

  (define with-output-to-file
    (lambda (filename thunk)
      (rnrs:with-output-to-file
        filename
        (lambda ()
          (parameterize ([current-output-port (rnrs:current-output-port)])
            (thunk))))))

  (define read-char
    (case-lambda
      [()
        (rnrs:read-char (current-input-port))]
      [(port)
        (rnrs:read-char port)]))

  (define peek-char
    (case-lambda
      [()
        (rnrs:peek-char (current-input-port))]
      [(port)
        (rnrs:peek-char port)]))

  (define read
    (case-lambda
      [()
        (rnrs:read (current-input-port))]
      [(port)
        (rnrs:read port)]))

  (define write-char
    (case-lambda
      [(char)
        (rnrs:write-char char (current-output-port))]
      [(char port)
        (rnrs:write-char char port)]))

  (define newline
    (case-lambda
      [()
        (rnrs:newline (current-output-port))]
      [(port)
        (rnrs:newline port)]))

  (define display
    (case-lambda
      [(obj)
        (rnrs:display obj (current-output-port))]
      [(obj port)
        (rnrs:display obj port)]))

  (define write
    (case-lambda
      [(obj)
        (rnrs:write obj (current-output-port))]
      [(obj port)
        (rnrs:write obj port)]))

  ;; Exceptions

  (define handler-stack-continuation-mark-key
    (let ([mark-key (make-continuation-mark-key 'exception-handler)])
      (lambda ()
        mark-key)))

  (define exception-handler-stack
    (lambda ()
      (marks-ref (current-marks) (handler-stack-continuation-mark-key))))

  (define current-exception-handler
    (lambda ()
      (car (exception-handler-stack))))

  (define/who with-exception-handler
    (lambda (handler thunk)
      (unless (procedure? handler)
        (assertion-violation who "not a procedure" handler))
      (unless (procedure? thunk)
        (assertion-violation who "not a procedure" thunk))
      (with-continuation-mark (handler-stack-continuation-mark-key)
        (cons handler (exception-handler-stack))
        (thunk))))

  (define-syntax/who guard
    (lambda (stx)
      (syntax-case stx ()
        [(_ (id c1 c2 ...) e1 e2 ...)
          (identifier? #'id)
          #`(call-with-current-continuation
             (lambda (guard-k)
              (with-exception-handler
               (lambda (c)
                (call-with-current-continuation
                 (lambda (handler-k)
                  (call-in-continuation guard-k
                   (lambda ()
                    (let ([id c])
                     #,(let f ([c1 #'c1] [c2* #'(c2 ...)])
                        (syntax-case c2* ()
                          [()
                            (with-syntax
                              ([rest
                                  #'(call-in-continuation handler-k
                                     (lambda ()
                                      (raise-continuable c)))])
                              (syntax-case c1 (else =>)
                                [(else e1 e2 ...)
                                  #'(begin e1 e2 ...)]
                                [(e0) #'e0]
                                [(e0 => e1)
                                  #'(let ([t e0]) (if t (e1 t) rest))]
                                [(e0 e1 e2 ...)
                                  #'(if e0
                                     (begin e1 e2 ...)
                                     rest)]))]
                          [(c2 c3 ...)
                            (with-syntax ([rest (f #'c2 #'(c3 ...))])
                              (syntax-case c1 (=>)
                                [(e0) #'(let ([t e0]) (if t t rest))]
                                [(e0 => e1)
                                  #'(let ([t e0]) (if t (e1 t) rest))]
                                [(e0 e1 e2 ...)
                                  #'(if e0
                                     (begin e1 e2 ...)
                                     rest)]))]))))))))
               (lambda ()
                e1
                e2
                ...))))]
        [_
          (syntax-violation who "invalid syntax" stx)])))

  (define-record-type thread-local
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable default) inheritable?)
    (protocol
      (lambda (new)
        (define make-thread-local
          (case-lambda
            [(default)
              (make-thread-local default #f)]
            [(default inheritable?)
              (new default inheritable?)]))
        make-thread-local)))

  (define make-storage
    (case-lambda
      [()
        (make-ephemeron-eq-hashtable)]
      [(storage)
        (let ([new-storage (make-storage)])
          (let-values ([(tls vals)
                         (hashtable-entries storage)])
            (vector-for-each
              (lambda (tl val)
                (when (thread-local-inheritable? tl)
                  (hashtable-set! new-storage tl val)))
              tls
              vals))
          new-storage)]))

  (define/who tlref
    (lambda (tl)
      (unless (thread-local? tl)
        (assertion-violation who "not a thread local" tl))
      (hashtable-ref (current-thread-storage) tl (thread-local-default tl))))

  (define/who tlset!
    (lambda (tl obj)
      (unless (thread-local? tl)
        (assertion-violation who "not a thread local" tl))
      (hashtable-set! (current-thread-storage) tl obj)))

  (define current-thread-storage
    (lambda ()
      (thread-storage (current-thread))))

  ;; Threads

  (define-enumeration thread-state
    (new runnable terminated)
    thread-state-set)

  (define current-thread
    (lambda ()
      (dynamic-environment-thread (%current-dynamic-environment))))

  (define-record-type thread-impl
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable thunk)
      (mutable end-continuation)
      (mutable %thread)
      name
      (mutable current-state)
      (mutable end-result)
      (mutable end-exception)
      (mutable %mutex)
      (mutable %condition-variable)
      storage))

  (define-record-type (thread make-thread maybe-thread?)
    (nongenerative thread-9a516626-3b09-42eb-8160-e6ff1984f1d7)
    (fields %impl)
    (protocol
      (lambda (p)
        (define/who make-thread
          (case-lambda
            [(thunk) (make-thread thunk #f)]
            [(thunk name)
              (unless (procedure? thunk)
                (assertion-violation who "not a procedure" thunk))
              (let ([thread
                      (p
                        (make-thread-impl #f #f #f name (thread-state new) '() #f (make-%mutex) (make-%condition-variable) (make-storage (current-thread-storage))))])
                (thread-thunk-set! thread (make-thread-thunk thread (current-parameterization) thunk))
                thread)]))
        make-thread)))

  (define make-primordial-thread
    (let ([p (record-constructor
              (make-record-constructor-descriptor (record-type-descriptor thread) #f #f))])
      (lambda ()
        (p (make-thread-impl
            #f
            #f
            (%current-thread)
            'primordial
            (thread-state runnable)
            '()
            #f
            (make-%mutex)
            (make-%condition-variable)
            (make-storage))))))

  (define thread?
    (lambda (obj)
      (and (maybe-thread? obj)
        (thread-impl? (thread-%impl obj)))))

  (define thread-thunk
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-thunk (thread-%impl thread))))

  (define thread-thunk-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-thunk-set! (thread-%impl thread) obj)))

  (define thread-end-continuation
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-end-continuation (thread-%impl thread))))

  (define thread-end-continuation-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-end-continuation-set! (thread-%impl thread) obj)))

  (define thread-%thread
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-%thread (thread-%impl thread))))

  (define thread-%thread-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-%thread-set! (thread-%impl thread) obj)))

  (define thread-name
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-name (thread-%impl thread))))

  (define thread-current-state
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-current-state (thread-%impl thread))))

  (define thread-current-state-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-current-state-set! (thread-%impl thread) obj)))

  (define thread-end-result
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-end-result (thread-%impl thread))))

  (define thread-end-result-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-end-result-set! (thread-%impl thread) obj)))

  (define thread-end-exception
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-end-exception (thread-%impl thread))))

  (define thread-end-exception-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-end-exception-set! (thread-%impl thread) obj)))

  (define thread-%mutex
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-%mutex (thread-%impl thread))))

  (define thread-%mutex-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-%mutex-set! (thread-%impl thread) obj)))

  (define thread-%condition-variable
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-%condition-variable (thread-%impl thread))))

  (define thread-%condition-variable-set!
    (lambda (thread obj)
      (assert (thread? thread))
      (thread-impl-%condition-variable-set! (thread-%impl thread) obj)))

  (define thread-storage
    (lambda (thread)
      (assert (thread? thread))
      (thread-impl-storage (thread-%impl thread))))

  (define make-thread-thunk
    (lambda (thread ps thunk)
      (lambda ()
        (let ([mtx (thread-%mutex thread)]
              [cv (thread-%condition-variable thread)])
          (%call-with-current-continuation
            (lambda (end-k)
              (%mutex-lock! mtx)
              (thread-end-continuation-set! thread end-k)
              (%condition-variable-broadcast! cv)
              (%mutex-unlock! mtx)
              (call-with-values
                (lambda ()
                  (%call-with-current-continuation
                    (lambda (k)
                      (%current-dynamic-environment
                        (make-initial-dynamic-environment
                          k
                          ps
                          failure-handler
                          thread))
                      (abort thunk))))
                success-handler)))
          (assert (eqv? (current-thread) thread))
          (thread-current-state-set! thread (thread-state terminated))
          (%mutex-unlock! mtx)))))

  (define/who thread-start!
    (lambda (thread)
      (unless (thread? thread)
        (assertion-violation who "not a thread" thread))
      (unless (symbol=? (thread-current-state thread)
               (thread-state new))
        (error who "thread already started" thread))
      (let ([mtx (thread-%mutex thread)]
            [cv (thread-%condition-variable thread)])
        (%mutex-lock! mtx)
        (let ([t (%thread-start! (thread-thunk thread))])
          (thread-thunk-set! thread #f)
          (thread-%thread-set! thread t)
          (thread-current-state-set! thread (thread-state runnable))
          (%mutex-unlock! mtx cv)
          thread))))

  (define thread-yield!
    (lambda ()
      (%thread-yield!)))

  ;; TODO: Schedule exit before terminated thread becomes unblocked.

  (define/who thread-schedule-terminate!
    (lambda (thread)
      (unless (thread? thread)
        (assertion-violation who "not a thread" thread))
      ;; We have to start a helper thread to be able to do some
      ;; clean-up as we may terminate the current thread.
      (let ([helper-thread
              (%thread-start!
                (lambda ()
                  (let ([mtx (thread-%mutex thread)]
                        [cv (thread-%condition-variable thread)])
                    (%mutex-lock! mtx)
                    (let ([s (thread-current-state thread)])
                      (unless (symbol=? s (thread-state terminated))
                        (thread-current-state-set! thread (thread-state terminated))
                        (thread-end-exception-set! thread (make-thread-already-terminated-condition))
                        (if (symbol=? s (thread-state new))
                          (%condition-variable-broadcast! cv)
                          (%thread-terminate! (thread-%thread thread))))
                      (%mutex-unlock! mtx)))))])
        (when (eqv? thread (current-thread))
          (%thread-join! thread (current-thread))))))

  (define/who thread-terminate!
    (lambda (thread)
      (unless (thread? thread)
        (assertion-violation who "not a thread" thread))
      (thread-schedule-terminate! thread)
      (with-exception-handler
        (lambda (exc)
          (if (thread-condition? exc)
            (values)
            (raise exc)))
        (lambda ()
          (thread-join! thread)))))

  (define/who thread-join!
    (lambda (thread)
      (unless (thread? thread)
        (assertion-violation who "not a thread" thread))
      (let ([mtx (thread-%mutex thread)]
            [cv (thread-%condition-variable thread)])
        (%mutex-lock! mtx)
        (let f ()
          (when (symbol=? (thread-current-state thread) (thread-state new))
            (%mutex-unlock! mtx cv)
            (%mutex-lock! mtx)
            (f)))
        (%mutex-unlock! mtx)
        (%thread-join! (thread-%thread thread))
        (if (thread-end-exception thread)
          (raise-continuable (thread-end-exception thread))
          (apply values (thread-end-result thread))))))

  ;; TODO: Implement current-interrupt-level.
  ;; TODO: Implement mutexes and condition-variables.
  ;; TODO: Handle blocked threads.
  (define/who thread-interrupt!
    (lambda (thread thunk)
      (unless (thread? thread)
        (assertion-violation who "not a thread" thread))
      (unless (procedure? thunk)
        (assertion-violation who "not a thunk" thunk))
      (%thread-interrupt! (thread-%thread thread) thunk)))

  ;; Promises

  (define force-continuation-mark-key
    (let ([mark-key (make-continuation-mark-key 'force)])
      (lambda ()
        mark-key)))

  (define-record-type (promise %make-promise promise?)
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable content promise-ref promise-set!)
      %mutex)
    (protocol
      (lambda (p)
        (lambda (done? thunk)
          (p (make-promise-content done? thunk)
            (make-%mutex))))))

  (define-record-type promise-content
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields (mutable done?) (mutable thunk)))

  (define promise-thunk
    (lambda (p)
      (promise-content-thunk (promise-ref p))))

  (define promise-done?
    (lambda (p)
      (promise-content-done? (promise-ref p))))

  (define promise-done?-set!
    (lambda (p done?)
      (promise-content-done?-set! (promise-ref p) done?)))

  (define promise-thunk-set!
    (lambda (p thunk)
      (promise-content-thunk-set! (promise-ref p) thunk)))

  (define promise-lock!
    (lambda (p)
      (%mutex-lock! (promise-%mutex p))))

  (define promise-unlock!
    (lambda (p)
      (%mutex-unlock! (promise-%mutex p))))

  (define-syntax/who delay
    (lambda (stx)
      (syntax-case stx ()
        [(_ e1 e2 ...)
          #'(let ([ps (current-parameterization)])
             (%make-promise
              #f
              (lambda ()
               (call-with-parameterization ps
                (lambda () e1 e2 ...)))))]
        [_
          (syntax-violation who "invalid syntax" stx)])))

  (define make-promise
    (lambda obj*
      (%make-promise #t (lambda () (apply values obj*)))))

  (define call-in-initial-continuation
    (lambda (thunk)
      (let* ([saved-env (%current-dynamic-environment)]
             [thunk
               (%call-with-current-continuation
                 (lambda (end-k)
                   (thunkify
                     (%call-with-current-continuation
                       (lambda (k)
                         (%current-dynamic-environment
                           (make-initial-dynamic-environment
                             k
                             (current-parameterization)
                             (lambda (con)
                               (end-k (lambda ()
                                       (raise-continuable (make-uncaught-exception-condition con)))))
                             (current-thread)))
                         (abort thunk))))))])
        (%current-dynamic-environment saved-env)
        (thunk))))

  (define/who force
    (lambda (p)
      (unless (promise? p)
        (assertion-violation who "not a promise" p))
      (let f ()
        (if (promise-done? p)
          ((promise-thunk p))
          (call-with-immediate-continuation-mark (force-continuation-mark-key)
            (lambda (c)
              (if c
                (c p)
                (let* ([q #f]
                       [thunk
                         (guard (exc [else (lambda () (raise-continuable exc))])
                           (thunkify
                             (call-in-initial-continuation
                               (lambda ()
                                 (with-continuation-mark (force-continuation-mark-key)
                                   (lambda (p) (set! q p))
                                   ((promise-thunk p)))))))])
                  (promise-lock! p)
                  (cond
                    [(promise-done? p)
                      (promise-unlock! p)
                      ((promise-thunk p))]
                    [q
                      (promise-thunk-set! p (promise-thunk q))
                      (promise-done?-set! p (promise-done? q))
                      (promise-set! q (promise-ref p))
                      (promise-unlock! p)
                      (f)]
                    [else
                      (promise-done?-set! p #t)
                      (promise-thunk-set! p thunk)
                      (promise-unlock! p)
                      (thunk)]))))))))))
