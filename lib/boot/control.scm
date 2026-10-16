(define (current-continuation-marks)
  (current-continuation-marks))

(define current-exception-handler
  (let ([f (make-thread-local-fluid #f)])
    (lambda args
      (if (null? args)
        (fluid-ref f)
        (let ([old (fluid-ref f)])
          (fluid-set! f (car args))
          old)))))

(define parent-exception-handler
  (let ([f (make-thread-local-fluid #f)])
    (lambda args
      (if (null? args)
        (fluid-ref f)
        (let ([old (fluid-ref f)])
          (fluid-set! f (car args))
          old)))))

(define current-dynamic-wind-record
  (let ([f (make-thread-local-fluid '())])
    (lambda args
      (if (null? args)
        (fluid-ref f)
        (let ([old (fluid-ref f)])
          (fluid-set! f (car args))
          old)))))

(define (dynamic-wind in body out)
  (unless (procedure? in)
    (assertion-violation 'dynamic-wind "not a procedure" in))
  (unless (procedure? body)
    (assertion-violation 'dynamic-wind "not a procedure" body))
  (unless (procedure? out)
    (assertion-violation 'dynamic-wind "not a procedure" out))
  (let ((old-winders ($winders)))
    (in)
    ;; A winder is `(in out marks)`; the marks snapshot is what the
    ;; in/out thunks observe when this winder is crossed
    ($winders (cons (list in out ($continuation-marks-markss (current-continuation-marks)))
                    old-winders))
    (call-with-values
      body
      (lambda ans
        ($winders old-winders)
        (out)
        (apply values ans)))))

;; Native reified continuation: winders + raw marks live in free vars.
(define (call/cc f)
  (%call/cc f))

(define call-with-current-continuation call/cc)

;; One-shot call/cc: the continuation traps if invoked more than once, and
;; must be invoked before returning past the capture.
(define (call/1cc f)
  (%call/1cc f))

(define (continuation? x)
  (%continuation? x))

(define (continuation-next-marks k)
  (unless (continuation? k)
    (assertion-violation 'continuation-marks "expected a continuation" k))
  (%continuation-next-marks k))

(define (call-in-continuation c proc . args)
  (unless (continuation? c)
    (error 'call-in-continuation "not a continuation" c))

  (cond
    [(null? args)
      (unless (procedure? proc)
        (error 'call-in-continuation "not a procedure" proc))
      ;; Wind to the target before running proc
      ($do-wind ($continuation-winders c))
      (%set-continuation-marks! c)
      (receive vals (proc)
        (apply c vals))]
    [else
      (let ([set proc]
            [proc (car args)])
        (unless (null? (cdr args))
          (error 'call-in-continuation "wrong number of arguments" (cons c (cons set args))))
        (unless (procedure? proc)
          (error 'call-in-continuation "not a procedure" proc))
        (unless (continuation-marks? set)
          (error 'call-in-continuation
            "expected continuation-marks object"
            set))
        ;; The marks must be the continuation's own marks or a one-frame
        ;; extension of them
        (let ([markss ($continuation-marks-markss set)]
              [c-markss ($continuation-marks-markss (%continuation-next-marks c))])
          (unless (or (eq? markss c-markss)
                      (and (pair? markss) (eq? (cdr markss) c-markss)))
            (error 'call-in-continuation
              "not an extension of the continuation's marks"
              set)))
        ($do-wind ($continuation-winders c))
        ($set-attachments! set)
        (receive vals (proc)
          (apply c vals)))]))

(define $null-continuation (%null-continuation))
