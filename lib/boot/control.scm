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

(define (perform-dynamic-wind new cont args)
  (define common-tail
    (lambda (x y)
      (let ((nx (length x)) (ny (length y)))
        (do
          ((x (if (> nx ny) (list-tail x (- nx ny)) x) (cdr x))
            (y (if (> ny nx) (list-tail y (- ny nx)) y) (cdr y)))
          ((eq? x y) x)))))
  (if (eq? new ($winders))
    (apply cont args)
    (begin
      (let ((tail (common-tail new ($winders))))
        (let loop ((rec ($winders)))
          (cond ((not (eq? rec tail)) ($winders (cdr rec)) ((cdar rec)) (loop (cdr rec)))))
        (let loop ((rec new))
          (cond ((not (eq? rec tail)) (loop (cdr rec)) ((caar rec)) ($winders rec)))))
      (apply cont args))))

(define (dynamic-wind in body out)
  (in)
  ($winders (cons (cons in out) ($winders)))
  (call-with-values
    body
    (lambda ans
      ($winders (cdr ($winders)))
      (out)
      (apply values ans))))

;; Native reified continuation: winders + raw marks live in free vars.
(define (call/cc f)
  (%call/cc f))

(define call-with-current-continuation call/cc)

;; Escape-only contract (same implementation under CPS; capture is already O(1)).
(define call/1cc call/cc)

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
        ($set-attachments! set)
        (receive vals (proc)
          (apply c vals)))]))

(define $null-continuation (%null-continuation))
