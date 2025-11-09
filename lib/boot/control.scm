
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
    (let ((tail (common-tail new ($winders))))
      (let loop ((rec ($winders)))
        (cond ((not (eq? rec tail)) ($winders (cdr rec)) ((cdar rec)) (loop (cdr rec)))))
      (let loop ((rec new))
        (cond ((not (eq? rec tail)) (loop (cdr rec)) ((caar rec)) ($winders rec)))))
    (apply cont args))

(define (dynamic-wind in body out)
  (in)
  ($winders (cons (cons in out) ($winders)))
  (call-with-values 
    body
    (lambda ans 
      ($winders (cdr ($winders)))
      (out)
      (apply values ans))))



(define (call/cc f)
  (define (attach-cont-props winders attachments k)
    (set-procedure-property! k 'continuation
      (tuple winders attachments))
    k)

  (define saved-record ($winders))
  (define marks (current-continuation-marks))
  (.call/cc-unsafe 
    (lambda (k)
      (f (attach-cont-props 
        saved-record
        marks
        (lambda args 
          ($set-attachments! marks)
          (perform-dynamic-wind saved-record k args)))))))

(define call-with-current-continuation call/cc)

(define (continuation? x)
  (and (procedure? x)
       (if (procedure-property x 'continuation)
         #t
         #f)))

(define (continuation-next-marks k)
  (unless (continuation? k)
    (assertion-violation 'continuation-marks "expected a continuation" k))
  (let ([props (procedure-property k 'continuation)])
    (tuple-ref props 1)))
  
(define (call-in-continuation c proc . args)
    (define (cont-attachments c)
        (tuple-ref (cdr (procedure-property c 'continuation)) 1))
    (unless (continuation? c)
        (error 'call-in-continuation "not a continuation" c))

    (cond 
        [(null? args)
            ($set-attachments! (cont-attachments c))
            (receive vals (proc)
                (apply c vals))]
        [else 
            (let ([set proc]
                  [proc (car args)])
                (unless (continuation-marks? set)
                    (error 'call-in-continuation 
                        "expected continuation-marks object" set))
                (let* ([markss ($continuation-marks-markss set)]
                       [c-marks (cont-attachments c)])
                    (unless (or (eq? markss c-marks)
                                (and (pair? markss) (eq? (cdr markss) c-markss)))
                        (error 'call-in-continuation "mark set is not an extension of current marks" set c)))
                    ($set-attachments! markss)
                    (receive vals (proc)
                        (apply c vals))) ]))

(define $null-continuation 
  (let ([k (lambda () (exit 1))])
    (set-procedure-property! k 'continuation
      (tuple '() '()))
    k 
  ))


(define (unhandled-exception-error val)
  (.return-error val))


(define *basic-exception-handlers* (list unhandled-exception-error))

(define *current-exception-handlers*
  (let ([f (make-thread-local-fluid *basic-exception-handlers*)])
    (lambda args
      (if (null? args)
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))


(define (raise obj)
  (.raise obj))

(define (raise-continuable obj)
  (let ((handlers (*current-exception-handlers*)))
    (with-exception-handler (cadr handlers)
      (lambda () ((car handlers) obj)))))

(define (with-exception-handler handler thunk)
  (let* ([previous-handlers (*current-exception-handlers*)]
         [new-handlers (if handler
                          (cons handler previous-handlers)
                          previous-handlers)])
    (dynamic-wind
      (lambda ()
        (*current-exception-handlers* new-handlers))
      (lambda() (.with-handler handler thunk))
      (lambda ()
        (*current-exception-handlers* previous-handlers)))))