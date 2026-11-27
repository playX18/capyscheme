;;; The expanders of SRFI 197. They run at expansion time, so they live in their own library
;;; that (srfi 197) imports.
(library (srfi 197 helpers)
  (export expand-chain expand-chain-and expand-chain-when expand-chain-lambda expand-nest)
  (import (rnrs))

  (define (syntax->list stx)
    (syntax-case stx ()
      [() '()]
      [(x . rest) (cons #'x (syntax->list #'rest))]))

  (define (same-id? a b)
    (and (identifier? a) (free-identifier=? a b)))

  ;; A step split into its datums with each placeholder replaced by a fresh identifier.
  ;; Returns (values datums temps rest-temp) where TEMPS are the fresh identifiers in order and
  ;; REST-TEMP is the identifier of a trailing `placeholder ellipsis`, or #f.
  (define (split-step who step p e)
    (let ([items (syntax-case step ()
                   [(x ...) (syntax->list #'(x ...))]
                   [_ (syntax-violation who "a step must be a list" step)])])
      (let loop ([items items] [out '()] [temps '()])
        (cond
          [(null? items) (values (reverse out) (reverse temps) #f)]
          [(and (same-id? (car items) p)
                (pair? (cdr items))
                (same-id? (cadr items) e))
           (unless (null? (cddr items))
             (syntax-violation who "a placeholder with an ellipsis must end the step" step))
           (let ([rest (car (generate-temporaries '(rest)))])
             (values (reverse out) (reverse temps) rest))]
          [(same-id? (car items) p)
           (let ([t (car (generate-temporaries '(x)))])
             (loop (cdr items) (cons t out) (cons t temps)))]
          [else (loop (cdr items) (cons (car items) out) temps)]))))

  ;; One step applied to the value(s) of PREV.
  (define (apply-step who prev step p e)
    (let-values ([(datums temps rest) (split-step who step p e)])
      (cond
        [rest
         (with-syntax ([(d ...) datums] [(t ...) temps] [r rest] [prev prev])
           #'(call-with-values (lambda () prev) (lambda (t ... . r) (apply d ... r))))]
        [(null? temps)
         ;; No placeholder: the value is the last argument.
         (with-syntax ([(d ...) datums] [prev prev])
           #'(let ([x prev]) (d ... x)))]
        [(null? (cdr temps))
         (with-syntax ([(d ...) datums] [t (car temps)] [prev prev])
           #'(let ([t prev]) (d ...)))]
        [else
         (with-syntax ([(d ...) datums] [(t ...) temps] [prev prev])
           #'(call-with-values (lambda () prev) (lambda (t ...) (d ...))))])))

  (define (expand-chain init p e steps)
    (let loop ([acc init] [steps (syntax->list steps)])
      (if (null? steps)
        acc
        (loop (apply-step 'chain acc (car steps) p e) (cdr steps)))))

  ;; A step of `chain-and`, `chain-when` and `nest`: exactly one value in one placeholder (or
  ;; none, for the chains, meaning the last argument).
  (define (single-step who step p var allow-none?)
    (let-values ([(datums temps rest) (split-step who step p (car (generate-temporaries '(no))))])
      (cond
        [(and (null? temps) allow-none?)
         (with-syntax ([(d ...) datums] [v var]) #'(d ... v))]
        [(and (pair? temps) (null? (cdr temps)))
         (with-syntax ([(d ...) datums] [t (car temps)] [v var])
           #'(let ([t v]) (d ...)))]
        [else (syntax-violation who "a step must have exactly one placeholder" step)])))

  (define (expand-chain-and init p steps)
    (let loop ([steps (reverse (syntax->list steps))] [body #f])
      (if (null? steps)
        (with-syntax ([init init] [body (or body #'x)])
          #'(let ([x init]) (and x body)))
        (let ([step (car steps)])
          (with-syntax ([next (single-step 'chain-and step p #'x #t)])
            (loop (cdr steps)
                  (if body
                    (with-syntax ([body body])
                      #'(let ([x next]) (and x body)))
                    #'next)))))))

  (define (expand-chain-when init p steps)
    (let loop ([acc init] [steps (syntax->list steps)])
      (if (null? steps)
        acc
        (syntax-case (car steps) ()
          [(guard step)
           (with-syntax ([acc acc]
                         [next (single-step 'chain-when #'step p #'x #t)])
             (loop #'(let ([x acc]) (if guard next x)) (cdr steps)))]
          [_ (syntax-violation 'chain-when "a step must be (guard step)" (car steps))]))))

  (define (expand-chain-lambda p e step0 steps)
    (let-values ([(datums temps rest) (split-step 'chain-lambda step0 p e)])
      (let ([temps (if (and (null? temps) (not rest))
                     (generate-temporaries '(x))
                     temps)]
            [datums (if (and (null? temps) (not rest))
                      #f
                      datums)])
        (with-syntax ([(t ...) temps])
          (let* ([first (cond
                          [rest (with-syntax ([(d ...) datums] [r rest])
                                  #'(apply d ... r))]
                          [datums (with-syntax ([(d ...) datums]) #'(d ...))]
                          [else (with-syntax ([(d ...) (syntax->list step0)] [x (car temps)])
                                  #'(d ... x))])]
                 [body (expand-chain first p e steps)])
            (if rest
              (with-syntax ([r rest] [body body]) #'(lambda (t ... . r) body))
              (with-syntax ([body body]) #'(lambda (t ...) body))))))))

  ;; STEPS from the innermost out; each holds the previous expression in its placeholder.
  (define (expand-nest p steps init)
    (let loop ([acc init] [steps (syntax->list steps)])
      (if (null? steps)
        acc
        (let-values ([(datums temps rest)
                      (split-step 'nest (car steps) p (car (generate-temporaries '(no))))])
          (unless (and (pair? temps) (null? (cdr temps)))
            (syntax-violation 'nest "a step must have exactly one placeholder" (car steps)))
          (loop (map (lambda (d) (if (eq? d (car temps)) acc d)) datums)
                (cdr steps)))))))
