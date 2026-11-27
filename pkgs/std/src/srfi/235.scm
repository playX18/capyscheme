;;; SRFI 235: Combinators.
;;;
;;; The procedures are written with fixed-arity fast cases where the common call has one or
;;; two arguments, so the JIT sees a direct call instead of an `apply` of a rest list.
(library (srfi 235)
  (export
    constantly complement swap flip on-left on-right conjoin disjoin
    each-of all-of any-of on left-section right-section apply-chain
    arguments-drop arguments-drop-right arguments-take arguments-take-right
    group-by
    begin-procedure if-procedure when-procedure unless-procedure value-procedure
    case-procedure and-procedure eager-and-procedure or-procedure eager-or-procedure
    funcall-procedure loop-procedure while-procedure until-procedure
    always never boolean)
  (import (rnrs))

  (define constantly
    (case-lambda
      [(obj) (lambda _ obj)]
      [objs (lambda _ (apply values objs))]))

  (define (complement proc)
    (case-lambda
      [(x) (not (proc x))]
      [(x y) (not (proc x y))]
      [args (not (apply proc args))]))

  (define (swap proc)
    (case-lambda
      [(a b) (proc b a)]
      [(a b . rest) (apply proc b a rest)]))

  (define (flip proc)
    (case-lambda
      [() (proc)]
      [(a) (proc a)]
      [(a b) (proc b a)]
      [args (apply proc (reverse args))]))

  (define (on-left proc) (lambda (a b) (proc a)))
  (define (on-right proc) (lambda (a b) (proc b)))

  ;; True when every predicate is true of the arguments; the value of the last one, or #t.
  (define (conjoin . preds)
    (lambda args
      (let loop ([ps preds] [last #t])
        (if (null? ps)
          last
          (let ([v (apply (car ps) args)])
            (and v (loop (cdr ps) v)))))))

  (define (disjoin . preds)
    (lambda args
      (let loop ([ps preds])
        (and (pair? ps)
             (or (apply (car ps) args) (loop (cdr ps)))))))

  (define (each-of . procs)
    (lambda args
      (for-each (lambda (p) (apply p args)) procs)))

  (define (all-of pred)
    (lambda (lst)
      (let loop ([l lst] [last #t])
        (if (null? l)
          last
          (let ([v (pred (car l))])
            (and v (loop (cdr l) v)))))))

  (define (any-of pred)
    (lambda (lst)
      (let loop ([l lst])
        (and (pair? l) (or (pred (car l)) (loop (cdr l)))))))

  (define (on reducer mapper)
    (case-lambda
      [(a) (reducer (mapper a))]
      [(a b) (reducer (mapper a) (mapper b))]
      [args (apply reducer (map mapper args))]))

  (define (left-section proc . left)
    (lambda args (apply proc (append left args))))

  ;; The section arguments come after the call's arguments, in reverse order.
  (define (right-section proc . right)
    (let ([right (reverse right)])
      (lambda args (apply proc (append args right)))))

  ;; Composition, right to left: ((apply-chain f g) x) is (f (g x)), passing every value.
  (define (apply-chain . procs)
    (if (null? procs)
      values
      (let ([procs (reverse procs)])
        (lambda args
          (let loop ([ps (cdr procs)] [thunk (lambda () (apply (car procs) args))])
            (if (null? ps)
              (thunk)
              (let ([p (car ps)])
                (loop (cdr ps) (lambda () (call-with-values thunk p))))))))))

  (define (list-head lst k)
    (let loop ([l lst] [k k] [acc '()])
      (if (fx=? k 0) (reverse acc) (loop (cdr l) (fx- k 1) (cons (car l) acc)))))

  (define (arguments-drop proc n)
    (lambda args (apply proc (list-tail args n))))

  (define (arguments-drop-right proc n)
    (lambda args (apply proc (list-head args (- (length args) n)))))

  (define (arguments-take proc n)
    (lambda args (apply proc (list-head args n))))

  (define (arguments-take-right proc n)
    (lambda args (apply proc (list-tail args (- (length args) n)))))

  ;; Groups the elements of a list by key, in the order each key first appears.
  (define group-by
    (case-lambda
      [(key) (group-by key equal?)]
      [(key =)
       (lambda (lst)
         (let loop ([l lst] [groups '()])
           (if (null? l)
             (reverse (map (lambda (g) (reverse (cdr g))) groups))
             (let* ([x (car l)] [k (key x)])
               (let find ([gs groups])
                 (cond
                   [(null? gs) (loop (cdr l) (cons (list k x) groups))]
                   [(= k (car (car gs)))
                    (set-cdr! (car gs) (cons x (cdr (car gs))))
                    (loop (cdr l) groups)]
                   [else (find (cdr gs))]))))))]))

  (define (begin-procedure . thunks)
    (let loop ([ts thunks])
      (cond
        [(null? ts) (if #f #f)]
        [(null? (cdr ts)) ((car ts))]
        [else ((car ts)) (loop (cdr ts))])))

  (define (if-procedure value then-thunk else-thunk)
    (if value (then-thunk) (else-thunk)))

  (define (when-procedure value . thunks)
    (when value (for-each (lambda (t) (t)) thunks)))

  (define (unless-procedure value . thunks)
    (unless value (for-each (lambda (t) (t)) thunks)))

  (define (value-procedure value then-proc else-thunk)
    (if value (then-proc value) (else-thunk)))

  (define case-procedure
    (case-lambda
      [(value alist)
       (case-procedure value alist
         (lambda () (assertion-violation 'case-procedure "no clause matches" value)))]
      [(value alist else-thunk)
       (let ([entry (assv value alist)])
         (if entry ((cdr entry)) (else-thunk)))]))

  (define (and-procedure . thunks)
    (let loop ([ts thunks] [last #t])
      (if (null? ts)
        last
        (let ([v ((car ts))])
          (and v (loop (cdr ts) v))))))

  (define (eager-and-procedure . thunks)
    (let loop ([ts thunks] [result #t])
      (if (null? ts)
        result
        (let ([v ((car ts))])
          (loop (cdr ts) (and result v))))))

  (define (or-procedure . thunks)
    (let loop ([ts thunks])
      (and (pair? ts) (or ((car ts)) (loop (cdr ts))))))

  (define (eager-or-procedure . thunks)
    (let loop ([ts thunks] [result #f])
      (if (null? ts)
        result
        (let ([v ((car ts))])
          (loop (cdr ts) (or result v))))))

  (define (funcall-procedure thunk) (thunk))

  (define (loop-procedure thunk)
    (let loop () (thunk) (loop)))

  (define (while-procedure thunk)
    (let loop () (when (thunk) (loop))))

  (define (until-procedure thunk)
    (let loop () (unless (thunk) (loop))))

  (define (always . _) #t)
  (define (never . _) #f)
  (define (boolean obj) (if obj #t #f)))
