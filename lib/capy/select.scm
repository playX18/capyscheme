(library (capy select)
  (export select channel-recv/timeout)
  (import (rnrs)
    (capy channel)
    (capy timer))

  ;; Clauses (checked in this order; a `default` clause may appear anywhere):
  ;;   ((recv ch) => proc)     proc is called with the received value and #t
  ;;                           (or #f and #f once ch is closed and drained)
  ;;   ((recv ch) body ...)    like above, with the values ignored
  ;;   ((send ch v) body ...)  runs body after v is sent. Sending on a
  ;;                           channel that is closed raises a violation.
  ;;   ((timeout secs) body ...) runs body if nothing else is ready within
  ;;                           secs seconds
  ;;   (default body ...)      runs body if no other clause is ready now
  ;;
  ;; Channel and value expressions are evaluated once, in source order, when
  ;; the select is entered. When several clauses are ready one is picked at
  ;; random. If none is ready and there is no default, the thread blocks
  ;; until one becomes ready (no polling).

  ;; Called by the expansion. `cases` holds #(kind channel value) vectors. For
  ;; a timeout case the value slot holds its timer, which is stopped once the
  ;; select is over so that its sleeping thread does not outlive it.
  (define (run-select cases handlers default?)
    (let-values ([(i v ok?) (channel-select cases default?)])
      (let loop ([k 0])
        (when (< k (vector-length cases))
          (let ([c (vector-ref cases k)])
            (when (and (eq? (vector-ref c 0) 'recv) (timer? (vector-ref c 2)))
              (timer-stop! (vector-ref c 2))))
          (loop (+ k 1))))
      ((vector-ref handlers (if (= i -1) (vector-length cases) i)) v ok?)))

  (define-syntax select
    (lambda (stx)
      (define (syntax-list s)
        (syntax-case s ()
          [(a . d) (cons #'a (syntax-list #'d))]
          [() '()]))
      (define (parse clause)
        (syntax-case clause (recv send timeout default =>)
          [((recv ch) => proc)
            (list 'case
              #'(vector 'recv ch #f)
              #'(lambda (v ok) (proc v ok)))]
          [((recv ch) body ...)
            (list 'case
              #'(vector 'recv ch #f)
              #'(lambda (v ok) body ...))]
          [((send ch val) body ...)
            (list 'case
              #'(vector 'send ch val)
              #'(lambda (v ok) body ...))]
          [((timeout secs) body ...)
            (list 'case
              #'(let ([t (make-timer secs)])
                  (vector 'recv (timer-channel t) t))
              #'(lambda (v ok) body ...))]
          [(default body ...)
            (list 'default #f #'(lambda (v ok) body ...))]
          [_ (syntax-violation 'select "invalid select clause" clause)]))
      (syntax-case stx ()
        [(_ clause ...)
          (let* ([parsed (map parse (syntax-list #'(clause ...)))]
                 [cases (filter (lambda (p) (eq? (car p) 'case)) parsed)]
                 [defaults (filter (lambda (p) (eq? (car p) 'default)) parsed)]
                 [case-exprs (map cadr cases)]
                 [handler-exprs (append (map caddr cases) (map caddr defaults))]
                 [has-default (datum->syntax stx (pair? defaults))])
            (with-syntax ([(ce ...) case-exprs]
                          [(he ...) handler-exprs]
                          [df has-default])
              #'(run-select (vector ce ...) (vector he ...) df)))])))
  (define (channel-recv/timeout ch secs)
    "Receive from `ch` within `secs` seconds. Returns three values:
    the value, #t and #f on success; #f, #f and #f when `ch` is closed
    and drained; #f, #f and #t on timeout. The third value tells a
    timeout apart from a closed channel, which `channel-recv` cannot."
    (select
      ((recv ch) => (lambda (v ok) (values v ok #f)))
      ((timeout secs) (values #f #f #t))))

  )
