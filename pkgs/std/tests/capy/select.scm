(import (rnrs)
        (srfi 64)
        (capy channel)
        (capy select)
        (capy timer)
        (capy sync)
        (capy concurrency))

(test-begin "capy select")

(define (spawn n body)
  (let loop ([i 0] [acc '()])
    (if (= i n)
      (reverse acc)
      (loop (+ i 1) (cons (go (lambda () (body i))) acc)))))

(define (join-all threads)
  (for-each thread-join threads))

(test-group "ready cases"
  (test-equal "recv => receives the value and ok flag"
    (let ([ch (make-channel 1)])
      (channel-send ch 'hi)
      (select ((recv ch) => (lambda (v ok) (list v ok)))))
    '(hi #t))

  (test-equal "recv body form runs its body"
    (let ([ch (make-channel 1)])
      (channel-send ch 'hi)
      (select ((recv ch) 'got)))
    'got)

  (test-equal "closed channel is ready with (#f #f)"
    (let ([ch (make-channel 0)])
      (channel-close ch)
      (select ((recv ch) => (lambda (v ok) (list v ok)))))
    '(#f #f))

  (test-equal "send case sends and runs body"
    (let ([ch (make-channel 1)])
      (let ([r (select ((send ch 'out) 'sent))])
        (list r (channel-recv ch))))
    '(sent out))

  (test-equal "send to a waiting receiver"
    (let* ([ch (make-channel 0)]
           [receiver (go (lambda () (channel-recv ch)))])
      (let loop ([tries 0])
        (let ([r (select ((send ch 'x) 'sent) (default 'busy))])
          (cond
            [(eq? r 'sent) (thread-join receiver)]
            [(> tries 100000) 'gave-up]
            [else (yield) (loop (+ tries 1))]))))
    'x)

  (test-equal "send on a closed channel raises"
    (guard (c [(closed-channel-violation? c) 'raised])
      (let ([ch (make-channel 1)])
        (channel-close ch)
        (select ((send ch 1) 'sent))))
    'raised))

(test-group "default"
  (test-equal "default runs when nothing is ready"
    (select ((recv (make-channel 0)) 'recv) (default 'default))
    'default)

  (test-equal "default is not taken when a case is ready"
    (let ([ch (make-channel 1)])
      (channel-send ch 1)
      (select (default 'default) ((recv ch) 'recv)))
    'recv)

  (test-equal "select with only default"
    (select (default 'only))
    'only))

(test-group "blocking"
  (test-equal "blocks until a sender arrives"
    (let ([ch (make-channel 0)])
      (go (lambda () (sleep 0.02) (channel-send ch 'late)))
      (select ((recv ch) => (lambda (v ok) v))))
    'late)

  (test-equal "picks the ready channel among several"
    (let ([a (make-channel 0)] [b (make-channel 0)])
      (go (lambda () (sleep 0.02) (channel-send b 'from-b)))
      (select ((recv a) 'a) ((recv b) => (lambda (v ok) v))))
    'from-b)

  (test-equal "a select that blocks on two channels completes on one only"
    (let ([a (make-channel 0)] [b (make-channel 0)])
      (go (lambda () (sleep 0.01) (channel-send a 1)))
      (go (lambda () (sleep 0.01) (channel-send b 2)))
      (let* ([first (select ((recv a) => (lambda (v ok) v))
                            ((recv b) => (lambda (v ok) v)))]
             [rest (if (= first 1)
                     (channel-recv b)
                     (channel-recv a))])
        (and (memv first '(1 2)) (memv rest '(1 2)) (not (= first rest)) #t)))
    #t)

  (test-equal "a blocked select is not woken by a stale case"
    ;; The select below stops waiting on `a` when `b` fires. A later send on
    ;; `a` must not be swallowed by the finished select.
    (let ([a (make-channel 0)] [b (make-channel 0)])
      (go (lambda () (sleep 0.01) (channel-send b 'b)))
      (select ((recv a) 'a) ((recv b) 'b))
      (go (lambda () (channel-send a 'a-later)))
      (channel-recv a))
    'a-later)

  (test-equal "blocked send case is completed by a receiver"
    (let ([ch (make-channel 0)])
      (go (lambda () (sleep 0.01) (channel-recv ch)))
      (select ((send ch 'v) 'sent)))
    'sent))

(test-group "timeout"
  (test-equal "timeout fires when nothing is ready"
    (select ((recv (make-channel 0)) 'recv) ((timeout 0.02) 'timeout))
    'timeout)

  (test-equal "timeout is not taken when a case is ready first"
    (let ([ch (make-channel 0)])
      (go (lambda () (channel-send ch 'v)))
      (select ((recv ch) => (lambda (v ok) v)) ((timeout 5) 'timeout)))
    'v)

  (test-equal "timeout clause body runs when the time is up"
    (select ((timeout 0.0) (quote expired)))
    (quote expired)))

(test-group "fairness and stress"
  (test-equal "random choice reaches both ready cases"
    (let ([a (make-channel 1)] [b (make-channel 1)] [picks (make-atomic '())])
      (do ([i 0 (+ i 1)]) ((= i 200))
        (channel-send a 'a)
        (channel-send b 'b)
        (let ([r (select ((recv a) 'a) ((recv b) 'b))])
          (atomic-set! picks (cons r (atomic-ref picks))))
        ;; drain whichever one was not chosen
        (select (default #f) ((recv a) #f) ((recv b) #f)))
      (let ([ps (atomic-ref picks)])
        (and (memq 'a ps) (memq 'b ps) #t)))
    #t)

  (test-equal "select racing a sender and a closer always completes once"
    (let ([wins (make-atomic 0)])
      (do ([i 0 (+ i 1)]) ((= i 500))
        (let* ([a (make-channel 0)]
               [b (make-channel 0)]
               [t1 (go
                     (lambda () (guard (e [#t 'closed]) (channel-send a 'x))))]
               [t2 (go (lambda () (channel-close b)))])
          (select ((recv a) 'a) ((recv b) 'b))
          (channel-close a)
          (thread-join t1)
          (thread-join t2)
          (atomic-add! wins 1)))
      (atomic-ref wins))
    500)

  (test-equal "many selectors and senders deliver every value once"
    (let* ([a (make-channel 0)]
           [b (make-channel 0)]
           [per 40]
           [senders (spawn 10
                      (lambda (i)
                        (do ([k 0 (+ k 1)]) ((= k per))
                          (channel-send (if (even? k) a b) (+ (* i per) k)))))]
           [sum (make-atomic 0)]
           [count (make-atomic 0)]
           [receivers (spawn 4
                        (lambda (_)
                          (let loop ()
                            (when (< (atomic-ref count) (* 10 per))
                              (select
                                ((recv a) => (lambda (v ok)
                                               (atomic-add! sum v)
                                               (atomic-add! count 1)))
                                ((recv b) => (lambda (v ok)
                                               (atomic-add! sum v)
                                               (atomic-add! count 1)))
                                ((timeout 0.5) #f))
                              (loop)))))])
      (join-all senders)
      (join-all receivers)
      (list (atomic-ref count) (atomic-ref sum)))
    (let ([n (* 10 40)])
      (list n (/ (* n (- n 1)) 2)))))

(test-end "capy select")
