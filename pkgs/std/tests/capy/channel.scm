(import (rnrs)
        (srfi 64)
        (capy channel)
        (capy concurrency))

(test-begin "capy channel")

(define (closed-channel-error? thunk)
  (guard (c
          [(closed-channel-violation? c) #t]
          [else #f])
    (thunk)
    #f))

;; Start `n` threads running `(body i)`; returns the list of thread handles.
(define (spawn n body)
  (let loop ([i 0] [acc '()])
    (if (= i n)
      (reverse acc)
      (loop (+ i 1) (cons (go (lambda () (body i))) acc)))))

(define (join-all threads)
  (for-each thread-join threads))

(define (all-true? lst)
  (or (null? lst) (and (car lst) (all-true? (cdr lst)))))

(test-group "buffered"
  (test-equal "recv returns buffered values in send order"
    (let ([ch (make-channel 2)])
      (channel-send ch 'first)
      (channel-send ch 'second)
      (list (channel-recv ch)
            (channel-recv ch)))
    '(first second))

  (test-equal "closed channel drains buffered values before eof"
    (let ([ch (make-channel 2)])
      (channel-send ch 'first)
      (channel-send ch 'second)
      (channel-close ch)
      (list (channel-closed? ch)
            (channel-recv ch)
            (channel-recv ch)
            (channel-recv ch)))
    '(#t first second #f))

  (test-assert "send on closed channel raises closed-channel violation"
    (closed-channel-error?
      (lambda ()
        (let ([ch (make-channel 1)])
          (channel-close ch)
          (channel-send ch 'value)))))

  (test-equal "channel-length and channel-capacity"
    (let ([ch (make-channel 3)])
      (channel-send ch 1)
      (channel-send ch 2)
      (list (channel-length ch) (channel-capacity ch)))
    '(2 3))

  (test-equal "channel-capacity of an unbuffered channel is zero"
    (channel-capacity (make-channel 0))
    0)

  (test-equal "blocked sender resumes when a slot frees"
    (let* ([ch (make-channel 1)]
           [sender (go
                     (lambda ()
                       (channel-send ch 'a)
                       (channel-send ch 'b)
                       'done))])
      (let* ([first (channel-recv ch)]
             [second (channel-recv ch)])
        (list first second (thread-join sender))))
    '(a b done)))

(test-group "unbuffered"
  (test-equal "send rendezvous delivers value to waiting receiver"
    (let* ([ch (make-channel 0)]
           [receiver (go
                       (lambda ()
                         (channel-recv ch)))])
      (channel-send ch 'ping)
      (thread-join receiver))
    'ping)

  (test-equal "recv rendezvous takes value from waiting sender"
    (let* ([ch (make-channel 0)]
           [sender (go
                     (lambda ()
                       (channel-send ch 'pong)
                       'sent))]
           [value (channel-recv ch)])
      (list value (thread-join sender)))
    '(pong sent))

  (test-equal "closed unbuffered receive returns eof"
    (let ([ch (make-channel 0)])
      (channel-close ch)
      (list (channel-closed? ch)
            (channel-recv ch)))
    '(#t #f))

  (test-equal "two senders on an unbuffered channel both deliver"
    (let* ([ch (make-channel 0)]
           [a (go (lambda () (channel-send ch 'a)))]
           [b (go (lambda () (channel-send ch 'b)))]
           [x (channel-recv ch)]
           [y (channel-recv ch)])
      (thread-join a)
      (thread-join b)
      (and (memq 'a (list x y)) (memq 'b (list x y)) #t))
    #t)

  (test-equal "many senders, one receiver: no value lost or duplicated"
    (let* ([ch (make-channel 0)]
           [senders (spawn 50
                      (lambda (i)
                        (do ([k 0 (+ k 1)]) ((= k 20))
                          (channel-send ch (+ (* i 100) k)))))]
           [sum (let loop ([n 0] [sum 0])
                  (if (= n 1000)
                    sum
                    (loop (+ n 1) (+ sum (channel-recv ch)))))])
      (join-all senders)
      sum)
    ;; sum over i<50, k<20 of (100 i + k)
    (+ (* 2000 (/ (* 49 50) 2)) (* 50 (/ (* 19 20) 2))))

  (test-assert "N senders and M receivers exchange every value exactly once"
    (let* ([ch (make-channel 0)]
           [senders 20]
           [per 50]
           [seen (make-vector (* senders per) 0)]
           [lock (make-mutex)]
           [send-threads
             (spawn senders
               (lambda (i)
                 (do ([k 0 (+ k 1)]) ((= k per))
                   (channel-send ch (+ (* i per) k)))))]
           [recv-threads
             (spawn 4
               (lambda (_)
                 (let loop ()
                   (let-values ([(v ok?) (channel-recv/ok ch)])
                     (when ok?
                       (with-mutex lock
                         (vector-set! seen v (+ 1 (vector-ref seen v))))
                       (loop))))))])
      (join-all send-threads)
      (channel-close ch)
      (join-all recv-threads)
      (all-true? (map (lambda (n) (= n 1)) (vector->list seen)))))

  (test-equal "closing wakes blocked receivers with eof"
    (let* ([ch (make-channel 0)]
           [receivers (spawn 5 (lambda (_) (channel-recv/ok ch)))])
      (channel-close ch)
      (map (lambda (t)
             (call-with-values (lambda () (thread-join t))
               (lambda (v ok?) (list v ok?))))
        receivers))
    '((#f #f) (#f #f) (#f #f) (#f #f) (#f #f)))

  (test-assert "closing wakes a blocked sender with a violation"
    (let* ([ch (make-channel 0)]
           [sender (go
                     (lambda ()
                       (guard (c [(closed-channel-violation? c) 'raised])
                         (channel-send ch 'x)
                         'sent)))])
      (channel-close ch)
      (eq? (thread-join sender) 'raised))))

(test-group "channel-recv/ok"
  (test-equal "distinguishes a received #f from closed"
    (let ([ch (make-channel 2)])
      (channel-send ch #f)
      (channel-close ch)
      (call-with-values (lambda () (channel-recv/ok ch))
        (lambda (v ok?) (list v ok?))))
    '(#f #t))

  (test-equal "closed and drained gives (values #f #f)"
    (let ([ch (make-channel 0)])
      (channel-close ch)
      (call-with-values (lambda () (channel-recv/ok ch))
        (lambda (v ok?) (list v ok?))))
    '(#f #f)))

(test-group "non-blocking"
  (test-equal "try-send reports would-block on a full buffer"
    (let ([ch (make-channel 1)])
      (list (channel-try-send ch 'a)
            (channel-try-send ch 'b)))
    '(sent would-block))

  (test-equal "try-send on an unbuffered channel without a receiver"
    (channel-try-send (make-channel 0) 'a)
    'would-block)

  (test-equal "try-send hands off to a waiting receiver"
    (let* ([ch (make-channel 0)]
           [receiver (go (lambda () (channel-recv ch)))])
      (let loop ([tries 0])
        (let ([status (channel-try-send ch 'hello)])
          (cond
            [(eq? status 'sent) (thread-join receiver)]
            [(> tries 100000) 'gave-up]
            [else (yield) (loop (+ tries 1))]))))
    'hello)

  (test-equal "try-send reports closed without raising"
    (let ([ch (make-channel 1)])
      (channel-close ch)
      (channel-try-send ch 'a))
    'closed)

  (test-equal "try-recv reports empty, then value, then closed"
    (let ([ch (make-channel 1)])
      (channel-send ch 'v)
      (call-with-values (lambda () (channel-try-recv ch))
        (lambda (s1 v1)
          (call-with-values (lambda () (channel-try-recv ch))
            (lambda (s2 v2)
              (channel-close ch)
              (call-with-values (lambda () (channel-try-recv ch))
                (lambda (s3 v3)
                  (list s1 v1 s2 s3 v3))))))))
    '(value v empty closed #f)))

(test-end "capy channel")
