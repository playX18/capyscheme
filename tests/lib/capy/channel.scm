(import (rnrs)
        (srfi 64)
        (capy channel)
        (core threading))

(test-begin "capy channel")

(define (closed-channel-error? thunk)
  (guard (c
          [(closed-channel-violation? c) #t]
          [else #f])
    (thunk)
    #f))

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
          (channel-send ch 'value))))))

(test-group "unbuffered"
  (test-equal "send rendezvous delivers value to waiting receiver"
    (let* ([ch (make-channel 0)]
           [receiver (call-with-new-thread
                       (lambda ()
                         (channel-recv ch)))])
      (channel-send ch 'ping)
      (join-thread receiver))
    'ping)

  (test-equal "recv rendezvous takes value from waiting sender"
    (let* ([ch (make-channel 0)]
           [sender (call-with-new-thread
                     (lambda ()
                       (channel-send ch 'pong)
                       'sent))]
           [value (channel-recv ch)])
      (list value (join-thread sender)))
    '(pong sent))

  (test-equal "closed unbuffered receive returns eof"
    (let ([ch (make-channel 0)])
      (channel-close ch)
      (list (channel-closed? ch)
            (channel-recv ch)))
    '(#t #f)))

(test-end "capy channel")
