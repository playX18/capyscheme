(library (capy channel)
  (export
    closed-channel-violation?
    channel-close
    channel-send
    channel-recv
    channel-closed? 
    make-channel

  )
  (import (rnrs)
          (capy deque)
          (core threading))

  (define-record-type slot 
    (fields 
      value))

  (define-condition-type &closed-channel &violation 
    make-closed-channel-violation
    closed-channel-violation?)

  (define (channel-closed-violation who msg)
    (let ((c (condition 
              (make-closed-channel-violation)
              (make-message-condition msg))))
      (raise 
        (if who 
          (condition c (make-who-condition who))
          c))))

  ;; A Go-like channel.
  ;; - When `cap` is zero: unbuffered
  ;; - When `cap` is positive: buffered
  ;; Close semantics: 
  ;; - close() forbids further sends (raises <closed-channel-condition> on send)
  ;; - recv() returns value, `channel-closed` singleton when closed and drained, blocks otherwise.
  (define-record-type channel 
    (fields 
      lock
      (mutable %closed?) 
      buf 
      (mutable slot)
      (mutable recv-waiting)
      (mutable send-waiting)
      cv-send
      cv-recv 
      cap)
    (protocol 
      (lambda (new)
        (lambda (cap)
          (new 
            (make-mutex)
            #f 
            (make-deque cap) 
            #f 
            0 
            0 
            (make-condition) 
            (make-condition)
            cap)))))

  (define chan (make-channel 0))

  (define (channel-close chan)
    (with-mutex (channel-lock chan)
      (channel-%closed?-set! chan #t)
      (condition-broadcast (channel-cv-recv chan))
      (condition-broadcast (channel-cv-send chan))))
  
  (define (channel-send chan v)
    "Send a value `v` to the channel `chan`.
    - Blocks until it can be delivered (buffer space or rendezvous).
    - Raises channel-closed-violation if the channel is closed."

    (with-mutex (channel-lock chan)
      (when (channel-%closed? chan)
        (channel-closed-violation 'send "send on closed channel"))
      (cond 
        [(= (channel-cap chan) 0) ; unbounded send
          (channel-send-waiting-set! chan (+ 1 (channel-send-waiting chan)))
          ;; rendezvous: wait for a receiver, then handoff via slot.
          (let loop () 
            (when (and (not (channel-%closed? chan))
                       (= (channel-recv-waiting chan) 0))

              (condition-wait (channel-cv-send chan) (channel-lock chan))
              (loop)))
          (channel-send-waiting-set! chan 
                                     (- (channel-send-waiting chan) 1))
          ;; somebody might've closed the channel while we waited
          
          (when (channel-%closed? chan)
            (channel-closed-violation 'send "send on closed channel"))
          
          (channel-slot-set! chan (make-slot v))
          
          (condition-signal (channel-cv-recv chan))
          
          
          ;; wait until receiver takes it (slot becomes #f)
          (let wait-loop ()
            (when (and (not (channel-%closed? chan))
                       (channel-slot chan))

              (condition-wait (channel-cv-send chan) (channel-lock chan))
              (wait-loop)))]
        [else 
          ;; buffered: wait for space
          (let loop () 
            (when (and (not (channel-%closed? chan))
                       (= (deque-length (channel-buf chan))
                          (channel-cap chan)))

              (condition-wait (channel-cv-send chan) (channel-lock chan))
              (loop)))
          (when (channel-%closed? chan)
            (channel-closed-violation 'send "send on closed channel"))
          (deque-push-back! (channel-buf chan) v)
          (condition-signal (channel-cv-recv chan))])))

  (define (channel-recv chan)

    (with-mutex (channel-lock chan)
      (cond 
        [(= (channel-cap chan) 0) ; unbuffered 
          (channel-recv-waiting-set! chan (+ 1 (channel-recv-waiting chan)))
          (condition-signal (channel-cv-send chan)) ; wake a sender
          (let loop ()
            (when (and (not (channel-%closed? chan))
                       (not (channel-slot chan)))
              (condition-wait (channel-cv-recv chan) (channel-lock chan))
              (loop)))
          (channel-recv-waiting-set! chan (- (channel-recv-waiting chan) 1))
          
          (cond 
            [(channel-slot chan) => 
              (lambda (slot) 
                (channel-slot-set! chan #f)
                (condition-signal (channel-cv-send chan))
                (slot-value slot))]
            [else 
              #f])]
        [else 
          (let loop () 
            (when (and (not (channel-%closed? chan))
                       (= (deque-length (channel-buf chan)) 0))
              
              (condition-wait (channel-cv-recv chan) (channel-lock chan))
              (loop)))
          (if (> (deque-length (channel-buf chan)) 0)
            (let ((v (deque-pop-front! (channel-buf chan))))
              (condition-signal (channel-cv-send chan))
              v)
            #f)])))
  
  (define (channel-closed? chan)
    "Returns #t if the channel is closed, #f otherwise.
    
    Note: a closed channel may still have buffered values to receive."
    (with-mutex (channel-lock chan)
      (channel-%closed? chan))))