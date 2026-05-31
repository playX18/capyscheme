(import (rnrs)
        (srfi 64)
        (core threading))

(test-begin "capy thread interrupts")

(define (wait-until pred)
  (let loop ([n 2000])
    (cond
      [(pred) #t]
      [(zero? n) #f]
      [else
        (let spin ([m 200])
          (when (> m 0)
            (spin (- m 1))))
        (loop (- n 1))])))

(test-equal "same-thread interrupt runs immediately"
  (let ([seen '()])
    (thread-interrupt! (current-thread)
      (lambda ()
        (set! seen (cons 'interrupt seen))))
    seen)
  '(interrupt))

(test-equal "interrupts run FIFO and target resumes"
  (let* ([started #f]
         [release? #f]
         [mutex (make-mutex)]
         [cv (make-condition)]
         [events '()]
         [worker (call-with-new-thread
                   (lambda ()
                     (call-without-interrupts
                       (lambda ()
                         (mutex-acquire mutex)
                         (set! started #t)
                         (condition-broadcast cv)
                         (let loop ()
                           (unless release?
                             (condition-wait cv mutex)
                             (loop)))
                         (mutex-release mutex)))
                     (set! events (cons 'done events))))])
    (mutex-acquire mutex)
    (let loop ()
      (unless started
        (condition-wait cv mutex)
        (loop)))
    (thread-interrupt! worker (lambda () (set! events (cons 'first events))))
    (thread-interrupt! worker (lambda () (set! events (cons 'second events))))
    (set! release? #t)
    (condition-broadcast cv)
    (mutex-release mutex)
    (join-thread worker)
    (reverse events))
  '(first second done))

(test-equal "masked interrupts wait until unmasked"
  (let* ([started #f]
         [events '()]
         [worker (call-with-new-thread
                   (lambda ()
                     (call-without-interrupts
                       (lambda ()
                         (set! events (cons 'masked events))
                         (set! started #t)
                         (let loop ([n 0])
                           (when (< n 200000)
                             (loop (+ n 1))))
                         (set! events (cons 'unmasking events))))
                     (set! events (cons 'after events))))])
    (wait-until (lambda () started))
    (thread-interrupt! worker (lambda () (set! events (cons 'interrupt events))))
    (join-thread worker)
    (reverse events))
  '(masked unmasking interrupt after))

(test-equal "blocked condition wait accepts interrupt"
  (let* ([mutex (make-mutex)]
         [cv (make-condition)]
         [events '()]
         [worker (call-with-new-thread
                   (lambda ()
                     (mutex-acquire mutex)
                     (set! events (cons 'waiting events))
                     (condition-wait cv mutex)
                     (set! events (cons 'resumed events))
                     (mutex-release mutex)))])
    (wait-until (lambda () (memq 'waiting events)))
    (thread-interrupt! worker (lambda () (set! events (cons 'interrupt events))))
    (condition-broadcast cv)
    (join-thread worker)
    (reverse events))
  '(waiting interrupt resumed))

(test-end "capy thread interrupts")
