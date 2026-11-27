(import (rnrs)
        (only (capy) call-without-interrupts)
        (srfi 64)
        (capy concurrency))

(test-begin "capy thread interrupts")

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
         [cv (make-condition-variable)]
         [events '()]
         [worker (go
                   (lambda ()
                     (call-without-interrupts
                       (lambda ()
                         (mutex-acquire mutex)
                         (set! started #t)
                         (condition-variable-broadcast cv)
                         (let loop ()
                           (unless release?
                             (condition-variable-wait cv mutex)
                             (loop)))
                         (mutex-release mutex)))
                     (set! events (cons 'done events))))])
    (mutex-acquire mutex)
    (let loop ()
      (unless started
        (condition-variable-wait cv mutex)
        (loop)))
    (thread-interrupt! worker (lambda () (set! events (cons 'first events))))
    (thread-interrupt! worker (lambda () (set! events (cons 'second events))))
    (set! release? #t)
    (condition-variable-broadcast cv)
    (mutex-release mutex)
    (thread-join worker)
    (reverse events))
  '(first second done))

;; The cases below hand-shake with the worker instead of spinning for a fixed
;; number of iterations: a thread cannot be interrupted before it has started,
;; and an interrupt sent to a thread that has finished never runs.

(test-equal "masked interrupts wait until unmasked"
  (let* ([started #f]
         [sent? #f]
         [mutex (make-mutex)]
         [cv (make-condition-variable)]
         [events '()]
         [worker (go
                   (lambda ()
                     (call-without-interrupts
                       (lambda ()
                         (set! events (cons 'masked events))
                         (mutex-acquire mutex)
                         (set! started #t)
                         (condition-variable-broadcast cv)
                         (mutex-release mutex)
                         ;; Stay busy rather than blocked until the interrupt
                         ;; is pending, then pass more interrupt points.
                         (let loop () (unless sent? (loop)))
                         (let loop ([n 0])
                           (when (< n 200000)
                             (loop (+ n 1))))
                         (set! events (cons 'unmasking events))))
                     (set! events (cons 'after events))))])
    (mutex-acquire mutex)
    (let loop ()
      (unless started
        (condition-variable-wait cv mutex)
        (loop)))
    (mutex-release mutex)
    (thread-interrupt! worker (lambda () (set! events (cons 'interrupt events))))
    (set! sent? #t)
    (thread-join worker)
    (reverse events))
  '(masked unmasking interrupt after))

(test-equal "blocked condition wait accepts interrupt"
  (let* ([release? #f]
         [mutex (make-mutex)]
         [cv (make-condition-variable)]
         [events '()]
         [worker (go
                   (lambda ()
                     (mutex-acquire mutex)
                     (set! events (cons 'waiting events))
                     (condition-variable-broadcast cv)
                     (let loop ()
                       (unless release?
                         (condition-variable-wait cv mutex)
                         (loop)))
                     (set! events (cons 'resumed events))
                     (mutex-release mutex)))])
    ;; The worker holds the mutex from 'waiting until it blocks, so once this
    ;; thread sees 'waiting with the mutex held, the worker is in the wait.
    (mutex-acquire mutex)
    (let loop ()
      (unless (memq 'waiting events)
        (condition-variable-wait cv mutex)
        (loop)))
    (thread-interrupt! worker (lambda () (set! events (cons 'interrupt events))))
    (set! release? #t)
    (condition-variable-broadcast cv)
    (mutex-release mutex)
    (thread-join worker)
    (reverse events))
  '(waiting interrupt resumed))

(test-end "capy thread interrupts")
