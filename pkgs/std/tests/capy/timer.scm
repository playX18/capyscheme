(import (rnrs)
        (srfi 64)
        (capy channel)
        (capy timer)
        (capy concurrency))

(test-begin "capy timer")

(define (recv-within ch secs)
  (let-values ([(v ok?) (channel-recv/ok ch)])
    ok?))

(test-group "after and make-timer"
  (test-equal "after delivers one value"
    (let ([ch (after 0.02)])
      (let-values ([(v ok?) (channel-recv/ok ch)])
        (list (real? v) ok?)))
    '(#t #t))

  (test-equal "timer fires once and nothing more arrives"
    (let* ([t (make-timer 0.01)]
           [ch (timer-channel t)])
      (channel-recv ch)
      (channel-try-recv ch)
      (call-with-values (lambda () (channel-try-recv ch))
        (lambda (status v) status)))
    'empty)

  (test-equal "timer-stop! prevents firing"
    (let* ([t (make-timer 0.02)]
           [stopped (timer-stop! t)])
      (sleep 0.05)
      (call-with-values (lambda () (channel-try-recv (timer-channel t)))
        (lambda (status v) (list stopped status))))
    '(#t empty))

  (test-equal "timer-stop! on a fired timer returns #f"
    (let ([t (make-timer 0.0)])
      (channel-recv (timer-channel t))
      (timer-stop! t))
    #f)

  (test-equal "timer-reset! restarts a stopped timer"
    (let ([t (make-timer 0.5)])
      (timer-stop! t)
      (let ([was-active? (timer-reset! t 0.01)])
        (list was-active? (recv-within (timer-channel t) 1))))
    '(#f #t))

  (test-equal "timer-reset! on a pending timer returns #t"
    (let ([t (make-timer 5)])
      (let ([was (timer-reset! t 5)])
        (timer-stop! t)
        was))
    #t))

(test-group "tick"
  (test-equal "ticker sends repeatedly until stopped"
    (let* ([k (tick 0.01)]
           [ch (ticker-channel k)])
      (let loop ([n 0])
        (if (= n 3)
          (begin
            (ticker-stop! k)
            (sleep 0.03)
            ;; At most one tick can slip in between the last receive and the stop.
            (let drain ([extra 0])
              (call-with-values (lambda () (channel-try-recv ch))
                (lambda (status v)
                  (if (eq? status 'value)
                    (drain (+ extra 1))
                    (list n (<= extra 1)))))))
          (begin
            (channel-recv ch)
            (loop (+ n 1))))))
    '(3 #t))

  (test-equal "ticker? recognises tickers"
    (let ([k (tick 1)])
      (ticker-stop! k)
      (list (ticker? k) (ticker? 42)))
    (quote (#t #f))))

(test-group "call-with-timeout"
  (test-equal "returns the thunk's values when it finishes in time"
    (call-with-values
      (lambda () (call-with-timeout 1 (lambda () (values 1 2))))
      list)
    '(1 2))

  (test-equal "raises timeout-expired when the thunk is too slow"
    (guard (e [(timeout-expired? e) 'expired])
      (call-with-timeout 0.02 (lambda () (sleep 0.5) 'slow)))
    'expired)

  (test-equal "re-raises a condition from the thunk"
    (guard (e [(error? e) 'error-raised])
      (call-with-timeout 1 (lambda () (error 'thunk "failed"))))
    'error-raised))

(test-end "capy timer")
