(import (rnrs)
        (only (core primitives) current-interrupt-level)
        (scheme time)
        (srfi 64)
        (capy concurrency)
        (capy threads))

(test-begin "capy thread scopes")

(define (iota* n)
  (let loop ([i (- n 1)] [acc '()])
    (if (< i 0) acc (loop (- i 1) (cons i acc)))))

;; A thread that sleeps until cancelled and records that it unwound.
(define (sleeper log tag)
  (go (lambda ()
        (dynamic-wind
          (lambda () (entered!))
          (lambda () (sleep 30))
          (lambda () (note! log tag))))))

(define entered-lock (make-mutex))
(define entered 0)
(define (entered!)
  (mutex-acquire entered-lock) (set! entered (+ entered 1)) (mutex-release entered-lock))
(define (wait-entered! n)
  (let wait ([tries 10000])
    (mutex-acquire entered-lock)
    (let ([e entered]) (mutex-release entered-lock)
      (when (and (< e n) (> tries 0))
        (sleep 0.001)
        (wait (- tries 1)))))
  (mutex-acquire entered-lock) (set! entered 0) (mutex-release entered-lock))

(define-record-type cell (fields (mutable value)))
(define (box v) (make-cell v))
(define (unbox b) (cell-value b))
(define (set-box! b v) (cell-value-set! b v))
(define log-lock (make-mutex))
;; Several threads append to the same log: make the update atomic.
(define (note! log tag)
  (with-mutex log-lock (set-box! log (cons tag (unbox log)))))

(test-equal "scope returns the body's value after joining its children"
  (let ([log (box '())])
    (let ([v (with-thread-scope
               (go (lambda () (sleep 0.05) (set-box! log '(child-done))))
               'body)])
      (list v (unbox log))))
  '(body (child-done)))

(test-assert "a failing child cancels its siblings and the error reaches the owner"
  (let* ([log (box '())]
         [r (guard (e [#t (list (uncaught-exception? e) (unbox log))])
              (with-thread-scope
                (sleeper log 'sibling)
                (go (lambda () (wait-entered! 1) (error 'child "boom")))
                (sleep 30)
                'not-reached))])
    (equal? r '(#t (sibling)))))

(test-equal "the owner can fail too: its children are cancelled"
  (let ([log (box '())])
    (guard (e [#t #f])
      (with-thread-scope
        (sleeper log 'child)
        (wait-entered! 1)
        (error 'owner "boom")))
    (unbox log))
  '(child))

(test-equal "a deadline cancels the scope"
  (let ([log (box '())])
    (list
      (guard (e [(deadline-exceeded? e) (deadline-exceeded-seconds e)])
        (with-thread-deadline 1.0
          (sleeper log 'child)
          (wait-entered! 1)
          (sleep 30)
          'not-reached))
      (unbox log)))
  '(1.0 (child)))

(test-equal "a scope that finishes before its deadline is not cancelled"
  (with-thread-deadline 5
    (go (lambda () (sleep 0.01)))
    'ok)
  'ok)

(test-equal "cancelling an outer scope unwinds the grandchildren"
  (let ([log (box '())])
    (guard (e [(deadline-exceeded? e) #t])
      (with-thread-deadline 1.0
        (go (lambda ()
              (with-thread-scope
                (sleeper log 'grandchild)
                (dynamic-wind
                  (lambda () (entered!))
                  (lambda () (sleep 30))
                  (lambda () (note! log 'child))))))
        (wait-entered! 2)
        (sleep 30)))
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
      (unbox log)))
  '(child grandchild))

(test-equal "a supervisor lets the other children finish"
  (let ([log (box '())])
    (guard (e [#t #f])
      (with-thread-supervisor
        (go (lambda () (sleep 0.1) (set-box! log '(slow-finished))))
        (go (lambda () (error 'child "boom")))))
    (unbox log))
  '(slow-finished))

(test-equal "thread-scope-cancel! stops the children and ends the scope"
  (let ([log (box '())])
    (with-thread-scope
      (sleeper log 'child)
      (wait-entered! 1)
      (thread-scope-cancel!))
    (unbox log))
  '(child))

(test-equal "go-in starts a thread in a named scope"
  (let ([s #f] [result #f])
    (with-thread-scope
      (set! s (current-thread-scope))
      (go-in s (lambda () (set! result 'ran))))
    result)
  'ran)

;; Each failing scope interrupts its owner. If an interrupt that raises left
;; the owner masked, the second scope would wait out the whole sleep.
(test-assert "an interrupt that raises leaves interrupts unmasked"
  (let ([before (current-interrupt-level)])
    (guard (e [#t #f])
      (with-thread-scope
        (go (lambda () (error 'child "boom")))
        (sleep 30)))
    (= before (current-interrupt-level))))

(test-assert "repeated failing scopes each cancel promptly"
  (let ([start (* 1000 (current-second))])
    (for-each
      (lambda (_)
        (guard (e [#t #f])
          (with-thread-scope
            (go (lambda () (error 'child "boom")))
            (sleep 30))))
      (iota* 30))
    (< (- (* 1000 (current-second)) start) 5000)))

(test-assert "thread-cancel! unwinds threads blocked in every kind of wait"
  (let* ([m (make-mutex)]
         [cvm (make-mutex)]
         [cv (make-condition-variable)]
         [_ (mutex-acquire m)]
         [log (box '())]
         [blocked
           (list
             (sleeper log 'sleep)
             (go (lambda ()
                   (dynamic-wind
                     (lambda () (entered!))
                     (lambda () (mutex-acquire m))
                     (lambda () (note! log 'mutex)))))
             (go (lambda ()
                   (dynamic-wind
                     (lambda () (entered!))
                     (lambda ()
                       (mutex-acquire cvm)
                       (condition-variable-wait cv cvm))
                     (lambda () (note! log 'condition)))))
             (go (lambda ()
                   (dynamic-wind
                     (lambda () (entered!))
                     (lambda () (let loop () (loop)))
                     (lambda () (note! log 'busy))))))])
    (wait-entered! 4)
    (sleep 0.2)
    (for-each thread-cancel! blocked)
    (for-each (lambda (t) (guard (e [#t #f]) (thread-join t))) blocked)
    (= 4 (length (unbox log)))))

(test-equal "cancelling a finished thread reports false"
  (let ([t (go (lambda () 1))])
    (thread-join t)
    (thread-cancel! t))
  #f)

;; A select with a timeout starts a timer thread. The scope must not wait for
;; it, neither while it is pending nor after the select has stopped it.
(test-assert "a select timeout does not hold up the scope"
  (let ([start (* 1000 (current-second))]
        [ch (make-channel 1)])
    (channel-try-send ch 1)
    (with-thread-scope
      (select
        ((recv ch) 'got)
        ((timeout 30) 'late)))
    (< (- (* 1000 (current-second)) start) 5000)))

(test-assert "a stopped timer leaves no thread behind"
  ;; Threads cancelled by earlier tests may still be retiring, so the count
  ;; can only go down. The timer's thread, asleep for 30 seconds, would keep
  ;; it above `before` for the whole wait.
  (let* ([before (length (thread-list))]
         [t (make-timer 30)])
    (timer-stop! t)
    (let loop ([n 200])
      (cond
        [(<= (length (thread-list)) before) #t]
        [(zero? n) #f]
        [else (sleep 0.01) (loop (- n 1))]))))

(test-end "capy thread scopes")
