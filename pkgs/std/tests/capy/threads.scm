(import (rnrs)
        (srfi 64)
        (capy concurrency)
        (capy threads))

(test-begin "capy threads introspection")

(define (find-info id)
  (let loop ([l (thread-list)])
    (cond [(null? l) #f]
          [(= (thread-info-id (car l)) id) (car l)]
          [else (loop (cdr l))])))

(define-syntax begin0
  (syntax-rules ()
    [(_ first rest ...) (let ([v first]) rest ... v)]))

(define (string-contains* s sub)
  (let ([n (string-length s)] [m (string-length sub)])
    (let loop ([i 0])
      (cond [(> (+ i m) n) #f]
            [(string=? (substring s i (+ i m)) sub) #t]
            [else (loop (+ i 1))]))))

(define (wait-for pred)
  (let loop ([n 200])
    (cond [(pred) #t]
          [(zero? n) #f]
          [else (sleep 0.01) (loop (- n 1))])))

;; `thread-id` is 0 until the thread has started.
(define (started-id t)
  (wait-for (lambda () (positive? (thread-id t))))
  (thread-id t))

(test-assert "the calling thread is listed as running"
  (let ([info (find-info (thread-id))])
    (or (not info) (eq? 'running (thread-info-state info)))))

(test-equal "a sleeping thread is blocked on sleep, with its name and parent"
  (let* ([t (go (lambda () (set-thread-name! "napper") (sleep 30)))]
         [id (started-id t)])
    (wait-for (lambda () (let ([i (find-info id)]) (and i (eq? 'sleep (thread-info-wait i))))))
    (let ([i (find-info id)])
      (begin0
        (list (thread-info-name i) (thread-info-state i) (thread-info-wait i)
          (= (thread-info-parent i) (thread-id)))
        (thread-cancel! t))))
  '("napper" blocked sleep #t))

(test-equal "a joiner reports the thread it waits for"
  (let* ([target (go (lambda () (sleep 30)))]
         [joiner (go (lambda () (thread-join target)))]
         [jid (started-id joiner)])
    (wait-for (lambda () (let ([i (find-info jid)]) (and i (eq? 'join (thread-info-wait i))))))
    (let ([i (find-info jid)])
      (begin0
        (list (thread-info-wait i) (= (thread-info-wait-on i) (started-id target)))
        (thread-cancel! joiner)
        (thread-cancel! target))))
  '(join #t))

(test-equal "a thread that blocks on a mutex shows its holder"
  (let* ([m (make-mutex)]
         [holder (go (lambda () (mutex-acquire m) (sleep 30)))]
         [hid (started-id holder)]
         ;; The holder sleeps only after it has taken the mutex.
         [_ (wait-for (lambda () (let ([i (find-info hid)]) (and i (eq? 'sleep (thread-info-wait i))))))]
         [waiter (go (lambda () (mutex-acquire m)))]
         [wid (started-id waiter)])
    (wait-for (lambda () (let ([i (find-info wid)]) (and i (eq? 'mutex (thread-info-wait i))))))
    (let ([i (find-info wid)])
      (begin0
        (list (thread-info-wait i) (= (thread-info-wait-on i) (started-id holder)))
        (thread-cancel! waiter)
        (thread-cancel! holder))))
  '(mutex #t))

(test-equal "no deadlock without a cycle"
  (find-deadlocks)
  '())

(test-equal "two threads taking two mutexes in opposite order deadlock"
  (let* ([a (make-mutex)] [b (make-mutex)]
         [t1 (go (lambda () (mutex-acquire a) (sleep 0.1) (mutex-acquire b)))]
         [t2 (go (lambda () (mutex-acquire b) (sleep 0.1) (mutex-acquire a)))]
         [ids (list (started-id t1) (started-id t2))])
    (wait-for (lambda () (pair? (find-deadlocks))))
    (let ([cycles (find-deadlocks)])
      (thread-cancel! t1)
      (thread-cancel! t2)
      (list (length cycles) (equal? (car cycles) (list-sort < ids)))))
  '(1 #t))

(test-assert "dump-threads names a deadlock"
  (let* ([a (make-mutex)] [b (make-mutex)]
         [t1 (go (lambda () (set-thread-name! "alice") (mutex-acquire a) (sleep 0.1) (mutex-acquire b)))]
         [t2 (go (lambda () (set-thread-name! "bob") (mutex-acquire b) (sleep 0.1) (mutex-acquire a)))])
    (define (dump) (call-with-string-output-port (lambda (p) (dump-threads p))))

    (define (our-cycle? text)
      (let ([p (open-string-input-port text)])
        (let loop ()
          (let ([line (get-line p)])
            (and (not (eof-object? line))
              (or (and (string-contains* line "deadlock among 2 threads")
                       (string-contains* line "(alice)")
                       (string-contains* line "(bob)"))
                  (loop)))))))
    (wait-for (lambda () (our-cycle? (dump))))
    (let ([text (dump)])
      (thread-cancel! t1)
      (thread-cancel! t2)
      (our-cycle? text))))

(test-assert "the tree lists children under their parent"
  (let* ([t (go (lambda () (set-thread-name! "kid") (sleep 30)))])
    (wait-for (lambda () (let ([i (find-info (started-id t))]) (and i (thread-info-name i)))))
    (let ([text (call-with-string-output-port (lambda (p) (print-thread-tree p)))])
      (thread-cancel! t)
      (string-contains* text "kid"))))

(test-assert "thread-backtrace reads the stack of a blocked thread"
  (let* ([t (go (lambda () (sleep 30)))])
    (sleep 0.1)
    (let ([frames (thread-backtrace t 2.0)])
      (thread-cancel! t)
      (and frames (pair? frames) (vector? (car frames))))))

(test-equal "thread-backtrace of a finished thread gives #f"
  (let ([t (go (lambda () 1))])
    (thread-join t)
    (thread-backtrace t 0.2))
  #f)

(test-assert "a trace records a span per thread state as Chrome trace JSON"
  (let ([path "/tmp/capy-thread-trace-test.json"])
    (call-with-thread-trace path 0.002
      (lambda ()
        (let ([t (go (lambda () (set-thread-name! "napper \"x\"") (sleep 0.05)))])
          (thread-join t))))
    (let ([text (call-with-input-file path get-string-all)])
      (delete-file path)
      (and (> (string-length text) 20)
           (string=? (substring text 0 16) "{\"traceEvents\":[")
           (let loop ([i 0])
             (cond
               [(> (+ i 17) (string-length text)) #f]
               [(string=? (substring text i (+ i 17)) "blocked on sleep\"") #t]
               [else (loop (+ i 1))]))
           ;; the quote in the thread's name is escaped
           (let loop ([i 0])
             (cond
               [(> (+ i 10) (string-length text)) #f]
               [(string=? (substring text i (+ i 10)) "napper \\\"x") #t]
               [else (loop (+ i 1))]))))))

(test-end "capy threads introspection")
