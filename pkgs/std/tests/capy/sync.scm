(import (rnrs)
        (only (scheme base) modulo)
        (srfi 64)
        (capy sync)
        (capy concurrency))

(test-begin "capy sync")

(define (spawn n body)
  (let loop ([i 0] [acc '()])
    (if (= i n)
      (reverse acc)
      (loop (+ i 1) (cons (go (lambda () (body i))) acc)))))

(define (join-all threads)
  (for-each thread-join threads))

(define (raises-assertion? thunk)
  (guard (c [(assertion-violation? c) #t]
            [(message-condition? c) #t]
            [else #f])
    (thunk)
    #f))

(test-group "wait-group"
  (test-equal "wait returns immediately at zero"
    (let ([wg (make-wait-group)])
      (wait-group-wait wg)
      'done)
    'done)

  (test-equal "wait blocks until all done"
    (let* ([wg (make-wait-group)]
           [flag (make-atomic 0)])
      (wait-group-add! wg 3)
      (spawn 3 (lambda (_)
                 (atomic-add! flag 1)
                 (wait-group-done! wg)))
      (wait-group-wait wg)
      (atomic-ref flag))
    3)

  (test-assert "negative counter raises"
    (raises-assertion?
      (lambda ()
        (let ([wg (make-wait-group)])
          (wait-group-done! wg)))))

  (test-equal "go/wg counts threads and runs them all"
    (let* ([wg (make-wait-group)]
           [count (make-atomic 0)])
      (do ([i 0 (+ i 1)]) ((= i 500))
        (go/wg wg (lambda () (atomic-add! count 1))))
      (wait-group-wait wg)
      (atomic-ref count))
    500)

  (test-equal "go/wg still calls done when the thunk raises"
    (let* ([wg (make-wait-group)])
      (go/wg wg (lambda () (error 'test "boom")))
      (wait-group-wait wg)
      'returned)
    'returned))

(test-group "once"
  (test-equal "once-do runs the thunk only once"
    (let ([o (make-once)] [n (make-atomic 0)])
      (once-do o (lambda () (atomic-add! n 1)))
      (once-do o (lambda () (atomic-add! n 1)))
      (atomic-ref n))
    1)

  (test-equal "concurrent callers all see one run, and it finished first"
    (let* ([o (make-once)]
           [n (make-atomic 0)]
           [threads (spawn 20
                      (lambda (_)
                        (once-do o (lambda ()
                                     (sleep 0.01)
                                     (atomic-add! n 1)))
                        (atomic-ref n)))])
      (apply + (map thread-join threads)))
    20))

(test-group "rw-mutex"
  (test-equal "readers share the lock"
    (let* ([rw (make-rw-mutex)]
           [inside (make-atomic 0)]
           [peak (make-atomic 0)]
           [threads
             (spawn 8
               (lambda (_)
                 (with-read-lock rw
                   (lambda ()
                     (let ([now (atomic-add! inside 1)])
                       (let retry ()
                         (let ([p (atomic-ref peak)])
                           (when (and (> now p) (not (atomic-cas! peak p now)))
                             (retry))))
                       (sleep 0.02)
                       (atomic-add! inside -1))))))])
      (join-all threads)
      (> (atomic-ref peak) 1))
    #t)

  (test-equal "writers are exclusive"
    (let* ([rw (make-rw-mutex)]
           [counter 0]
           [threads
             (spawn 10
               (lambda (_)
                 (do ([k 0 (+ k 1)]) ((= k 200))
                   (with-write-lock rw
                     (lambda ()
                       (let ([v counter])
                         (yield)
                         (set! counter (+ v 1))))))))])
      (join-all threads)
      counter)
    2000)

  (test-equal "with-write-lock releases on raise"
    (let ([rw (make-rw-mutex)])
      (guard (e [#t #t])
        (with-write-lock rw (lambda () (error 'test "boom"))))
      (with-write-lock rw (lambda () 'again)))
    'again))

(test-group "concurrent-hash-table"
  (test-equal "set, get, default and delete"
    (let ([t (make-concurrent-hash-table)])
      (concurrent-hash-table-set! t "a" 1)
      (list (concurrent-hash-table-get t "a")
            (concurrent-hash-table-get t "missing")
            (concurrent-hash-table-get t "missing" 'dflt)
            (begin (concurrent-hash-table-delete! t "a")
                   (concurrent-hash-table-get t "a" 'gone))))
    '(1 #f dflt gone))

  (test-equal "update! applies to current or default value"
    (let ([t (make-concurrent-hash-table)])
      (concurrent-hash-table-update! t 'k (lambda (x) (+ x 1)) 0)
      (concurrent-hash-table-update! t 'k (lambda (x) (+ x 1)) 0)
      (concurrent-hash-table-get t 'k))
    2)

  (test-equal "concurrent increments are not lost"
    (let ([t (make-concurrent-hash-table)])
      (join-all
        (spawn 20
          (lambda (_)
            (do ([k 0 (+ k 1)]) ((= k 200))
              (concurrent-hash-table-update! t (modulo k 7)
                (lambda (x) (+ x 1)) 0)))))
      (apply + (map (lambda (k) (concurrent-hash-table-get t k 0)) '(0 1 2 3 4 5 6))))
    4000)

  (test-equal "size and alist cover all shards"
    (let ([t (make-concurrent-hash-table 4)])
      (do ([i 0 (+ i 1)]) ((= i 100))
        (concurrent-hash-table-set! t i (* i i)))
      (list (concurrent-hash-table-size t)
            (length (concurrent-hash-table->alist t))))
    '(100 100)))

(test-group "atomic"
  (test-equal "ref, set!, add! return values"
    (let ([a (make-atomic 5)])
      (atomic-set! a 10)
      (list (atomic-ref a) (atomic-add! a 3) (atomic-ref a)))
    '(10 13 13))

  (test-equal "cas succeeds only on expected value"
    (let ([a (make-atomic 'x)])
      (list (atomic-cas! a 'y 'z)
            (atomic-cas! a 'x 'z)
            (atomic-ref a)))
    '(#f #t z))

  (test-equal "concurrent add! loses no updates"
    (let ([a (make-atomic 0)])
      (join-all (spawn 16 (lambda (_)
                            (do ([k 0 (+ k 1)]) ((= k 500))
                              (atomic-add! a 1)))))
      (atomic-ref a))
    8000))

(test-group "semaphore"
  (test-equal "try-acquire respects the count"
    (let ([s (make-semaphore 2)])
      (list (semaphore-try-acquire! s)
            (semaphore-try-acquire! s)
            (semaphore-try-acquire! s)
            (begin (semaphore-release! s)
                   (semaphore-try-acquire! s))))
    '(#t #t #f #t))

  (test-equal "limits concurrency to the permit count"
    (let* ([s (make-semaphore 3)]
           [inside (make-atomic 0)]
           [peak (make-atomic 0)]
           [threads
             (spawn 12
               (lambda (_)
                 (with-semaphore s
                   (lambda ()
                     (let ([now (atomic-add! inside 1)])
                       (let retry ()
                         (let ([p (atomic-ref peak)])
                           (when (and (> now p) (not (atomic-cas! peak p now)))
                             (retry))))
                       (sleep 0.005)
                       (atomic-add! inside -1))))))])
      (join-all threads)
      (atomic-ref peak))
    3))

(test-group "errgroup"
  (test-equal "errgroup-wait returns #f when nothing raised"
    (let ([g (make-errgroup)])
      (errgroup-go g (lambda () 1))
      (errgroup-go g (lambda () 2))
      (errgroup-wait g))
    #f)

  (test-equal "errgroup-wait returns the raised condition"
    (let ([g (make-errgroup)])
      (errgroup-go g (lambda () (error 'job "failed")))
      (let ([e (errgroup-wait g)])
        (and (condition? e) (error? e))))
    #t)

  (test-equal "errgroup waits for every thread"
    (let ([g (make-errgroup)] [n (make-atomic 0)])
      (do ([i 0 (+ i 1)]) ((= i 50))
        (errgroup-go g (lambda () (sleep 0.001) (atomic-add! n 1))))
      (errgroup-wait g)
      (atomic-ref n))
    50))

(test-end "capy sync")
