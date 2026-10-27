(import (rnrs)
        (srfi 64)
        (capy channel)
        (capy concurrency)
        (capy sync)
        (capy pool))

(test-begin "capy pool")

(test-group "worker pool"
  (test-equal "submit returns a future with the thunk's values"
    (let* ([p (make-worker-pool 2)]
           [f (pool-submit! p (lambda () (values 1 2)))])
      (let ([r (call-with-values (lambda () (pool-future-get f)) list)])
        (pool-shutdown! p)
        r))
    '(1 2))

  (test-equal "future-get re-raises the job's condition"
    (let* ([p (make-worker-pool 1)]
           [f (pool-submit! p (lambda () (error 'job "failed")))])
      (let ([r (guard (e [(error? e) 'raised]) (pool-future-get f) 'no)])
        (pool-shutdown! p)
        r))
    'raised)

  (test-equal "future-done? becomes true after the job runs"
    (let* ([p (make-worker-pool 1)]
           [f (pool-submit! p (lambda () (sleep 0.01) 'x))])
      (let ([before (pool-future-done? f)])
        (pool-future-get f)
        (pool-shutdown! p)
        (list before (pool-future-done? f))))
    '(#f #t))

  (test-equal "all submitted jobs run, many more than workers"
    (let* ([p (make-worker-pool 4)]
           [count (make-atomic 0)]
           [futures (let loop ([i 0] [acc '()])
                      (if (= i 1000)
                        acc
                        (loop (+ i 1)
                          (cons (pool-submit! p (lambda () (atomic-add! count 1) i))
                                acc))))])
      (for-each pool-future-get futures)
      (pool-shutdown! p)
      (atomic-ref count))
    1000)

  (test-equal "shutdown waits for queued jobs"
    (let* ([p (make-worker-pool 2)]
           [count (make-atomic 0)])
      (do ([i 0 (+ i 1)]) ((= i 10))
        (pool-submit! p (lambda () (sleep 0.005) (atomic-add! count 1))))
      (pool-shutdown! p)
      (atomic-ref count))
    10)

  (test-assert "submit after shutdown raises"
    (let ([p (make-worker-pool 1)])
      (pool-shutdown! p)
      (guard (e [(assertion-violation? e) #t] [else #f])
        (pool-submit! p (lambda () 1))
        #f)))

  (test-assert "pool? recognises pools"
    (let ([p (make-worker-pool 1)])
      (pool-shutdown! p)
      (and (pool? p) (not (pool? 'nope))))))

(test-group "parallel-map"
  (test-equal "results come back in order"
    (parallel-map (lambda (x) (* x x)) '(1 2 3 4 5))
    '(1 4 9 16 25))

  (test-equal "empty list"
    (parallel-map (lambda (x) x) '())
    '())

  (test-equal "limited parallelism still gives all results"
    (let ([active (make-atomic 0)] [peak (make-atomic 0)])
      (let ([r (parallel-map
                 (lambda (x)
                   (let ([now (atomic-add! active 1)])
                     (let retry ()
                       (let ([p (atomic-ref peak)])
                         (when (and (> now p) (not (atomic-cas! peak p now)))
                           (retry))))
                     (sleep 0.002)
                     (atomic-add! active -1)
                     (* 2 x)))
                 (iota-list 40)
                 3)])
        (list (length r) (apply + r) (<= (atomic-ref peak) 3))))
    (list 40 (* 2 (/ (* 40 41) 2)) #t))

  (test-equal "raises the first error after all calls finish"
    (guard (e [(error? e) 'raised])
      (parallel-map (lambda (x) (if (= x 3) (error 'map "bad") x)) '(1 2 3 4))
      'no)
    'raised))

(test-group "fan-out, fan-in and pipelines"
  (test-equal "fan-out applies f to every value"
    (let* ([in (make-channel 0)]
           [out (fan-out in (lambda (x) (* 10 x)) 4)])
      (go (lambda ()
            (do ([i 1 (+ i 1)]) ((= i 21))
              (channel-send in i))
            (channel-close in)))
      (let loop ([acc '()])
        (let-values ([(v ok?) (channel-recv/ok out)])
          (if ok?
            (loop (cons v acc))
            (apply + acc)))))
    2100)

  (test-equal "fan-in merges and closes when all inputs close"
    (let* ([a (make-channel 0)]
           [b (make-channel 0)]
           [out (fan-in (list a b))])
      (go (lambda () (channel-send a 1) (channel-close a)))
      (go (lambda () (channel-send b 2) (channel-close b)))
      (let loop ([acc '()])
        (let-values ([(v ok?) (channel-recv/ok out)])
          (if ok?
            (loop (cons v acc))
            (sort-numbers acc)))))
    '(1 2))

  (test-equal "map-stage keeps order with one worker"
    (let* ([in (make-channel 0)]
           [out (pipeline in (map-stage (lambda (x) (+ x 1))) (map-stage (lambda (x) (* x 2))))])
      (go (lambda ()
            (do ([i 0 (+ i 1)]) ((= i 5))
              (channel-send in i))
            (channel-close in)))
      (let loop ([acc '()])
        (let-values ([(v ok?) (channel-recv/ok out)])
          (if ok?
            (loop (cons v acc))
            (reverse acc)))))
    '(2 4 6 8 10))

  (test-equal "pipeline with no stages returns its input"
    (let ([ch (make-channel 1)])
      (eq? (pipeline ch) ch))
    #t))

(define (iota-list n)
  (let loop ([i (- n 1)] [acc '()])
    (if (< i 0) acc (loop (- i 1) (cons (+ i 1) acc)))))

(define (sort-numbers lst)
  (if (or (null? lst) (null? (cdr lst)))
    lst
    (let ([sorted (sort-numbers (cdr lst))])
      (if (< (car lst) (car sorted))
        (cons (car lst) sorted)
        (cons (car sorted) (sort-numbers (cons (car lst) (cdr sorted))))))))

(test-end "capy pool")
