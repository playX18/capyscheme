;; A simple benchmark to test the adaptive GC performance. ;
;; Run with deliberately small heap to test the adaptive GC performance.
;; Example: capy --log-trace --gc-max-heap=64M --gc-heuristic=adaptive -s benchmarks/adaptive-gc.scm
;; Compare with MMTk by setting `--gc-trigger=DynamicHeapSize:32M,64M` or `--gc-trigger=FixedHeapSize:64M`

(import (core arithmetic)
  (scheme process-context)
  (capy))

(gc-logging-enable! #t)

(define checksum 0)

(define (touch x)
  (set! checksum (+ checksum (if (pair? x) 1 0)))
  x)

(define (make-chain n seed)
  (let loop ([i n] [acc seed])
    (if (= i 0)
      acc
      (loop (- i 1) (cons i acc)))))

(define survivor-count 128)
(define survivors (make-vector survivor-count '()))

(define (allocate-batch chains chain-len retain-mod round)
  (let loop ([i 0] [sum 0])
    (if (= i chains)
      sum
      (let ([x (touch (make-chain chain-len i))])
        (if (= (remainder i retain-mod) 0)
          (vector-set! survivors
            (remainder (+ i round) survivor-count)
            x)
          #f)
        (loop (+ i 1) (+ sum i))))))

(define (phase name rounds chains chain-len retain-mod growth)
  (printf "phase ~a~%" name)
  (let loop ([r 0] [c chains] [total 0])
    (if (= r rounds)
      total
      (loop (+ r 1)
        (+ c growth)
        (+ total (allocate-batch c chain-len retain-mod r))))))

(define start (microsecond))

;; Fill GC-time/allocation-rate histories.
(phase "learning" 80 400 32 97 0)

;; Establish a stable allocation-rate baseline.
(phase "steady" 100 700 32 113 0)

;; Monotonic allocation-rate increase: targets acceleration path.
(phase "ramp" 80 200 40 89 60)

;; Briefly lower pressure, then create a burst: targets spike path.
(phase "quiet" 40 120 24 101 0)
(phase "spike" 10 8000 48 83 0)

;; Grow retained data somewhat, changing post-GC free-at-end feedback.
(phase "survivor-pressure" 100 900 48 7 20)

(define end (microsecond))
(printf "checksum ~a~%" checksum)
(printf "elapsed ~a ms~%" (/ (- end start) 1000.0))
