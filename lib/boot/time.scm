(define %time-loads? (make-parameter #f))

(define (statistics)
  (let ([gc-stats (gc-statistics)])
    (list (cpu-time)
          (real-time)
          (vector-ref gc-stats 1)
          (vector-ref gc-stats 0)
          (vector-ref gc-stats 3)
          (vector-ref gc-stats 4)
          (vector-ref gc-stats 2))))

(define (sstats-cpu s) (list-ref s 0))
(define (sstats-real s) (list-ref s 1))
(define (sstats-bytes s) (list-ref s 2))
(define (sstats-gc-count s) (list-ref s 3))
(define (sstats-gc-cpu s) (list-ref s 4))
(define (sstats-gc-real s) (list-ref s 5))
(define (sstats-gc-bytes s) (list-ref s 6))

(define (sstats-difference a b)
  (list (- (sstats-cpu a) (sstats-cpu b))
        (- (sstats-real a) (sstats-real b))
        (max 0 (- (sstats-bytes a) (sstats-bytes b)))
        (max 0 (- (sstats-gc-count a) (sstats-gc-count b)))
        (max 0 (- (sstats-gc-cpu a) (sstats-gc-cpu b)))
        (max 0 (- (sstats-gc-real a) (sstats-gc-real b)))
        (max 0 (- (sstats-gc-bytes a) (sstats-gc-bytes b)))))

(define (sstats-print s . maybe-port)
  (let ([port (if (null? maybe-port) (console-output-port) (car maybe-port))]
        [collections (sstats-gc-count s)])
    (if (= collections 0)
      (fprintf port
        "    no collections~%    ~a ms elapsed cpu time~%    ~a ms elapsed real time~%    ~a bytes allocated~%"
        (sstats-cpu s)
        (sstats-real s)
        (sstats-bytes s))
      (fprintf port
        "    ~a ~a~%    ~a ms elapsed cpu time, including ~a ms collecting~%    ~a ms elapsed real time, including ~a ms collecting~%    ~a bytes allocated, including ~a bytes reclaimed~%"
        collections
        (if (= collections 1) "collection" "collections")
        (sstats-cpu s)
        (sstats-gc-cpu s)
        (sstats-real s)
        (sstats-gc-real s)
        (sstats-bytes s)
        (sstats-gc-bytes s)))))

;; Runs thunk `t`, prints timing statistics labelled with datum `e` to the
;; console output port, and returns the thunk's values.
(define ($as-time-goes-by e t)
  (define (sanitize s)
    (list (max 0 (sstats-cpu s))
          (max 0 (sstats-real s))
          (max 0 (sstats-bytes s))
          (max 0 (sstats-gc-count s))
          (max 0 (sstats-gc-cpu s))
          (max 0 (sstats-gc-real s))
          (max 0 (sstats-gc-bytes s))))
  (define (prstats b1 b2)
    (let ([a (statistics)])
      ;; Echo `(time <expr>)`, bounding the print depth and restoring it.
      (let ([old-level (print-level)] [old-length (print-length)])
        (dynamic-wind
          (lambda () (print-level 2) (print-length 2))
          (lambda () (fprintf (console-output-port) "(time ~s)~%" e))
          (lambda () (print-level old-level) (print-length old-length))))
      ;; Subtract the measurement overhead (b2 - b1) from the thunk's own
      ;; stats, mirroring Chez's `$as-time-goes-by`.
      (let ([elapsed (sstats-difference a b2)])
        (let ([overhead (sstats-difference b2 b1)])
          (let ([adjusted (sanitize (sstats-difference elapsed overhead))])
            (sstats-print adjusted (console-output-port)))))
      (flush-output-port (console-output-port))))
  (let ([b1 (statistics)])
    (let ([b2 (statistics)])
      (call-with-values t
        (lambda vs
          (prstats b1 b2)
          (apply values vs))))))
