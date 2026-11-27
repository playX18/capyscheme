;; P2.1 micro benchmark: record field read, mutation, predicate, construction (4 fields).
;;   cd pkgs && XDG_CACHE_HOME=$(mktemp -d) \
;;     ../stage-2/capy-rt -L core/src -s core/tools/p21-records-bench.scm
;; P21_N (default 3000000) iterations per measurement, P21_ROUNDS (default 5), median reported.
(import (rnrs) (capy))
(define (env-int name default)
  (let ([v (getenv name)]) (if v (string->number v) default)))
(define N (env-int "P21_N" 3000000))
(define ROUNDS (env-int "P21_ROUNDS" 5))

(define (median l) (list-ref (list-sort < l) (quotient (length l) 2)))

(define (run name thunk)
  (when (let ([only (getenv "P21_ONLY")]) (or (not only) (string=? only name)))
  (thunk) (thunk) (thunk)
  (let ([times (let loop ([i 0] [acc '()])
                 (if (= i ROUNDS) acc
                   (let* ([t0 (current-jiffy)] [r (thunk)] [t1 (current-jiffy)])
                     (loop (+ i 1) (cons (- t1 t0) acc)))))])
    (let ([ns (/ (* 1e9 (median times)) (jiffies-per-second) N)])
      (display name) (display " ") (display (/ (round (* 10 ns)) 10.0)) (display " ns/iter") (newline) (flush-output-port (current-output-port))))))


;; The benchmark code is a body, so its procedures are known to the compiler (letrec*) and
;; its record accessors can be inlined, as in a library.
(define (main)
(define-record-type point (fields (mutable x) (mutable y) z w))
(define-record-type (point3 make-point3 point3?) (parent point) (fields (mutable q)))
(define-record-type (sealed-pt make-sealed-pt sealed-pt?) (sealed #t) (fields (mutable a) b))
(define-record-type (pp make-pp pp?) (fields a b)
  (protocol (lambda (n) (lambda (a b) (n a (+ b 1))))))

(define p (make-point 1 2 3 4))
(define p3 (make-point3 1 2 3 4 5))
(define s (make-sealed-pt 1 2))
(define notp (vector 1 2 3))

(define (bench-read)
  (let loop ([i 0] [acc 0])
    (if (= i N) acc
      (loop (+ i 1) (+ acc (point-x p) (point-y p) (point-z p) (point-w p))))))
(define (bench-read-sub)
  (let loop ([i 0] [acc 0])
    (if (= i N) acc
      (loop (+ i 1) (+ acc (point-x p3) (point3-q p3))))))
(define (bench-read-sealed)
  (let loop ([i 0] [acc 0])
    (if (= i N) acc
      (loop (+ i 1) (+ acc (sealed-pt-a s) (sealed-pt-b s))))))
(define (bench-write)
  (let loop ([i 0])
    (if (= i N) (point-x p)
      (begin (point-x-set! p i) (point-y-set! p i) (loop (+ i 1))))))
(define (bench-pred)
  (let loop ([i 0] [acc 0])
    (if (= i N) acc
      (loop (+ i 1) (if (and (point? p) (point? p3) (not (point? notp)) (sealed-pt? s)) (+ acc 1) acc)))))
(define sink #f)
(define (bench-make)
  (let loop ([i 0])
    (if (= i N) sink
      (begin (set! sink (make-point i i i i)) (loop (+ i 1))))))
(define (bench-make-protocol)
  (let loop ([i 0])
    (if (= i N) sink
      (begin (set! sink (make-pp i i)) (loop (+ i 1))))))

  (run "read x4 (root type)" bench-read)
(run "read x2 (subtype instance)" bench-read-sub)
(run "read x2 (sealed)" bench-read-sealed)
(run "write x2" bench-write)
(run "predicate x4" bench-pred)
(run "construct 4 fields" bench-make)
(run "construct protocol" bench-make-protocol)
)
(main)
