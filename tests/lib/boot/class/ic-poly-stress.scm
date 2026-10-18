(import (srfi 64)
        (rnrs)
        (capy))

(test-begin "boot class ic poly stress")

;; Six classes, each with the slot at a different index, are read through
;; one site by several threads. The site holds the first class inline and
;; three more in its extra entries, so the other two keep missing and
;; refilling while the hot ones hit. Every read must return the value of
;; its own object: a torn or stale entry would read the wrong index.

(define-class <ps-0> () ((v #:init-value 0)))
(define-class <ps-1> () ((pad1 #:init-value -1) (v #:init-value 1)))
(define-class <ps-2> () ((pad1 #:init-value -1) (pad2 #:init-value -1) (v #:init-value 2)))
(define-class <ps-3> () ((a #:init-value -1) (b #:init-value -1) (c #:init-value -1) (v #:init-value 3)))
(define-class <ps-4> () ((a #:init-value -1) (b #:init-value -1) (c #:init-value -1) (d #:init-value -1) (v #:init-value 4)))
(define-class <ps-5> () ((a #:init-value -1) (b #:init-value -1) (c #:init-value -1) (d #:init-value -1) (e #:init-value -1) (v #:init-value 5)))

(define objs
  (vector (make-instance <ps-0>) (make-instance <ps-1>) (make-instance <ps-2>)
          (make-instance <ps-3>) (make-instance <ps-4>) (make-instance <ps-5>)))

(define (read-v o) (slot-ref o 'v))

(define lock (make-mutex))
(define finished 0)
(define bad 0)

(define (worker seed)
  (lambda ()
    (let loop ([i 0] [k seed])
      (when (< i 40000)
        (let* ([n (modulo k 6)]
               [v (read-v (vector-ref objs n))])
          (unless (= v n)
            (mutex-acquire lock) (set! bad (+ bad 1)) (mutex-release lock)))
        (loop (+ i 1) (+ k 1 (modulo i 3)))))
    (mutex-acquire lock) (set! finished (+ finished 1)) (mutex-release lock)))

(do ([t 0 (+ t 1)]) ((= t 4)) (fork-thread (worker t)))

(let wait ()
  (mutex-acquire lock)
  (let ([f finished]) (mutex-release lock)
    (when (< f 4) (usleep 2000) (wait))))

(test-equal "no torn or stale reads" 0 bad)

(test-end "boot class ic poly stress")
