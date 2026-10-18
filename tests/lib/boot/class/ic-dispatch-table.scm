(import (srfi 64)
        (srfi 1)
        (rnrs)
        (capy))

(test-begin "boot class ic dispatch table")

;; Generic dispatch keys on the classes of the arguments that methods
;; specialize, not on all of them, and the key table stays correct when it
;; holds many classes, grows, and is cleared by a new method.

(define-class <dt-base> () ())
(define-generic dt-f)
(define-method (dt-f (o <dt-base>)) 'base)

(define n 200)
(define classes
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1)
              (cons (make-class (string->symbol (string-append "dt-c" (number->string i)))
                                '()
                                (list <dt-base>))
                    acc)))))
(define objs (map make-instance classes))

(define (all-base?)
  (for-all (lambda (o) (eq? (dt-f o) 'base)) objs))

(test-group "many receiver classes"
  (test-assert "cold" (all-base?))
  (do ([i 0 (+ i 1)]) ((= i 20)) (all-base?))
  (test-assert "hot" (all-base?))
  ;; A method on one class takes effect at once, others are unchanged.
  (let ([target (list-ref classes 123)])
    (define-method (dt-f (o target)) 'special)
    (test-eq "new method visible" 'special (dt-f (list-ref objs 123)))
    (test-eq "neighbour unchanged" 'base (dt-f (list-ref objs 124)))
    (test-assert "rest unchanged"
      (for-all (lambda (i) (or (= i 123) (eq? (dt-f (list-ref objs i)) 'base)))
               (iota n)))))

(define-generic dt-v)
(define-method (dt-v (o <dt-base>) . rest) (length rest))

(test-group "variadic calls share keys"
  (let ([o (car objs)])
    (do ([i 0 (+ i 1)]) ((= i 30))
      (test-eqv (string-append "rest " (number->string i)) i
                (apply dt-v o (iota i))))))

(define-generic dt-w)
(define-method (dt-w (o <dt-base>)) 'one)
(define-method (dt-w (o <dt-base>) a) 'two)
(define-method (dt-w (o <dt-base>) a b) 'three)

(test-group "argument count is part of the key"
  (let ([o (car objs)])
    (do ([i 0 (+ i 1)]) ((= i 3))
      (test-eq "1" 'one (dt-w o))
      (test-eq "2" 'two (dt-w o 1))
      (test-eq "3" 'three (dt-w o 1 2)))))

(define-generic dt-t)
(define-method (dt-t (o <dt-base>) b c) (list 'tail b c))

(test-group "arguments past the specialized ones do not matter"
  (let ([o (car objs)])
    (do ([i 0 (+ i 1)]) ((= i 3))
      (test-equal (list 'tail 1 2) (dt-t o 1 2))
      (test-equal (list 'tail "s" #f) (dt-t o "s" #f))
      (test-equal (list 'tail '() 'x) (dt-t o '() 'x))
      (test-equal (list 'tail 1.5 #\a) (dt-t o 1.5 #\a)))))

(define-generic dt-3)
(define-method (dt-3 (a <top>) (b <top>) (c <dt-base>)) 'base)
(define-method (dt-3 (a <top>) (b <top>) (c <fixnum>)) 'int)

(test-group "dispatch on a later argument"
  (let ([o (car objs)])
    (do ([i 0 (+ i 1)]) ((= i 3))
      (test-eq 'base (dt-3 1 2 o))
      (test-eq 'int (dt-3 'x 'y 5))
      (test-eq 'base (dt-3 'p 'q (cadr objs))))))

;; A generic over 200 classes is served from the dispatch table without
;; entering the Rust dispatcher (the machine stub probes the table for
;; `NO_NEXT` methods).
(define (spin f calls)
  (let loop ([i 0] [sum 0])
    (if (= i calls)
        sum
        (loop (+ i 1) (+ sum (f (list-ref objs (modulo i n))))))))

(test-group "table serves calls without the Rust entry"
  (spin (lambda (o) (dt-f o) 1) 4000)
  (%generic-stats! #t)
  (test-eqv "sum" 4000 (spin (lambda (o) (dt-f o) 1) 4000))
  (test-assert "few Rust entries" (< (%generic-rust-entries) 400))
  (%generic-stats! #f))

;; Readers vs. a writer replacing the method, over many classes.
(define rounds 200)
(define lock (make-mutex))
(define done 0)
(define failures '())
(define writer-finished #f)
(define-generic dt-g)
(define-method (dt-g (o <dt-base>)) 0)

(define (note! what)
  (mutex-acquire lock)
  (set! failures (cons what failures))
  (mutex-release lock))

(define (finish!)
  (mutex-acquire lock) (set! done (+ done 1)) (mutex-release lock))

(define (reader seed)
  (lambda ()
    (let loop ([i seed] [last 0])
      (let* ([o (list-ref objs (modulo i n))]
             [v (dt-g o)])
        (when (< v last) (note! (list 'backwards last v)))
        (if writer-finished
            (let ([final (dt-g o)])
              (unless (= final rounds) (note! (list 'stale final))))
            (loop (+ i 7) (if (eq? o (car objs)) v last)))))
    (finish!)))

(define (writer)
  (do ([i 1 (+ i 1)]) ((> i rounds))
    (let ([k i])
      (define-method (dt-g (o <dt-base>)) k)))
  (set! writer-finished #t)
  (finish!))

(test-group "readers vs. method replacement"
  (fork-thread (reader 0))
  (fork-thread (reader 3))
  (fork-thread (reader 11))
  (fork-thread writer)
  (let wait ([spins 0])
    (cond
      [(= (begin (mutex-acquire lock) (let ([d done]) (mutex-release lock) d)) 4) #t]
      [(> spins 200000) #f]
      [else (wait (+ spins 1))]))
  (test-equal "no failures" '() failures)
  (test-eqv "final body" rounds (dt-g (list-ref objs 77))))

;; `make` asks `initialize` per class: a custom method on one class makes
;; only that class (and its subclasses) pay for the call.
(define-class <mk-a> () ((seen #:init-value 0)))
(define-class <mk-sub> (<mk-a>) ())
(define-class <mk-b> () ((seen #:init-value 0)))
(define init-count 0)

(test-group "make and initialize per class"
  (do ([i 0 (+ i 1)]) ((= i 5)) (make <mk-a>) (make <mk-sub>) (make <mk-b>))
  (test-eqv "no initialize yet" 0 init-count)
  (define-method (initialize (o <mk-a>) initargs)
    (set! init-count (+ init-count 1))
    (next-method))
  (make <mk-a>)
  (test-eqv "custom initialize runs" 1 init-count)
  (make <mk-sub>)
  (test-eqv "and for subclasses" 2 init-count)
  (make <mk-b>)
  (test-eqv "not for unrelated classes" 2 init-count)
  (do ([i 0 (+ i 1)]) ((= i 50)) (make <mk-a>) (make <mk-b>))
  (test-eqv "stays on the method" 52 init-count))

(test-end "boot class ic dispatch table")
