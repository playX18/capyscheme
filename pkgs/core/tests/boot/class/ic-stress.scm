(import (srfi 64)
        (rnrs)
        (capy)
        (only (capy concurrency) go))

(test-begin "boot class ic stress")

;; Two readers call a generic while a third thread keeps replacing the
;; method body. A reader must never see the value go backwards, and once the
;; writer is done every reader must observe the final body: a cache fill
;; computed against an old method set must not outlive the clear.

(define-class <st-a> () ())
(define-generic st-f)
(define-method (st-f (o <st-a>)) 0)

(define rounds 300)
(define done 0)
(define lock (make-mutex))

(define (bump-done!)
  (mutex-acquire lock)
  (set! done (+ done 1))
  (mutex-release lock))

(define (done-count)
  (mutex-acquire lock)
  (let ([n done])
    (mutex-release lock)
    n))

(define writer-finished #f)
(define failures '())

(define (note-failure! what)
  (mutex-acquire lock)
  (set! failures (cons what failures))
  (mutex-release lock))

(define (reader)
  (let ([o (make <st-a>)])
    (let loop ([last 0])
      (let ([v (st-f o)])
        (when (< v last) (note-failure! (list 'backwards last v)))
        (if writer-finished
            (let ([final (st-f o)])
              (unless (= final rounds) (note-failure! (list 'stale final))))
            (loop v))))
    (bump-done!)))

(define (writer)
  (do ([i 1 (+ i 1)]) ((> i rounds))
    (let ([k i])
      (define-method (st-f (o <st-a>)) k)))
  (set! writer-finished #t)
  (bump-done!))

(test-group "readers vs. method replacement"
  (go reader)
  (go reader)
  (go writer)
  (let wait ([n 0])
    (when (and (< (done-count) 3) (< n 20000))
      (usleep 1000)
      (wait (+ n 1))))
  (test-equal 3 (done-count))
  (test-equal '() failures)
  (test-equal rounds (st-f (make <st-a>))))

;; A hot slot read races repeated redefinition of its class. Each
;; redefinition changes where `kept` lives, so a read that pairs a cached
;; index with the wrong slots array would index out of bounds or return the
;; wrong slot. Every read must see the migrated value.

(define-values (redef-reader-done redef-writer-done) (values #f #f))
(define redef-failures 0)

(test-group "slot reads vs. class redefinition"
  (define cls (make-class 'st-redef '(kept)))
  (define inst (make-instance cls #:kept 42))
  (define stop #f)
  (define reads 0)
  (define (reader-loop)
    (let loop ([n 0])
      (if stop
          (begin (set! reads n) (set! redef-reader-done #t))
          (begin
            ;; A redefinition between the touch and the protocol dispatch in
            ;; the slow path raises a class/instance mismatch; that is the
            ;; documented price of unsynchronized redefinition. A returned
            ;; value must still be the migrated one.
            (guard (e [#t #f])
              (unless (eqv? (slot-ref inst 'kept) 42)
                (set! redef-failures (+ redef-failures 1))))
            (loop (+ n 1))))))
  (go reader-loop)
  (do ([i 0 (+ i 1)]) ((= i 400))
    (set! cls
      (redefine-class! cls 'st-redef
        (if (even? i) '(pad kept) '(kept)))))
  (set! stop #t)
  (let wait ([n 0])
    (when (and (not redef-reader-done) (< n 20000))
      (usleep 1000)
      (wait (+ n 1))))
  (test-assert redef-reader-done)
  (test-equal 0 redef-failures)
  (test-equal 42 (slot-ref inst 'kept)))

(test-end "boot class ic stress")
