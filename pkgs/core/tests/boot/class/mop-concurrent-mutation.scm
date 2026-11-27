(import (srfi 64)
        (rnrs)
        (capy)
        (only (capy concurrency) go))

(test-begin "boot class concurrent metaobject mutation")

;; Threads mutate one generic and one class at once. Without the metaobject lock the
;; read-copy-write of the method array and of the dependents list loses updates.

(define threads 4)
(define per-thread 200)

(define-generic cm-gen 1)
(define-class <cm-class> () ())

(define lock (make-mutex))
(define finished 0)

(define (done!)
  (mutex-acquire lock) (set! finished (+ finished 1)) (mutex-release lock))

(define (wait-for-all)
  (let wait ()
    (mutex-acquire lock)
    (let ([f finished])
      (mutex-release lock)
      (when (< f threads) (usleep 2000) (wait))))
  (mutex-acquire lock) (set! finished 0) (mutex-release lock))

(define (run-all body)
  (do ([t 0 (+ t 1)]) ((= t threads))
    (let ([t t]) (go (lambda () (body t) (done!)))))
  (wait-for-all))

(define baseline (length (generic-methods cm-gen)))

(run-all
 (lambda (t)
   (do ([i 0 (+ i 1)]) ((= i per-thread))
     (let ([key (+ (* t per-thread) i)])
       (add-method! cm-gen (list (intern-eql-specializer key)) 1
                    (lambda (next x) key))))))

(test-equal "every method added by every thread is there"
  (+ baseline (* threads per-thread))
  (length (generic-methods cm-gen)))

(test-equal "each one dispatches" '(0 399 799)
  (list (cm-gen 0) (cm-gen 399) (cm-gen 799)))

(define dependents
  (do ([i 0 (+ i 1)] [acc '() (cons (list 'dep i) acc)]) ((= i (* threads per-thread)) acc)))

(run-all
 (lambda (t)
   (do ([i 0 (+ i 1)]) ((= i per-thread))
     (add-dependent <cm-class> (list-ref dependents (+ (* t per-thread) i))))))

(test-equal "every dependent added by every thread is there"
  (* threads per-thread)
  (let ([n 0]) (map-dependents <cm-class> (lambda (d) (set! n (+ n 1)))) n))

(test-end)
