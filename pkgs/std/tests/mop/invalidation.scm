;; mop/invalidation: adding and removing methods, class redefinition with its
;; subclass fan-out, and the dependents protocol.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature invalidation")

(define (method-on gf class)
  (let loop ([ms (generic-methods gf)])
    (cond
      [(null? ms) #f]
      [(eq? (car (method-specializers (car ms))) class) (car ms)]
      [else (loop (cdr ms))])))

(define (dependents-of metaobject)
  (let ([acc '()])
    (map-dependents metaobject (lambda (d) (set! acc (cons d acc))))
    acc))

(define-class inv-base () ())
(define-class inv-mid (inv-base) ())
(define-class inv-leaf (inv-mid) ())
(define-generic inv-f)
(define-method (inv-f (o inv-base)) 'base)
(define-method (inv-f (o inv-mid)) 'mid)

(test-group "method changes"
  (test-equal "a method answers before the change" 'mid (inv-f (make inv-leaf)))
  (test-assert "remove-method! takes a method out"
    (remove-method! inv-f (method-on inv-f inv-mid)))
  (test-equal "the change is visible at once" 'base (inv-f (make inv-leaf))))

(define-class inv-slotted (inv-base) ((v #:init-value 1)))
(define-generic inv-slot)
(define-method (inv-slot (o inv-slotted)) (slot-ref o 'v))

(test-group "class redefinition"
  (test-equal "the method reads the slot" 1 (inv-slot (make inv-slotted))))

(define-class inv-slotted (inv-base) ((v #:init-value 2)))

(test-group "after redefinition"
  (test-equal "the method reads the new default" 2
    (inv-slot (make inv-slotted))))

(define-class inv-dep-target () ())
(define inv-watcher (list 'watcher))

(test-group "dependents"
  (test-assert "add-dependent records a dependent"
    (begin (add-dependent inv-dep-target inv-watcher)
      (memq inv-watcher (dependents-of inv-dep-target))))
  (test-assert "adding the same dependent twice keeps one"
    (begin (add-dependent inv-dep-target inv-watcher)
      (= 1 (length (dependents-of inv-dep-target)))))
  (test-assert "update-dependent defaults to a no-op"
    (not (update-dependent inv-dep-target inv-watcher 'changed)))
  (test-assert "remove-dependent removes it"
    (begin (remove-dependent inv-dep-target inv-watcher)
      (not (memq inv-watcher (dependents-of inv-dep-target))))))

(test-end "mop feature invalidation")
