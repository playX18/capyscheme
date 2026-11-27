;; mop/library: (capy mop) binds its names, the feature symbols are listed,
;; and cond-expand sees the capy-mop feature.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature library")

(define (every-member? items expected)
  (or (null? expected)
      (and (memq (car expected) items)
           (every-member? items (cdr expected)))))

(test-group "cond-expand"
  (test-equal "capy-mop is a cond-expand feature" #t
    (cond-expand (capy-mop #t) (else #f)))
  (test-equal "capy-mop-partial is gone at conformance" #f
    (cond-expand (capy-mop-partial #t) (else #f))))

(test-group "feature symbols"
  (test-assert "every landed phase appends its symbol"
    (let ([features (capy-mop-features)])
      (every-member? features
        '(mop/library mop/combination mop/long-combination-subset
          mop/invocation mop/class-protocol mop/dfun mop/custom-dfun
          mop/slot-protocol mop/sealing mop/precompute mop/metaclass
          mop/resolved-keys mop/cpl mop/default-initargs mop/forward-classes
          mop/eql mop/qualifiers mop/invalidation))))
  (test-assert "the list has no duplicates"
    (let loop ([xs (capy-mop-features)])
      (or (null? xs)
          (and (not (memq (car xs) (cdr xs)))
               (loop (cdr xs)))))))

(test-group "library names"
  (test-assert "standard-class aliases class" (eq? <standard-class> <class>))
  (test-assert "class-precedence-list is bound" (procedure? class-precedence-list))
  (test-assert "capy-mop-features is bound" (procedure? capy-mop-features)))

(test-end "mop feature library")
