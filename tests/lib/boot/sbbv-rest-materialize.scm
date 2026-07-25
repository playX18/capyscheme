(import (srfi 64)
        (capy))

(test-begin "boot sbbv rest materialize")

(test-group "call/cc escape returning apply-values thunk"
  ;; Direct shape of the original failure path (gk26/gf42/gf40):
  ;; call/cc returns a nullary thunk; the continuation immediately applies it;
  ;; the thunk does `(apply values <list>)` into a one-value receiver.
  (test-equal "single value via apply values list"
    ((call-with-current-continuation
       (lambda (k)
         (k (lambda () (apply values (list 42)))))))
    42)

  (test-equal "multiple values via apply values list"
    (call-with-values
      (lambda ()
        ((call-with-current-continuation
           (lambda (k)
             (k (lambda () (apply values (list 1 2 3))))))))
      list)
    '(1 2 3))

  (test-equal "empty apply values yields zero values"
    (call-with-values
      (lambda ()
        ((call-with-current-continuation
           (lambda (k)
             (k (lambda () (apply values (list))))))))
      (lambda args args))
    '()))

(test-group "apply values without call/cc"
  (test-equal "apply values one element"
    (call-with-values (lambda () (apply values (list 'a))) list)
    '(a))

  (test-equal "apply values several elements"
    (call-with-values (lambda () (apply values (list 1 2 3 4))) list)
    '(1 2 3 4))

  (test-equal "apply values empty list"
    (call-with-values (lambda () (apply values (list))) (lambda args args))
    '())

  (test-equal "apply values with register-filling prefix"
    (call-with-values (lambda () (apply values 1 (list 2 3))) list)
    '(1 2 3)))

(test-group "rest formal used vs unused"
  (test-equal "unused rest formal still accepts extra args"
    ((lambda (a b . rest) (list a b)) 1 2 3 4)
    '(1 2))

  (test-equal "used rest formal preserves extras"
    ((lambda (a b . rest) (list a b rest)) 1 2 3 4)
    '(1 2 (3 4)))

  (test-equal "rest-only lambda receives all args"
    ((lambda args args) 1 2 3)
    '(1 2 3))

  (test-equal "rest-only lambda with zero args"
    ((lambda args args))
    '()))

(test-group "values into continuations"
  (test-equal "zero values into rest-only receiver"
    (call-with-values (lambda () (values)) (lambda args args))
    '())

  (test-equal "one value into fixed receiver"
    (call-with-values (lambda () (values 7)) (lambda (x) x))
    7)

  (test-equal "two values into fixed receiver"
    (call-with-values (lambda () (values 'x 'y)) list)
    '(x y))

  (test-error "zero values into one-arg receiver"
    &assertion-violation
    (call-with-values (lambda () (values)) (lambda (x) x))))

(test-group "srfi-64 canary"
  ;; The original user-facing failure: test-equal expands through paths that
  ;; compile call/cc + apply/values under SBBV.
  (test-equal "simple" 1 1)
  (test-equal "symbols" 'ok 'ok)
  (test-assert "true" #t))

(test-group "rest formal case length dispatch"
  ;; Direct (lambda args (case (length args) ...)) shape — not case-lambda.
  ;; Rest lowering runs before SBBV so this stays on RestLength/RestRef even
  ;; when SBBV would otherwise expand `car` into pair?/car/unchecked.
  (define (foo . x)
    (case (length x)
      ((0) 'zero)
      ((1) (car x))
      ((2) 'two)
      (else 'many)))

  (test-equal "case length zero" 'zero (foo))
  (test-equal "case length one" 'a (foo 'a))
  (test-equal "case length two" 'two (foo 'a 'b))
  (test-equal "case length many" 'many (foo 'a 'b 'c)))

(test-group "quoted literal hygiene"
  ;; Mutating a freshly allocated list must not poison later quoted constants
  ;; (see prim.scm filter-group regression).
  (let ((circular (list 1 2 3)))
    (set-cdr! circular circular)
    (test-assert "mutated list is circular"
      (eq? circular (cdr circular)))
    (test-equal "quoted literal remains proper"
      '(1 2 3)
      '(1 2 3))
    (test-assert "quoted literal is not the circular object"
      (not (eq? circular '(1 2 3))))))

(test-end)
