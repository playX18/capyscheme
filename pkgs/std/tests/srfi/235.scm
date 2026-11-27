(import (rnrs)
  (srfi 64)
  (srfi 235))

(test-begin "srfi 235")

(test-group "basic combinators"
  (test-equal 1 ((constantly 1) 'a 'b))
  (test-equal '(1 2) (call-with-values (lambda () ((constantly 1 2))) list))
  (test-equal '() (call-with-values (lambda () ((constantly))) list))
  (test-equal #f ((complement symbol?) 'a))
  (test-equal #t ((complement symbol?) 1))
  (test-equal #t ((complement <) 2 1))
  (test-equal '(2 1 3 4) ((swap list) 1 2 3 4))
  (test-equal '(2 1) ((swap list) 1 2))
  (test-equal '(3 2 1) ((flip list) 1 2 3))
  (test-equal '() ((flip list)))
  (test-equal 1 ((on-left car) '(1) '(2)))
  (test-equal 2 ((on-right car) '(1) '(2))))

(test-group "conjoin and disjoin"
  (test-equal #t ((conjoin number? exact?) 1))
  (test-equal #f ((conjoin number? exact?) 1.5))
  (test-equal #t ((conjoin) 1))
  (test-equal 3 ((conjoin number? (lambda (x) (+ x 2))) 1))
  (test-equal #t ((conjoin < <=) 1 2 3))
  (test-equal #t ((disjoin symbol? number?) 1))
  (test-equal #f ((disjoin symbol? number?) "x"))
  (test-equal #f ((disjoin) 1)))

(test-group "each-of all-of any-of on"
  (let ([log '()])
    ((each-of (lambda (x) (set! log (cons (list 'a x) log)))
              (lambda (x) (set! log (cons (list 'b x) log))))
     1)
    (test-equal '((b 1) (a 1)) log))
  (test-equal #t ((all-of number?) '(1 2 3)))
  (test-equal #f ((all-of number?) '(1 a 3)))
  (test-equal #t ((all-of number?) '()))
  (test-equal 3 ((all-of (lambda (x) (and (number? x) x))) '(1 2 3)))
  (test-equal #t ((any-of symbol?) '(1 a 3)))
  (test-equal #f ((any-of symbol?) '()))
  (test-equal 5 ((on + string-length) "ab" "cde"))
  (test-equal '(1 4 9) ((on list (lambda (x) (* x x))) 1 2 3)))

(test-group "sections and chains"
  (test-equal '(1 2 3 4) ((left-section list 1 2) 3 4))
  (test-equal '(3 4 2 1) ((right-section list 1 2) 3 4))
  (test-equal 2 ((apply-chain car cdr) '(1 2 3)))
  (test-equal '(3) ((apply-chain list +) 1 2 (- 1 2) 1))
  (test-equal 7 ((apply-chain + (lambda (x) (values x 4))) 3))
  (test-equal '(1 2) ((apply-chain) '(1 2))))

(test-group "argument selection"
  (test-equal '(3 4) ((arguments-drop list 2) 1 2 3 4))
  (test-equal '(1 2) ((arguments-drop-right list 2) 1 2 3 4))
  (test-equal '(1 2) ((arguments-take list 2) 1 2 3 4))
  (test-equal '(3 4) ((arguments-take-right list 2) 1 2 3 4)))

(test-group "group-by"
  (test-equal '((1 3 5) (2 4)) ((group-by odd?) '(1 2 3 4 5)))
  (test-equal '(("apple" "avocado") ("banana") ("cherry" "cranberry"))
    ((group-by (lambda (s) (string-ref s 0)) char=?)
     '("apple" "banana" "avocado" "cherry" "cranberry")))
  (test-equal '() ((group-by odd?) '())))

(test-group "procedural control"
  (test-equal 3 (begin-procedure (lambda () 1) (lambda () 2) (lambda () 3)))
  (test-equal 'yes (if-procedure #t (lambda () 'yes) (lambda () 'no)))
  (test-equal 'no (if-procedure #f (lambda () 'yes) (lambda () 'no)))
  (let ([n 0])
    (when-procedure #t (lambda () (set! n (+ n 1))) (lambda () (set! n (+ n 1))))
    (when-procedure #f (lambda () (set! n 100)))
    (unless-procedure #f (lambda () (set! n (+ n 10))))
    (unless-procedure #t (lambda () (set! n 100)))
    (test-equal 12 n))
  (test-equal 4 (value-procedure 2 (lambda (x) (* x 2)) (lambda () 'none)))
  (test-equal 'none (value-procedure #f (lambda (x) x) (lambda () 'none)))
  (test-equal 'two (case-procedure 2 `((1 . ,(lambda () 'one)) (2 . ,(lambda () 'two)))))
  (test-equal 'other (case-procedure 3 `((1 . ,(lambda () 'one))) (lambda () 'other)))
  (test-error (case-procedure 3 `((1 . ,(lambda () 'one))))))

(test-group "and/or procedures"
  (test-equal #t (and-procedure))
  (test-equal 2 (and-procedure (lambda () 1) (lambda () 2)))
  (let ([called #f])
    (test-equal #f (and-procedure (lambda () #f) (lambda () (set! called #t) 1)))
    (test-equal "and-procedure short-circuits" #f called))
  (let ([called #f])
    (test-equal #f (eager-and-procedure (lambda () #f) (lambda () (set! called #t) 1)))
    (test-equal "eager-and-procedure calls every thunk" #t called))
  (test-equal 2 (eager-and-procedure (lambda () 1) (lambda () 2)))
  (test-equal #f (or-procedure))
  (test-equal 1 (or-procedure (lambda () #f) (lambda () 1) (lambda () 2)))
  (let ([called #f])
    (test-equal 1 (eager-or-procedure (lambda () 1) (lambda () (set! called #t) 2)))
    (test-equal #t called)))

(test-group "loops"
  (test-equal 5 (funcall-procedure (lambda () 5)))
  (let ([n 0])
    (while-procedure (lambda () (set! n (+ n 1)) (< n 5)))
    (test-equal 5 n))
  (let ([n 0])
    (until-procedure (lambda () (set! n (+ n 1)) (>= n 7)))
    (test-equal 7 n))
  (test-equal 10
    (call/cc
      (lambda (k)
        (let ([n 0])
          (loop-procedure (lambda () (set! n (+ n 1)) (when (= n 10) (k n)))))))))

(test-group "constants"
  (test-equal #t (always))
  (test-equal #t (always 1 2))
  (test-equal #f (never 'x))
  (test-equal #t (boolean 'x))
  (test-equal #f (boolean #f)))

(test-end "srfi 235")
