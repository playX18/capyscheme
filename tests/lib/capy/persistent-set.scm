(import (rnrs)
        (rnrs sorting)
        (srfi 64)
        (capy persistent-set))

(test-begin "capy persistent-set")

(test-equal "empty set has count 0"
  (persistent-set-count (make-persistent-set-eq))
  0)

(test-equal "add and contains"
  (let* ([s0 (make-persistent-set-eq)]
         [s1 (persistent-set-add s0 'a)]
         [s2 (persistent-set-add s1 'b)])
    (list (persistent-set-count s2)
          (persistent-set-contains? s2 'a)
          (persistent-set-contains? s2 'b)
          (persistent-set-contains? s2 'c)))
  '(2 #t #t #f))

(test-assert "add does not mutate original"
  (let* ([s0 (make-persistent-set-eq)]
         [s1 (persistent-set-add s0 'x)])
    (and (= (persistent-set-count s0) 0)
         (not (persistent-set-contains? s0 'x))
         (= (persistent-set-count s1) 1))))

(test-equal "remove drops element"
  (let* ([s0 (persistent-set-add (make-persistent-set-eq) 'a)]
         [s1 (persistent-set-add s0 'b)]
         [s2 (persistent-set-remove s1 'a)])
    (list (persistent-set-count s2)
          (persistent-set-contains? s2 'a)
          (persistent-set-contains? s2 'b)))
  '(1 #f #t))

(test-equal "list round trip"
  (let ([s (persistent-set-add
             (persistent-set-add (make-persistent-set-eq) 'a)
             'b)])
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
      (persistent-set->list
        (list->persistent-set (persistent-set->list s) 'eq))))
  '(a b))

(test-equal "union"
  (let ([s1 (list->persistent-set '(a b) 'eq)]
        [s2 (list->persistent-set '(b c) 'eq)])
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
      (persistent-set->list (persistent-set-union s1 s2))))
  '(a b c))

(test-equal "intersection"
  (let ([s1 (list->persistent-set '(a b c) 'eq)]
        [s2 (list->persistent-set '(b c d) 'eq)])
    (list-sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
      (persistent-set->list (persistent-set-intersection s1 s2))))
  '(b c))

(test-equal "difference"
  (let ([s1 (list->persistent-set '(a b c) 'eq)]
        [s2 (list->persistent-set '(b c d) 'eq)])
    (persistent-set->list (persistent-set-difference s1 s2)))
  '(a))

(test-end)
