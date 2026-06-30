(import (rnrs)
        (rnrs sorting)
        (srfi 64)
        (capy persistent-map))

(test-begin "capy persistent-map")

(test-equal "empty map has count 0"
  (persistent-map-count (make-persistent-map-eq))
  0)

(test-equal "assoc and ref"
  (let* ([m0 (make-persistent-map-eq)]
         [m1 (persistent-map-assoc m0 'a 1)]
         [m2 (persistent-map-assoc m1 'b 2)])
    (list (persistent-map-count m2)
          (persistent-map-ref m2 'a)
          (persistent-map-ref m2 'b)))
  '(2 1 2))

(test-assert "assoc does not mutate original"
  (let* ([m0 (make-persistent-map-eq)]
         [m1 (persistent-map-assoc m0 'x 10)])
    (and (= (persistent-map-count m0) 0)
         (not (persistent-map-contains? m0 'x))
         (= (persistent-map-count m1) 1))))

(test-equal "dissoc returns map without key"
  (let* ([m0 (persistent-map-assoc (make-persistent-map-eq) 'a 1)]
         [m1 (persistent-map-assoc m0 'b 2)]
         [m2 (persistent-map-dissoc m1 'a)])
    (list (persistent-map-count m2)
          (persistent-map-contains? m2 'a)
          (persistent-map-ref m2 'b)))
  '(1 #f 2))

(test-equal "ref with default"
  (persistent-map-ref (make-persistent-map-eq) 'missing 'not-found)
  'not-found)

(test-equal "alist round trip"
  (let ([m (persistent-map-assoc
             (persistent-map-assoc (make-persistent-map-eq) 'x 1)
             'y 2)])
    (list-sort (lambda (a b) (string<? (symbol->string (car a)) (symbol->string (car b))))
      (persistent-map->alist
        (alist->persistent-map (persistent-map->alist m)))))
  '((x . 1) (y . 2)))

(test-equal "fold accumulates values"
  (persistent-map-fold
    (lambda (acc key value)
      (+ acc value))
    0
    (persistent-map-assoc
      (persistent-map-assoc (make-persistent-map-eq) 'a 1)
      'b 2))
  3)

(test-equal "equal maps compare equal"
  (let* ([m1 (persistent-map-assoc (persistent-map-assoc (make-persistent-map-equal) 'a 1) 'b 2)]
         [m2 (alist->persistent-map '((a . 1) (b . 2)) 'equal)])
    (equal? m1 m2))
  #t)

(test-end)
