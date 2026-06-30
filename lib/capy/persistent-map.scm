(library (capy persistent-map)
  (export
    persistent-map?
    make-persistent-map-eq
    make-persistent-map-eqv
    make-persistent-map-equal
    persistent-map-ref
    persistent-map-assoc
    persistent-map-dissoc
    persistent-map-contains?
    persistent-map-count
    persistent-map->alist
    alist->persistent-map
    persistent-map-fold
    persistent-map-for-each)
  (import (rnrs)
          (only (capy)
            core-persistent-map?
            make-core-persistent-map-eq
            make-core-persistent-map-eqv
            make-core-persistent-map-equal
            core-persistent-map-ref
            core-persistent-map-assoc
            core-persistent-map-dissoc
            core-persistent-map-contains?
            core-persistent-map-count
            core-persistent-map->alist
            alist->core-persistent-map-eq
            alist->core-persistent-map-eqv
            alist->core-persistent-map-equal)))

  (define (persistent-map? obj)
    (core-persistent-map? obj))

  (define make-persistent-map-eq make-core-persistent-map-eq)
  (define make-persistent-map-eqv make-core-persistent-map-eqv)
  (define make-persistent-map-equal make-core-persistent-map-equal)

  (define (persistent-map-ref map key . default)
    (apply core-persistent-map-ref map key default))

  (define persistent-map-assoc core-persistent-map-assoc)
  (define persistent-map-dissoc core-persistent-map-dissoc)
  (define persistent-map-contains? core-persistent-map-contains?)
  (define persistent-map-count core-persistent-map-count)
  (define persistent-map->alist core-persistent-map->alist)

  (define (alist->persistent-map alist . comparator)
    (cond
      [(null? comparator) (alist->core-persistent-map-eq alist)]
      [(eq? (car comparator) 'eq) (alist->core-persistent-map-eq alist)]
      [(eq? (car comparator) 'eqv) (alist->core-persistent-map-eqv alist)]
      [(eq? (car comparator) 'equal) (alist->core-persistent-map-equal alist)]
      [else (alist->core-persistent-map-eq alist)]))

  (define (persistent-map-fold proc init map)
    (let loop ((pairs (persistent-map->alist map))
               (acc init))
      (if (null? pairs)
        acc
        (let ((pair (car pairs)))
          (loop (cdr pairs)
            (proc acc (car pair) (cdr pair)))))))

  (define (persistent-map-for-each proc map)
    (persistent-map-fold
      (lambda (acc key value)
        (proc key value)
        acc)
      #f
      map))
