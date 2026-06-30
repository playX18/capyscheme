(library (capy persistent-set)
  (export
    persistent-set?
    make-persistent-set-eq
    make-persistent-set-eqv
    make-persistent-set-equal
    persistent-set-contains?
    persistent-set-add
    persistent-set-remove
    persistent-set-count
    persistent-set->list
    list->persistent-set
    persistent-set-union
    persistent-set-intersection
    persistent-set-difference)
  (import (rnrs)
          (only (capy)
            core-persistent-set?
            make-core-persistent-set-eq
            make-core-persistent-set-eqv
            make-core-persistent-set-equal
            core-persistent-set-contains?
            core-persistent-set-add
            core-persistent-set-remove
            core-persistent-set-count
            core-persistent-set->list
            list->core-persistent-set-eq
            list->core-persistent-set-eqv
            list->core-persistent-set-equal
            core-persistent-set-union
            core-persistent-set-intersection
            core-persistent-set-difference)))

  (define (persistent-set? obj)
    (core-persistent-set? obj))

  (define make-persistent-set-eq make-core-persistent-set-eq)
  (define make-persistent-set-eqv make-core-persistent-set-eqv)
  (define make-persistent-set-equal make-core-persistent-set-equal)

  (define persistent-set-contains? core-persistent-set-contains?)
  (define persistent-set-add core-persistent-set-add)
  (define persistent-set-remove core-persistent-set-remove)
  (define persistent-set-count core-persistent-set-count)
  (define persistent-set->list core-persistent-set->list)

  (define (list->persistent-set lst . comparator)
    (cond
      [(null? comparator) (list->core-persistent-set-eq lst)]
      [(eq? (car comparator) 'eq) (list->core-persistent-set-eq lst)]
      [(eq? (car comparator) 'eqv) (list->core-persistent-set-eqv lst)]
      [(eq? (car comparator) 'equal) (list->core-persistent-set-equal lst)]
      [else (list->core-persistent-set-eq lst)]))

  (define persistent-set-union core-persistent-set-union)
  (define persistent-set-intersection core-persistent-set-intersection)
  (define persistent-set-difference core-persistent-set-difference)
