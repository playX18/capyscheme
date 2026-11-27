;; The implementation is (core persistent-set): the compiler of core uses it.
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
  (import (core persistent-set)))
