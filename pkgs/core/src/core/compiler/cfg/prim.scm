;; Primitive access for the CFG passes.

(library (core compiler cfg prim)
  (export prim=? prim-name prim-name-string)
  (import (core) (capy))

  (define (prim=? a b) (eq? a b))
  (define (prim-name p) p)
  (define (prim-name-string p) (symbol->string p)))
