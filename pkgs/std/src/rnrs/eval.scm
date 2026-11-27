(library (rnrs eval (6))
  (export eval environment)
  (import (core primitives))

  ;; A fresh namespace that sees the bindings of IMPORT-SPECS only, and is frozen once
  ;; they are in.
  (define (environment . import-specs)
    (let ([ns (make-namespace #f '())])
      (for-each (lambda (spec) (namespace-import! ns spec)) import-specs)
      (namespace-freeze! ns)
      ns)))
