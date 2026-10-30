(define-library (scheme eval)
  (import (capy))
  (export environment eval)
  (begin
    ;; A fresh namespace with `import` and the given import specs. R7RS does not
    ;; provide the core bindings here.
    (define (environment . import-specs)
      (let ([ns (make-namespace #f '((only (capy) import)))])
        (for-each (lambda (ispec) (namespace-import! ns ispec)) import-specs)
        (namespace-freeze! ns)
        ns))))
