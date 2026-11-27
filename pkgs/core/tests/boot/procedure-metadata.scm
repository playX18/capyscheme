(import (core) (capy) (srfi 13) (srfi 64))

(test-begin "procedure-metadata")

;; Native closures carry attached metadata (source, documentation) built by
;; capy-derive. The keys must be symbols, like the expander stores for Scheme
;; closures, or `procedure-property` lookups miss. Regression test: the derive
;; macro once emitted string keys, making every native doc unreachable.
(test-group "native metadata"
  (test-assert "native metadata keys are symbols"
    (symbol? (caar (procedure-properties car))))
  (test-assert "native documentation reachable"
    (string? (procedure-documentation car)))
  (test-assert "native property lookup by symbol"
    (string? (procedure-property car 'documentation)))
  (test-assert "native documentation keeps user docs"
    (string-contains (procedure-documentation procedure-property) "procedure-property"))
  (test-assert "native source reachable"
    (vector? (procedure-sourcev car))))

(test-group "scheme metadata"
  (define (documented x)
    "Adds one."
    (+ x 1))
  (define (undocumented x)
    (+ x 1))
  (test-equal "scheme documentation"
    (procedure-documentation documented) "Adds one.")
  (test-equal "scheme name"
    (procedure-name documented) 'documented)
  (test-equal "missing scheme documentation is #f"
    (procedure-documentation undocumented) #f))

(test-end "procedure-metadata")
