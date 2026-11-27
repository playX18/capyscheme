;; Per-procedure CFG cleanup pipeline.

(library (core compiler cfg finish)
  (export finish-procedure)
  (import (core) (capy)
    (core compiler cfg rest)
    (core compiler cfg effects)
    (core compiler cfg switch)
    (core compiler cfg cache))

  (define (finish-procedure procedure)
    (lower-rest-arguments procedure)
    (eliminate-dead-instructions procedure)
    (infer-switches procedure)
    (lower-cache-operations procedure)))
