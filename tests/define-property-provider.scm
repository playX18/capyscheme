(library (tests define-property-provider)
  (export computer answer)
  (import (core)
          (core syntax-case)
          (srfi srfi-213))

  (define computer "the-computer")
  (define answer #f)

  (define-property computer answer 42))
