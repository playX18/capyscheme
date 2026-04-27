(library (tests define-property-reexport)
  (export computer answer extra-answer)
  (import (core)
          (core syntax-case)
          (srfi srfi-213)
          (tests define-property-provider))

  (define extra-answer #f)

  (define-property computer extra-answer 43))
