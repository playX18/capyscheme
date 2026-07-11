(library (tests phase0 define-property-reexport)
  (export computer answer extra-answer)
  (import (core)
          (core syntax-case)
          (tests phase0 define-property-provider))

  (define extra-answer #f)

  (define-property computer extra-answer 43))
