(library (tests phase0 define-property-provider)
  (export computer answer)
  (import (core)
          (core syntax-case))

  (define computer "the-computer")
  (define answer #f)

  (define-property computer answer 42))
