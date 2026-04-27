(library (tests implicit-app-consumer)
  (export imported-result imported-primitive-result)
  (import (core)
          (core syntax-case)
          (tests implicit-app-provider))

  (define imported-result (anything 1 2))
  (define imported-primitive-result (+ 1 2)))
