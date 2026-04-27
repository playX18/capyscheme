(library (tests implicit-app-provider)
  (export |#%app|)
  (import (core)
          (core syntax-case))

  (define-syntax |#%app|
    (lambda (stx)
      (syntax-case stx ()
        [(_ rator rand ...)
         #'(quote imported-app-intercepted)]))))
