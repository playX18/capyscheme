
(define-library (scheme eval)
  (import (core primitives))
  (export environment eval)
(begin
  (define (environment . import-specs)
    (define module (make-module))
    (beautify-user-module! module)
    (purify-module! module)

    (module-use! module (resolve-interface '(capy) '(import) '() #f))
    (for-each (lambda (ispec)
      (eval (list 'import ispec) module))
      import-specs)
    (set-module-uses! module (cdr (module-uses module)))
    (core-hash-clear! (module-import-obarray module))
    module)))
