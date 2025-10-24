
(library (rnrs eval (6))
  (export eval environment)
  (import (core primitives))
  
  (define (environment . import-specs)
    (define module (make-module))
    (define needs-purify? (not (member '(capy) import-specs)))
    (for-each (lambda (spec)
      (eval (list 'import spec) module))
      import-specs)
    (if needs-purify? 
      (set-module-uses! (cdr (module-uses module))))
    module))
