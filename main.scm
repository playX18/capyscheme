(library (main)
(export #%app)
(import (rnrs) (prefix (only (capy) #%app printf) orig-))

(define-syntax #%app
  (syntax-rules () 
    [(_ expr ...)
      (begin
        (|orig-#%app| orig-printf "at ~s~%" '(expr ...))
        (|orig-#%app| expr ...))]))
)