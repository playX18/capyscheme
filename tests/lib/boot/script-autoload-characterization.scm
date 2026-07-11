;; Run directly with:
;;   capy -L lib --fresh-auto-compile --script tests/lib/boot/script-autoload-characterization.scm
;;
;; `make test` uses the equivalent short `-s` spelling.
(import (core)
        (tests phase0 autoload-provider))

(define (check name ok?)
  (if ok?
    (begin (display "PASS ") (display name) (newline))
    (begin (display "FAIL ") (display name) (newline) (exit 1))))

(define autoloaded-result (autoloaded-call 'ok))
(define autoloaded-call-ok?
  (and (pair? autoloaded-result)
       (eq? (car autoloaded-result) 'phase0-autoloaded)
       (pair? (cdr autoloaded-result))
       (eq? (cadr autoloaded-result) 'ok)
       (null? (cddr autoloaded-result))))

(check "autoloaded library binding is available in script" (eq? autoloaded-value 'phase0-autoloaded))
(check "autoloaded procedure runs in script" autoloaded-call-ok?)
