;;; The program `capy`.

(import (rnrs) (capy pkg cli))

(exit (run-cli (cdr (command-line))))
