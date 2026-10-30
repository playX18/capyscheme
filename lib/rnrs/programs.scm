(library (rnrs programs (6))
  (export command-line exit)
  (import (core primitives)
    (only (scheme process-context) command-line)))
