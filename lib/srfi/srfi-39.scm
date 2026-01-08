(library (srfi srfi-39)
  (export make-parameter parameterize
    current-input-port
    current-output-port
    current-error-port)
  (import (core parameters) (core primitives)))
