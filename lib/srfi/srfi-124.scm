(library (srfi srfi-124)
  (export 
    make-ephemeron
    ephemeron-key
    ephemeron-datum
    reference-barrier
    ephemeron?)
  (import (only (capy) make-ephemeron ephemeron-key ephemeron-datum reference-barrier ephemeron?)))