(library (srfi srfi-157)
  (export
    with-continuation-mark
    current-continuation-marks
    continuation-marks?
    continuation-mark-set->list
    continuation-mark-set->list*
    continuation-mark-set-first
    continuation-marks)
  (import (core primitives)))
