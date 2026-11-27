;; c-include turns `static const` (and plain `const`) objects with constant initializers into
;; Scheme constants, evaluated by the parser with the declared type: the way webgpu.h and other
;; headers declare flags instead of with #define.

(import (rnrs) (srfi 64) (capy c) (core foreign))

;; Tests run in the package directory.
(c-include "static-const.h" #:include-dirs '("tests/capy/_c") #:quote-include #t #:prefix k:)

(test-begin "c-include static const")

(test-group "integers take their declared type"
  (test-equal "zero" 0 k:FLAG_NONE)
  (test-equal "hex" 1 k:FLAG_READ)
  (test-equal "or of constants" 3 k:FLAG_RW)
  (test-equal "cast and shift" (expt 2 40) k:FLAG_HIGH)
  (test-equal "-1 as uint32_t" #xFFFFFFFF k:ALL_BITS)
  (test-equal "enum-typed" 4 k:DEFAULT_MODE)
  (test-equal "enumerator shifted" 8 k:SHIFTED)
  (test-equal "non-static const naming a constant" 9 k:PLAIN)
  (test-equal "sizeof" 4 k:FROM_COUNTER))

(test-group "floats, strings and pointers"
  (test-equal "double" 0.5 k:HALF)
  (test-assert "float rounded to single precision"
    (and (< (abs (- k:THIRD (/ 1.0 3.0))) 1e-7) (not (= k:THIRD (/ 1.0 3.0)))))
  (test-equal "char array" "hello" k:GREETING)
  (test-assert "null pointer" (null-pointer? k:NOTHING)))

(test-end "c-include static const")
