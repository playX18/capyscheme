;; Array fields inside by-value structs, described as (vector T N) nested in a list
;; (what c-include emits for e.g. `struct { Shader s; float params[4]; }`).

(import (rnrs)
        (srfi 64)
        (core foreign)
        (core foreign-library))

(test-begin "foreign nested vector")

(test-group "sizeof and alignof"
  (test-equal "alignof vector" 4 (alignof (vector float 4)))
  (test-equal "alignof struct with array field" 8 (alignof (list '* (vector float 4))))
  (test-equal "alignof struct of float array" 4 (alignof (list (vector float 4))))
  (test-equal "sizeof struct with array field" 24 (sizeof (list '* (vector float 4)))))

(define libc (load-foreign-library #f))

(test-group "by-value call"
  ;; {int32 v[2]} is passed in one integer register, like the int64 llabs takes.
  (let ([llabs* (foreign-library-function libc "llabs"
                  #:return int64
                  #:arguments (list (list (vector int32 2)))
                  #:blocking? #f)]
        [bv (make-bytevector 8 0)])
    (bytevector-s64-native-set! bv 0 -12345)
    (test-equal "array-in-struct argument" 12345 (llabs* bv))))

(test-end "foreign nested vector")
