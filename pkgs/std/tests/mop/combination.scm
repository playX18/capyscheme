;; mop/combination and mop/long-combination-subset: the standard method
;; combination, the short form, and the long form subset (decision D7).
(import (srfi 64)
        (capy)
        (capy mop)
        (rnrs eval))

(test-begin "mop feature combination")

(define-class mc-a () ())
(define-class mc-b (mc-a) ())

(define-generic mc-speak)
(define-method (mc-speak (o mc-a)) 'a)
(define-method (mc-speak (o mc-b)) (list 'b (next-method)))

(test-group "standard combination"
  (test-assert "a generic without a combination uses the standard one"
    (method-combination? (generic-method-combination mc-speak)))
  (test-equal "the standard combination is named standard" 'standard
    (method-combination-name (generic-method-combination mc-speak))))

(test-group "primaries chain through next-method"
  (test-equal "the most specific primary runs first" '(b a)
    (mc-speak (make mc-b))))

(define-method-combination mc-tagged
  #:operator (lambda xs (cons 'tagged xs))
  #:identity-with-one-argument #f)
(define-generic mc-tag #:method-combination mc-tagged)
(define-method (mc-tag (o mc-a)) #:mc-tagged 'a)
(define-method (mc-tag (o mc-b)) #:mc-tagged 'b)

(test-group "short form"
  (test-equal "the operator folds the primaries, most specific first"
    '(tagged b a)
    (mc-tag (make mc-b)))
  (test-equal "the combination is found by name" 'mc-tagged
    (method-combination-name (find-method-combination 'mc-tagged))))

(define-method-combination mc-listed ()
  ((around (#:around))
    (primary (#:mc-listed) #:order #:most-specific-first #:required #t))
  (let ([inner (call-method (car primary) (cdr primary))])
    (if (null? around)
      inner
      (call-method (car around) (append (cdr around) (list inner))))))
(define-generic mc-long #:method-combination mc-listed)
(define-method (mc-long (o mc-a)) #:mc-listed (list 'a))
(define-method (mc-long (o mc-b)) #:mc-listed (cons 'b (next-method)))
(define-method (mc-long (o mc-b)) #:around (list 'around (next-method)))

(test-group "long form subset"
  (test-equal "around wraps the primary chain" '(around (b a))
    (mc-long (make mc-b))))

; Known gap (D7): the CLHS :arguments option is parsed and ignored, so the
; variable below keeps its top-level value instead of the call's arguments.
(define args 'top-level)
(define-method-combination mc-args ((arguments args))
  ((p (#:mc-args)))
  (list 'args args))
(define-generic mc-args-gf #:method-combination mc-args)
(define-method (mc-args-gf (o <top>)) #:mc-args 'value)

(test-group "long form gaps"
  (test-expect-fail "the CLHS :arguments option binds the call's arguments")
  (test-equal "the CLHS :arguments option binds the call's arguments"
    '(args (1))
    (mc-args-gf 1)))

(test-end "mop feature combination")
