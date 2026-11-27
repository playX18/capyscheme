;; mop/eql: eql specializers take precedence over class specializers and
;; are interned per object.
(import (srfi 64)
        (capy)
        (capy mop))

(test-begin "mop feature eql")

(define-class eq-thing () ())
(define-generic eq-classify)
(define-method (eq-classify (x <top>)) 'other)
(define-method (eq-classify (x (eql 0))) 'zero)
(define-method (eq-classify (x (eql 'sym))) 'symbol)
(define-method (eq-classify (x <number>)) 'number)
(define-method (eq-classify (x eq-thing)) 'thing)

(define (eql-method-for gf n)
  (let loop ([ms (generic-methods gf)])
    (cond
      [(null? ms) #f]
      [(let ([s (car (method-specializers (car ms)))])
         (and (is-a? s <eql-specializer>) (eqv? (eql-specializer-object s) n)))
        (car ms)]
      [else (loop (cdr ms))])))

(test-group "dispatch"
  (test-equal "an eql method beats its class" 'zero (eq-classify 0))
  (test-equal "a non-matching value of the same class uses the class" 'number (eq-classify 7))
  (test-equal "a symbol eql method" 'symbol (eq-classify 'sym))
  (test-equal "another symbol falls through" 'other (eq-classify 'other))
  (test-equal "class methods still apply" 'thing (eq-classify (make eq-thing))))

(test-group "specializer objects"
  (test-assert "eql specializers are interned" 
    (eq? (intern-eql-specializer 0) (intern-eql-specializer 0)))
  (test-equal "the specializer names its object" 0
    (eql-specializer-object (intern-eql-specializer 0)))
  (test-assert "the method's specializer is an eql specializer"
    (is-a? (car (method-specializers (eql-method-for eq-classify 0)))
      <eql-specializer>)))

(test-end "mop feature eql")
