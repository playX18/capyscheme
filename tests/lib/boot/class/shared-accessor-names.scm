(import (srfi 64)
        (capy))

(test-begin "shared accessor names")

;; Several classes may name the same generic in #:accessor / #:getter /
;; #:setter; each define-class adds a method instead of replacing the generic.
(define-class <sa-person> () ((pname #:init-keyword #:n #:accessor sa-label #:getter sa-get #:setter sa-put!)))
(define-class <sa-city> () ((title #:init-keyword #:n #:accessor sa-label #:getter sa-get #:setter sa-put!)))
(define-class <sa-fixed> () ((v #:init-value 7 #:accessor sa-label)))

(define person (make <sa-person> #:n "ann"))
(define city (make <sa-city> #:n "oslo"))

(test-group "readers"
  (test-equal "accessor, first class" "ann" (sa-label person))
  (test-equal "accessor, second class" "oslo" (sa-label city))
  (test-equal "accessor, third class" 7 (sa-label (make <sa-fixed>)))
  (test-equal "getter, first class" "ann" (sa-get person))
  (test-equal "getter, second class" "oslo" (sa-get city)))

(test-group "writers"
  (sa-put! person "bob")
  (sa-put! city "rome")
  (test-equal "setter option, first class" "bob" (slot-ref person 'pname))
  (test-equal "setter option, second class" "rome" (slot-ref city 'title))
  (set! (sa-label person) "cy")
  (set! (sa-label city) "oslo2")
  (test-equal "accessor setter, first class" "cy" (sa-label person))
  (test-equal "accessor setter, second class" "oslo2" (sa-label city)))

(define-generic sa-size)
(define-method (sa-size (x <string>)) (string-length x))
(define-class <sa-box> () ((n #:init-keyword #:n #:accessor sa-size)))

(test-group "later classes extend a hand-made generic"
  (test-equal "existing method kept" 3 (sa-size "abc"))
  (test-equal "accessor method added" 9 (sa-size (make <sa-box> #:n 9))))

(define-class <sa-pt> () ((x #:init-keyword #:x #:accessor sa-pt-x)))
(define-class <sa-pt2> (<sa-pt>) ((k #:init-keyword #:k)))
(define-method (sa-pt-x (p <sa-pt2>)) (* (slot-ref p 'k) (next-method)))

(test-group "overriding a generated reader"
  (test-equal "base" 3 (sa-pt-x (make <sa-pt> #:x 3)))
  (test-equal "override" 30 (sa-pt-x (make <sa-pt2> #:x 3 #:k 10))))

(define-class <sa-re> () ((a #:init-keyword #:a #:accessor sa-re-a)))
(define-class <sa-re> () ((a #:init-keyword #:a #:accessor sa-re-a)))

(test-group "redefining a class keeps its accessor"
  (test-equal "after redefinition" 5 (sa-re-a (make <sa-re> #:a 5))))

(test-end "shared accessor names")
