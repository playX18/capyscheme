(import (srfi 64) (capy))

(test-group "prims.scm"
  ;; filter
  (test-group "filter"
    (test-equal "#t pred" (filter (lambda (x) #t) '(1 2 3)) '(1 2 3))
    (test-equal "#f pred" (filter (lambda (x) #f) '(1 2 3)) '())
    (test-equal "(< x 2)" (filter (lambda (x) (< x 2)) '(1 2 3)) '(1))
    (test-error "dot list errors" &assertion-violation (filter (lambda (x) #t) '(1 2 . 3)))
    (test-error "non list errors" &assertion-violation (filter (lambda (x) #t) 1))
    (define circular-list '(1 2 3))
    (set-cdr! circular-list circular-list)
    ;; TODO: Fix
    ;;(test-error "circular list errors" &assertion-violation (filter (lambda (x) #t) circular-list))
    )

  (test-group "eq?"
    (test-eq "fixnum equality" 1 1)
    (test-eq "flonum equality" 42.0 (string->number "42.0"))

    (let ([x (expt 2 34)] [y (expt 2 34)])
      (test-assert "bignums not eq?" (not (eq? x y)))))

  (test-group "eqv?"
    (test-eqv "fixnum eqv?" 1 1)
    (test-eqv "flonum eqv?" 42.0 (string->number "42.0"))
    (let ([x (expt 2 34)] [y (expt 2 34)])
      (test-assert "bignums eqv?" (eqv? x y))))

  (test-group "equal?"
    (test-equal "equal?: fixnum equality" 1 1)
    (test-equal "equal?: flonum equality" 42.0 (string->number "42.0"))
    (let ([x (expt 2 34)] [y (expt 2 34)])
      (test-assert "equal?: bignums equality" (equal? x y)))

    (define circular-list (eval ''(1 2 3)))
    (set-cdr! circular-list circular-list)
    (test-equal "equal?: circular list" circular-list circular-list)
    (define circular-vec #(1 2 3))
    (vector-set! circular-vec 0 circular-vec)
    (test-equal "equal?: circular vector" circular-vec circular-vec))

  (test-group "null?"
    (test-assert "(null? '())" (null? '()))
    (test-assert "(null? '(1 2))" (not (null? '(1 2))))
    (test-assert "(null? 1)" (not (null? 1))))

  (test-group "char?"
    (test-assert "(char? #\a)" (char? #\a))
    (test-assert "(char? 1)" (not (char? 1))))

  (test-group "char=?"
    (test-assert "(char=? #\\a #\\a)" (char=? #\a #\a))
    (test-assert "(char=? #\\a #\\b)" (not (char=? #\a #\b)))
    (test-error "(char=? 1 2)" &assertion-violation (char=? 1 2))
    (define a "abc")
    (test-assert "(char=? (string-ref a 0) #\\a)" (char=? (string-ref a 0) #\a)))
    (define unichar "😀")
    (test-assert "(char=? (string-ref unichar 0) #\\😀)" (char=? (string-ref unichar 0) #\😀))
    
  (test-group "char<?"
    (test-assert "(char<? #\\a #\\b)" (char<? #\a #\b))
    (test-assert "(char<? #\\a #\\c)" (char<? #\a #\c))
    (test-assert "(char<? #\\b #\\a)" (not (char<? #\b #\a)))
    (test-assert "(char<? #\\a #\\a)" (not (char<? #\a #\a))))

  (test-group "char>?"
    (test-assert "(char>? #\\b #\\a)" (char>? #\b #\a))
    (test-assert "(char>? #\\c #\\a)" (char>? #\c #\a))
    (test-assert "(char>? #\\a #\\b)" (not (char>? #\a #\b)))
    (test-assert "(char>? #\\a #\\a)" (not (char>? #\a #\a))))

  (test-group "char<=?"
    (test-assert "(char<=? #\\a #\\a)" (char<=? #\a #\a))
    (test-assert "(char<=? #\\a #\\b)" (char<=? #\a #\b))
    (test-assert "(char<=? #\\b #\\a)" (not (char<=? #\b #\a))))

  (test-group "char>=?"
    (test-assert "(char>=? #\\a #\\a)" (char>=? #\a #\a))
    (test-assert "(char>=? #\\b #\\a)" (char>=? #\b #\a))
    (test-assert "(char>=? #\\a #\\b)" (not (char>=? #\a #\b))))



  ;
  )
