(import (srfi 64)
        (capy))

(test-begin "boot class invocable scheme classes")

(test-group "invocable scheme classes"
  (define callable (make-invocable-class 'callable '(procedure label)))
  (define proc
    (make-instance callable
      #:procedure (lambda (self arg)
                    (list (slot-ref self 'label) arg))
      #:label 'wrapped))
  (define instance (procedure-property proc 'instance))

  (test-assert "invocable allocation returns procedure"
    (procedure? proc))
  (test-assert "invocable class is marked applicable"
    (class-applicable? callable))
  (test-assert "invocable wrapper carries instance property"
    (scheme-instance? instance))
  (test-assert "invocable wrapper carries class property"
    (eq? (procedure-property proc 'class) callable))
  (test-equal "invocable wrapper exposes raw instance slots"
    (slot-ref instance 'label)
    'wrapped)
  (test-equal "invocable wrapper calls stored procedure with instance"
    (proc 42)
    '(wrapped 42))
  (test-equal "make returns initialized invocable wrappers"
    (let ([made
            (make callable
              #:procedure
              (lambda (self arg)
                (list 'made (slot-ref self 'label) arg))
              #:label 'via-make)])
      (list
        (procedure? made)
        (scheme-instance? (procedure-property made 'instance))
        (made 5)))
    '(#t #t (made via-make 5)))
  (test-equal "define-class applicable option creates invocable classes"
    (eval
      '(begin
         (define-class macro-callable ()
           (procedure label)
           :applicable #t)
         (define macro-proc
           (make-instance macro-callable
             #:procedure (lambda (self arg)
                           (list 'macro (slot-ref self 'label) arg))
             #:label 'first))
         (define macro-instance (procedure-property macro-proc 'instance))
         (define-class macro-callable ()
           (procedure label extra)
           :applicable #t)
         (define macro-proc-v2
           (make-instance macro-callable
             #:procedure (lambda (self) (slot-ref self 'extra))
             #:extra 'new))
         (list
           (class-applicable? macro-callable)
           (procedure? macro-proc)
           (scheme-instance? macro-instance)
           (macro-proc 7)
           (class-applicable? (procedure-property macro-proc 'class))
           (class-name (current-class-of macro-instance))
           (slot-ref (procedure-property macro-proc-v2 'instance) 'extra)))
      (current-module))
    '(#t #t #t (macro first 7) #t "macro-callable" new))
  (test-error "invocable class requires procedure slot"
    &assertion-violation
    (make-invocable-class 'bad-callable '(label)))
  (test-error "define-class applicable option requires procedure slot"
    &assertion-violation
    (eval '(define-class bad-macro-callable () (label) #:applicable #t)
      (environment '(capy))))
  (test-error "define-class applicable option must be boolean"
    &syntax
    (eval '(define-class bad-applicable-option () (procedure) #:applicable 'yes)
      (environment '(capy))))
  (test-error "invocable instance requires procedure value"
    &assertion-violation
    (make-instance callable #:procedure 'not-a-procedure #:label 'bad)))

(test-end "boot class invocable scheme classes")
