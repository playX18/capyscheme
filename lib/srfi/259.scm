(library (srfi 259)
  (export
    define-procedure-tag)
  (import (capy))

  (define-syntax define-procedure-tag
    (lambda (stx)
      (syntax-case stx ()
        [(_ constructor predicate accessor)
          (and (identifier? #'constructor)
            (identifier? #'predicate)
            (identifier? #'accessor))
          #'(begin
             (define tag-key (cons 'constructor '()))
             (define (constructor tag proc)
              (set-procedure-property! proc tag-key tag)
              proc)
             (define (predicate proc)
              (and (procedure? proc)
               (procedure-property proc tag-key)
               #t))
             (define (accessor proc)
              (unless (predicate proc)
               (assertion-violation
                'accessor
                "not a tagged procedure in the protocol for this accessor"
                proc))
              (procedure-property proc tag-key)))]))))
