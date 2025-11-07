
(library (core enums)

  (export make-enumeration
          enum-set?
          enum-set-universe
          enum-set-indexer
          enum-set-constructor
          enum-set->list
          enum-set-member?
          enum-set-subset?
          enum-set=?
          enum-set-union
          enum-set-intersection
          enum-set-difference
          enum-set-complement
          enum-set-projection
          define-enumeration)

  (import (core primitives)
          (core struct)
          (core lists)
          (core sorting))


  (define-syntax define-enumeration
    (syntax-rules ()
      ((_ type-name (symbol1 ...) constructor-syntax)
       (begin
         (define constructor (enum-set-constructor (make-enumeration '(symbol1 ...))))
         (define-syntax type-name
           (lambda (x)
             (syntax-case x ()
               ((_ symbol2)
                (or (memq (syntax->datum (syntax symbol2)) '(symbol1 ...))
                    (syntax-violation 'type-name "excpectd symbols which belong to the universe" x))
                (syntax 'symbol2)))))
         (define-syntax constructor-syntax
           (lambda (x)
             (syntax-case x ()
               ((_ symbol3 (... ...))
                (or (for-all (lambda (e) (memq e '(symbol1 ...)))
                             (syntax->datum (syntax (symbol3 (... ...)))))
                    (syntax-violation 'constructor-syntax "excpectd symbols which belong to the universe" x))
                (syntax (constructor '(symbol3 (... ...))))))))))))

  ) ;[end]
