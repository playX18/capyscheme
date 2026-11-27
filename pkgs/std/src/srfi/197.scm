;;; SRFI 197: Pipeline operators.
;;;
;;; `chain`, `chain-and`, `chain-when`, `chain-lambda`, `nest` and `nest-reverse`. The
;;; placeholder is `_` and the ellipsis `...` unless the form names others. A step whose
;;; placeholders take one value binds it with `let`, so a pipeline of single-valued steps
;;; expands to nested `let`s with no `call-with-values`.
;;;
;;; Placeholders are only recognized at the top level of a step, as the specification says.
(library (srfi 197)
  (export chain chain-and chain-when chain-lambda nest nest-reverse)
  (import (rnrs) (srfi 197 helpers))

  (define-syntax chain
    (lambda (stx)
      (syntax-case stx ()
        [(_ init p e step ...)
         (and (identifier? #'p) (identifier? #'e))
         (expand-chain #'init #'p #'e #'(step ...))]
        [(_ init p step ...)
         (identifier? #'p)
         (expand-chain #'init #'p #'(... ...) #'(step ...))]
        [(_ init step ...)
         (expand-chain #'init #'_ #'(... ...) #'(step ...))])))

  (define-syntax chain-and
    (lambda (stx)
      (syntax-case stx ()
        [(_ init p step ...)
         (identifier? #'p)
         (expand-chain-and #'init #'p #'(step ...))]
        [(_ init step ...)
         (expand-chain-and #'init #'_ #'(step ...))])))

  (define-syntax chain-when
    (lambda (stx)
      (syntax-case stx ()
        [(_ init p step ...)
         (identifier? #'p)
         (expand-chain-when #'init #'p #'(step ...))]
        [(_ init step ...)
         (expand-chain-when #'init #'_ #'(step ...))])))

  (define-syntax chain-lambda
    (lambda (stx)
      (syntax-case stx ()
        [(_ p e step0 step ...)
         (and (identifier? #'p) (identifier? #'e))
         (expand-chain-lambda #'p #'e #'step0 #'(step ...))]
        [(_ p step0 step ...)
         (identifier? #'p)
         (expand-chain-lambda #'p #'(... ...) #'step0 #'(step ...))]
        [(_ step0 step ...)
         (expand-chain-lambda #'_ #'(... ...) #'step0 #'(step ...))])))

  (define-syntax nest
    (lambda (stx)
      (syntax-case stx ()
        [(_ p step ... init)
         (and (identifier? #'p) (pair? (syntax->datum #'(step ...))))
         (expand-nest #'p (reverse #'(step ...)) #'init)]
        [(_ step ... init)
         (expand-nest #'_ (reverse #'(step ...)) #'init)])))

  (define-syntax nest-reverse
    (lambda (stx)
      (syntax-case stx ()
        [(_ init p step ...)
         (identifier? #'p)
         (expand-nest #'p #'(step ...) #'init)]
        [(_ init step ...)
         (expand-nest #'_ #'(step ...) #'init)]))))
