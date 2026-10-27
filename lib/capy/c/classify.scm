;; Decides what a C `#define` becomes in the generated Scheme library.

(library (capy c classify)
  (export classify-macro)
  (import (rnrs))

  (define (classify-macro mac known?)
    (let ([hint (list-ref mac 7)])
      (cond
        [(not (pair? hint)) '(skip "not a constant")]
        [else
          (case (car hint)
            [(const) (classify-constant (cadr hint))]
            [(expr) (classify-expression (cadr hint) known?)]
            [(type) (classify-type (cadr hint) known?)]
            [(empty) '(skip "expands to nothing")]
            [else '(skip "not a constant")])])))

  (define (classify-expression ast known?)
    (cond
      [(and (pair? ast) (eq? (car ast) 'id))
        (if (known? (cadr ast))
          (list 'alias (cadr ast))
          '(skip "names something the library does not define"))]
      [(compound-literal ast) => values]
      [else '(skip "expression evaluated at each use")]))

  ;; (compound (typedef T) ((DESIGNATOR EXPR) ...)) with positional constant initialisers.
  (define (compound-literal ast)
    (and (pair? ast) (eq? (car ast) 'compound)
         (pair? (cadr ast)) (eq? (car (cadr ast)) 'typedef)
         (let ([values (map initialiser-value (caddr ast))])
           (and (for-all (lambda (v) v) values)
                (list 'construct (cadr (cadr ast)) (map car values))))))

  ;; An unadorned `(() (int N))` initialiser -> (N), anything else -> #f.
  (define (initialiser-value init)
    (and (pair? init) (null? (car init))
         (pair? (cadr init)) (memq (car (cadr init)) '(int float))
         (list (cadr (cadr init)))))

  (define (classify-type ty known?)
    (if (and (pair? ty) (eq? (car ty) 'typedef) (known? (cadr ty)))
      (list 'alias (cadr ty))
      '(skip "names a type")))

  (define (classify-constant c)
    (case (car c)
      [(int float) (list 'constant (cadr c))]
      [(string)
        (list 'constant (guard (e [#t (cadr c)]) (utf8->string (cadr c))))]
      [(ptr) (list 'constant `(ffi:make-pointer ,(cadr c)))]
      [else '(skip "unknown constant kind")])))
