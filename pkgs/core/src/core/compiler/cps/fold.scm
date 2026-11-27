;; Constant folding for graph CPS primitive calls.
;;
;; Port of crates/capy/src/compiler/cps/fold.rs.
;;
;; API:
;;   (try-fold-values prim args) => the folded value, or `fold-none'
;;       PRIM is the primitive name symbol (Primitive::name), ARGS the list of
;;       literal operand values.  `fold-none' is a unique object, because #f is
;;       a legitimate folded result; test it with (fold-none? x).
;;
;; Rust's table is keyed by Symbol Value and looked up only (never iterated);
;; here it is an eq? hashtable used the same way.
;;
;; Differences from the Rust table, all of them cases where the Rust fold
;; inspects representation details Scheme cannot see:
;;   * "%class-id?" folds only for the string class id (28, the only id the
;;     converter emits); any other class id is not folded.
;;   * "usize->value" is never folded (its operand is a raw machine word, never
;;     a literal in practice).
;;   * ".is-cell" is the "is a heap object" test: everything except fixnums,
;;     flonums, characters, booleans, '(), the unspecified value and eof.
;;   * "immediate?" never folds in Rust; it is simply absent here.
(library (core compiler cps fold)
  (export try-fold-values fold-none fold-none?)
  (import (core) (capy) (core compiler cps primitives))

  (define fold-none (list 'fold-none))
  (define (fold-none? x) (eq? x fold-none))

  ;; Number::compare(a, b) == Some(ordering): #f when either is not real
  ;; (compare returns None for complex numbers and NaN).
  (define (compare-op op)
    (lambda (a b)
      (if (and (real? a) (real? b)) (op a b) #f)))

  (define (heap-object? a)
    (not (or (fixnum? a) (flonum? a) (char? a) (boolean? a) (null? a)
           (unspecified? a) (eof-object? a))))

  (define (integer->char-or-none c)
    (if (and (fixnum? c)
             (<= 0 c #x10FFFF)
             (not (<= #xD800 c #xDFFF)))
      (integer->char c)
      fold-none))

  ;; (name arity procedure); a procedure returns fold-none to decline.
  (define (num1 f) (lambda (a) (if (number? a) (f a) fold-none)))
  (define (num2 f) (lambda (a b) (if (and (number? a) (number? b)) (f a b) fold-none)))

  (define table (make-eq-hashtable))
  (define (define-fold! name arity proc)
    (hashtable-set! table name (cons arity proc)))

  (define-fold! 'zero? 1 (num1 zero?))
  (define-fold! '+ 2 (num2 +))
  (define-fold! '- 2 (num2 -))
  (define-fold! '* 2 (num2 *))
  (define-fold! '/ 2
    (num2
      (lambda (a b)
        (if (and (zero? b) (exact? b) (exact? a))
          fold-none                       ; division by zero
          (/ a b)))))
  (define-fold! '= 2 (num2 =))
  (define-fold! '> 2 (num2 (compare-op >)))
  (define-fold! '< 2 (num2 (compare-op <)))
  (define-fold! '>= 2 (num2 (compare-op >=)))
  (define-fold! '<= 2 (num2 (compare-op <=)))
  (define-fold! 'not 1 (lambda (a) (not a)))
  (define-fold! 'pair? 1 pair?)
  (define-fold! 'null? 1 null?)
  (define-fold! 'unspecified? 1 unspecified?)
  (define-fold! 'list? 1 list?)
  (define-fold! 'vector? 1 vector?)
  (define-fold! 'bytevector? 1 bytevector?)
  (define-fold! 'symbol? 1 symbol?)
  (define-fold! 'string? 1 string?)
  (define-fold! 'boolean? 1 boolean?)
  (define-fold! 'number? 1 number?)
  (define-fold! 'char? 1 char?)
  (define-fold! 'char->integer 1
    (lambda (a) (if (char? a) (char->integer a) fold-none)))
  (define-fold! 'integer->char 1
    (lambda (a) (integer->char-or-none a)))
  (define-fold! 'string->symbol 1
    (lambda (a) (if (string? a) (string->symbol a) fold-none)))
  (define-fold! '%class-id? 2
    (lambda (a b)
      (if (eqv? b class-id-string) (string? a) fold-none)))
  (define-fold! '.is-cell 1 heap-object?)

  (define (try-fold-values prim args)
    (let ([entry (hashtable-ref table prim #f)])
      (if (and entry (= (car entry) (length args)))
        (guard (e [#t fold-none])
          (apply (cdr entry) args))
        fold-none))))
