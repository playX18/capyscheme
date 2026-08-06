(library (capy compiler tree-il primitives)
  (export interesting-primitive-names
    resolve-primitives
    expand-primitives)
  (import (capy)
    (core match)
    (rnrs hashtables)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms))

  (define interesting-primitive-names
    '(
      apply
      call-with-values
      current-continuation-marks
      $set-attachments!
      values
      eq?
      eqv?
      equal?
      memq
      memv
      =
      <
      <=
      >
      >=
      +
      -
      *
      /
      quotient
      remainder
      modulo
      exact->inexact
      inexact->exact
      expt
      ash
      logtest
      logbit?
      sqrt
      abs
      floor
      ceiling
      sin
      cos
      tan
      asin
      acos
      atan
      fx+
      fx-
      fx*
      fx+/ovf?
      fx-/ovf?
      fx*/ovf?
      fx<?
      fx<=?
      fx>?
      fx>=?
      fx=?
      fx+/unchecked
      fx-/unchecked
      fx*/unchecked
      fx+/ovf?/unchecked
      fx-/ovf?/unchecked
      fx*/ovf?/unchecked
      fx</unchecked
      fx<=/unchecked
      fx>/unchecked
      fx>=/unchecked
      fx=/unchecked
      fxand
      fxior
      fxxor
      fxnot
      fxlogand
      fxlogior
      fxlogxor
      fxarithmetic-shift-left
      fxarithmetic-shift-right
      fxand/unchecked
      fxior/unchecked
      fxxor/unchecked
      fxnot/unchecked
      fxarithmetic-shift-left/unchecked
      fxarithmetic-shift-right/unchecked
      fxzero?
      fxpositive?
      fxnegative?
      fxodd?
      fxeven?
      fxmin
      fxmax
      fxzero?/unchecked
      fxpositive?/unchecked
      fxnegative?/unchecked
      fxodd?/unchecked
      fxeven?/unchecked
      fxmin/unchecked
      fxmax/unchecked
      fl+
      fl-
      fl*
      fl/
      fl<?
      fl<=?
      fl>?
      fl>=?
      fl=?
      fl+/unchecked
      fl-/unchecked
      fl*/unchecked
      fl//unchecked
      fl</unchecked
      fl<=/unchecked
      fl>/unchecked
      fl>=/unchecked
      fl=/unchecked
      flzero?
      flpositive?
      flnegative?
      flnan?
      flinfinite?
      flfinite?
      flmin
      flmax
      flabs
      flfloor
      flceiling
      fltruncate
      flround
      flsin
      flcos
      fltan
      flexp
      fllog
      flasin
      flacos
      flsqrt
      flatan
      flzero?/unchecked
      flpositive?/unchecked
      flnegative?/unchecked
      flnan?/unchecked
      flinfinite?/unchecked
      flfinite?/unchecked
      flmin/unchecked
      flmax/unchecked
      flabs/unchecked
      flfloor/unchecked
      flceiling/unchecked
      fltruncate/unchecked
      flround/unchecked
      flsin/unchecked
      flcos/unchecked
      fltan/unchecked
      flexp/unchecked
      fllog/unchecked
      flasin/unchecked
      flacos/unchecked
      flsqrt/unchecked
      flatan/unchecked
      not
      fixnum?
      flonum?
      pair?
      null?
      list?
      symbol?
      vector?
      string?
      number?
      char?
      boolean?
      nil?
      eof-object?
      tuple?
      bytevector?
      symbol->string
      string->symbol
      procedure?
      complex?
      real?
      rational?
      inf?
      nan?
      integer?
      exact?
      inexact?
      even?
      odd?
      zero?
      positive?
      negative?
      exact-integer?
      char<?
      char<=?
      char>?
      char>=?
      char=?
      integer->char
      char->integer
      acons
      cons
      cons*
      append
      make-syntax
      list
      vector
      tuple
      car
      cdr
      set-car!
      set-cdr!
      caar
      cadr
      cdar
      cddr
      caaar
      caadr
      cadar
      caddr
      cdaar
      cdadr
      cddar
      cdddr
      caaaar
      caaadr
      caadar
      caaddr
      cadaar
      cadadr
      caddar
      cadddr
      cdaaar
      cdaadr
      cdadar
      cdaddr
      cddaar
      cddadr
      cdddar
      cddddr
      length
      make-vector
      vector-length
      vector-ref
      vector-set!
      make-tuple
      tuple-size
      tuple-ref
      tuple-set!
      variable?
      variable-ref
      variable-set!
      make-variable
      variable-bound?
      current-module
      define!
      string-length
      string-ref
      string-set!
      bytevector-length
      bytevector-u8-ref
      bytevector-u8-set!
      bytevector-s8-ref
      bytevector-s8-set!
      bytevector-u16-ref
      bytevector-u16-set!
      bytevector-s16-ref
      bytevector-s16-set!
      bytevector-u16-native-ref
      bytevector-u16-native-set!
      bytevector-s16-native-ref
      bytevector-s16-native-set!
      bytevector-u32-ref
      bytevector-u32-set!
      bytevector-s32-ref
      bytevector-s32-set!
      bytevector-u32-native-ref
      bytevector-u32-native-set!
      bytevector-s32-native-ref
      bytevector-s32-native-set!
      bytevector-u64-ref
      bytevector-u64-set!
      bytevector-s64-ref
      bytevector-s64-set!
      bytevector-u64-native-ref
      bytevector-u64-native-set!
      bytevector-s64-native-ref
      bytevector-s64-native-set!
      $winders
      call/cc
      call-with-current-continuation
      call/1cc
      unspecified
      unspecified?))
  (define *primitive-name->variable*
    (let ([m (resolve-module '(capy) #f #f)]
          [table (make-eq-hashtable)])
      (for-each
        (lambda (name)
          (hashtable-set! table name (or (module-variable m name)
                                      (module-ensure-local-variable! m name))))
        interesting-primitive-names)
      table))

  (define (scan-toplevel-definitions x set)
    (cond
      [(toplevel-define? x)
        (hashtable-set! set (toplevel-define-name x) #t)]
      [(sequence? x)
        (scan-toplevel-definitions (sequence-head x) set)
        (scan-toplevel-definitions (sequence-tail x) set)]))

  (define (resolve-primitives x m)
    (define local-definitions (make-eq-hashtable))
    (unless (eq? m (resolve-module '(capy) #f #f))
      (scan-toplevel-definitions x local-definitions))
    (post-order
      (lambda (x)
        (define src (term-src x))
        (cond
          [(toplevel-ref? x)
            (define name (toplevel-ref-name x))
            (cond
              [(and (not (hashtable-ref local-definitions name #f))
                  (let ([var (module-variable m name)]
                        [prim-var (hashtable-ref *primitive-name->variable* name #f)])
                    (and var prim-var (eq? var prim-var))))
                =>
                (lambda (_)
                  (make-primref src name))]
              [else x])]
          [(module-ref? x)
            (define module (module-ref-module x))
            (define name (module-ref-name x))
            (define public? (module-ref-public? x))
            (cond
              [(resolve-module module #f #f)
                =>
                (lambda (module)
                  (define iface (if public? (or (module-public-interface module) module) module))
                  (cond
                    [(let ([var (module-variable iface name)]
                           [prim-var (hashtable-ref *primitive-name->variable* name #f)])
                        (and var prim-var (eq? var prim-var)))
                      =>
                      (lambda (_) (make-primref src name))]
                    [else x]))]
              [else x])]
          [(application? x)
            (if (primref? (application-operator x))
              (make-primcall src (primref-prim (application-operator x)) (application-operands x))
              x)]
          [else x]))
      x))
  (define *primitive-expanders* (make-eq-hashtable))

  (define-syntax define-primitive-expansion!
    (syntax-rules ()
      [(_ sym proc)
        (hashtable-set! *primitive-expanders* sym proc)]))

  (define-syntax clause-driven-expander
    (lambda (stx)
      (define (expand-args args)
        (syntax-case args ()
          (() #''())
          ((a . b) #`(cons #,(expand-expr #'a) #,(expand-args #'b)))
          (a (expand-expr #'a))))
      (define (expand-expr body)
        (syntax-case body (quote)
          (id (identifier? #'id) #'id)
          ((quote x) #'(make-constant src 'x))
          ((op . args) #`(make-primcall src 'op #,(expand-args #'args)))
          (x (self-evaluating? (syntax->datum #'x)) #'(make-constant src x))))
      (define (match-clauses args+body)
        (syntax-case args+body (if)
          (() '())
          ((args body . args+body)
            (cons #`(args #,(expand-expr #'body))
              (match-clauses #'args+body)))))
      (syntax-case stx ()
        ((_ args+body ...)
          #`(lambda (src . args)
             (match args
              #,@(match-clauses #'(args+body ...))
              (_ #f)))))))

  (define-syntax define-primitive-expansion
    (syntax-rules ()
      [(_ sym . clauses)
        (define-primitive-expansion! 'sym (clause-driven-expander . clauses))]))

  (define-syntax declare-unexpandable-primitive
    (syntax-rules ()
      [(_ sym)
        (define-primitive-expansion! 'sym (lambda (src . args) #f))]))

  (define-syntax define-primitive-expansion*
    (syntax-rules ()
      [(_ sym (src args) . body)
        (define-primitive-expansion! 'sym (lambda (src . args) . body))]))

  (define-primitive-expansion identity (x) x)
  (define-primitive-expansion zero? (x)
    (= x 0))

  (define-primitive-expansion current-continuation-marks
    ()
    (current-continuation-marks))

  (define-primitive-expansion $set-attachments!
    (attachments)
    ($set-attachments! attachments))

  ;; TODO: Lower into function call
  (declare-unexpandable-primitive apply)
  (define-primitive-expansion* values (src args)
    (make-values src args))
  (define-primitive-expansion* call-with-values (src args)
    (cond
      ;; (call-with-values (lambda ()  ...) (lambda (x ...) ...))
      ;; =>
      ;; (receive (x ...) ...
      ;;      ...)
      [(and
          (= (length args) 2)
          (proc? (car args))
          (proc? (cadr args))
          (null? (proc-args (car args))))
        (define producer (car args))
        (define consumer (cadr args))

        (make-receive src
          (proc-ids consumer)
          (proc-args consumer)
          (proc-body producer)
          (proc-body consumer))]
      [else #f]))

  (define-primitive-expansion eq? (x y) (eq? x y))
  (define-primitive-expansion eqv? (x y) (eqv? x y))
  (define-primitive-expansion equal? (x y) (equal? x y))

  (define (expand-list-search src args op)
    (cond
      [(and (= (length args) 2)
          (constant? (cadr args))
          (list? (constant-value (cadr args)))
          (< (length (constant-value (cadr args))) 5))
        (define key (car args))
        (define ls (constant-value (cadr args)))
        (case (length ls)
          [(1) (make-if src (make-primcall src op (list key (make-constant src (car ls))))
                (make-constant src ls)
                (make-constant src #f))]
          [else
            (let loop ([it ls] [result (make-constant src #f)])
              (cond
                [(null? it)
                  result]
                [else
                  (let* ([val (car it)]
                         [rest-val it]
                         [it (cdr it)])
                    (loop
                      it
                      ;; (if (eqv? key val) rest-val result)
                      (make-if src
                        (make-primcall src op (list key (make-constant src val)))
                        (make-constant src rest-val)
                        result)))]))])]
      [else #f]))

  (define-primitive-expansion* memv (src args)
    (expand-list-search src args 'eqv?))

  (define-primitive-expansion* memq (src args)
    (expand-list-search src args 'eq?))

  (define-primitive-expansion* member (src args)
    (expand-list-search src args 'equal?))

  (define (expand-chain-comparison src predicate args not accept-zero?)
    (cond
      [(and (null? args) (not accept-zero?))
        #f]
      [(and (null? args) accept-zero?)
        (make-constant src #t)]
      [(null? (cdr args))
        ;; (seq args[0] #t) for side effect
        (make-sequence src (car args) (make-constant src #t))]
      [(null? (cddr args))
        (define lhs (car args))
        (define rhs (cadr args))
        (if not
          (make-if src
            (make-primcall src predicate (list lhs rhs))
            (make-constant src #f)
            (make-constant src #t))
          ;; valid comparison, no need to extend further
          (make-primcall src predicate (list lhs rhs)))]
      [else
        (define nargs (length args))

        (do* ((i (- nargs 1) (- i 1))
              (last #f current)
              (current (gensym) (gensym))
              (vars (list current) (cons current vars))
              (result (make-constant src #t)
                (if not
                  (make-if src
                    (make-primcall src
                      predicate
                      (list (make-lref src current current)
                        (make-lref src last last)))
                    (make-constant src #f)
                    result)
                  (make-if src
                    (make-primcall src
                      predicate
                      (list (make-lref src current current)
                        (make-lref src last last)))
                    result
                    (make-constant src #f)))))
          ((zero? i)
            (make-let src
              'let
              vars
              vars
              args
              result)))]))

  (define-primitive-expansion* = (src args)
    (expand-chain-comparison src '= args #f #f))

  (define-primitive-expansion* < (src args)
    (expand-chain-comparison src '< args #f #f))

  (define-primitive-expansion* > (src args)
    (expand-chain-comparison src '> args #f #f))

  (define-primitive-expansion* <= (src args)
    (expand-chain-comparison src '<= args #f #f))

  (define-primitive-expansion* >= (src args)
    (expand-chain-comparison src '>= args #f #f))

  (define (fold-nary-call src op args identity one? prefix?)
    (cond
      [(null? args)
        (and identity (make-constant src identity))]
      [(and (null? (cdr args)) one?)
        (or (and prefix? (prefix src (car args)))
          (car args))]
      [(and (null? (cdr args)) (not one?))
        #f]
      [(null? (cddr args)) ;; (op arg1 arg2)
        (make-primcall src op args)]
      [else
        (fold-call-args
          src
          op
          (car args)
          (cdr args))]))

  (define (fold-call-args src op first-arg args)
    (define next (cdr args))
    (define arg (car args))
    (cond
      [(null? next)
        (make-primcall src op (list first-arg arg))]
      [else
        (fold-call-args src op (make-primcall src op (list first-arg arg)) next)]))

  (define-primitive-expansion* + (src args)
    (fold-nary-call src '+ args 0 #t #f))

  (define-primitive-expansion* * (src args)
    (fold-nary-call src '* args 1 #t #f))

  (define-primitive-expansion* - (src args)
    (fold-nary-call src
      '-
      args
      #f
      #f
      (lambda (src arg)
        (make-primcall
          src
          '-
          (list (make-constant src 0) arg)))))

  (define-primitive-expansion* / (src args)
    (fold-nary-call src '/ args #f #f (lambda (src arg) (make-primcall src '/ (list (make-constant src 1) arg)))))

  (define-primitive-expansion quotient
    (x y)
    (quotient x y))

  (define-primitive-expansion remainder
    (x y)
    (remainder x y))

  (define-primitive-expansion modulo
    (x y)
    (modulo x y))

  (define-primitive-expansion exact->inexact
    (x)
    (exact->inexact x))

  (define-primitive-expansion inexact->exact
    (x)
    (inexact->exact x))

  (define-primitive-expansion expt
    (x y)
    (expt x y))

  (define-primitive-expansion ash
    (x y)
    (ash x y))

  (define-primitive-expansion bitwise-arithmetic-shift
    (x y)
    (ash x y))

  (define-primitive-expansion logtest
    (x y)
    (logtest x y))

  (define-primitive-expansion logbit?
    (x y)
    (logbit? x y))

  (define-primitive-expansion sqrt
    (x)
    (sqrt x))

  (define-primitive-expansion abs
    (x)
    (abs x))

  (define-primitive-expansion floor
    (x)
    (floor x))

  (define-primitive-expansion ceiling
    (x)
    (ceiling x))

  (define-primitive-expansion sin (x) (sin x))
  (define-primitive-expansion cos (x) (cos x))
  (define-primitive-expansion tan (x) (tan x))
  (define-primitive-expansion asin (x) (asin x))
  (define-primitive-expansion acos (x) (acos x))
  (define-primitive-expansion atan (x) (atan x))
  (define-primitive-expansion not (x) (not x))
  (define-primitive-expansion fixnum? (x) (fixnum? x))
  (define-primitive-expansion flonum? (x) (flonum? x))
  (define-primitive-expansion pair? (x) (pair? x))
  (define-primitive-expansion null? (x) (null? x))
  (define-primitive-expansion list? (x) (list? x))
  (define-primitive-expansion symbol? (x) (symbol? x))
  (define-primitive-expansion vector? (x) (vector? x))
  (define-primitive-expansion string? (x) (string? x))
  (define-primitive-expansion number? (x) (number? x))
  (define-primitive-expansion char? (x) (char? x))
  (define-primitive-expansion boolean? (x) (boolean? x))
  (define-primitive-expansion eof-object? (x) (eof-object? x))
  (define-primitive-expansion tuple? (x) (tuple? x))
  (define-primitive-expansion bytevector? (x) (bytevector? x))
  (define-primitive-expansion symbol->string (x) (symbol->string x))
  (define-primitive-expansion string->symbol (x) (string->symbol x))
  (define-primitive-expansion procedure? (x) (procedure? x))
  (define-primitive-expansion complex? (x) (complex? x))
  (define-primitive-expansion real? (x) (real? x))
  (define-primitive-expansion rational? (x) (rational? x))
  (define-primitive-expansion inf? (x) (inf? x))
  (define-primitive-expansion nan? (x) (nan? x))
  (define-primitive-expansion integer? (x) (integer? x))
  (define-primitive-expansion exact? (x) (exact? x))
  (define-primitive-expansion inexact? (x) (inexact? x))
  (define-primitive-expansion even? (x) (even? x))
  (define-primitive-expansion odd? (x) (odd? x))
  (define-primitive-expansion zero? (x) (= x 0))
  (define-primitive-expansion positive? (x) (> x 0))
  (define-primitive-expansion negative? (x) (< x 0))
  (define-primitive-expansion exact-integer? (x) (exact-integer? x))

  ;; (char=? x y ...)
  ;; =>
  ;; (= (char->integer x) (char->integer y) ...)

  (define (expand-char-comparison src args op)
    (define nargs (length args))
    (cond
      [(< nargs 2) #f]
      [else
        (let lp ([ints '()] [ls args])
          (cond
            [(null? ls)
              (expand-single-primcall
                (make-primcall src op (reverse ints)))]
            [else
              (lp (cons
                   (make-primcall src 'char->integer (list (car ls)))
                   ints)
                (cdr ls))]))]))

  (define-primitive-expansion* char=? (src args)
    (expand-char-comparison src args '=))

  (define-primitive-expansion* char<? (src args)
    (expand-char-comparison src args '<))

  (define-primitive-expansion* char>? (src args)
    (expand-char-comparison src args '>))

  (define-primitive-expansion* char<=? (src args)
    (expand-char-comparison src args '<=))

  (define-primitive-expansion* char>=? (src args)
    (expand-char-comparison src args '>=))

  (define-primitive-expansion char->integer (x) (char->integer x))
  (define-primitive-expansion integer->char (x) (integer->char x))

  (define-primitive-expansion cons (x y) (cons x y))
  (define-primitive-expansion* cons* (src args)
    (cond
      [(null? args) #f]
      [(null? (cdr args)) (car args)]
      [(null? (cddr args)) (make-primcall src 'cons args)]
      [else
        (make-primcall
          src
          'cons
          (list
            (car args)
            (expand-single-primcall (make-primcall src 'cons* (cdr args)))))]))

  (define-primitive-expansion* append (src args)
    (cond
      [(null? args) (make-constant src '())]
      [(null? (cdr args))
        (car args)]
      [(null? (cddr args))
        (make-primcall src 'append args)]
      [else
        (make-primcall
          src
          'append
          (list
            (car args)
            (expand-single-primcall (make-primcall src 'append (cdr args)))))]))

  (define-primitive-expansion acons (x y z)
    (cons (cons x y) z))

  (define-primitive-expansion set-car! (pair val)
    (set-car! pair val))

  (define-primitive-expansion set-cdr! (pair val)
    (set-cdr! pair val))

  (define-primitive-expansion car (x) (car x))
  (define-primitive-expansion cdr (x) (cdr x))

  (define-primitive-expansion caar (x) (car (car x)))
  (define-primitive-expansion cadr (x) (car (cdr x)))
  (define-primitive-expansion cdar (x) (cdr (car x)))
  (define-primitive-expansion cddr (x) (cdr (cdr x)))
  (define-primitive-expansion caaar (x) (car (car (car x))))
  (define-primitive-expansion caadr (x) (car (car (cdr x))))
  (define-primitive-expansion cadar (x) (car (cdr (car x))))
  (define-primitive-expansion caddr (x) (car (cdr (cdr x))))
  (define-primitive-expansion cdaar (x) (cdr (car (car x))))
  (define-primitive-expansion cdadr (x) (cdr (car (cdr x))))
  (define-primitive-expansion cddar (x) (cdr (cdr (car x))))
  (define-primitive-expansion cdddr (x) (cdr (cdr (cdr x))))
  (define-primitive-expansion caaaar (x) (car (car (car (car x)))))
  (define-primitive-expansion caaadr (x) (car (car (car (cdr x)))))
  (define-primitive-expansion caadar (x) (car (car (cdr (car x)))))
  (define-primitive-expansion caaddr (x) (car (car (cdr (cdr x)))))
  (define-primitive-expansion cadaar (x) (car (cdr (car (car x)))))
  (define-primitive-expansion cadadr (x) (car (cdr (car (cdr x)))))
  (define-primitive-expansion caddar (x) (car (cdr (cdr (car x)))))
  (define-primitive-expansion cadddr (x) (car (cdr (cdr (cdr x)))))
  (define-primitive-expansion cdaaar (x) (cdr (car (car (car x)))))
  (define-primitive-expansion cdaadr (x) (cdr (car (car (cdr x)))))
  (define-primitive-expansion cdadar (x) (cdr (car (cdr (car x)))))
  (define-primitive-expansion cdaddr (x) (cdr (car (cdr (cdr x)))))
  (define-primitive-expansion cddaar (x) (cdr (cdr (car (car x)))))
  (define-primitive-expansion cddadr (x) (cdr (cdr (car (cdr x)))))
  (define-primitive-expansion cdddar (x) (cdr (cdr (cdr (car x)))))
  (define-primitive-expansion cddddr (x) (cdr (cdr (cdr (cdr x)))))
  (define-primitive-expansion length (x) (length x))
  (define-primitive-expansion* list (src args)
    (cond
      [(null? args) (make-constant src '())]
      [(null? (cdr args)) (make-primcall src 'cons (list (car args) (make-constant src '())))]
      [else
        (make-primcall
          src
          'cons
          (list
            (car args)
            (expand-single-primcall (make-primcall src 'list (cdr args)))))]))

  (define-primitive-expansion* vector (src args)
    (define tmp (gensym "vec-alloc"))
    (define len (length args))
    (define init (make-primcall src 'make-vector (list (make-constant src len))))
    (let loop ([i 0] [args args] [result (make-lref src tmp tmp)])
      (cond
        [(null? args)
          (if (lref? result)
            init
            (make-let src
              'let
              (list tmp)
              (list tmp)
              (list init)
              result))]
        [else
          (loop
            (+ i 1)
            (cdr args)
            (make-sequence
              src
              (make-primcall
                src
                'vector-set!
                (list (make-lref src tmp tmp) (make-constant src i) (car args)))
              result))])))

  (define-primitive-expansion* tuple (src args)
    (define tmp (gensym "tuple-alloc"))
    (define len (length args))
    (define init (make-primcall src 'make-tuple (list (make-constant src len))))
    (let loop ([i 0] [args args] [result (make-lref src tmp tmp)])
      (cond
        [(null? args)
          (if (lref? result)
            init
            (make-let src
              'let
              (list tmp)
              (list tmp)
              (list init)
              result))]
        [else
          (loop
            (+ i 1)
            (cdr args)
            (make-sequence
              src
              (make-primcall
                src
                'tuple-set!
                (list (make-lref src tmp tmp) (make-constant src i) (car args)))
              result))])))

  (define-primitive-expansion make-tuple
    (len)
    (make-tuple len (unspecified))
    (len init)
    (make-tuple len init))

  (define-primitive-expansion make-vector
    (len)
    (make-vector len (unspecified))
    (len init)
    (make-vector len init))

  (define-primitive-expansion vector? (x) (vector? x))
  (define-primitive-expansion vector-length (x) (vector-length x))
  (define-primitive-expansion vector-ref (x i) (vector-ref x i))
  (define-primitive-expansion vector-set! (x i v) (vector-set! x i v))

  (define-primitive-expansion tuple-size (x) (tuple-size x))
  (define-primitive-expansion tuple-ref (x i) (tuple-ref x i))
  (define-primitive-expansion tuple-set! (x i v) (tuple-set! x i v))
  (define-primitive-expansion current-module
    ()
    (current-module)
    (mod)
    (current-module mod))
  (define-primitive-expansion define! (x y) (define! x y))
  (define-primitive-expansion make-syntax
    (x y z w)
    (make-syntax x y z w '())
    (x y z w props)
    (make-syntax x y z w props))
  (define-primitive-expansion $winders
    ()
    ($winders)
    (x)
    ($winders x))

  (define-primitive-expansion unspecified () (unspecified))
  (define-primitive-expansion unspecified? (x) (unspecified? x))

  (define (expand-single-primcall x)
    (cond
      [(primref? x)
        (define src (term-src x))
        (make-module-ref src '(capy) (primref-prim x) #f)]
      [(primcall? x)
        (let ([src (term-src x)]
              [args (primcall-args x)]
              [expand (hashtable-ref *primitive-expanders* (primcall-prim x) #f)])
          (cond
            ;; if expander exists and returns a term, use it.
            [(and expand (apply expand src args))
              =>
              (lambda (term) term)]
            ;; expander does not exist or failed to expand,
            ;; expand to full application.
            [else (make-application src
                   (make-module-ref src '(capy) (primcall-prim x) #f)
                   args)]))]
      [else x]))

  (define (expand-primitives x)
    (pre-order expand-single-primcall x)))
