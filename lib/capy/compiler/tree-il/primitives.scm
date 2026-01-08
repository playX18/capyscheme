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
      not
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
      unspecified
      unspecified?))
  (define *interesting-primitive-vars* 
    (let ([m (resolve-module '(capy) #f #f)] 
          [table (make-eq-hashtable)])
      (for-each
        (lambda (name)
          (hashtable-set! table (module-ensure-local-variable! m name) name)) 
        interesting-primitive-names )
      table))

  (define (collect-local-definitions x set)
    (cond 
      [(toplevel-define? x)
        (hashtable-set! set (toplevel-define-name x) #t)]
      [(sequence? x)
        (collect-local-definitions (sequence-head x) set)
        (collect-local-definitions (sequence-tail x) set)]))
  
  (define (resolve-primitives x m)
    (define local-definitions (make-eq-hashtable))
    (unless (eq? m (resolve-module '(capy) #f #f))
      (collect-local-definitions x local-definitions))
    (post-order
      (lambda (x)
        (define src (term-src x))
        (cond 
          [(toplevel-ref? x)
            (define name (toplevel-ref-name x))
            (cond 
              [(and (not (hashtable-ref local-definitions name #f))
                    (hashtable-ref *interesting-primitive-vars* (module-variable m name)))
                => (lambda (name)
                    (make-primref src name))]
              [else x])]
          [(module-ref? x)
            (define module (module-ref-module x))
            (define name (module-ref-name x))
            (define public? (module-ref-public? x))
            (cond 
              [(resolve-module module #f #f)
                => (lambda (module)
                  (define iface (if public? (or (module-public-interface module) module) module))
                  (cond 
                    [(hashtable-ref *interesting-primitive-vars* (module-variable iface name))
                      => (lambda (name) (make-primref src name))]
                    [else x]))]
              [else x])]
          [(application? x)
            (if (primref? (application-operator x))
              (make-primcall src (primref-prim (application-operator x)) (application-operands x))
              x)]
          [else x]))
      x))
  (define *primitive-expand-table* (make-eq-hashtable))

  (define-syntax define-primitive-expander! 
    (syntax-rules () 
      [(_ sym proc)
        (hashtable-set! *primitive-expand-table* sym proc)]))
  
  (define-syntax primitive-expander
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
  
  (define-syntax define-primitive-expander 
    (syntax-rules () 
      [(_ sym . clauses)
        (define-primitive-expander! 'sym (primitive-expander . clauses))]))

  (define-syntax define-unhandled-primitive 
    (syntax-rules () 
      [(_ sym)
        (define-primitive-expander! 'sym (lambda (src . args) #f))]))

  (define-syntax define-primitive-expander* 
    (syntax-rules () 
      [(_ sym (src args) . body)
        (define-primitive-expander! 'sym (lambda (src . args) . body))]))

  (define-primitive-expander identity (x) x)
  (define-primitive-expander zero? (x)
    (= x 0))
  


  (define-primitive-expander current-continuation-marks 
    () (current-continuation-marks))
  
  (define-primitive-expander $set-attachments! 
    (attachments) ($set-attachments! attachments))
  
  ;; TODO: Lower into function call
  (define-unhandled-primitive apply)

  (define-primitive-expander* call-with-values (src args)
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
                       (proc-body consumer))
      
      ] 
      [else #f])) 

  (define-primitive-expander eq? (x y) (eq? x y))
  (define-primitive-expander eqv? (x y) (eqv? x y))
  (define-primitive-expander equal? (x y) (equal? x y))

  (define (expand-memop src args op)
   (cond 
      [(and (= (length args 2))
            (constant? (cadr args))
            (list? (constant-value (cadr args)))
            (< (length (constant-value (cadr args))) 5))
        (define key (car args))
        (define ls (cadr args))
        (case (length ls)
          [(1) (make-if src (make-primcall src op (list key (car ls)))
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
                      ;; (if (eqv? key val) rest-val result)
                      (make-if src 
                              (make-primcall src op (list key val))
                              (make-constant src rest-val)
                              result)))]))])]
      [else #f]))
  

  (define-primitive-expander* memv (src args)
    (expand-memop src args 'eqv?))
  
  (define-primitive-expander* memq (src args)
    (expand-memop src args 'eq?))
  
  (define-primitive-expander* member (src args)
    (expand-memop src args 'equal?))

  (define (multi-compare src predicate args not accept-zero?)
    (cond 
      [(and (null? args) (not accept-zero?))
        #f]
      [(and (null? args) accept-zero?)
        (make-constant src #t)]
      [(null? (cdr args))
        ;; (seq args[0] #t) for side effect
        (make-seq src (car args) (make-constant src #t))]
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

  (define-primitive-expander* = (src args) 
    (multi-compare src '= args #f #f))

  (define-primitive-expander* < (src args)
    (multi-compare src '< args #f #f))
  
  (define-primitive-expander* > (src args)
    (multi-compare src '> args #f #f))
  
  (define-primitive-expander* <= (src args)
    (multi-compare src '<= args #f #f))
  
  (define-primitive-expander* >= (src args)
    (multi-compare src '>= args #f #f))
  
  (define (transitive src op args identity one? prefix?)
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
        (associate-args 
          src 
          op 
          (car args)
          (cdr args))]))

  (define (associate-args src op first-arg args)
    (define next (cdr args))
    (define arg (car args))
    (cond 
      [(null? next)
        (make-primcall src op (list first-arg arg))]
      [else 
        (associate-args src op (make-primcall src op (list first-arg arg)) next)]))

  (define-primitive-expander* + (src args)
    (transitive src '+ args 0 #t #f))
  
  (define-primitive-expander* * (src args)
    (transitive src '* args 1 #t #f))
  
  (define-primitive-expander* - (src args)
    (transitive src 
                '- 
                args 
                #f 
                #f 
                (lambda (src arg) 
                  (make-primcall 
                    src 
                    '- 
                    (list (make-constant src 0) arg)))))
  
  (define-primitive-expander* / (src args)
    (transitive src '/ args #f #f (lambda (src arg) (make-primcall src '/ (list (make-constant src 1) arg)))))
  
  (define-primitive-expander quotient 
    (x y) (quotient x y))
  
  (define-primitive-expander remainder 
    (x y) (remainder x y))
  
  (define-primitive-expander modulo 
    (x y) (modulo x y))

  (define-primitive-expander exact->inexact 
    (x) (exact->inexact x))
  
  (define-primitive-expander inexact->exact 
    (x) (inexact->exact x))
  
  (define-primitive-expander expt 
    (x y) (expt x y))
  
  (define-primitive-expander ash 
    (x y) (ash x y))

  (define-primitive-expander logtest
    (x y) (logtest x y))
  
  (define-primitive-expander logbit? 
    (x y) (logbit? x y))
  
  (define-primitive-expander sqrt 
    (x) (sqrt x))
  
  (define-primitive-expander abs 
    (x) (abs x))
  
  (define-primitive-expander floor 
    (x) (floor x))

  (define-primitive-expander ceiling 
    (x) (ceiling x))
  
  (define-primitive-expander sin (x) (sin x))
  (define-primitive-expander cos (x) (cos x))
  (define-primitive-expander tan (x) (tan x))
  (define-primitive-expander asin (x) (asin x))
  (define-primitive-expander acos (x) (acos x))
  (define-primitive-expander atan (x) (atan x))
  (define-primitive-expander not (x) (not x))
  (define-primitive-expander pair? (x) (pair? x))
  (define-primitive-expander null? (x) (null? x))
  (define-primitive-expander list? (x) (list? x))
  (define-primitive-expander vector? (x) (vector? x))
  (define-primitive-expander number? (x) (number? x))
  (define-primitive-expander char? (x) (char? x))
  (define-primitive-expander boolean? (x) (boolean? x))
  (define-primitive-expander eof-object? (x) (eof-object? x))
  (define-primitive-expander tuple? (x) (tuple? x))
  (define-primitive-expander bytevector? (x) (bytevector? x))
  (define-primitive-expander symbol->string (x) (symbol->string x))
  (define-primitive-expander string->symbol (x) (string->symbol x))
  (define-primitive-expander procedure? (x) (procedure? x))
  (define-primitive-expander complex? (x) (complex? x))
  (define-primitive-expander real? (x) (real? x))
  (define-primitive-expander rational? (x) (rational? x))
  (define-primitive-expander inf? (x) (inf? x))
  (define-primitive-expander nan? (x) (nan? x))
  (define-primitive-expander integer? (x) (integer? x))
  (define-primitive-expander exact? (x) (exact? x))
  (define-primitive-expander inexact? (x) (inexact? x))
  (define-primitive-expander even? (x) (even? x))
  (define-primitive-expander odd? (x) (odd? x))
  (define-primitive-expander zero? (x) (zero? x))
  (define-primitive-expander positive? (x) (positive? x))
  (define-primitive-expander negative? (x) (negative? x))
  (define-primitive-expander exact-integer? (x) (exact-integer? x))

  ;; (char=? x y ...)
  ;; => 
  ;; (= (char->integer x) (char->integer y) ...)

  (define (expand-charcmp src args op)
    (define nargs (length args))
    (cond 
      [(< nargs 2) #f]
      [(= nargs 2)
        (make-primcall src 
          op
          (make-primcall src 'char->integer (car args))
          (make-primcall src 'char->integer (cadr args)))]
      [else 
        (let lp ([ints '()][ls args])
          (cond 
            [(null? ls)
              (expand-primcall 
                (make-primcall src op ints))]
            [else 
              (lp (cons
                    (make-primcall src 'char->integer (car ls))
                    ints)
                  ls)]))]))

  (define-primitive-expander* char=? (src args)
    (expand-charcmp src args '=))
  
  (define-primitive-expander* char<? (src args)
    (expand-charcmp src args '<))

  (define-primitive-expander* char>? (src args)
    (expand-charcmp src args '>))

  (define-primitive-expander* char<=? (src args)
    (expand-charcmp src args '<=))

  (define-primitive-expander* char>=? (src args)
    (expand-charcmp src args '>=))

  (define-primitive-expander char->integer (x) (char->integer x))
  (define-primitive-expander integer->char (x) (integer->char x))

  (define-primitive-expander cons (x y) (cons x y))
  (define-primitive-expander* cons* (src args)
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
            (expand-primcall (make-primcall src 'cons* (cdr args)))))]))

  (define-primitive-expander* append (src args)
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
            (expand-primcall (make-primcall src 'append (cdr args)))))]))
  
  (define-primitive-expander acons (x y z)
    (cons (cons x y) z))
  
  (define-primitive-expander set-car! (pair val)
    (set-car! pair val))
  
  (define-primitive-expander set-cdr! (pair val)
    (set-cdr! pair val))
  
  (define-primitive-expander car (x) (car x))
  (define-primitive-expander cdr (x) (cdr x))

  (define-primitive-expander caar (x) (car (car x)))
  (define-primitive-expander cadr (x) (car (cdr x)))
  (define-primitive-expander cdar (x) (cdr (car x)))
  (define-primitive-expander cddr (x) (cdr (cdr x)))
  (define-primitive-expander caaar (x) (car (car (car x))))
  (define-primitive-expander caadr (x) (car (car (cdr x))))
  (define-primitive-expander cadar (x) (car (cdr (car x))))
  (define-primitive-expander caddr (x) (car (cdr (cdr x))))
  (define-primitive-expander cdaar (x) (cdr (car (car x))))
  (define-primitive-expander cdadr (x) (cdr (car (cdr x))))
  (define-primitive-expander cddar (x) (cdr (cdr (car x))))
  (define-primitive-expander cdddr (x) (cdr (cdr (cdr x))))
  (define-primitive-expander caaaar (x) (car (car (car (car x)))))
  (define-primitive-expander caaadr (x) (car (car (car (cdr x)))))
  (define-primitive-expander caadar (x) (car (car (cdr (car x)))))
  (define-primitive-expander caaddr (x) (car (car (cdr (cdr x)))))
  (define-primitive-expander cadaar (x) (car (cdr (car (car x)))))
  (define-primitive-expander cadadr (x) (car (cdr (car (cdr x)))))
  (define-primitive-expander caddar (x) (car (cdr (cdr (car x)))))
  (define-primitive-expander cadddr (x) (car (cdr (cdr (cdr x)))))
  (define-primitive-expander cdaaar (x) (cdr (car (car (car x)))))
  (define-primitive-expander cdaadr (x) (cdr (car (car (cdr x)))))
  (define-primitive-expander cdadar (x) (cdr (car (cdr (car x)))))
  (define-primitive-expander cdaddr (x) (cdr (car (cdr (cdr x)))))
  (define-primitive-expander cddaar (x) (cdr (cdr (car (car x)))))
  (define-primitive-expander cddadr (x) (cdr (cdr (car (cdr x)))))
  (define-primitive-expander cdddar (x) (cdr (cdr (cdr (car x)))))
  (define-primitive-expander cddddr (x) (cdr (cdr (cdr (cdr x)))))
  (define-primitive-expander length (x) (length x))
  (define-primitive-expander* list (src args)
    (cond 
      [(null? args) (make-constant src '())]
      [(null? (cdr args)) (make-primcall src 'cons (car args) (make-constant src '()))]
      [else 
        (make-primcall 
          src 
          'cons 
          (list 
            (car args)
            (expand-primcall (make-primcall src 'list (cdr args)))))]))

  (define-primitive-expander* vector (src args)
    (define tmp (gensym "vec-alloc"))
    (define len (length args))
    (define init (make-primcall src 'make-vector (list (make-constant src len))))
    (let loop ([i 0] [args args] [result (make-lref src tmp tmp)])
      (cond 
        [(null? args)
          (if (lref? result)
            result 
            (make-let src 
                      'let 
                      (list tmp) (list tmp)
                      init 
                      result))]
        [else 
          (loop 
            (+ i 1)
            (cdr args)
            (make-seq 
              src 
              (make-primcall
                src 
                'vector-set! 
                (list (make-constant src i) (car args)))
              result))])))
  
  (define-primitive-expander* tuple (src args)
    (define tmp (gensym "tuple-alloc"))
    (define len (length args))
    (define init (make-primcall src 'make-tuple (list (make-constant src len))))
    (let loop ([i 0] [args args] [result (make-lref src tmp tmp)])
      (cond 
        [(null? args)
          (if (lref? result)
            result 
            (make-let src 
                      'let 
                      (list tmp) (list tmp)
                      init 
                      result))]
        [else 
          (loop 
            (+ i 1)
            (cdr args)
            (make-seq 
              src 
              (make-primcall
                src 
                'tuple-set! 
                (list (make-constant src i) (car args)))
              result))])))

  (define-primitive-expander make-tuple 
    (len) (make-tuple len (unspecified))
    (len init) (make-tuple len init))
  
  (define-primitive-expander make-vector 
    (len) (make-vector len (unspecified))
    (len init) (make-vector len init))
  
  (define-primitive-expander vector? (x) (vector? x))
  (define-primitive-expander vector-length (x) (vector-length x))
  (define-primitive-expander vector-ref (x i) (vector-ref x i))
  (define-primitive-expander vector-set! (x i v) (vector-set! x i v))
  
  (define-primitive-expander tuple-size (x) (tuple-size x))
  (define-primitive-expander tuple-ref (x i) (tuple-ref x i))
  (define-primitive-expander tuple-set! (x i v) (tuple-set! x i v))
  (define-primitive-expander current-module 
    () (current-module)
    (mod) (current-module mod))
  (define-primitive-expander define! (x y) (define! x y))
  (define-primitive-expander make-syntax 
    (x y z w) (make-syntax x y z w '())
    (x y z w props) (make-syntax x y z w props))
  (define-primitive-expander $winders 
    () ($winders)
    (x) ($winders x))
  
  (define-primitive-expander unspecified () (unspecified))
  (define-primitive-expander unspecified? (x) (unspecified? x))

  (define (expand-primcall x)
    (cond 
      [(primcall? x)
        (let ([src (term-src x)] 
              [args (primcall-args x)] 
              [expand (hashtable-ref *primitive-expand-table* (primcall-prim x) #f)])
          (cond 
            ;; if expander exists and returns a term, use it.
            [(and expand (apply expand src args))
              => (lambda (term) term)]
            ;; expander does not exist or failed to expand, 
            ;; expand to full application.
            [else (make-application src
                                    (make-module-ref src '(capy) (primcall-prim x) #f)
                                    args)]))]
      [else x]))
  
  (define (expand-primitives x)
    (pre-order expand-primcall x))
)

