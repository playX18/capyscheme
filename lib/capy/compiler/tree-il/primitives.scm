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
        (define-primitive-expander! sym (primitive-expander . clauses))]))

  (define-primitive-expander identity (x) x)
  (define-primitive-expander zero? (x)
    (= x 0))
  
  (define-primitive-expander + 
    () 0
    (x) (values x)
    (x y) (+ x y)
    (x y z ... last) (+ (+ x y . z) last))
  
  (define-primitive-expander * 
    () 1
    (x) (values x)
    (x y z ... last) (* (* x y . z) last))
  
  (define-primitive-expander -
    (x) (- 0 x)
    (x y) (- x y)
    (x y z ... last) (- (- x y . z) last))
    
  (define-primitive-expander /
    (x) (/ 1 x)
    (x y z ... last) (/ (/ x y . z) last))

  (define (expand-primcall x)
    (cond 
      [(primcall? x)
        (let ([src (term-src x)] 
              [args (primcall-args x)] 
              [expand (hashtable-ref *primitive-expand-table* (primcall-prim x) #f)])
          (if expand
            (apply expand src args)
            x))]))
  
  (define (expand-primitives x)
    (post-order expand-primcall x))
)

