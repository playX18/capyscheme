#!nobacktrace

(library (capy compiler tree-il letrectify)
  (export letrectify)
  (import
    (only (core hashtables) hashtable->alist)
    (rnrs hashtables)
    (rnrs)
    (srfi 257)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms)
    (capy compiler tree-il)
    (capy)
    (core lists)
    (capy pretty-print))

  (define for-each-fold (make-tree-il-folder))

  (define (tree-il-for-each f x)
    (for-each-fold x (lambda (x) (f x) (values)) (lambda (x) (values))))

  (define (compute-declarative-toplevels exp)
    (define dynamic (make-hashtable equal-hash equal?))
    (define defined (make-hashtable equal-hash equal?))
    (define assigned (make-hashtable equal-hash equal?))

    (tree-il-for-each
      (lambda (exp)
        (match exp
          [(~toplevel-set src mod name _)
            (if mod
              (hashtable-set! assigned (cons mod name) #t)
              (hashtable-set! dynamic name #t))]
          [(~toplevel-define _ mod name expr)
            (if mod
              (hashtable-set! (if (hashtable-ref defined (cons mod name) #f)
                               assigned
                               defined)

                (cons mod name)
                expr)
              (hashtable-set! dynamic name #t))]
          [_ #f]))
      exp)

    (define declarative (make-hashtable equal-hash equal?))

    (define (declarative-module? mod)
      ;; all modules are declarative by default
      (define m (resolve-module mod #f #f))
      (or (not m) (module-declarative? m)))
    (for-each
      (lambda (kv)
        (define k (car kv))
        (define expr (cdr kv))

        (match k
          [(~cons mod name)
            (unless (or (hashtable-ref assigned k #f)
                     (hashtable-ref dynamic name #f)
                     (not (declarative-module? mod)))
              (hashtable-set! declarative k expr))]
          [_ #f]))
      (hashtable->alist defined))
    declarative)

  (define (compute-private-toplevels declarative)

    (define exports (make-hashtable equal-hash equal?))
    (define exports-macro? (make-hashtable equal-hash equal?))

    (define alist (hashtable->alist declarative))

    (for-each
      (lambda (kv)
        (define k (car kv))

        (match k
          [(~cons mod name)
            (if (equal? mod '(capy))
              (hashtable-set! exports-macro? mod #t)
              (unless (hashtable-contains? exports-macro? mod)
                (hashtable-set! exports-macro? mod #f)
                (let ([iface (module-public-interface (resolve-module mod #t #f))])
                  (when iface
                    (module-for-each
                      (lambda (sym var)
                        (hashtable-set! exports var k)
                        (when (and (variable-bound? var) (macro? (variable-ref var)))
                          (hashtable-set! exports-macro? mod #t)))
                      iface)))))]
          [_ #f]))
      alist)

    (let ([private (make-hashtable equal-hash equal?)])
      (for-each
        (lambda (kv)
          (define k (car kv))
          (match k
            [(~cons mod name)
              (unless (or (hashtable-ref exports-macro? mod #f)
                       (hashtable-ref exports
                         (module-local-variable (resolve-module mod #t #f) name)
                         #f))
                (hashtable-set! private k #t))]
            [_ #f]))
        alist)
      private))

  (define (all? pred lst)
    (or (null? lst)
      (and (pred (car lst))
        (all? pred (cdr lst)))))

  (define (transparent? exp)
    (match exp
      [(~or
          (~void _)
          (~constant _ _)
          (~lref _ _ _)
          (~proc _ _ _ _ _))
        #t]
      [(~if _ test then else) (and
                               (transparent? test)
                               (transparent? then)
                               (transparent? else))]
      [(~sequence _ head tail)
        (and (transparent? head) (transparent? tail))]
      [(~receive _ _ _ producer consumer)
        (and (transparent? producer) (transparent? consumer))]
      [_ #f]))

  (define (letrectify exp . opt)
    (define seal-private-bindings? (if (pair? opt) (car opt) #f))

    (define declarative (compute-declarative-toplevels exp))
    (define private (if seal-private-bindings?
                     (compute-private-toplevels declarative)
                     (make-hashtable equal-hash equal?)))
    (define declarative-box+value
      (let ([tab (make-hashtable equal-hash equal?)])
        (for-each
          (lambda (kv)
            (define key (car kv))
            (define val (cdr kv))

            (define box (and (not (hashtable-ref private key #f)) (gensym)))
            (define val (gensym))

            (hashtable-set! tab key (cons box val)))
          (hashtable->alist declarative))
        (lambda (mod name) (hashtable-ref tab (cons mod name) #f))))

    (define (add-binding name var val tail)
      (match tail
        [(~let src 'letrec* names vars vals tail)
          (make-let src
            'letrec*
            (cons name names)
            (cons var vars)
            (cons val vals)
            tail)]
        [_
          (make-let (term-src tail)
            'letrec*
            (list name)
            (list var)
            (list val)
            tail)]))
    (define (add-statement src stmt tail)
      (if (transparent? stmt)
        tail
        (add-binding
          '_
          (gensym "_")
          (make-sequence src stmt (make-void src))
          tail)))

    (define (visit-expr expr)
      (post-order
        (lambda (expr)
          (match expr
            [(~toplevel-ref src mod name)
              (match (declarative-box+value mod name)
                ['#f expr]
                [(~cons box value)
                  (make-lref src name value)])]
            [_ expr]))
        expr))
    (define (visit-top-level expr mod-vars)
      (match expr
        [(~toplevel-define src mod name exp)
          (match (declarative-box+value mod name)
            ['#f (values (visit-expr expr) mod-vars)]
            [(~cons '#f value)
              (values (add-binding
                       name
                       value
                       (visit-expr exp)
                       (make-void src))
                mod-vars)]
            [(~cons box value)
              (match (assoc mod mod-vars)
                ['#f
                  (let* ([mod-var (gensym "mod")]
                         [mod-vars (cons (cons mod mod-var) mod-vars)])
                    (receive (tail mod-vars) (visit-top-level expr mod-vars)
                      (values
                        (add-binding
                          'mod
                          mod-var
                          (make-primcall src 'current-module '())
                          tail)
                        mod-vars)))]
                [(~cons _ mod-var)
                  (define loc (make-primcall src 'module-ensure-local-variable!
                               (list (make-lref src 'mod mod-var)
                                 (make-constant src name))))
                  (define exp* (visit-expr exp))
                  (define ref (make-lref src name value))
                  (define init (make-primcall
                                src
                                'variable-set!
                                (list (make-lref src name box) ref)))
                  (values
                    (add-binding
                      name
                      box
                      loc
                      (add-binding
                        name
                        value
                        exp*
                        (add-statement src init (make-void src))))
                    mod-vars)])])]
        [(~let src style names vars vals body)
          (let loop ([names names] [vars vars] [vals vals] [mod-vars mod-vars])
            (match (vector names vars vals)
              [(~vector '() '() '())
                (values (visit-expr body) mod-vars)]
              [(~vector (~cons name names) (~cons var vars) (~cons val vals))
                (let* ([val (visit-expr val)]
                       [mod-vars
                         (match val
                           [(~application _
                               (~module-ref _ '(capy) 'define-module* _)
                               (~cons (~constant _ mod) _))
                             (cons (cons mod var) mod-vars)]
                           [_ mod-vars])])
                  (receive (exp mod-vars) (loop names vars vals mod-vars)
                    (values (add-binding name var val exp) mod-vars)))]))]
        [(~sequence src head tail)
          (receive (head mod-vars) (visit-top-level head mod-vars)
            (receive (tail mod-vars) (visit-top-level tail mod-vars)
              (values
                (match head
                  [(~let src2 'letrec* names vars vals head)
                    (fold-right
                      add-binding
                      (add-statement src head tail)
                      names
                      vars
                      vals)]
                  [_ (add-statement src head tail)])
                mod-vars)))]
        [_ (values (visit-expr expr) mod-vars)]))
    (receive (exp mod-vars) (visit-top-level exp '())

      exp)))
