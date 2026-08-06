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

  ;; A plain folder used for whole-term walks that need no seed.

  (define plain-folder (make-tree-il-folder))

  (define (for-each-term f x)
    (plain-folder x (lambda (x) (f x) (values)) (lambda (x) (values))))

  ;; Collects the toplevel definitions that are safe to turn into
  ;; lexicals: those whose module is declarative and which are neither
  ;; assigned nor shadowed by a dynamic (unqualified) definition.
  ;;
  ;; - toplevel-set! on an unqualified name poisons the name itself;
  ;;   on a qualified name it poisons the (module . name) pair.
  ;; - a second toplevel-define of the same (module . name) moves the
  ;;   pair from `defined` to `assigned`.
  ;; - unqualified defines poison the name as dynamic.
  (define (collect-declarative-toplevels exp)
    (define dynamic (make-hashtable equal-hash equal?))
    (define defined (make-hashtable equal-hash equal?))
    (define assigned (make-hashtable equal-hash equal?))

    (for-each-term
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

    (define (module-is-declarative? mod)
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
                     (not (module-is-declarative? mod)))
              (hashtable-set! declarative k expr))]
          [_ #f]))
      (hashtable->alist defined))
    declarative)

  ;; Of the declarative toplevels, which are not reachable through
  ;; their module's public interface?  A module that exports a macro
  ;; cannot have any of its toplevels privatized, because macros are
  ;; expanded at compile time and the expander may not see the
  ;; binding's final value.

  (define (collect-private-toplevels declarative)

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

  ;; A term with no observable effect (no calls, no assignments, no
  ;; control flow) can be dropped instead of hoisted into a binding.

  (define (side-effect-free? exp)
    (match exp
      [(~or
          (~void _)
          (~constant _ _)
          (~lref _ _ _)
          (~proc _ _ _ _ _))
        #t]
      [(~if _ test then else) (and
                               (side-effect-free? test)
                               (side-effect-free? then)
                               (side-effect-free? else))]
      [(~sequence _ head tail)
        (and (side-effect-free? head) (side-effect-free? tail))]
      [(~receive _ _ _ producer consumer)
        (and (side-effect-free? producer) (side-effect-free? consumer))]
      [_ #f]))

  (define (letrectify exp . opt)
    (define seal-private? (if (pair? opt) (car opt) #f))

    (define declarative (collect-declarative-toplevels exp))
    (define private (if seal-private?
                     (collect-private-toplevels declarative)
                     (make-hashtable equal-hash equal?)))
    ;; Each declarative toplevel becomes a box (if exported) and a
    ;; value gensym.  Boxed bindings are initialized with the module
    ;; accessor and later patched with variable-set!; unboxed ones are
    ;; bound directly to their initial value.
    (define declaration-cell
      (let ([tab (make-hashtable equal-hash equal?)])
        (for-each
          (lambda (kv)
            (define key (car kv))

            (define box (and (not (hashtable-ref private key #f)) (gensym)))
            (define value (gensym))

            (hashtable-set! tab key (cons box value)))
          (hashtable->alist declarative))
        (lambda (mod name) (hashtable-ref tab (cons mod name) #f))))

    (define (prepend-binding name var val tail)
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
    (define (hoist-statement src stmt tail)
      (if (side-effect-free? stmt)
        tail
        (prepend-binding
          '_
          (gensym "_")
          (make-sequence src stmt (make-void src))
          tail)))

    ;; Rewrites references to declarative toplevels into direct
    ;; lexical references to their value or box.

    (define (rewrite-expr expr)
      (post-order
        (lambda (expr)
          (match expr
            [(~toplevel-ref src mod name)
              (match (declaration-cell mod name)
                ['#f expr]
                [(~cons box value)
                  (make-lref src name value)])]
            [_ expr]))
        expr))
    (define (rewrite-toplevel expr module-vars)
      (match expr
        [(~toplevel-define src mod name exp)
          (match (declaration-cell mod name)
            ['#f (values (rewrite-expr expr) module-vars)]
            [(~cons '#f value)
              (values (prepend-binding
                       name
                       value
                       (rewrite-expr exp)
                       (make-void src))
                module-vars)]
            [(~cons box value)
              (match (assoc mod module-vars)
                ['#f
                  (let* ([module-var (gensym "mod")]
                         [module-vars (cons (cons mod module-var) module-vars)])
                    (receive (tail module-vars) (rewrite-toplevel expr module-vars)
                      (values
                        (prepend-binding
                          'mod
                          module-var
                          (make-primcall src 'current-module '())
                          tail)
                        module-vars)))]
                [(~cons _ module-var)
                  (define loc (make-primcall src 'module-ensure-local-variable!
                               (list (make-lref src 'mod module-var)
                                 (make-constant src name))))
                  (define exp* (rewrite-expr exp))
                  (define ref (make-lref src name value))
                  (define init (make-primcall
                                src
                                'variable-set!
                                (list (make-lref src name box) ref)))
                  (values
                    (prepend-binding
                      name
                      box
                      loc
                      (prepend-binding
                        name
                        value
                        exp*
                        (hoist-statement src init (make-void src))))
                    module-vars)])])]
        [(~let src style names vars vals body)
          (let loop ([names names] [vars vars] [vals vals] [module-vars module-vars])
            (match (vector names vars vals)
              [(~vector '() '() '())
                (values (rewrite-expr body) module-vars)]
              [(~vector (~cons name names) (~cons var vars) (~cons val vals))
                (let* ([val (rewrite-expr val)]
                       [module-vars
                         (match val
                           [(~application _
                               (~module-ref _ '(capy) 'define-module* _)
                               (~cons (~constant _ mod) _))
                             (cons (cons mod var) module-vars)]
                           [_ module-vars])])
                  (receive (exp module-vars) (loop names vars vals module-vars)
                    (values (prepend-binding name var val exp) module-vars)))]))]
        [(~sequence src head tail)
          (receive (head module-vars) (rewrite-toplevel head module-vars)
            (receive (tail module-vars) (rewrite-toplevel tail module-vars)
              (values
                (match head
                  [(~let let-src 'letrec* names vars vals head)
                    (fold-right
                      prepend-binding
                      (hoist-statement src head tail)
                      names
                      vars
                      vals)]
                  [_ (hoist-statement src head tail)])
                module-vars)))]
        [_ (values (rewrite-expr expr) module-vars)]))
    (receive (exp module-vars) (rewrite-toplevel exp '())

      exp)))
