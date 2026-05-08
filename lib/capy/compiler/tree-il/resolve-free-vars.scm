(library (capy compiler tree-il resolve-free-vars)
  (export resolve-free-vars
    make-resolver)
  (import
    (capy)
    (rnrs)
    (rnrs hashtables)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms)
    (srfi 257)
    (srfi 1))

  (define (compute-assigned-lexicals exp)
    (define assigned-lexicals '())
    (define (add-assigned-lexical! var)
      (set! assigned-lexicals (cons var assigned-lexicals)))

    ((make-tree-il-folder)
      exp
      (lambda (exp)
        (match exp
          [(~lset _ _ var _)
            (add-assigned-lexical! var)
            (values)]
          [_ (values)]))
      (lambda (exp) (values)))
    assigned-lexicals)

  (define (make-resolver mod local-definitions)
    ;; Given that module A imports B and C, and X is free in A,
    ;; unfortunately there are a few things preventing us from knowing
    ;; whether the binding proceeds from B or C, just based on the text:
    ;;
    ;;  - Renamers are evaluated at run-time.
    ;;  - Just using B doesn't let us know what definitions are in B.
    ;;
    ;; So instead of using the source program to determine where a binding
    ;; comes from, we use the first-class module interface.
    (define (imported-resolver iface)
      (let ((by-var (make-eq-hashtable)))
        ;; When resolving a free variable, Guile visits all used modules
        ;; to see if there is a binding.  If one of those imports is an
        ;; autoload, it's possible that the autoload interface fails to
        ;; load.  In that case Guile will issue a warning and consider the
        ;; binding not found in that module.  Here we try to produce the
        ;; same behavior at optimization time that we do at expand time
        ;; that we would do at run time.
        (guard (e (else #f))
          (let ([public-iface (resolve-interface (module-name iface) #f '() #f)])
            (module-for-each
              (lambda (name var)
                (hashtable-set! by-var var name))
              public-iface)))

        (lambda (name)
          (let ((var (module-variable iface name)))
            (and var
              (cons (module-name iface)
                (hashtable-ref by-var var)))))))

    (define the-module (resolve-module mod #t #f))
    (define resolvers
      (map imported-resolver (module-uses the-module)))

    (lambda (name)
      (cond
        ((or (module-local-variable the-module name)
            (memq name local-definitions))
          'local)
        (else
          (match (filter-map (lambda (resolve)
                              (resolve name))
                  resolvers)
            ('() 'unknown)
            ((~list (~cons mod '#f)) 'unknown)
            ((~list (~cons mod public-name)) (cons mod public-name))
            (_ 'duplicate))))))

  (define (compute-free-var-resolver exp)
    (define assigned-lexicals (compute-assigned-lexicals exp))
    (define module-definitions '())
    (define module-lexicals '())
    (define bindings '())
    (define (add-module-definition! mod args)
      (set! module-definitions (cons (cons mod args) module-definitions)))
    (define (add-module-lexical! var mod)
      (unless (memq var assigned-lexicals)
        (set! module-lexicals (cons (cons var mod) module-lexicals))))
    (define (add-binding! mod name)
      (set! bindings (cons (cons mod name) bindings)))

    (define (record-bindings! mod vars vals)
      (for-each
        (lambda (var val)
          (match val
            [(~application _ (~module-ref _ '(capy) 'define-module* _)
                (~cons (~constant _ mod) args))
              (add-module-definition! mod args)
              (add-module-lexical! var mod)]
            [(~primcall _ 'current-module '())
              (when mod
                (add-module-lexical! var mod))]
            [_ #f]))
        vars
        vals))

    (define (visit exp) (visit/mod exp #f))
    (define (visit* exp)
      (unless (null? exp)
        (visit (car exp))
        (visit* (cdr exp))))

    (define (visit+ exps mod)
      (match exps
        ['() mod]
        [(~cons exp rest)
          (let loop ([mod* (visit/mod exp mod)] [exps rest])
            (match exps
              ['() mod*]
              [(~cons exp rest)
                (let ([mod** (visit/mod exp mod*)])
                  (loop (and (equal? mod* mod**) mod*) rest))]))]))

    (define (visit/mod exp mod)
      (match exp
        [(~or
            (~void _)
            (~constant _ _)
            (~primref _ _)
            (~lref _ _ _)
            (~module-ref _ _ _ _)
            (~toplevel-ref _ _ _))
          mod]
        [(~application _
            (~module-ref _ '(capy) 'current-module _)
            (~list (~lref _ _ var)))
          (cond
            [(assq var module-lexicals) => cdr]
            [else #f])]
        [(~application _ proc args)
          (visit proc)
          (visit* args)
          #f]
        [(~primcall _ 'current-module (~list (~lref _ _ var)))
          (cond
            [(assq var module-lexicals) => cdr]
            [else #f])]
        [(~primcall _ _ args)
          (visit+ args mod)]

        [(~if _ test consequent alternate)
          (visit+ (list test consequent alternate) mod)]
        [(~lset _ _ _ val)
          (visit/mod val mod)]
        [(~toplevel-set _ _ _ val) (visit/mod val mod)]
        [(~module-set _ _ _ _ val) (visit/mod val mod)]
        [(~toplevel-define _ mod name val)
          (add-binding! mod name)
          (visit/mod val mod)]
        [(~proc _ _ body _ _)
          (visit body)
          mod]
        [(~sequence _ head tail)
          (visit/mod tail (visit/mod head mod))]
        [(~let src style ids lhs rhs body)
          (record-bindings! mod lhs rhs)
          (visit/mod body (visit+ rhs mod))]
        [(~fix src ids lhs rhs body)
          (record-bindings! mod lhs rhs)
          (visit/mod body (visit+ rhs mod))]
        [(~receive src ids vars producer consumer)
          (visit/mod consumer (visit/mod producer mod))]
        [(~values _ vals)
          (visit* vals)
          #f]
        [(~wcm _ key mark result)
          (visit/mod result (visit/mod mark (visit/mod key mod)))]
        [_ mod]))

    (visit exp)

    (define declarative-modules
      (let loop ([defs module-definitions] [not-declarative '()] [declarative '()])
        (match defs
          ['() declarative]
          [(~cons (~cons mod args) defs)
            (cond
              [(member mod not-declarative)
                (loop defs not-declarative declarative)]
              [(assoc mod defs)
                (loop defs (cons mod not-declarative) declarative)]
              [else
                (loop defs not-declarative (cons mod declarative))])])))
    (define resolvers
      (map (lambda (mod)
            (define resolve
              (make-resolver mod
                (filter-map (lambda (binding)
                             (match binding
                               [(~cons mod* name)
                                 (and (equal? mod* mod) name)]
                               [_ #f]))
                  bindings)))
            (cons mod resolve))
        declarative-modules))

    (lambda (mod name)

      (cond
        [(assoc mod resolvers) =>
          (lambda (cell)

            (define resolve (cdr cell))
            (resolve name))]
        [else 'unknown])))

  (define (resolve-free-vars exp)
    "Traverse exp, extracting module level definitions."

    (define resolve (compute-free-var-resolver exp))

    (post-order
      (lambda (exp)
        (match exp
          [(~toplevel-ref src mod name)
            (match (resolve mod name)
              [(~or 'unknown 'duplicate 'local) exp]
              [(~cons mod name)
                (make-module-ref src mod name #t)])]
          [(~toplevel-set src mod name val)
            (match (resolve mod name)

              [(~or 'unknown 'duplicate 'local) exp]
              [(~cons mod name)
                (make-module-set src mod name #t val)])]
          [exp exp]))
      exp)))
