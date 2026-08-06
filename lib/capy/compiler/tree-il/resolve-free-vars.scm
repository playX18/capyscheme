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

  ;; Collects the lexicals that are ever the target of an lset!, so
  ;; that they are never treated as module-valued.

  (define (collect-assigned-lexicals exp)
    (define assigned-lexicals '())
    (define (record-assigned-lexical! var)
      (set! assigned-lexicals (cons var assigned-lexicals)))

    ((make-tree-il-folder)
      exp
      (lambda (exp)
        (match exp
          [(~lset _ _ var _)
            (record-assigned-lexical! var)
            (values)]
          [_ (values)]))
      (lambda (exp) (values)))
    assigned-lexicals)

  ;; Builds a function that resolves a name to its binding module for
  ;; one imported interface: (module . public-name), or #f when the
  ;; name is not found there.
  ;;
  ;; The text of a program cannot tell us which of several used
  ;; interfaces provides a binding: renamers run at run-time, and using
  ;; an interface does not reveal what it defines.  The first-class
  ;; module interface is the only reliable source.
  ;;
  ;; An interface backed by an autoload may fail to load; in that case
  ;; the binding is treated as absent from that module, matching what
  ;; happens at expand time and run time.

  (define (make-resolver mod local-definitions)
    (define (make-import-resolver iface)
      (let ((public-names (make-eq-hashtable)))
        (guard (e (else #f))
          (let ([public-iface (resolve-interface (module-name iface) #f '() #f)])
            (module-for-each
              (lambda (name var)
                (hashtable-set! public-names var name))
              public-iface)))

        (lambda (name)
          (let ((var (module-variable iface name)))
            (and var
              (cons (module-name iface)
                (hashtable-ref public-names var)))))))

    (define target-module (resolve-module mod #t #f))
    (define import-resolvers
      (map make-import-resolver (module-uses target-module)))

    (lambda (name)
      (cond
        ((or (module-local-variable target-module name)
            (memq name local-definitions))
          'local)
        (else
          (match (filter-map (lambda (resolver)
                              (resolver name))
                  import-resolvers)
            ('() 'unknown)
            ((~list (~cons mod '#f)) 'unknown)
            ((~list (~cons mod public-name)) (cons mod public-name))
            (_ 'duplicate))))))

  ;; Analyzes a whole term and returns a resolver for toplevel
  ;; references: (lambda (mod name) verdict) where verdict is 'unknown,
  ;; 'local, 'duplicate, or (module . public-name).

  (define (make-free-var-resolver exp)
    (define assigned-lexicals (collect-assigned-lexicals exp))
    (define module-defs '())
    (define module-valued-lexicals '())
    (define toplevel-bindings '())
    (define (record-module-definition! mod args)
      (set! module-defs (cons (cons mod args) module-defs)))
    (define (record-module-lexical! var mod)
      (unless (memq var assigned-lexicals)
        (set! module-valued-lexicals (cons (cons var mod) module-valued-lexicals))))
    (define (record-toplevel-binding! mod name)
      (set! toplevel-bindings (cons (cons mod name) toplevel-bindings)))

    ;; A let/fix whose right-hand sides include a define-module* call
    ;; (or a current-module reference) tells us which module the bound
    ;; lexicals stand for; that lets us rewrite references to them in
    ;; terms of that module even though they are lexicals.

    (define (collect-module-bindings! mod vars vals)
      (for-each
        (lambda (var val)
          (match val
            [(~application _ (~module-ref _ '(capy) 'define-module* _)
                (~cons (~constant _ mod) args))
              (record-module-definition! mod args)
              (record-module-lexical! var mod)]
            [(~primcall _ 'current-module '())
              (when mod
                (record-module-lexical! var mod))]
            [_ #f]))
        vars
        vals))

    (define (scan exp) (scan-in-module exp #f))
    (define (scan-each exp)
      (unless (null? exp)
        (scan (car exp))
        (scan-each (cdr exp))))

    ;; Scans a sequence of expressions, threading the current module.
    ;; The result is the module implied by the last expression, or #f
    ;; once the thread breaks (an expression whose module is unknown
    ;; or disagrees with the running thread).

    (define (scan-threaded exps mod)
      (match exps
        ['() mod]
        [(~cons exp rest)
          (let loop ([first-mod (scan-in-module exp mod)] [exps rest])
            (match exps
              ['() first-mod]
              [(~cons exp rest)
                (let ([next-mod (scan-in-module exp first-mod)])
                  (loop (and (equal? first-mod next-mod) first-mod) rest))]))]))

    (define (scan-in-module exp mod)
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
            [(assq var module-valued-lexicals) => cdr]
            [else #f])]
        [(~application _ proc args)
          (scan proc)
          (scan-each args)
          #f]
        [(~primcall _ 'current-module (~list (~lref _ _ var)))
          (cond
            [(assq var module-valued-lexicals) => cdr]
            [else #f])]
        [(~primcall _ _ args)
          (scan-threaded args mod)]

        [(~if _ test consequent alternate)
          (scan-threaded (list test consequent alternate) mod)]
        [(~lset _ _ _ val)
          (scan-in-module val mod)]
        [(~toplevel-set _ _ _ val) (scan-in-module val mod)]
        [(~module-set _ _ _ _ val) (scan-in-module val mod)]
        [(~toplevel-define _ mod name val)
          (record-toplevel-binding! mod name)
          (scan-in-module val mod)]
        [(~proc _ _ body _ _)
          (scan body)
          mod]
        [(~sequence _ head tail)
          (scan-in-module tail (scan-in-module head mod))]
        [(~let src style ids lhs rhs body)
          (collect-module-bindings! mod lhs rhs)
          (scan-in-module body (scan-threaded rhs mod))]
        [(~fix src ids lhs rhs body)
          (collect-module-bindings! mod lhs rhs)
          (scan-in-module body (scan-threaded rhs mod))]
        [(~receive src ids vars producer consumer)
          (scan-in-module consumer (scan-in-module producer mod))]
        [(~values _ vals)
          (scan-each vals)
          #f]
        [(~wcm _ key mark result)
          (scan-in-module result (scan-in-module mark (scan-in-module key mod)))]
        [_ mod]))

    (scan exp)

    ;; A module is usable as a resolution target only if it is defined
    ;; exactly once in the term; modules defined more than once are
    ;; excluded.

    (define singly-defined-modules
      (let loop ([defs module-defs] [not-declarative '()] [declarative '()])
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
    (define module-resolvers
      (map (lambda (mod)
            (define resolver
              (make-resolver mod
                (filter-map (lambda (binding)
                             (match binding
                               [(~cons mod* name)
                                 (and (equal? mod* mod) name)]
                               [_ #f]))
                  toplevel-bindings)))
            (cons mod resolver))
        singly-defined-modules))

    (lambda (mod name)

      (cond
        [(assoc mod module-resolvers) =>
          (lambda (cell)

            (define resolver (cdr cell))
            (resolver name))]
        [else 'unknown])))

  (define (resolve-free-vars exp)
    "Traverses exp, extracting module-level definitions and rewriting
     free toplevel references into public module references."

    (define resolve (make-free-var-resolver exp))

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
