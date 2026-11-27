;; Tree IL -> graph CPS conversion.
(library (core compiler cps convert)
  (export
    cps-toplevel
    graph-function-program? graph-function-program-graph graph-function-program-entry
    graph-function-program-root
    make-graph-program graph-program? graph-program-graph graph-program-root
    make-convert-error convert-error? convert-error-kind convert-error-arg
    convert-error->string
    make-graph-cps-builder cps-builder-graph fresh-variable bind-lvar use-lvar
    new-child-link emit-continue emit-raise emit-app emit-if emit-if-continue
    emit-let-val emit-prim-let emit-fix emit-letk emit-branch-prim let-prim
    bind-literal make-cont make-proc-func cps-func convert convert-arg convert-args
    primitive-transformer-ref try-expand-primitive
    assertion-violation* ensure-string ensure-pair prim-box cached-prim-box
    convert-il)
  (import
    (core) (capy)
    (core compiler cps graph)
    (core compiler cps primitives)
    (prefix (core compiler tree-il terms) il:))

  ;; =====================================================================
  ;; Result types (cps/convert.rs)

  (define-record-type (convert-error make-convert-error convert-error?)
    (fields kind arg))

  (define (convert-error->string e)
    (let ([arg (convert-error-arg e)])
      (case (convert-error-kind e)
        [(dead-term-link)
          (string-append "dead term link while lowering graph: "
            (entity-display-name "term_link" arg))]
        [(dead-expr-link)
          (string-append "dead expr link while lowering graph: "
            (entity-display-name "expr_link" arg))]
        [(dead-function-link)
          (string-append "dead function link while lowering graph: "
            (entity-display-name "function_link" arg))]
        [(function-in-fix-has-no-return-continuation)
          (string-append "function in fix has no return continuation: "
            (entity-display-name "function" arg))]
        [(function-in-letk-has-return-continuation)
          (string-append "continuation in letk has a return continuation: "
            (entity-display-name "function" arg))]
        [else "unknown convert error"])))

  (define-record-type (graph-program make-graph-program graph-program?)
    (fields graph root))

  (define-record-type (graph-function-program make-graph-function-program graph-function-program?)
    (fields graph entry))

  (define (graph-function-program-root p)
    (function-body (graph-function (graph-function-program-graph p)
                     (graph-function-program-entry p))))

  ;; =====================================================================
  ;; The converter's term view (expander::core::Term).  Fields by kind:
  ;;
  ;;   lref         a=lvar
  ;;   const        a=value
  ;;   call         a=proc term       b=list of arg terms
  ;;   primcall     a=prim symbol     b=list of arg terms
  ;;   primref      a=name
  ;;   if           a=test b=then c=else
  ;;   let          a=style b=lhs lvars c=rhs terms d=body
  ;;   fix          a=lhs lvars b=list of cprocs c=body
  ;;   proc         a=cproc
  ;;   receive      a=lvars b=variadic lvar or #f c=producer d=consumer
  ;;   values       a=list of terms
  ;;   seq          a=head b=tail
  ;;   wcm          a=key b=mark c=result
  ;;   global-ref   a=key b=name c=slot       slot = (box . lvar) | (variable . v)
  ;;   global-set   a=key b=name c=slot d=value term
  ;;   linklet      a=#(name imports exports slots body meta)
  ;;   case-lambda  a=list of cprocs

  (define-record-type (cterm make-cterm cterm?)
    (fields kind src a b c d))

  ;; expander::core::Proc: name source args(lvars) variadic(lvar|#f) body meta
  (define-record-type (cproc make-cproc cproc?)
    (fields name source args variadic body meta))

  (define undefined-value (if #f #f))

  ;; =====================================================================
  ;; TermConverter: (boot expand) records -> cterms

  ;; Normalize a &term source slot to a 9-vector sourcev or #f
  ;; (term_sourcev / props_to_sourcev).
  (define (il-sourcev t)
    (let ([src (il:term-src t)])
      (cond
        [(pair? src)
          (let ([prop (lambda (key)
                        (let ([e (assq key src)]) (if e (cdr e) #f)))])
            (vector (prop 'filename) (prop 'line) (prop 'column) (prop 'end-line)
              (prop 'end-column) (prop 'start-byte) (prop 'end-byte) (prop 'origin)
              (prop 'related-spans)))]
        [(vector? src) src]
        [else #f])))

  (define (safe-assq key alist)
    (let loop ([l alist])
      (cond
        [(not (pair? l)) #f]
        [(and (pair? (car l)) (eq? (caar l) key)) (car l)]
        [else (loop (cdr l))])))

  ;; Convert a Tree IL record into a cterm.  The symbol -> lvar table plays
  ;; the role of TermConverter::lvars (lookup only).
  (define (convert-il il)
    (define lvars (make-eq-hashtable))

    (define (bind-new! sym id)
      (let ([lvar (make-lvar sym id)])
        (hashtable-set! lvars sym lvar)
        lvar))

    (define (lookup sym)
      (or (hashtable-ref lvars sym #f)
        (assertion-violation 'cps-toplevel "unbound lvar" sym)))

    (define (global-slot slot)
      (cond
        [(variable? slot) (cons 'variable slot)]
        [(hashtable-ref lvars slot #f) => (lambda (lvar) (cons 'box lvar))]
        [else #f]))

    (define (first-or-false l) (if (pair? l) (car l) #f))

    ;; Shared by plain lambdas and by every clause of a case-lambda.
    (define (conv-proc t)
      (let* ([source (il-sourcev t)]
             [args (il:proc-args t)]
             [body (il:proc-body t)]
             [meta (il:proc-meta t)]
             [ids (il:proc-ids t)]
             [name (let ([e (safe-assq 'name meta)]) (if e (cdr e) #f))])
        (let loop ([ls-args args] [ls-ids ids] [acc '()])
          (if (pair? ls-args)
            (let ([lvar (bind-new! (car ls-args) (first-or-false ls-ids))])
              (loop (cdr ls-args) (if (pair? ls-ids) (cdr ls-ids) ls-ids) (cons lvar acc)))
            (let* ([variadic (and (not (null? ls-args))
                               (bind-new! ls-args
                                 (if (pair? ls-ids) (car ls-ids) ls-ids)))]
                   [body (conv body)])
              (make-cproc name source (reverse acc) variadic body meta))))))

    (define (conv-list ts)
      (let loop ([ts ts] [acc '()])
        (if (pair? ts)
          (let ([x (conv (car ts))])
            (loop (cdr ts) (cons x acc)))
          (reverse acc))))

    (define (conv t)
      (cond
        [(not (il:term? t)) (assertion-violation 'cps-toplevel "not a Tree IL term" t)]
        [else
          (let ([source (il-sourcev t)])
            (cond
              [(il:lref? t) (make-cterm 'lref source (lookup (il:lref-sym t)) #f #f #f)]
              [(il:lset? t)
                (lookup (il:lset-sym t))
                (conv (il:lset-value t))
                (make-cterm 'lset source #f #f #f #f)]
              [(il:constant? t) (make-cterm 'const source (il:constant-value t) #f #f #f)]
              [(il:void? t) (make-cterm 'const source undefined-value #f #f #f)]
              [(il:application? t)
                (let* ([op (conv (il:application-operator t))]
                       [args (conv-list (il:application-operands t))])
                  (make-cterm 'call source op args #f #f))]
              [(il:primcall? t)
                (let ([prim (il:primcall-prim t)])
                  (if (eq? prim '%case-lambda)
                    (let loop ([ls (il:primcall-args t)] [procs '()])
                      (cond
                        [(pair? ls)
                          (unless (il:proc? (car ls))
                            (assertion-violation 'cps-toplevel "bad case-lambda clause" t))
                          (let ([p (conv-proc (car ls))])
                            (loop (cdr ls) (cons p procs)))]
                        [(null? ls)
                          (make-cterm 'case-lambda source (reverse procs) #f #f #f)]
                        [else (assertion-violation 'cps-toplevel "bad case-lambda" t)]))
                    (make-cterm 'primcall source prim (conv-list (il:primcall-args t)) #f #f)))]
              [(il:primref? t) (make-cterm 'primref source (il:primref-prim t) #f #f #f)]
              [(il:if? t)
                (let* ([c (conv (il:if-test t))]
                       [th (conv (il:if-then t))]
                       [el (conv (il:if-else t))])
                  (make-cterm 'if source c th el #f))]
              [(il:let? t)
                (let ([style (il:let-style t)])
                  (unless (memq style '(let let* letrec letrec*))
                    (assertion-violation 'cps-toplevel "bad let style" t))
                  (let loop ([ids (il:let-ids t)] [lhs (il:let-lhs t)] [rhs (il:let-rhs t)]
                             [lvs '()] [exprs '()])
                    (if (pair? ids)
                      (let ([lvar (bind-new! (car lhs) (car ids))])
                        (loop (cdr ids) (cdr lhs) (cdr rhs)
                          (cons lvar lvs) (cons (car rhs) exprs)))
                      (let* ([rhs (conv-list (reverse exprs))]
                             [body (conv (il:let-body t))])
                        (make-cterm 'let source style (reverse lvs) rhs body)))))]
              [(il:fix? t)
                (let loop ([ids (il:fix-ids t)] [lhs (il:fix-lhs t)] [rhs (il:fix-rhs t)]
                           [lvs '()] [exprs '()])
                  (if (pair? ids)
                    (let ([lvar (bind-new! (car lhs) (car ids))])
                      (loop (cdr ids) (cdr lhs) (cdr rhs)
                        (cons lvar lvs) (cons (car rhs) exprs)))
                    (let* ([procs
                             (let ploop ([exprs (reverse exprs)] [acc '()])
                               (if (null? exprs)
                                 (reverse acc)
                                 (let ([c (conv (car exprs))])
                                   (unless (eq? (cterm-kind c) 'proc)
                                     (assertion-violation 'cps-toplevel "fix rhs is not a proc" (car exprs)))
                                   (ploop (cdr exprs) (cons (cterm-a c) acc)))))]
                           [body (conv (il:fix-body t))])
                      (make-cterm 'fix source (reverse lvs) procs body #f))))]
              [(il:proc? t) (make-cterm 'proc source (conv-proc t) #f #f #f)]
              [(il:receive? t)
                (let loop ([vars (il:receive-vars t)] [ids (il:receive-ids t)] [lvs '()])
                  (if (pair? vars)
                    (let ([lvar (bind-new! (car vars) (first-or-false ids))])
                      (loop (cdr vars) (if (pair? ids) (cdr ids) ids) (cons lvar lvs)))
                    (let* ([variadic (and (not (null? vars))
                                       (bind-new! vars (if (pair? ids) (cdr ids) #f)))]
                           [producer (conv (il:receive-producer t))]
                           [consumer (conv (il:receive-consumer t))])
                      (make-cterm 'receive source (reverse lvs) variadic producer consumer))))]
              [(il:values? t)
                (make-cterm 'values source (conv-list (il:values-values t)) #f #f #f)]
              [(il:sequence? t)
                ;; Flattened and rebuilt right-nested, like seq_from_slice: only
                ;; the outermost node keeps the source.
                (let loop ([cur t] [acc '()])
                  (if (il:sequence? cur)
                    (let ([h (conv (il:sequence-head cur))])
                      (loop (il:sequence-tail cur) (cons h acc)))
                    (let* ([last (conv cur)]
                           [heads (reverse acc)]
                           [built
                             (let build ([heads heads])
                               (if (null? (cdr heads))
                                 (make-cterm 'seq #f (car heads) last #f #f)
                                 (make-cterm 'seq #f (car heads) (build (cdr heads)) #f #f)))])
                      (make-cterm 'seq source (cterm-a built) (cterm-b built) #f #f))))]
              [(il:wcm? t)
                (let* ([key (conv (il:wcm-key t))]
                       [mark (conv (il:wcm-mark t))]
                       [body (conv (il:wcm-result t))])
                  (make-cterm 'wcm source key mark body #f))]
              [(il:global-ref? t)
                (let ([slot (global-slot (il:global-ref-slot t))])
                  (unless slot (assertion-violation 'cps-toplevel "unresolved global slot" t))
                  (make-cterm 'global-ref source (il:global-ref-key t) (il:global-ref-name t) slot #f))]
              [(il:global-set? t)
                (let ([slot (global-slot (il:global-set-slot t))])
                  (unless slot (assertion-violation 'cps-toplevel "unresolved global slot" t))
                  (let ([value (conv (il:global-set-value t))])
                    (make-cterm 'global-set source (il:global-set-key t) (il:global-set-name t)
                      slot value)))]
              [(il:linklet-term? t)
                (let* ([name (il:linklet-term-name t)]
                       [imports (il:linklet-term-imports t)]
                       [exports (il:linklet-term-exports t)]
                       [slots (il:linklet-term-slots t)]
                       [meta (il:linklet-term-meta t)]
                       [lvs (let loop ([ls slots] [acc '()])
                              (if (pair? ls)
                                (loop (cdr ls) (cons (bind-new! (car ls) #f) acc))
                                (reverse acc)))]
                       [body (conv (il:linklet-term-body t))])
                  (make-cterm 'linklet source (vector name imports exports lvs body meta) #f #f #f))]
              [else (assertion-violation 'cps-toplevel "unsupported Tree IL term" t)]))]))

    (conv il))

  ;; =====================================================================
  ;; The builder (GraphCpsBuilder)

  ;; Shape of a continuation variable, recorded at make-cont so attachment
  ;; pop-wraps can forward values unchanged.
  (define-record-type (cont-shape make-cont-shape cont-shape?)
    (fields nargs has-rest (mutable is-consumer cont-shape-is-consumer cont-shape-is-consumer-set!)))

  (define-record-type (cps-builder %make-cps-builder cps-builder?)
    (fields
      g
      vars                              ; eq-hashtable lvar -> bound var (lookup only)
      cont-shapes                       ; eq-hashtable lvar -> cont-shape (lookup only)
      (mutable varcount builder-varcount builder-varcount-set!)
      (mutable current-meta builder-current-meta builder-current-meta-set!)
      unbound-checked                   ; eq-hashtable lvar -> #t (lookup only)
      (mutable case-group-count builder-case-group-count builder-case-group-count-set!)))

  (define (make-graph-cps-builder)
    (%make-cps-builder (make-graph) (make-eq-hashtable) (make-eq-hashtable) 0 '()
      (make-eq-hashtable) 0))

  (define (cps-builder-graph cps) (cps-builder-g cps))
  (define (cps-graph cps) (cps-builder-g cps))

  (define (fresh-case-group cps)
    (let ([id (builder-case-group-count cps)])
      (builder-case-group-count-set! cps (+ id 1))
      id))

  ;; A fresh uninterned variable named PREFIX<n>.
  (define (fresh-variable cps prefix)
    (let* ([ix (builder-varcount cps)]
           [var (make-lvar
                  (string->uninterned-symbol (string-append prefix (number->string ix)))
                  #f)])
      (builder-varcount-set! cps (+ ix 1))
      var))

  (define (bind-lvar cps lvar)
    (or (hashtable-ref (cps-builder-vars cps) lvar #f)
      (let ([bound (new-bound-var (cps-graph cps) lvar)])
        (hashtable-set! (cps-builder-vars cps) lvar bound)
        bound)))

  (define (use-lvar cps lvar owner)
    (let ([bound (bind-lvar cps lvar)])
      (new-free-occ-for-binder (cps-graph cps) bound owner)))

  ;; BINDS is a box holding the reversed list of literal binds
  ;; (bound-var value source); the most recent bind comes first.
  (define (use-atom cps atom source owner binds)
    (if (lvar? atom)
      (use-lvar cps atom owner)
      (let* ([binding (fresh-variable cps "literal")]
             [bound (bind-lvar cps binding)])
        (set-box! binds (cons (list bound atom source) (unbox binds)))
        (new-free-occ-for-binder (cps-graph cps) bound owner))))

  (define (use-atoms cps atoms source owner binds)
    (let ([vars (let loop ([atoms atoms] [acc '()])
                  (if (null? atoms)
                    (reverse acc)
                    (let ([v (use-atom cps (car atoms) source owner binds)])
                      (loop (cdr atoms) (cons v acc)))))])
      (new-free-vars (cps-graph cps) vars)))

  (define (new-child-link cps) (new-term-link (cps-graph cps) #f))

  (define (retarget-direct-free-owners cps term owner)
    (let* ([g (cps-graph cps)]
           [vars (direct-free-vars-of-term g term)]
           [kind (term-kind (graph-term g term))]
           [expr (and (k-let-val? kind) (read-expr-link g (k-let-val-expr kind)))]
           [vars (if expr (append vars (free-vars-of-expr g expr)) vars)])
      (for-each (lambda (var) (set-free-owner! g var owner)) vars)))

  ;; LITERAL-BINDS: reversed list as in use-atom; wrapping goes from the last
  ;; pushed bind outward.
  (define (wrap-literal-binds cps owner actual-term literal-binds)
    (if (null? literal-binds)
      actual-term
      (let* ([g (cps-graph cps)]
             [old-parent (read-parent-link g (term-uplink (graph-term g actual-term)))]
             [child-link (new-term-link g actual-term)])
        (retarget-direct-free-owners cps actual-term child-link)
        (let loop ([binds literal-binds] [child-term actual-term] [outer actual-term]
                   [child-link child-link])
          (if (null? binds)
            (begin
              (when old-parent
                (set-parent-link! g (term-uplink (graph-term g outer)) old-parent))
              (set-term-link! g owner outer)
              outer)
            (let* ([binding (car (car binds))]
                   [value (cadr (car binds))]
                   [source (caddr (car binds))]
                   [expr-link (new-expr-link g #f)]
                   [euplink (new-parent-link g #f)]
                   [expr (new-expr g euplink (make-k-literal value) source)])
              (set-expr-link! g expr-link expr)
              (let* ([wrapper-uplink (new-parent-link g #f)]
                     [wrapper (new-term g wrapper-uplink
                                (make-k-let-val binding expr-link child-link) source)])
                (set-parent-link! g (term-uplink (graph-term g child-term)) (parent-term wrapper))
                (set-parent-link! g euplink (parent-term wrapper))
                (loop (cdr binds) wrapper wrapper
                  (if (pair? (cdr binds)) (new-term-link g wrapper) child-link)))))))))

  (define (emit-term cps owner kind source literal-binds)
    (let* ([g (cps-graph cps)]
           [uplink (new-parent-link g #f)]
           [term (new-term g uplink kind source)])
      (set-term-link! g owner term)
      (backpatch-subterms! g term (subterms-of g term))
      (backpatch-subexprs! g term owner (subexprs-of g term))
      (wrap-literal-binds cps owner term literal-binds)))

  (define (emit-expr cps kind source)
    (let* ([g (cps-graph cps)]
           [link (new-expr-link g #f)]
           [uplink (new-parent-link g #f)]
           [expr (new-expr g uplink kind source)])
      (set-expr-link! g link expr)
      link))

  (define (emit-continue cps owner k args source)
    (let* ([binds (box '())]
           [cont (use-lvar cps k owner)]
           [args (use-atoms cps args source owner binds)])
      (emit-term cps owner (make-k-continue cont args) source (unbox binds))))

  ;; (branch-prim (prim args...) (k-true true-args...) (k-false false-args...))
  (define (emit-branch-prim cps owner prim args consequent true-args alternative false-args
            hints source)
    (let ([op (make-branch-op prim (length args) (length true-args) hints)]
          [binds (box '())])
      (let* ([consequent (use-lvar cps consequent owner)]
             [alternative (use-lvar cps alternative owner)]
             [vars (use-atoms cps (append args true-args false-args) source owner binds)])
        (emit-term cps owner (make-k-branch-prim op consequent alternative vars)
          source (unbox binds)))))

  (define (emit-raise cps owner kind args source)
    (let* ([binds (box '())]
           [args (use-atoms cps args source owner binds)])
      (emit-term cps owner (make-k-raise kind args) source (unbox binds))))

  (define (emit-app cps owner func k args source)
    (let* ([binds (box '())]
           [func (use-atom cps func source owner binds)]
           [args (use-atoms cps args source owner binds)]
           [cont (use-lvar cps k owner)])
      (emit-term cps owner (make-k-app func args cont) source (unbox binds))))

  (define (emit-if cps owner test consequent alternate hints source)
    (let* ([binds (box '())]
           [test (use-atom cps test source owner binds)])
      (emit-term cps owner (make-k-if test consequent alternate hints) source (unbox binds))))

  (define (emit-if-continue cps owner test consequent consequent-args alternate alternate-args
            hints source)
    (let ([consequent-link (new-child-link cps)])
      (emit-continue cps consequent-link consequent consequent-args source)
      (let ([alternate-link (new-child-link cps)])
        (emit-continue cps alternate-link alternate alternate-args source)
        (emit-if cps owner test consequent-link alternate-link hints source))))

  (define (emit-let-val cps owner binding expr body source literal-binds)
    (let ([binding (bind-lvar cps binding)])
      (emit-term cps owner (make-k-let-val binding expr body) source literal-binds)))

  (define (emit-prim-let cps owner binding prim args source body)
    (let* ([binds (box '())]
           [args (use-atoms cps args source owner binds)]
           [expr (emit-expr cps (make-k-prim-call prim args) source)])
      (emit-let-val cps owner binding expr body source (unbox binds))))

  ;; BODY: (lambda (cps owner binding-lvar) term-id)
  (define (let-prim cps owner prefix prim args source body)
    (let* ([binding (fresh-variable cps prefix)]
           [body-link (new-child-link cps)])
      (body cps body-link binding)
      (emit-prim-let cps owner binding prim args source body-link)))

  ;; BODY: (lambda (cps owner atom) term-id)
  (define (bind-literal cps owner value source body)
    (let* ([binding (fresh-variable cps "literal")]
           [body-link (new-child-link cps)])
      (body cps body-link binding)
      (let ([expr (emit-expr cps (make-k-literal value) source)])
        (emit-let-val cps owner binding expr body-link source '()))))

  (define (make-function-links cps functions)
    (let* ([g (cps-graph cps)]
           [links (let loop ([fs functions] [acc '()])
                    (if (null? fs)
                      (reverse acc)
                      (let ([link (new-function-link g #f)])
                        (set-function-link! g link (car fs))
                        (loop (cdr fs) (cons link acc)))))])
      (new-function-links g links)))

  (define (emit-fix cps owner functions body source)
    (let ([links (make-function-links cps functions)])
      (emit-term cps owner (make-k-fix links body) source '())))

  (define (emit-letk cps owner continuations body source)
    (let ([links (make-function-links cps continuations)])
      (emit-term cps owner (make-k-letk links body) source '())))

  (define (new-function* cps function)
    (let* ([g (cps-graph cps)]
           [id (new-function g function)])
      (backpatch-function! g id)
      id))

  (define (make-cont cps binding args variadic body source cold noinline meta)
    (let ([g (cps-graph cps)])
      (hashtable-set! (cps-builder-cont-shapes cps) binding
        (make-cont-shape (length args) (and variadic #t) #f))
      (let* ([var (bind-lvar cps binding)]
             [args (let loop ([as args] [acc '()])
                     (if (null? as)
                       (reverse acc)
                       (let ([b (bind-lvar cps (car as))])
                         (loop (cdr as) (cons b acc)))))]
             [variadic (and variadic (bind-lvar cps variadic))]
             [vars (new-bound-vars g args)])
        (new-function* cps
          (make-function #f source var vars variadic #f (and variadic #t) body
            #f cold noinline #f meta #f)))))

  (define (make-proc-func cps proc binding is-rec)
    (let* ([g (cps-graph cps)]
           [old-meta (builder-current-meta cps)])
      (builder-current-meta-set! cps (cproc-meta proc))
      (let* ([return-cont (fresh-variable cps "return")]
             [var (bind-lvar cps binding)]
             [cont (bind-lvar cps return-cont)]
             [args (let loop ([as (cproc-args proc)] [acc '()])
                     (if (null? as)
                       (reverse acc)
                       (let ([b (bind-lvar cps (car as))])
                         (loop (cdr as) (cons b acc)))))]
             [variadic (and (cproc-variadic proc) (bind-lvar cps (cproc-variadic proc)))]
             [vars (new-bound-vars g args)]
             [body (new-child-link cps)])
        (convert cps (cproc-body proc) return-cont body 'tail)
        (builder-current-meta-set! cps old-meta)
        (new-function* cps
          (make-function (cproc-name proc) (cproc-source proc) var vars variadic cont
            (and variadic #t) body is-rec #f #f #f (cproc-meta proc) #f)))))

  (define (cps-func cps proc binding) (make-proc-func cps proc binding #f))

  ;; =====================================================================
  ;; Entry point (cps_toplevel)

  (define (cps-toplevel il)
    (let* ([form (convert-il il)]
           [proc (make-cproc #f (cterm-src form) '() #f form '())]
           [cps (make-graph-cps-builder)]
           [bind (fresh-variable cps "toplevel")]
           [entry (cps-func cps proc bind)])
      (make-graph-function-program (cps-graph cps) entry)))

  ;; =====================================================================
  ;; Primitive transformers

  (define normal-hints '(Normal Normal))

  (define transformers (make-eq-hashtable))

  (define (primitive-transformer-ref prim) (hashtable-ref transformers prim #f))

  ;; PrimitiveTable::try_expand: a term id, or #f.
  (define (try-expand-primitive cps owner src prim args k)
    (let ([t (hashtable-ref transformers prim #f)])
      (and t (t cps owner src prim args k))))
  (define (assertion-violation* cps owner src opc message irritants)
    (emit-raise cps owner 'AssertionViolation (cons* opc (string-copy message) irritants) src))

  (define (primitive-wrong-number-of-arguments cps owner src kind got)
    (emit-raise cps owner kind (list got) src))

  (define (ensure-string cps owner src op x have-length)
    (let* ([not-string (fresh-variable cps "not_string")]
           [k (fresh-variable cps "k")]
           [check-string (fresh-variable cps "check_string")]
           [not-string-body (new-child-link cps)])
      (assertion-violation* cps not-string-body src op "not a string" (list x))
      (let* ([not-string-cont (make-cont cps not-string '() #f not-string-body #f #t #f #f)]
             [k-body (new-child-link cps)])
        (let-prim cps k-body "length" '%refptr (list x string-length-offset) src
          (lambda (cps owner length) (have-length cps owner length)))
        (let* ([k-cont (make-cont cps k '() #f k-body #f #f #f #f)]
               [check-body (new-child-link cps)])
          (let-prim cps check-body "is_string" '%class-id? (list x class-id-string) src
            (lambda (cps owner is-string)
              (emit-if-continue cps owner is-string k '() not-string '() normal-hints #f)))
          (let* ([check-cont (make-cont cps check-string '() #f check-body #f #f #f #f)]
                 [body (new-child-link cps)])
            (let-prim cps body "is_immediate" 'immediate? (list x) src
              (lambda (cps owner is-immediate)
                (emit-if-continue cps owner is-immediate not-string '() check-string '()
                  normal-hints #f)))
            (emit-letk cps owner (list not-string-cont k-cont check-cont) body #f))))))

  (define (ensure-pair cps owner src op x have-pair)
    (let* ([not-pair (fresh-variable cps "not_pair")]
           [k (fresh-variable cps "k")]
           [not-pair-body (new-child-link cps)])
      (assertion-violation* cps not-pair-body src op "not a pair" (list x))
      (let* ([not-pair-cont (make-cont cps not-pair '() #f not-pair-body #f #t #f #f)]
             [k-body (new-child-link cps)])
        (have-pair cps k-body x)
        (let* ([k-cont (make-cont cps k '() #f k-body #f #f #f #f)]
               [body (new-child-link cps)])
          (let-prim cps body "is_pair" 'pair? (list x) src
            (lambda (cps owner is-pair)
              (emit-if-continue cps owner is-pair k '() not-pair '() normal-hints #f)))
          (emit-letk cps owner (list not-pair-cont k-cont) body #f)))))

  (define (ensure-variable cps owner src op x have-variable)
    (let* ([not-variable (fresh-variable cps "not_variable")]
           [k (fresh-variable cps "k")]
           [not-variable-body (new-child-link cps)])
      (assertion-violation* cps not-variable-body src op
        (string-append "not a variable " (symbol->string (lvar-name not-variable)))
        (list x))
      (let* ([not-variable-cont (make-cont cps not-variable '() #f not-variable-body #f #t #f #f)]
             [k-body (new-child-link cps)])
        (have-variable cps k-body x)
        (let* ([k-cont (make-cont cps k '() #f k-body #f #f #f #f)]
               [body (new-child-link cps)])
          (let-prim cps body "is_variable" 'variable? (list x) src
            (lambda (cps owner is-variable)
              (emit-if-continue cps owner is-variable k '() not-variable '() normal-hints #f)))
          (emit-letk cps owner (list not-variable-cont k-cont) body #f)))))

  (define-syntax define-transformer
    (syntax-rules ()
      [(_ prim (cps owner src op args k) body ...)
        (hashtable-set! transformers 'prim
          (lambda (cps owner src op args k) body ...))]))

  (define (list-of-length? l n) (and (list? l) (= (length l) n)))

  (define-transformer assertion-violation (cps owner src op args k)
    ;; The direct raise ABI has four fixed payload slots.  Keep larger
    ;; variadic calls on the ordinary primitive path so all irritants are
    ;; preserved.
    (cond
      [(> (length args) 4) #f]
      [(< (length args) 2)
        (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArguments (length args))]
      [else (emit-raise cps owner 'AssertionViolation args src)]))

  (define-transformer string-length (cps owner src op args k)
    (and (pair? args)
      (let ([x (car args)])
        (if (and (not (lvar? x)) (string? x))
          (emit-continue cps owner k (list (string-length x)) #f)
          (ensure-string cps owner src op x
            (lambda (cps owner len)
              (let-prim cps owner "vlen" 'usize->value (list len) src
                (lambda (cps owner vlen)
                  (emit-continue cps owner k (list vlen) #f)))))))))

  (define-transformer vector-length (cps owner src op args k)
    (and (pair? args)
      (let ([x (car args)])
        (if (and (not (lvar? x)) (vector? x))
          (emit-continue cps owner k (list (vector-length x)) #f)
          (let-prim cps owner "vlen" 'vector-length (list x) src
            (lambda (cps owner vlen)
              (emit-continue cps owner k (list vlen) #f)))))))

  (define-transformer car (cps owner src op args k)
    (if (not (= (length args) 1))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsCar (length args))
      (let-prim cps owner "car" 'car (list (car args)) src
        (lambda (cps owner v) (emit-continue cps owner k (list v) #f)))))

  (define-transformer cdr (cps owner src op args k)
    (if (not (= (length args) 1))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsCdr (length args))
      (let-prim cps owner "cdr" 'cdr (list (car args)) src
        (lambda (cps owner v) (emit-continue cps owner k (list v) src)))))

  (define-transformer set-car! (cps owner src op args k)
    (if (not (= (length args) 2))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsSetCar (length args))
      (let ([pair (car args)] [value (cadr args)])
        (ensure-pair cps owner src op pair
          (lambda (cps owner pair)
            (let-prim cps owner "_v" 'set-car! (list pair value) src
              (lambda (cps owner _v) (emit-continue cps owner k (list undefined-value) src))))))))

  (define-transformer set-cdr! (cps owner src op args k)
    (if (not (= (length args) 2))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsSetCdr (length args))
      (let ([pair (car args)] [value (cadr args)])
        (ensure-pair cps owner src op pair
          (lambda (cps owner pair)
            (let-prim cps owner "_v" 'set-cdr! (list pair value) src
              (lambda (cps owner _v) (emit-continue cps owner k (list undefined-value) src))))))))

  (define-transformer variable-ref (cps owner src op args k)
    (if (not (= (length args) 1))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsVariableRef
        (length args))
      (ensure-variable cps owner src op (car args)
        (lambda (cps owner var)
          (let-prim cps owner "value" 'variable-ref (list var) src
            (lambda (cps owner value) (emit-continue cps owner k (list value) src)))))))

  (define-transformer variable-set! (cps owner src op args k)
    (if (not (= (length args) 2))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsVariableSet
        (length args))
      (let ([var (car args)] [value (cadr args)])
        (ensure-variable cps owner src op var
          (lambda (cps owner var)
            (let-prim cps owner "_v" 'variable-set! (list var value) src
              (lambda (cps owner _v) (emit-continue cps owner k (list undefined-value) src))))))))

  (define-transformer variable-bound? (cps owner src op args k)
    (if (not (= (length args) 1))
      (primitive-wrong-number-of-arguments cps owner src 'WrongNumberOfArgumentsVariableBound
        (length args))
      (ensure-variable cps owner src op (car args)
        (lambda (cps owner var)
          (let-prim cps owner "bound" 'variable-bound? (list var) src
            (lambda (cps owner bound) (emit-continue cps owner k (list bound) src)))))))

  ;; =====================================================================
  ;; Globals, prim boxes

  ;; Whether META is the decl of a program: the vector #(#%library key ...) of a
  ;; library whose key is (%program ...).
  (define (program-meta? meta)
    (and (vector? meta)
      (>= (vector-length meta) 2)
      (eq? (vector-ref meta 0) '|#%library|)
      (let ([key (vector-ref meta 1)])
        (and (pair? key) (eq? (car key) '%program)))))

  (define (saturating-sub1-length l)
    (if (list? l) (max 0 (- (length l) 1)) 0))

  (define (checked-global-ref cps owner src k var name)
    (let-prim cps owner "bound" 'variable-bound? (list var) src
      (lambda (cps owner bound)
        (let ([bound-link (new-child-link cps)])
          (let-prim cps bound-link "val" 'variable-ref (list var) src
            (lambda (cps owner val) (emit-continue cps owner k (list val) src)))
          (let* ([unbound-link (new-child-link cps)]
                 [message (string-append "undefined variable: " (display->string name))])
            (emit-raise cps unbound-link 'Undefined (list #f message) src)
            (emit-if cps owner bound bound-link unbound-link '(Normal Cold) src))))))

  (define (display->string x)
    (call-with-string-output-port (lambda (p) (display x p))))

  ;; SLOT: (box . lvar) | (variable . value).  HAVE-VAR: (lambda (cps owner atom)).
  (define (global-slot-atom cps owner src slot have-var)
    (if (eq? (car slot) 'box)
      (have-var cps owner (cdr slot))
      (bind-literal cps owner (cdr slot) src have-var)))

  (define (prim-box cps owner val-proc name src)
    (let* ([kbox (fresh-variable cps "kbox")]
           [var (fresh-variable cps "var")]
           [cont-body (new-child-link cps)])
      (val-proc cps cont-body var)
      (let* ([cont (make-cont cps kbox (list var) #f cont-body #f #f #f #f)]
             [body (new-child-link cps)])
        (cached-prim-box cps body kbox src name)
        (emit-letk cps owner (list cont) body #f))))

  (define (cached-prim-box cps owner k src name)
    (let* ([cache-key (cons '|#%capy| name)]
           [merge (fresh-variable cps "merge")]
           [cached-arg (fresh-variable cps "cached")]
           [merge-body (new-child-link cps)])
      (emit-continue cps merge-body k (list cached-arg) #f)
      (let* ([merge-cont (make-cont cps merge (list cached-arg) #f merge-body #f #f #t #f)]
             [kinit (fresh-variable cps "kinit")]
             [kinit-body (new-child-link cps)])
        (let-prim cps kinit-body "var" 'capy-variable (list name) src
          (lambda (cps owner var)
            (let-prim cps owner "_k" 'cache-set! (list cache-key var) src
              (lambda (cps owner _k)
                (emit-continue cps owner merge (list var) #f)))))
        (let* ([kinit-cont (make-cont cps kinit '() #f kinit-body #f #t #f #f)]
               [body (new-child-link cps)])
          (let-prim cps body "cache_entry" 'cache-ref (list cache-key) src
            (lambda (cps owner cache-entry)
              (let* ([kok (fresh-variable cps "kok")]
                     [kok-body (new-child-link cps)])
                (emit-continue cps kok-body merge (list cache-entry) #f)
                (let ([kok-cont (make-cont cps kok '() #f kok-body #f #f #f #f)])
                  (let-prim cps owner "is_heap_obj" 'heap-object? (list cache-entry) src
                    (lambda (cps owner is-heap-obj)
                      (let ([if-body (new-child-link cps)])
                        (emit-if-continue cps if-body is-heap-obj kok '() kinit '() normal-hints #f)
                        (emit-letk cps owner (list kok-cont) if-body #f))))))))
          (emit-letk cps owner (list merge-cont kinit-cont) body #f)))))

  ;; =====================================================================
  ;; Conversion proper

  (define (is-single-valued? exp)
    (case (cterm-kind exp)
      [(values) (= (length (cterm-a exp)) 1)]
      [(global-ref lref) #t]
      ;; Every primitive continues exactly once with one value or raises.
      [(primcall) #t]
      [(seq) (is-single-valued? (cterm-b exp))]
      [(receive) (is-single-valued? (cterm-d exp))]
      [(let) (is-single-valued? (cterm-d exp))]
      [(wcm) (is-single-valued? (cterm-c exp))]
      [(const) #t]
      [else #f]))

  ;; K: (lambda (cps owner atom) term-id)
  (define (convert-arg cps exp owner k)
    (let ([src (cterm-src exp)])
      (case (cterm-kind exp)
        [(lref) (k cps owner (cterm-a exp))]
        [(const) (bind-literal cps owner (cterm-a exp) src k)]
        [else
          (cond
            ;; A primitive with no expansion binds its result directly, with no
            ;; continuation in between.
            [(and (eq? (cterm-kind exp) 'primcall)
                (not (primitive-transformer-ref (cterm-a exp))))
              (let ([prim (cterm-a exp)])
                (convert-args cps (cterm-b exp) owner
                  (lambda (cps owner args)
                    (let-prim cps owner "arg" prim args src
                      (lambda (cps owner atom) (k cps owner atom))))))]
            [(is-single-valued? exp)
              (let* ([karg (fresh-variable cps "karg")]
                     [arg (fresh-variable cps "arg")]
                     [cont-body (new-child-link cps)])
                (k cps cont-body arg)
                (let* ([cont (make-cont cps karg (list arg) #f cont-body src #f #f #f)]
                       [body (new-child-link cps)])
                  (convert cps exp karg body 'non-none)
                  (emit-letk cps owner (list cont) body src)))]
            [else
              (let* ([karg (fresh-variable cps "karg")]
                     [arg (fresh-variable cps "arg")]
                     [rest (fresh-variable cps "rest")]
                     [cont-body (new-child-link cps)])
                (k cps cont-body arg)
                (let* ([cont (make-cont cps karg (list arg) rest cont-body src #f #f #f)]
                       [body (new-child-link cps)])
                  (convert cps exp karg body 'non-none)
                  (emit-letk cps owner (list cont) body src)))])])))

  ;; FK: (lambda (cps owner atoms) term-id)
  (define (convert-args cps exps owner fk)
    (if (null? exps)
      (fk cps owner '())
      (convert-arg cps (car exps) owner
        (lambda (cps owner arg)
          (convert-args cps (cdr exps) owner
            (lambda (cps owner args)
              (fk cps owner (cons arg args))))))))

  (define (convert-let-bindings cps lhs rhs index body k owner source mode)
    (let ([binding (list-ref lhs index)]
          [expr (list-ref rhs index)])
      (let* ([let-k (fresh-variable cps "let")]
             [rest (and (not (is-single-valued? expr)) (fresh-variable cps "rest"))]
             [cont-body (new-child-link cps)])
        (if (= index 0)
          (convert cps body k cont-body mode)
          (convert-let-bindings cps lhs rhs (- index 1) body k cont-body source mode))
        (let* ([cont (make-cont cps let-k (list binding) rest cont-body source #f #f #f)]
               [expr-body (new-child-link cps)])
          (convert cps expr let-k expr-body 'non-none)
          (emit-letk cps owner (list cont) expr-body source)))))

  ;; The primitive an `if' can branch on directly, or #f.
  (define (branch-primitive test)
    (and (eq? (cterm-kind test) 'primcall)
      (let ([name (cterm-a test)] [args (cterm-b test)])
        (and (not (primitive-transformer-ref name))
          (<= (length args) 255)
          (symbol? name)
          (primitive-known? name)
          (primitive-branch-fusable? name)
          name))))

  (define (convert-branch-prim cps test consequent alternate k owner mode)
    (let* ([prim (branch-primitive test)]
           [src (cterm-src test)]
           [args (cterm-b test)])
      (let-values ([(if-true true-cont) (branch-arm cps "ktrue" consequent k mode)])
        (let-values ([(if-false false-cont) (branch-arm cps "kfalse" alternate k mode)])
          (let ([body (new-child-link cps)])
            (convert-args cps args body
              (lambda (cps owner args)
                (emit-branch-prim cps owner prim args if-true '() if-false '() normal-hints src)))
            (emit-letk cps owner (list true-cont false-cont) body src))))))

  ;; A zero-argument continuation that runs BODY and continues to K: returns
  ;; (values binding function-id).
  (define (branch-arm cps name body k mode)
    (let* ([binding (fresh-variable cps name)]
           [link (new-child-link cps)])
      (convert cps body k link mode)
      (let ([cont (make-cont cps binding '() #f link (cterm-src body) #f #f #f)])
        (values binding cont))))

  (define (convert-nested-if cps test consequent alternate k owner mode)
    (let ([src (cterm-src test)])
      (let-values ([(if-true true-cont) (branch-arm cps "ktrue" consequent k mode)])
        (let-values ([(if-false false-cont) (branch-arm cps "kfalse" alternate k mode)])
          (let ([body (new-child-link cps)])
            (convert-condition cps test if-true if-false body)
            (emit-letk cps owner (list true-cont false-cont) body src))))))

  (define (const-truthy? term) (not (eq? (cterm-a term) #f)))

  ;; Control reaches IF-TRUE or IF-FALSE (zero-argument continuations) by the
  ;; truthiness of TEST.
  (define (convert-condition cps test if-true if-false owner)
    (let ([src (cterm-src test)])
      (cond
        [(eq? (cterm-kind test) 'const)
          (let ([target (if (const-truthy? test) if-true if-false)])
            (emit-continue cps owner target '() src))]
        [(eq? (cterm-kind test) 'if)
          (let* ([cond-term (cterm-a test)]
                 [sides '()]
                 [side (lambda (term)
                         (if (eq? (cterm-kind term) 'const)
                           (if (const-truthy? term) if-true if-false)
                           (let* ([binding (fresh-variable cps "kcond")]
                                  [link (new-child-link cps)])
                             (convert-condition cps term if-true if-false link)
                             (set! sides
                               (cons (make-cont cps binding '() #f link (cterm-src term) #f #f #f)
                                 sides))
                             binding)))]
                 [then-target (side (cterm-b test))]
                 [else-target (side (cterm-c test))])
            (if (null? sides)
              (convert-condition cps cond-term then-target else-target owner)
              (let ([body (new-child-link cps)])
                (convert-condition cps cond-term then-target else-target body)
                (emit-letk cps owner (reverse sides) body src))))]
        [(branch-primitive test)
          => (lambda (prim)
               (convert-args cps (cterm-b test) owner
                 (lambda (cps owner args)
                   (emit-branch-prim cps owner prim args if-true '() if-false '() normal-hints src))))]
        [else
          (convert-arg cps test owner
            (lambda (cps owner test)
              (emit-if-continue cps owner test if-true '() if-false '() normal-hints src)))])))


  (define (in-some-scope? mode) (and (memq mode '(tail-some non-some)) #t))

  ;; Run BODY with a fresh continuation that pops the attachment, then
  ;; continues K with the same values.  BODY: (lambda (cps k owner) term-id).
  (define (with-pop-continuation cps owner src k nargs has-rest body)
    (let* ([pop (fresh-variable cps "kpop")]
           [inner (new-child-link cps)])
      (body cps pop inner)
      (let* ([fixed (let loop ([i 0] [acc '()])
                      (if (= i nargs)
                        (reverse acc)
                        (loop (+ i 1) (cons (fresh-variable cps "pop_arg") acc))))]
             [rest (and has-rest (fresh-variable cps "pop_rest"))]
             [forward fixed]
             [pop-body (new-child-link cps)])
        (let-prim cps pop-body "pop" '$attachment-pop! '() src
          (lambda (cps owner _) (emit-continue cps owner k forward src)))
        (let ([cont (make-cont cps pop fixed rest pop-body src #f #f #f)])
          (emit-letk cps owner (list cont) inner src)))))

  ;; Single-valued scope exit: wrap K with a pop when MODE is in a scope.
  (define (with-pop-1 cps owner src k mode body)
    (if (not (in-some-scope? mode))
      (body cps k owner)
      (with-pop-continuation cps owner src k 1 #f body)))

  (define (with-pop-n cps owner src k mode nargs body)
    (if (not (in-some-scope? mode))
      (body cps k owner)
      (with-pop-continuation cps owner src k nargs #f body)))

  ;; Scope-exit call in non-some mode: wrap K with a pop, copying K's shape.
  (define (with-pop-like cps owner src k body)
    (let ([shape (hashtable-ref (cps-builder-cont-shapes cps) k #f)])
      (unless shape
        (assertion-violation 'with-pop-like "pop-wrap of a continuation with unknown shape" k))
      (with-pop-continuation cps owner src k (cont-shape-nargs shape)
        (cont-shape-has-rest shape) body)))

  ;; True when K takes a rest list as a `receive' consumer, or its shape is
  ;; unknown.
  (define (cont-needs-thunk-fallback? cps k)
    (let ([shape (hashtable-ref (cps-builder-cont-shapes cps) k #f)])
      (if (not shape)
        #t
        (and (cont-shape-is-consumer shape) (cont-shape-has-rest shape)))))

  ;; with-continuation-mark through the native call-with-continuation-mark.
  (define (convert-wcm-thunk cps key mark result k owner src mode)
    (let* ([thunk-proc (make-cproc #f src '() #f result (builder-current-meta cps))]
           [thunk (make-cterm 'proc src thunk-proc #f #f #f)]
           [mref (make-cterm 'primref src 'call-with-continuation-mark #f #f #f)]
           [call (make-cterm 'call src mref (list key mark thunk) #f #f)])
      (convert cps call k owner mode)))

  (define (convert-wcm cps key mark result k mode owner src)
    (if (and (eq? mode 'non-none) (cont-needs-thunk-fallback? cps k))
      (convert-wcm-thunk cps key mark result k owner src mode)
      (convert-arg cps key owner
        (lambda (cps owner key)
          (convert-arg cps mark owner
            (lambda (cps owner mark)
              (let ([update-mark '$update-mark]
                    [null '()])
                (case mode
                  [(non-none)
                    (let-prim cps owner "table" update-mark (list null key mark) src
                      (lambda (cps owner table)
                        (let-prim cps owner "push" '$attachment-push! (list table) src
                          (lambda (cps owner _)
                            (convert cps result k owner 'non-some)))))]
                  [(tail)
                    (let-prim cps owner "marks" '$attachment-consume-reify! (list null) src
                      (lambda (cps owner marks)
                        (let-prim cps owner "table" update-mark (list marks key mark) src
                          (lambda (cps owner table)
                            (let-prim cps owner "push" '$attachment-push! (list table) src
                              (lambda (cps owner _)
                                (convert cps result k owner 'tail-some)))))))]
                  [else
                    (let-prim cps owner "marks" '$attachment-consume! '() src
                      (lambda (cps owner marks)
                        (let-prim cps owner "table" update-mark (list marks key mark) src
                          (lambda (cps owner table)
                            (let-prim cps owner "set" '$attachment-set! (list table) src
                              (lambda (cps owner _)
                                (convert cps result k owner mode)))))))]))))))))


  (define (convert cps exp k owner mode)
    (convert-term cps exp k owner mode))

  (define (convert-term cps exp k owner mode)
    (let ([src (cterm-src exp)])
      (case (cterm-kind exp)
        [(values)
          (let ([vals (cterm-a exp)])
            (with-pop-n cps owner src k mode (length vals)
              (lambda (cps k owner)
                (convert-args cps vals owner
                  (lambda (cps owner args) (emit-continue cps owner k args #f))))))]

        [(if)
          (let ([test (cterm-a exp)] [consequent (cterm-b exp)] [alternate (cterm-c exp)])
            (cond
              [(branch-primitive test)
                (convert-branch-prim cps test consequent alternate k owner mode)]
              [(eq? (cterm-kind test) 'if)
                (convert-nested-if cps test consequent alternate k owner mode)]
              [else
                (convert-arg cps test owner
                  (lambda (cps owner test)
                    (let ([consequent-link (new-child-link cps)])
                      (convert cps consequent k consequent-link mode)
                      (let ([alternate-link (new-child-link cps)])
                        (convert cps alternate k alternate-link mode)
                        (emit-if cps owner test consequent-link alternate-link normal-hints #f)))))]))]

        [(lset) (assertion-violation 'convert "unexpected lset" exp)]

        [(lref)
          (with-pop-1 cps owner src k mode
            (lambda (cps k owner)
              (emit-continue cps owner k (list (cterm-a exp)) src)))]

        [(const)
          (with-pop-1 cps owner src k mode
            (lambda (cps k owner)
              (bind-literal cps owner (cterm-a exp) src
                (lambda (cps owner atom) (emit-continue cps owner k (list atom) src)))))]

        [(primref)
          (with-pop-1 cps owner src k mode
            (lambda (cps k owner)
              (prim-box cps owner
                (lambda (cps owner var)
                  (let-prim cps owner "val" 'variable-ref (list var) src
                    (lambda (cps owner val) (emit-continue cps owner k (list val) #f))))
                (cterm-a exp) src)))]

        [(global-ref)
          (let ([name (cterm-b exp)] [slot (cterm-c exp)])
            (if (eq? (car slot) 'box)
              (let ([var (cdr slot)])
                (with-pop-1 cps owner src k mode
                  (lambda (cps k owner)
                    (if (hashtable-ref (cps-builder-unbound-checked cps) var #f)
                      ;; A slot of the namespace of a namespace linklet: it is made
                      ;; before the body runs, but nothing may have defined it yet.
                      (checked-global-ref cps owner src k var name)
                      ;; Imports are instantiated before the linklet body runs, so
                      ;; the box is bound.
                      (let-prim cps owner "val" 'variable-ref (list var) src
                        (lambda (cps owner val) (emit-continue cps owner k (list val) src)))))))
              (let ([var (cdr slot)])
                (with-pop-1 cps owner src k mode
                  (lambda (cps k owner)
                    (bind-literal cps owner var src
                      (lambda (cps owner var)
                        (checked-global-ref cps owner src k var name))))))))]

        [(global-set)
          (let ([slot (cterm-c exp)] [value (cterm-d exp)])
            (with-pop-1 cps owner src k mode
              (lambda (cps k owner)
                (convert-arg cps value owner
                  (lambda (cps owner atom)
                    (global-slot-atom cps owner src slot
                      (lambda (cps owner var)
                        (let-prim cps owner "_val" 'variable-set! (list var atom) src
                          (lambda (cps owner val)
                            (emit-continue cps owner k (list val) src))))))))))]

        [(linklet)
          (let* ([data (cterm-a exp)]
                 [name (vector-ref data 0)]
                 [imports (vector-ref data 1)]
                 [exports (vector-ref data 2)]
                 [slots (vector-ref data 3)]
                 [body (vector-ref data 4)]
                 [meta (vector-ref data 5)])
            (when (and (pair? meta) (eq? (car meta) '|#%namespace|))
              (when (pair? imports)
                (let ([count (saturating-sub1-length (car imports))])
                  (let loop ([i 0] [ss slots])
                    (when (and (< i count) (pair? ss))
                      (hashtable-set! (cps-builder-unbound-checked cps) (car ss) #t)
                      (loop (+ i 1) (cdr ss)))))))
            (when (program-meta? meta)
              (let ([count (let loop ([groups imports] [count 0])
                             (if (pair? groups)
                               (loop (cdr groups) (+ count (saturating-sub1-length (car groups))))
                               count))])
                (let loop ([i 0] [ss slots])
                  (when (pair? ss)
                    (when (>= i count)
                      (hashtable-set! (cps-builder-unbound-checked cps) (car ss) #t))
                    (loop (+ i 1) (cdr ss))))))
            (let* ([proc (make-cterm 'proc src (make-cproc name src slots #f body meta) #f #f #f)]
                   [args (list (make-cterm 'const #f name #f #f #f)
                           (make-cterm 'const #f imports #f #f #f)
                           (make-cterm 'const #f exports #f #f #f)
                           proc
                           (make-cterm 'const #f meta #f #f #f))]
                   [make-ref (make-cterm 'primref src 'make-linklet #f #f #f)]
                   [make (make-cterm 'call src make-ref args #f #f)])
              (convert cps make k owner mode)))]

        [(proc)
          (with-pop-1 cps owner src k mode
            (lambda (cps k owner)
              (let* ([tmp (fresh-variable cps "proc")]
                     [func (make-proc-func cps (cterm-a exp) tmp #t)]
                     [body (new-child-link cps)])
                (emit-continue cps body k (list tmp) src)
                (emit-fix cps owner (list func) body src))))]

        [(case-lambda)
          (with-pop-1 cps owner src k mode
            (lambda (cps k owner)
              (let* ([group (fresh-case-group cps)]
                     [bindings '()]
                     [funcs (let loop ([procs (cterm-a exp)] [acc '()])
                              (if (null? procs)
                                (reverse acc)
                                (let* ([binding (fresh-variable cps "case-lambda-clause")]
                                       [_ (set! bindings (cons binding bindings))]
                                       [func (make-proc-func cps (car procs) binding #t)])
                                  (function-case-group-set! (graph-function (cps-graph cps) func) group)
                                  (loop (cdr procs) (cons func acc)))))]
                     [vars (reverse bindings)]
                     [fix-body (new-child-link cps)])
                (let-prim cps fix-body "case-lambda" '%case-lambda vars src
                  (lambda (cps body value) (emit-continue cps body k (list value) src)))
                (emit-fix cps owner funcs fix-body src))))]

        [(call)
          (let* ([orig-proc (cterm-a exp)] [args (cterm-b exp)]
                 ;; `(apply f a ... lst)' of the real `apply' is a call of the
                 ;; marker with the operands `f a ... lst'.
                 [apply-call? (and (eq? (cterm-kind orig-proc) 'primref)
                                (eq? (cterm-a orig-proc) 'apply)
                                (pair? args)
                                (pair? (cdr args)))]
                 [proc (if apply-call?
                         (make-cterm 'const src apply-call-marker #f #f #f)
                         orig-proc)]
                 [call-with (lambda (cps k owner)
                              (convert-arg cps proc owner
                                (lambda (cps owner proc)
                                  (convert-args cps args owner
                                    (lambda (cps owner args)
                                      (emit-app cps owner proc k args src))))))])
            ;; A call in tail scope stays a tail call: the reified frame's
            ;; underflow restore pops for it.
            (if (eq? mode 'non-some)
              (with-pop-like cps owner src k call-with)
              (call-with cps k owner)))]

        [(primcall)
          (let ([prim (cterm-a exp)] [args (cterm-b exp)])
            (with-pop-1 cps owner src k mode
              (lambda (cps k owner)
                (convert-args cps args owner
                  (lambda (cps owner args)
                    (or (try-expand-primitive cps owner src prim args k)
                      (let-prim cps owner "atom" prim args src
                        (lambda (cps owner atom)
                          (emit-continue cps owner k (list atom) src)))))))))]

        [(let)
          (let ([style (cterm-a exp)] [lhs (cterm-b exp)] [rhs (cterm-c exp)] [body (cterm-d exp)])
            (when (memq style '(letrec letrec* let*))
              (assertion-violation 'convert "unexpected let style" style))
            (if (null? lhs)
              (convert cps body k owner mode)
              (convert-let-bindings cps lhs rhs (- (length lhs) 1) body k owner src mode)))]

        [(seq)
          (let* ([head (cterm-a exp)] [tail (cterm-b exp)]
                 [ktail (fresh-variable cps "ktail")]
                 [cont-body (new-child-link cps)])
            (convert cps tail k cont-body mode)
            ;; A head that produces exactly one value binds it directly, only a
            ;; head of unknown arity needs the rest list.
            (let ([cont (if (is-single-valued? head)
                          (let ([arg (fresh-variable cps "arg")])
                            (make-cont cps ktail (list arg) #f cont-body #f #f #f #f))
                          (let ([vals (fresh-variable cps "vals")])
                            (make-cont cps ktail '() vals cont-body #f #f #f #f)))]
                  [body (new-child-link cps)])
              (convert cps head ktail body 'non-none)
              (emit-letk cps owner (list cont) body #f)))]

        [(fix)
          (let ([lhs (cterm-a exp)] [rhs (cterm-b exp)] [fbody (cterm-c exp)])
            (for-each (lambda (binding) (bind-lvar cps binding)) lhs)
            (let* ([funcs (let loop ([bs lhs] [ps rhs] [acc '()])
                            (if (null? bs)
                              (reverse acc)
                              (let ([f (make-proc-func cps (car ps) (car bs) #t)])
                                (loop (cdr bs) (cdr ps) (cons f acc)))))]
                   [body (new-child-link cps)])
              (convert cps fbody k body mode)
              (emit-fix cps owner funcs body src)))]

        [(receive)
          (let* ([vars (cterm-a exp)] [variadic (cterm-b exp)]
                 [producer (cterm-c exp)] [consumer (cterm-d exp)]
                 [consumer-k-var (fresh-variable cps "consumer")]
                 [body (new-child-link cps)])
            (convert cps consumer k body mode)
            (let ([consumer-k (make-cont cps consumer-k-var vars variadic body src #f #f
                                (builder-current-meta cps))])
              (let ([shape (hashtable-ref (cps-builder-cont-shapes cps) consumer-k-var #f)])
                (when shape (cont-shape-is-consumer-set! shape #t)))
              (let ([letk-body (new-child-link cps)])
                (convert cps producer consumer-k-var letk-body 'non-none)
                (emit-letk cps owner (list consumer-k) letk-body src))))]

        [(wcm)
          (convert-wcm cps (cterm-a exp) (cterm-b exp) (cterm-c exp) k mode owner src)]

        [else (assertion-violation 'convert "unknown term kind" (cterm-kind exp))]))))
