;;; Tree shaker of the whole-program static linker
;;;
;;; The linker merges the linklets of a program into the bindings of one `letrec*`: a
;;; definition binds a variable to a value, an effect binding runs an expression of the top
;;; level of a linklet. The shaker decides which of the bindings the program needs.
;;;
;;; Effects
;;; -------
;;; The analysis gives every expression an effect, one of
;;;
;;;   pure           nothing observable happens when the value is not used (it may allocate,
;;;                  read, or raise on bad arguments)
;;;   (target ...)   the only effect is to mutate objects that top-level variables hold
;;;   impure         anything else
;;;
;;; A binding is a root when it is impure. A definition that is pure and an effect that is
;;; pure are dropped when nothing uses them. An effect that mutates objects of the variables
;;; T is attached to T: it matters when one of the variables of T is live, and only then.
;;; Marking is a worklist, so a registration that became live by a root keeps what it references,
;;; and the fixpoint is the closure.
;;;
;;; `shake` returns a `shaken` record with the live bindings, the chain that made each
;;; one live, the roots and what each root keeps alive.
(library (capy compiler static shake)
  (export make-sbinding
    sbinding?
    sbinding-owner
    sbinding-id
    sbinding-lhs
    sbinding-rhs
    shake
    flatten-effect
    shaken-live?
    shaken-why
    retention-chain
    shaken-roots
    shaken-conditional-live
    shaken-conditional-dropped
    shaken-root-causes
    shaken-culprit
    describe-term)
  (import (rnrs)
    (rnrs hashtables)
    (capy)
    (capy compiler tree-il terms)
    (capy compiler static effects))

  (define-record-type sbinding
    (fields owner id lhs rhs))

  (define-record-type shaken
    (fields live-table why-table root-list conditional-live-count conditional-dropped-count causes-proc culprit-table))

  (define (shaken-live? s b)
    (and (hashtable-ref (shaken-live-table s) (sbinding-lhs b) #f) #t))
  ;; The binding whose liveness made B live, #f for a root.
  (define (shaken-why s b)
    (hashtable-ref (shaken-why-table s) (sbinding-lhs b) #f))
  ;; The bindings that made B live, the root first and B last.
  (define (retention-chain s b)
    (let loop ([b b] [chain '()])
      (if b (loop (shaken-why s b) (cons b chain)) chain)))
  (define (shaken-culprit s b) (hashtable-ref (shaken-culprit-table s) (sbinding-lhs b) #f))
  (define (shaken-roots s) (shaken-root-list s))
  (define (shaken-conditional-live s) (shaken-conditional-live-count s))
  (define (shaken-conditional-dropped s) (shaken-conditional-dropped-count s))
  (define (shaken-root-causes s limit) ((shaken-causes-proc s) limit))

  ;;; Flattening

  (define (flatten-effect t fresh-name)
    (reverse (flatten-effect* t fresh-name '())))

  (define (flatten-effect* t fresh-name acc0)
    (let loop ([t t] [acc acc0])
      (cond
        [(sequence? t) (loop (sequence-tail t) (loop (sequence-head t) acc))]
        [(void? t) acc]
        [(let? t)
          (loop (let-body t)
            (fold-left (lambda (acc id lhs rhs) (cons (list id lhs rhs) acc))
              acc (let-ids t) (let-lhs t) (let-rhs t)))]
        [(fix? t)
          (loop (fix-body t)
            (fold-left (lambda (acc id lhs rhs) (cons (list id lhs rhs) acc))
              acc (fix-ids t) (fix-lhs t) (fix-rhs t)))]
        [else (cons (list '%effect (fresh-name) t) acc)])))

  ;;; Children of a term, in evaluation order
  (define (term-children t)
    (cond
      [(or (constant? t) (void? t) (lref? t) (primref? t) (global-ref? t)) '()]
      [(lset? t) (list (lset-value t))]
      [(global-set? t) (list (global-set-value t))]
      [(if? t) (list (if-test t) (if-then t) (if-else t))]
      [(let? t) (append (let-rhs t) (list (let-body t)))]
      [(fix? t) (append (fix-rhs t) (list (fix-body t)))]
      [(receive? t) (list (receive-producer t) (receive-consumer t))]
      [(application? t) (cons (application-operator t) (application-operands t))]
      [(primcall? t) (primcall-args t)]
      [(proc? t) (list (proc-body t))]
      [(values? t) (values-values t)]
      [(sequence? t) (list (sequence-head t) (sequence-tail t))]
      [(wcm? t) (list (wcm-key t) (wcm-mark t) (wcm-result t))]
      [else '()]))

  ;; The lexical variables T refers to or assigns.
  (define (term-refs t)
    (let ([acc '()])
      (let walk ([t t])
        (cond
          [(lref? t) (set! acc (cons (lref-sym t) acc))]
          [(lset? t) (set! acc (cons (lset-sym t) acc)) (walk (lset-value t))]
          [else (for-each walk (term-children t))]))
      acc))

  (define (describe-term t)
    (cond
      [(application? t)
        (let ([op (application-operator t)])
          (cond
            [(global-ref? op) (global-ref-name op)]
            [(lref? op) (lref-name op)]
            [else 'application]))]
      [(lset? t) (list 'set! (lset-name t))]
      [(global-set? t) (list 'global-set (global-set-name t))]
      [(let? t) (list 'let (describe-term (let-body t)))]
      [(sequence? t) (list 'begin (describe-term (sequence-head t)))]
      [(if? t) 'if]
      [(proc? t) 'lambda]
      [(primcall? t) (primcall-prim t)]
      [else 'expression]))

  ;;; Effects

  ;; An effect is `pure`, `impure` or a list of targets: symbols (top-level variables) and
  ;; `(param . i)` pairs (the i-th argument of the procedure being analysed).
  (define (join a b)
    (cond
      [(or (eq? a 'impure) (eq? b 'impure)) 'impure]
      [(eq? a 'pure) b]
      [(eq? b 'pure) a]
      [else (let loop ([b b] [out a])
              (cond [(null? b) out]
                    [(member (car b) out) (loop (cdr b) out)]
                    [else (loop (cdr b) (cons (car b) out))]))]))

  (define (join* effects) (fold-left join 'pure effects))

  (define (shake bindings roots-terms options)
    (define expander-state
      (cond [(find (lambda (o) (and (pair? o) (eq? (car o) 'expander-state))) options) => cdr]
            [else '()]))
    (define observers (cond [(find (lambda (o) (and (pair? o) (eq? (car o) 'observed-by-expander))) options) => cdr] [else '()]))
    (define by-lhs (make-eq-hashtable))
    (for-each (lambda (b) (hashtable-set! by-lhs (sbinding-lhs b) b)) bindings)
    (define (top-level? v) (hashtable-ref by-lhs v #f))
    ;; Why the binding being analysed is impure: the first thing found.
    ;; How many branches of an `if` the expression being analysed is under.
    (define branch-depth 0)
    (define culprit #f)
    (define culprits (make-eq-hashtable))
    (define (impure! what)
      (unless culprit (set! culprit what))
      'impure)
    (define unknown-calls (make-eq-hashtable))
    (define trace-unknown? (memq 'trace-unknown options))

    ;; aliases
    (define (canonical v)
      (let loop ([v v] [n 0])
        (let ([b (hashtable-ref by-lhs v #f)])
          (if (and b (< n 50) (not (eq? (sbinding-id b) '%effect)) (lref? (sbinding-rhs b))
                (hashtable-ref by-lhs (lref-sym (sbinding-rhs b)) #f))
            (loop (lref-sym (sbinding-rhs b)) (+ n 1))
            v))))

    ;; the locals of the expressions: variable -> provenance
    ;; fresh | (top . v) | (param . i) | unknown
    (define prov (make-eq-hashtable))
    (define local-procs (make-eq-hashtable))     ; local variable -> proc term

    ;; The summaries: lhs -> #(effect fresh-result?) | in-progress
    (define summaries (make-eq-hashtable))

    ;; The primitive an operator is: a reference to one, or a variable bound to one.
    (define local-prims (make-eq-hashtable))
    (define (prim-name op)
      (cond
        [(and (global-ref? op) (equal? (global-ref-key op) '(|#%capy|))) (global-ref-name op)]
        [(primref? op) (primref-prim op)]
        [(lref? op)
          (or (hashtable-ref local-prims (lref-sym op) #f)
            (let ([b (hashtable-ref by-lhs (canonical (lref-sym op)) #f)])
              (and b (not (eq? (sbinding-id b) '%effect))
                (let ([rhs (sbinding-rhs b)])
                  (and (or (primref? rhs) (global-ref? rhs)) (not (lref? rhs))
                    (prim-name rhs))))))]
        [else #f]))
    (define (note-prim! v rhs)
      (let ([p (and (or (global-ref? rhs) (primref? rhs) (lref? rhs)) (prim-name rhs))])
        (when p (hashtable-set! local-prims v p))))

    ;; Whether the expression makes a new object nothing else refers to.
    (define (fresh? t)
      (cond
        [(proc? t) #t]
        [(primcall? t) (and (hashtable-ref fresh-prims (primcall-prim t) #f) #t)]
        [(application? t)
          (let ([name (prim-name (application-operator t))])
            (and name (hashtable-ref fresh-prims name #f) #t))]
        [(let? t) (fresh? (let-body t))]
        [(sequence? t) (fresh? (sequence-tail t))]
        [(if? t) (and (fresh? (if-then t)) (fresh? (if-else t)))]
        [(lref? t) (eq? (hashtable-ref prov (lref-sym t) #f) 'fresh)]
        [else #f]))

    ;; The targets an expression used as the object of a mutation stands for: 'none (a fresh
    ;; or local object), a list of targets, or 'impure.
    (define (target-of e)
      (cond
        [(lref? e)
          (let* ([v (lref-sym e)] [p (hashtable-ref prov v #f)])
            (cond
              [(eq? p 'fresh) 'pure]
              [(eq? p 'state) observers]
              [(and (pair? p) (eq? (car p) 'top)) (list (cdr p))]
              [(and (pair? p) (eq? (car p) 'param)) (list p)]
              [p 'impure]
              [(top-level? v) (list (canonical v))]
              [else 'impure]))]
        [(fresh? e) 'pure]
        [else 'impure]))

    ;; Whether the variable V of a `let` can be reached by anything but the primitives that
    ;; read it or change it, in BODY
    (define (escapes? v body)
      (let walk ([t body] [in-proc? #f])
        (cond
          [(lref? t) (eq? (lref-sym t) v)]
          [(lset? t) (or (eq? (lset-sym t) v) (walk (lset-value t) in-proc?))]
          [(proc? t) (walk (proc-body t) #t)]
          [(and (or (application? t) (primcall? t))
             (let* ([name (if (primcall? t) (primcall-prim t) (prim-name (application-operator t)))]
                    [args (if (primcall? t) (primcall-args t) (application-operands t))])
               (and name (pair? args) (lref? (car args)) (eq? (lref-sym (car args)) v)
                 (not in-proc?)
                 (or (hashtable-ref pure-prims name #f) (hashtable-ref mutator-prims name #f))
                 (cons name args))))
            => (lambda (info)
                 (exists (lambda (a) (walk a in-proc?)) (cddr info)))]
          [else (exists (lambda (c) (walk c in-proc?)) (term-children t))])))

    ;; The provenance a binding gives its variable. A fresh object stays fresh only while it
    ;; does not escape
    (define (provenance-of rhs)
      (cond
        [(fresh? rhs) 'fresh]
        [(and (lref? rhs) (hashtable-ref prov (lref-sym rhs) #f))
          => (lambda (p) p)]
        [(and (lref? rhs) (top-level? (lref-sym rhs)))
          (cons 'top (canonical (lref-sym rhs)))]
        [(and (application? rhs) (lref? (application-operator rhs))
           (top-level? (lref-sym (application-operator rhs)))
           (memq (canonical (lref-sym (application-operator rhs))) expander-state))
          'state]
        [else 'unknown]))

    ;; The binding of the procedure B calls to make its value, when B is bound to `(f ...)`
    ;; with F a top-level variable.
    (define (maker-of b)
      (and (not (eq? (sbinding-id b) '%effect))
        (let ([rhs (sbinding-rhs b)])
          (and (application? rhs)
            (let ([op (application-operator rhs)])
              (and (lref? op) (hashtable-ref by-lhs (canonical (lref-sym op)) #f)))))))

    (define (factory-product b)
      (let ([ob (maker-of b)])
        (and ob
          (system-ref record-factories (sbinding-owner ob) (sbinding-id ob)))))

    ;; Whether B is bound to `(make-parameter ...)` of the runtime.
    (define (parameter-binding? b)
      (let ([ob (maker-of b)])
        (and ob (system-binding? parameter-makers (sbinding-owner ob) (sbinding-id ob)))))

    ;; the effect of an expression
    (define (eff t)
      (cond
        [(or (constant? t) (void? t) (lref? t) (primref? t) (global-ref? t) (proc? t)) 'pure]
        [(lset? t)
          (let ([v (lset-sym t)])
            (join (eff (lset-value t))
              (cond
                [(let ([p (hashtable-ref prov v #f)])
                   (and (pair? p) (eq? (car p) 'top) (list (cdr p))))
                  => (lambda (targets) targets)]       ; variable of a closure
                [(hashtable-ref prov v #f) 'pure]    ; a local of the expression
                [(top-level? v) (list (canonical v))]
                [else 'pure])))]
        [(global-set? t) (impure! (list 'global-set (global-set-name t)))]
        [(if? t)
          (let* ([test (eff (if-test t))]
                 [entered (set! branch-depth (+ branch-depth 1))]
                 [then (eff (if-then t))]
                 [alt (eff (if-else t))])
            (set! branch-depth (- branch-depth 1))
            (join* (list test then alt)))]
        [(sequence? t) (join (eff (sequence-head t)) (eff (sequence-tail t)))]
        [(let? t)
          (for-each
            (lambda (v rhs)
              (hashtable-set! prov v
                (let ([p (provenance-of rhs)])
                  (if (and (eq? p 'fresh) (or (escapes? v (let-body t)) (exists (lambda (r) (escapes? v r)) (let-rhs t)))) 'unknown p)))
              (note-prim! v rhs)
              (when (proc? rhs) (hashtable-set! local-procs v rhs)))
            (let-lhs t) (let-rhs t))
          (join (join* (map eff (let-rhs t))) (eff (let-body t)))]
        [(fix? t)
          (for-each
            (lambda (v rhs)
              (hashtable-set! prov v 'unknown)
              (hashtable-set! local-procs v rhs))
            (fix-lhs t) (fix-rhs t))
          (join (join* (map eff (fix-rhs t))) (eff (fix-body t)))]
        [(receive? t)
          (for-each (lambda (v) (hashtable-set! prov v 'unknown)) (receive-vars t))
          (join (eff (receive-producer t)) (eff (receive-consumer t)))]
        [(values? t) (join* (map eff (values-values t)))]
        [(wcm? t) (join* (list (eff (wcm-key t)) (eff (wcm-mark t)) (eff (wcm-result t))))]
        [(primcall? t) (prim-effect (primcall-prim t) (primcall-args t) (join* (map eff (primcall-args t))))]
        [(application? t) (application-effect t)]
        [else (impure! 'unknown-term)]))

    (define (prim-effect name args args-effect)
      (let ([target-index (hashtable-ref mutator-prims name #f)])
        (cond
          [(hashtable-ref pure-prims name #f) args-effect]
          [(memq name expander-state-prims) (join args-effect observers)]
          [(and (eq? name 'apply) (pair? args) (prim-name (car args)))
            => (lambda (inner) (prim-effect inner (cdr args) args-effect))]
          [(and (eq? name 'apply) (pair? args) (lref? (car args)))
            ;; (apply f arg ...): what a call of f does, when it does not depend on the
            ;; arguments it is given.
            (let ([e (procedure-argument-effect (car args))])
              (if (eq? e 'impure)
                (impure! (list 'apply (lref-name (car args))))
                (join args-effect e)))]
          [(hashtable-ref raising-procs name #f) (join args-effect (raising-effect name))]
          [(and target-index (> (length args) target-index))
            (let ([target (target-of (list-ref args target-index))])
              (if (eq? target 'impure) (impure! (list 'mutates-unknown name)) (join args-effect target)))]
          [else (unknown! name) (impure! (list 'primitive name))])))

    (define (unknown! name)
      (when trace-unknown?
        (hashtable-set! unknown-calls name (+ 1 (hashtable-ref unknown-calls name 0)))))

    (define (application-effect t)
      (let* ([op (application-operator t)]
             [args (application-operands t)]
             [args-effect (join* (map eff args))]
             [name (prim-name op)])
        (cond
          [name (prim-effect name args args-effect)]
          [(proc? op)
            ;; ((lambda (x ...) body) arg ...)
            (let loop ([vs (proc-args op)] [as args])
              (cond
                [(and (pair? vs) (pair? as))
                  (hashtable-set! prov (car vs) (let ([p (provenance-of (car as))]) (if (eq? p 'fresh) 'unknown p)))
                  (loop (cdr vs) (cdr as))]
                [(pair? vs) (hashtable-set! prov (car vs) 'unknown) (loop (cdr vs) as)]
                [(symbol? vs) (hashtable-set! prov vs 'unknown)]))
            (join args-effect (eff (proc-body op)))]
          [(and (lref? op) (let ([p (hashtable-ref prov (lref-sym op) #f)])
                             (and (pair? p) (eq? (car p) 'param) p)))
            => (lambda (p) (join args-effect (list (cons 'call (cdr p)))))]
          [(lref? op) (join args-effect (call-effect (lref-sym op) args))]
          [else (impure! (list 'call (describe-term t)))])))

    (define (raising-effect who)
      (if (> branch-depth 0) 'pure (impure! (list 'always-raises who))))

    ;; The effect of calling the procedure bound to V with ARGS.
    (define (call-effect v args)
      (let* ([b (hashtable-ref by-lhs (canonical v) #f)]
             [owner (and b (sbinding-owner b))]
             [id (and b (sbinding-id b))]
             [mutator (and b (system-ref library-mutators owner id))])
        (cond
          [(and b (system-ref raising-procs owner id)) (raising-effect v)]
          [(and b (system-ref constructor-procs owner id)) 'pure]
          ;; Formatting into a string writes nowhere.
          [(and b (system-binding? formatter-procs owner id) (pair? args)
             (constant? (car args)) (not (constant-value (car args))))
            'pure]
          [(and b (factory-product b))
            => (lambda (kind)
                 (if (eq? kind 'mutator)
                   (if (pair? args)
                     (let ([target (target-of (car args))])
                       (if (eq? target 'impure) (impure! (list 'mutates-unknown v)) target))
                     (impure! (list 'mutator-arity v)))
                   'pure))]
          [(and b (parameter-binding? b))
            (if (null? args) 'pure (list (canonical v)))]
          [(and b (memq (sbinding-lhs b) expander-state)) observers]
          [mutator
            (if (> (length args) mutator)
              (let ([target (target-of (list-ref args mutator))])
                (if (eq? target 'impure) (impure! (list 'mutates-unknown v)) target))
              (impure! (list 'mutator-arity v)))]
          [else
            (let ([s (summary-of v)])
              (if s
                (let ([e (apply-summary (vector-ref s 0) args)])
                  (if (eq? e 'impure) (impure! (list 'impure-procedure v (vector-ref s 2))) e))
                (impure! (list 'unknown-procedure v))))])))

    (define (apply-summary effect args)
      (cond
        [(not (pair? effect)) effect]
        [else
          (let loop ([ts effect] [out 'pure])
            (cond
              [(null? ts) out]
              [(and (pair? (car ts)) (eq? (car (car ts)) 'param))
                (let ([i (cdr (car ts))])
                  (if (< i (length args))
                    (let ([target (target-of (list-ref args i))])
                      (if (eq? target 'impure) (impure! 'argument-not-tracked) (loop (cdr ts) (join out target))))
                    (impure! 'missing-argument)))]
              [(and (pair? (car ts)) (eq? (car (car ts)) 'call))
                (let ([i (cdr (car ts))])
                  (if (< i (length args))
                    (let ([e (procedure-argument-effect (list-ref args i))])
                      (if (eq? e 'impure) (impure! 'unknown-procedure-argument) (loop (cdr ts) (join out e))))
                    (impure! 'missing-argument)))]
              [else (loop (cdr ts) (join out (list (car ts))))]))]))

    ;; The effect of calling the procedure the expression A evaluates to, whatever it is
    ;; called with: only when that does not depend on the arguments.
    (define (procedure-argument-effect a)
      (define (clean e) (if (and (pair? e) (exists pair? e)) 'impure e))
      (cond
        [(proc? a)
          (let loop ([args (proc-args a)])
            (cond
              [(pair? args) (hashtable-set! prov (car args) 'unknown) (loop (cdr args))]
              [(symbol? args) (hashtable-set! prov args 'unknown)]))
          (clean (eff (proc-body a)))]
        [(prim-name a) => (lambda (name) (if (hashtable-ref pure-prims name #f) 'pure 'impure))]
        [(and (lref? a) (let ([p (hashtable-ref prov (lref-sym a) #f)])
                          (and (pair? p) (eq? (car p) 'param) p)))
          => (lambda (p) (list (cons 'call (cdr p))))]
        [(lref? a)
          (let ([s (summary-of (lref-sym a))])
            (if s (clean (vector-ref s 0)) 'impure))]
        [else 'impure]))

    (define (callee-summary op)
      (and (lref? op) (summary-of (lref-sym op))))

    ;; #(effect fresh-result? culprit) of the procedure bound to V, #f when V is not known.
    (define (closure-proc rhs)
      (cond
        [(proc? rhs) rhs]
        [(and (let? rhs) (proc? (let-body rhs))) (let-body rhs)]
        [else #f]))
    (define (summary-of v)
      (let* ([c (canonical v)]
             [b (hashtable-ref by-lhs c #f)]
             [p (cond
                  [(hashtable-ref local-procs v #f) => (lambda (p) p)]
                  [(and b (not (eq? (sbinding-id b) '%effect))) (closure-proc (sbinding-rhs b))]
                  [else #f])])
        (and p
          (let ([m (hashtable-ref summaries v #f)])
            (cond
              [(eq? m 'in-progress)
                (unless (eq? v self) (set! cycle-seen? #t))
                (vector 'pure #f #f)]
              [m m]
              [else
                (hashtable-set! summaries v 'in-progress)
                (set! depth (+ depth 1))
                (let ([saved self] [saved-culprit culprit] [saved-cycle cycle-seen?] [saved-branch branch-depth])
                  (set! cycle-seen? #f)
                  (set! branch-depth 0)
                  (set! self v)
                  (set! culprit #f)
                  (when (and b (let? (sbinding-rhs b)))
                    (for-each (lambda (lhs) (hashtable-set! prov lhs (cons 'top c)))
                      (let-lhs (sbinding-rhs b))))
                  (let ([result (analyse-proc p)])
                    (vector-set! result 2 culprit)
                    (set! self saved)
                    (set! culprit saved-culprit)
                    (set! depth (- depth 1))
                    (set! branch-depth saved-branch)
                    (hashtable-set! summaries v result)
                    (when (and cycle-seen? (> depth 0))
                      (set! tentative (cons v tentative)))
                    (when (= depth 0)
                      (when (eq? (vector-ref result 0) 'impure)
                        (for-each (lambda (t) (hashtable-delete! summaries t)) tentative))
                      (set! tentative '()))
                    (set! cycle-seen? (or saved-cycle (and cycle-seen? (> depth 0))))
                    result))])))))
    (define tentative '())
    (define depth 0)
    (define cycle-seen? #f)
    (define self #f)

    (define (analyse-proc p)
      (let loop ([args (proc-args p)] [i 0])
        (cond
          [(pair? args)
            (hashtable-set! prov (car args) (cons 'param i))
            (loop (cdr args) (+ i 1))]
          [(symbol? args) (hashtable-set! prov args 'unknown)]))
      (let ([e (eff (proc-body p))])
        (vector (if (eq? e 'impure) 'impure e) (fresh? (proc-body p)) #f)))

    ;; the classification of the bindings
    ;; effect-of: lhs -> effect of the value of the binding
    (define forced-owner
      (cond [(find (lambda (o) (and (pair? o) (eq? (car o) 'forced-owner))) options) => cdr] [else #f]))
    (define (binding-effect b)
      (set! culprit #f)
      (set! branch-depth 0)
      (let* ([e (eff (sbinding-rhs b))]
             [e (if (and (pair? e) (exists pair? e)) (impure! 'parameter-target) e)]
             [e (if (and (eq? (sbinding-id b) '%effect) (equal? (sbinding-owner b) forced-owner))
                  (impure! 'statement-of-the-program)
                  e)])
        (when (eq? e 'impure) (hashtable-set! culprits (sbinding-lhs b) culprit))
        e))

    ;;; Marking
    (define live (make-eq-hashtable))
    (define why (make-eq-hashtable))
    (define attached (make-eq-hashtable))        ; target variable -> (binding ...)
    (define root-list '())
    (define classes (make-eq-hashtable))         ; lhs -> 'pure | 'root | targets
    (define current #f)

    (for-each
      (lambda (b)
        (let ([e (binding-effect b)])
          (hashtable-set! classes (sbinding-lhs b) e)
          (cond
            [(eq? e 'impure) (set! root-list (cons b root-list))]
            [(pair? e)
              (for-each
                (lambda (target)
                  (hashtable-set! attached target (cons b (hashtable-ref attached target '()))))
                e)]
            [else #f])))
      bindings)
    (set! root-list (reverse root-list))

    (define (mark-binding! b)
      (unless (hashtable-ref live (sbinding-lhs b) #f)
        (hashtable-set! live (sbinding-lhs b) #t)
        (hashtable-set! why (sbinding-lhs b) current)
        (let ([saved current])
          (set! current b)
          (for-each mark-var! (term-refs (sbinding-rhs b)))
          ;; The effects on the variable run now.
          (for-each mark-binding! (hashtable-ref attached (canonical (sbinding-lhs b)) '()))
          (set! current saved))))

    (define (mark-var! v)
      (let ([b (hashtable-ref by-lhs v #f)])
        (when b
          (mark-binding! b)
          (let ([c (canonical v)])
            (unless (eq? c v)
              (let ([cb (hashtable-ref by-lhs c #f)])
                (when cb (mark-binding! cb))))))))

    (for-each mark-binding! root-list)
    (when (memq 'no-shake options) (for-each mark-binding! bindings))
    (for-each (lambda (t)
                (for-each mark-var! (term-refs t)))
      roots-terms)

    ;;; The roots and what each keeps alive
    ;; The effect bindings that are roots, and for each the number of bindings only it keeps.
    (define (closure-of b stop)
      (let ([seen (make-eq-hashtable)])
        (define (visit-binding b)
          (unless (or (hashtable-ref seen (sbinding-lhs b) #f) (hashtable-ref stop (sbinding-lhs b) #f))
            (hashtable-set! seen (sbinding-lhs b) #t)
            (for-each visit-var (term-refs (sbinding-rhs b)))
            (for-each visit-binding (hashtable-ref attached (canonical (sbinding-lhs b)) '()))))
        (define (visit-var v)
          (let ([vb (hashtable-ref by-lhs v #f)])
            (when vb (visit-binding vb))))
        (visit-binding b)
        seen))

    (define (causes limit)
      (let* ([export-owner (cond [(find (lambda (o) (and (pair? o) (eq? (car o) 'export-owner))) options) => cdr] [else #f])]
             [all-roots (filter (lambda (b) (not (equal? (sbinding-owner b) export-owner)))
                          root-list)]
             [others-seen (make-eq-hashtable)]
             [closures (map (lambda (b) (cons b (closure-of b (make-eq-hashtable)))) all-roots)]
             [counts (make-eq-hashtable)])
        ;; how many roots reach each variable
        (for-each
          (lambda (c)
            (vector-for-each
              (lambda (v) (hashtable-set! counts v (+ 1 (hashtable-ref counts v 0))))
              (hashtable-keys (cdr c))))
          closures)
        (let* ([scored
                 (map (lambda (c)
                        (let ([exclusive 0])
                          (vector-for-each
                            (lambda (v) (when (= 1 (hashtable-ref counts v 0)) (set! exclusive (+ exclusive 1))))
                            (hashtable-keys (cdr c)))
                          (list exclusive (hashtable-size (cdr c)) (car c))))
                   closures)]
               [sorted (list-sort (lambda (a b) (> (car a) (car b))) scored)])
          (let take ([l sorted] [n limit])
            (if (or (null? l) (= n 0)) '() (cons (car l) (take (cdr l) (- n 1))))))))

    (when trace-unknown?
      (let-values ([(keys vals) (hashtable-entries unknown-calls)])
        (let ([pairs (vector->list (vector-map cons keys vals))])
          (for-each
            (lambda (p) (format (current-error-port) ";; static: unclassified primitive ~a (~a)~%" (car p) (cdr p)))
            (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))))

    (make-shaken live why root-list
      (length (filter (lambda (b) (and (pair? (hashtable-ref classes (sbinding-lhs b) #f)) (hashtable-ref live (sbinding-lhs b) #f))) bindings))
      (length (filter (lambda (b) (and (pair? (hashtable-ref classes (sbinding-lhs b) #f)) (not (hashtable-ref live (sbinding-lhs b) #f)))) bindings))
      causes culprits)))
