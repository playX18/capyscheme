;; letrec-fixing: rewrite recursive bindings into non-recursive `let',
;; `fix' and `set!' sequences.
;;
;; Port of the Rust pass `rewrite_recursive_bindings'
;; (crates/capy/src/expander/recursive_bindings.rs).  Given a term whose
;; recursive bindings (letrec / letrec* / let with lambda RHSs) are still
;; intact, it:
;;
;;   1. removes `let*' bindings (nested plain `let's) to make the
;;      recursion analysis trivial;
;;   2. numbers every bound lexical with a fixnum id;
;;   3. collects referenced / assigned / "complex" bindings;
;;   4. per letrec, computes the strongly connected components of the
;;      binding dependency graph (using (capy graph)'s Tarjan SCC) and
;;      rewrites each component: single bindings become `seq', `fix',
;;      or `let'+`set!'; multi-binding components become a `fix' for the
;;      mutually recursive lambdas plus `let'+`set!' sequences for the
;;      complex (non-lambda or assigned) bindings.
;;
;; The pass is validated by running after it and observing that the Rust
;; `rewrite_recursive_bindings' becomes a no-op on the result; on
;; preprocessed IR the Rust pass is skipped entirely (see
;; `lower_expanded_to_cps' in crates/capy/src/compiler/pipeline.rs).

(library (capy compiler tree-il fix-letrec)
  (export fix-letrec)
  (import
    (rnrs)
    (rnrs hashtables)
    (capy)
    (srfi 257)
    (capy graph)
    (capy persistent-set)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms))

  ;; A `proc' or `receive' binder list may be improper: the dotted tail
  ;; is the rest (variadic) parameter.  `ids' holds the display names,
  ;; one per formal (fixed + variadic, as a proper list).  Splits both
  ;; into fixed and variadic parts.
  (define (split-formals args ids)
    (if (list? args)
      (values args #f ids #f)
      (let loop ([a args] [i ids] [fixed '()] [fixed-ids '()])
        (if (pair? a)
          (loop (cdr a) (cdr i) (cons (car a) fixed) (cons (car i) fixed-ids))
          (values (reverse fixed)
            a
            (reverse fixed-ids)
            (if (pair? i) (car i) #f))))))

  ;; A term with no observable effect (no calls, no assignments, no
  ;; control flow) can be dropped instead of hoisted into a binding.
  (define (transparent? x)
    (match x
      [(~or (~constant _ _) (~void _) (~lref _ _ _) (~proc _ _ _ _ _)) #t]
      [(~if _ test then else)
        (and (transparent? test) (transparent? then) (transparent? else))]
      [(~sequence _ head tail)
        (and (transparent? head) (transparent? tail))]
      [_ #f]))

  (define (any-proc? lst)
    (cond
      [(null? lst) #f]
      [(proc? (car lst)) #t]
      [else (any-proc? (cdr lst))]))

  ;; Number every bound lexical with a fixnum id, keyed by its sym.
  ;; Binders are numbered when their binding form is entered (pre-order),
  ;; mirroring the Rust `compute_ids'.
  (define (compute-ids t)
    (define sym-id (make-eq-hashtable))
    (define counter 0)
    (define (number! sym)
      (hashtable-set! sym-id sym counter)
      (set! counter (+ counter 1)))
    ((make-tree-il-folder)
      t
      (lambda (node)
        (match node
          [(~let _ _ _ lhs _ _)
            (for-each number! lhs)
            (values)]
          [(~fix _ _ lhs _ _)
            (for-each number! lhs)
            (values)]
          [(~proc _ args _ _ ids)
            (receive (fixed variadic fids vid) (split-formals args ids)
              (for-each number! fixed)
              (when variadic (number! variadic)))
            (values)]
          [(~receive _ ids vars _ _)
            (receive (fixed variadic fids vid) (split-formals vars ids)
              (for-each number! fixed)
              (when variadic (number! variadic)))
            (values)]
          [_ (values)]))
      (lambda (node) (values)))
    sym-id)

  ;; Collect the lexicals that are ever referenced (via lref) or assigned
  ;; (via lset) anywhere in the term.
  (define (compute-referenced-and-assigned t)
    (define referenced (make-eq-hashtable))
    (define assigned (make-eq-hashtable))
    ((make-tree-il-folder)
      t
      (lambda (node)
        (match node
          [(~lref _ _ sym)
            (hashtable-set! referenced sym #t)
            (values)]
          [(~lset _ _ sym _)
            (hashtable-set! assigned sym #t)
            (values)]
          [_ (values)]))
      (lambda (node) (values)))
    (values referenced assigned))

  ;; Let bindings whose lvar is assigned or whose RHS has effects are
  ;; "complex": they must be initialized before use.
  (define (compute-complex t assigned sym-id)
    (define complex (make-eq-hashtable))   ; keyed by sym id
    ((make-tree-il-folder)
      t
      (lambda (node) (values))
      (lambda (node)
        (match node
          [(~let _ _ _ lhs rhs _)
            (for-each
              (lambda (l r)
                (when (or (hashtable-ref assigned l #f) (not (transparent? r)))
                  (hashtable-set! complex (hashtable-ref sym-id l #f) #t)))
              lhs
              rhs)
            (values)]
          [_ (values)])))
    complex)

  ;; Memoized free-variable sets for sub-terms, in sym-id space.  A set
  ;; holds the ids of every lexical referenced by the term that is not
  ;; bound within it.  Port of `ComputeFreeVariables'
  ;; (recursive_bindings.rs).
  (define (make-compute-free-variables sym-id)
    (define memo (make-eq-hashtable))

    (define (empty) (make-persistent-set-eqv))
    (define (id sym)
      (or (hashtable-ref sym-id sym #f)
        (assertion-violation 'fix-letrec "lexical without an id" sym)))
    (define (ids->set syms)
      (list->persistent-set (map id syms) 'eqv))
    (define (union2 a b) (persistent-set-union a b))
    (define (diff a b) (persistent-set-difference a b))
    (define (adjoin s elt) (persistent-set-add s elt))
    (define (union-all sets)
      (fold-left (lambda (acc s) (persistent-set-union acc s)) (empty) sets))
    (define (formals-set args ids)
      (receive (fixed variadic fids vid) (split-formals args ids)
        (ids->set (if variadic (append fixed (list variadic)) fixed))))

    (define (visit-proc p)
      (diff (visit (proc-body p)) (formals-set (proc-args p) (proc-ids p))))

    (define (visit t)
      (match t
        [(~or (~constant _ _) (~void _) (~primref _ _)
              (~module-ref _ _ _ _) (~toplevel-ref _ _ _))
          (empty)]
        [(~lref _ _ sym)
          (adjoin (empty) (id sym))]
        [(~lset _ _ sym val)
          (adjoin (visit val) (id sym))]
        [(~toplevel-set _ _ _ val) (visit val)]
        [(~module-set _ _ _ _ val) (visit val)]
        [(~application _ func args)
          (union-all (cons (visit func) (map visit args)))]
        [(~primcall _ _ args)
          (union-all (map visit args))]
        [(~toplevel-define _ _ _ val) (visit val)]
        [(~sequence _ head tail)
          (union2 (visit head) (visit tail))]
        [(~if _ test then else)
          (union2 (union2 (visit test) (visit then)) (visit else))]
        [(~let _ style _ lhs rhs body)
          (if (memq style '(letrec letrec*))
            (diff (union2 (union-all (map visit rhs)) (visit body))
              (ids->set lhs))
            (union2 (union-all (map visit rhs))
              (diff (visit body) (ids->set lhs))))]
        [(~proc _ args body _ ids)
          (diff (visit body) (formals-set args ids))]
        [(~fix _ _ lhs rhs body)
          (diff (union2 (union-all (map visit-proc rhs)) (visit body))
            (ids->set lhs))]
        [(~receive _ ids vars producer consumer)
          (union2 (visit producer)
            (diff (visit consumer) (formals-set vars ids)))]
        [(~values _ vals)
          (union-all (map visit vals))]
        [(~wcm _ key mark result)
          (union2 (union2 (visit key) (visit mark)) (visit result))]
        [_ (empty)]))

    (define (get t)
      (or (hashtable-ref memo t #f)
        (let ([fv (visit t)])
          (hashtable-set! memo t fv)
          fv)))
    get)

  ;; The binding-dependency graph of one letrec: vertices are the
  ;; bindings in order; a binding has an edge to every binding it
  ;; references; for letrec* (in_order), consecutive complex bindings
  ;; are additionally chained so that initialization order is preserved.
  (define (compute-sccs ids lhs rhs in-order? sym-id complex fv-computer)
    (define g (make-graph))
    (define n (length lhs))
    (define id->vertex (make-eqv-hashtable))
    (define (id-of sym) (hashtable-ref sym-id sym #f))

    (for-each
      (lambda (name l r)
        (define idx (graph-add-vertex! g (list name l r)))
        (hashtable-set! id->vertex (id-of l) idx))
      ids
      lhs
      rhs)

    (let loop ([i 0])
      (when (< i n)
        (let* ([payload (graph-vertex-payload g i)]
               [init (caddr payload)])
          (for-each
            (lambda (fv-id)
              (let ([target (hashtable-ref id->vertex fv-id #f)])
                (when target (graph-add-edge! g i target))))
            (merge-sort-fixnums (persistent-set->list (fv-computer init))))
          (loop (+ i 1)))))

    (when in-order?
      (let loop ([i 0] [prev #f])
        (when (< i n)
          (when prev (graph-add-edge! g i prev))
          (if (hashtable-ref complex
                (id-of (cadr (graph-vertex-payload g i)))
                #f)
            (loop (+ i 1) i)
            (loop (+ i 1) prev)))))

    (map
      (lambda (scc) (map (lambda (i) (graph-vertex-payload g i)) scc))
      (tarjan-scc g)))

  ;; Sorting helper for fixnum lists (kept local to avoid depending on
  ;; the R6RS list-sort binding in a library-compile context).
  (define (merge-sort-fixnums lst)
    (define (merge a b)
      (cond
        [(null? a) b]
        [(null? b) a]
        [(< (car a) (car b))
          (cons (car a) (merge (cdr a) b))]
        [else (cons (car b) (merge a (cdr b)))]))
    (define (split lst)
      (let loop ([fast lst] [slow lst] [acc '()])
        (if (or (null? fast) (null? (cdr fast)))
          (values (reverse acc) slow)
          (loop (cddr fast) (cdr slow) (cons (car slow) acc)))))
    (if (or (null? lst) (null? (cdr lst)))
      lst
      (receive (left right) (split lst)
        (merge (merge-sort-fixnums left) (merge-sort-fixnums right)))))

  ;; Rewrite one SCC of bindings.  Binds is a list of
  ;; (display-name . (sym . init)) pairs, sorted by binding index.
  (define (fix-scc src binds body referenced assigned sym-id fv-computer)
    (match binds
      [(~list (~list name sym init))
        (cond
          ;; unreferenced: evaluate the init for its side effects
          [(not (hashtable-ref referenced sym #f))
            (make-sequence src init body)]
          ;; single lambda, never assigned: a plain fix
          [(and (proc? init) (not (hashtable-ref assigned sym #f)))
            (make-fix src (list name) (list sym) (list init) body)]
          ;; self-recursive non-lambda: pre-bind undefined, then set!
          [(persistent-set-contains?
             (fv-computer init)
             (hashtable-ref sym-id sym #f))
            (make-let src
              'let
              (list name)
              (list sym)
              (list (make-void src))
              (make-sequence src (make-lset src name sym init) body))]
          ;; ordinary binding
          [else
            (make-let src 'let (list name) (list sym) (list init) body)])]
      [_ ; multiple bindings
        (let-values ([(lambdas complex)
                      (partition-binds
                        (lambda (b)
                          (and (proc? (caddr b))
                            (not (hashtable-ref assigned (cadr b) #f))))
                        binds)])
          (let ([body
                  (fold-right
                    (lambda (b body)
                      (make-sequence src
                        (make-lset src (car b) (cadr b) (caddr b))
                        body))
                    body
                    complex)])
            (let ([body
                    (if (null? lambdas)
                      body
                      (make-fix src
                        (map car lambdas)
                        (map cadr lambdas)
                        (map caddr lambdas)
                        body))])
              (if (null? complex)
                body
                (make-let src
                  'let
                  (map car complex)
                  (map cadr complex)
                  (map (lambda (_) (make-void src)) complex)
                  body)))))]))

  (define (partition-binds pred lst)
    (let loop ([lst lst] [yes '()] [no '()])
      (if (null? lst)
        (values (reverse yes) (reverse no))
        (if (pred (car lst))
          (loop (cdr lst) (cons (car lst) yes) no)
          (loop (cdr lst) yes (cons (car lst) no))))))

  (define (fix-term src in-order? ids lhs rhs body sym-id referenced assigned
      complex fv-computer)
    (define sccs
      (compute-sccs ids lhs rhs in-order? sym-id complex fv-computer))
    (fold-right
      (lambda (scc body)
        (fix-scc src scc body referenced assigned sym-id fv-computer))
      body
      sccs))

  ;; let* -> nested plain lets (Rust `remove_letstar').
  (define (remove-letstar t)
    (post-order
      (lambda (x)
        (match x
          [(~let src 'let* ids lhs rhs body)
            (let loop ([ids (reverse ids)]
                       [lhs (reverse lhs)]
                       [rhs (reverse rhs)]
                       [body body])
              (if (null? ids)
                body
                (loop (cdr ids)
                  (cdr lhs)
                  (cdr rhs)
                  (make-let src
                    'let
                    (list (car ids))
                    (list (car lhs))
                    (list (car rhs))
                    body))))]
          [_ x]))
      t))

  (define (fix-letrec exp)
    (define t (remove-letstar exp))
    (define sym-id (compute-ids t))
    (define-values (referenced assigned) (compute-referenced-and-assigned t))
    (define complex (compute-complex t assigned sym-id))
    (define fv-computer (make-compute-free-variables sym-id))
    (post-order
      (lambda (x)
        (match x
          [(~lset src name sym val)
            (if (not (hashtable-ref referenced sym #f))
              (make-sequence #f val (make-constant #f #f))
              x)]
          [(~let src style ids lhs rhs body)
            (cond
              [(or (eq? style 'letrec) (eq? style 'letrec*))
                (fix-term src
                  (eq? style 'letrec*)
                  ids
                  lhs
                  rhs
                  body
                  sym-id
                  referenced
                  assigned
                  complex
                  fv-computer)]
              [(and (eq? style 'let) (any-proc? rhs))
                (fix-term src
                  #f
                  ids
                  lhs
                  rhs
                  body
                  sym-id
                  referenced
                  assigned
                  complex
                  fv-computer)]
              [else x])]
          [_ x]))
      t)))
