;; assignment elimination: convert lexical assignment into boxed
;; variable operations.

(library (capy compiler tree-il assignment-elimination)
  (export eliminate-assignments)
  (import
    (rnrs)
    (rnrs hashtables)
    (capy)
    (srfi 257)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms))

  ;; A `proc' or `receive' binder list may be improper: the dotted tail
  ;; is the rest (variadic) parameter.  Splits into (fixed . variadic)
  ;; syms plus their display names.
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

  ;; Every sym that is the target of an lset somewhere in the term.
  (define (collect-mutated t)
    (define mutated (make-eq-hashtable))
    ((make-tree-il-folder)
      t
      (lambda (node)
        (match node
          [(~lset _ _ sym _)
            (hashtable-set! mutated sym #t)
            (values)]
          [_ (values)]))
      (lambda (node) (values)))
    mutated)

  (define (box-ref display sym)
    (make-primcall #f 'variable-ref (list (make-lref #f display sym))))

  (define (box-set display sym value)
    (make-primcall #f 'variable-set! (list (make-lref #f display sym) value)))

  (define (pbox display sym)
    (make-primcall #f 'make-variable (list (make-lref #f display sym))))

  ;; let* -> nested plain lets (first binding outermost).
  (define (nest-let* src ids lhs rhs body)
    (let loop ([ids (reverse ids)] [lhs (reverse lhs)] [rhs (reverse rhs)] [body body])
      (if (null? ids)
        body
        (loop (cdr ids)
          (cdr lhs)
          (cdr rhs)
          (make-let src 'let (list (car ids)) (list (car lhs)) (list (car rhs)) body)))))

  ;; Wrap every mutated binder of LHS in a box: extend SUBST with a fresh
  ;; sym per mutated binder, recurse into BODY under the extended map,
  ;; and bind the fresh syms to (make-variable <orig>) around the result.
  (define (wrap-mutable lhs ids body subst mutated)
    (define binders
      (let loop ([lhs lhs] [ids ids] [acc '()])
        (cond
          [(null? lhs) (reverse acc)]
          [(pair? lhs) (loop (cdr lhs) (cdr ids) (cons (cons (car ids) (car lhs)) acc))]
          [else ; variadic tail
            (reverse (cons (cons (if (pair? ids) (car ids) 'rest) lhs) acc))])))
    (define mutated-binders
      (let loop ([bs binders] [acc '()])
        (if (null? bs)
          (reverse acc)
          (loop (cdr bs)
            (if (hashtable-ref mutated (cdr (car bs)) #f)
              (cons (car bs) acc)
              acc)))))
    (if (null? mutated-binders)
      (rec body subst mutated)
      (let ([new-syms (map (lambda (_) (gensym)) mutated-binders)])
        (for-each
          (lambda (b s) (hashtable-set! subst (cdr b) s))
          mutated-binders
          new-syms)
        (let ([body (rec body subst mutated)])
          (make-let (term-src body)
            'let
            (map car mutated-binders)
            new-syms
            (map (lambda (b) (pbox (car b) (cdr b))) mutated-binders)
            body)))))

  (define (rec term subst mutated)
    (match term
      [(~or (~toplevel-ref _ _ _) (~module-ref _ _ _ _) (~primref _ _)
            (~constant _ _) (~void _))
        term]
      [(~lref src name sym)
        (cond
          [(hashtable-ref subst sym #f)
            => (lambda (new-sym) (box-ref name new-sym))]
          [else
            (when (hashtable-ref mutated sym #f)
              (assertion-violation 'eliminate-assignments
                "lref of mutated lexical without substitution"
                term))
            term])]
      [(~lset src name sym val)
        (let ([new-sym (hashtable-ref subst sym #f)])
          (unless new-sym
            (assertion-violation 'eliminate-assignments
              "lset without substitution"
              term))
          (box-set name new-sym (rec val subst mutated)))]
      [(~application src operator operands)
        (make-application src
          (rec operator subst mutated)
          (map (lambda (x) (rec x subst mutated)) operands))]
      [(~primcall src prim args)
        (make-primcall src prim
          (map (lambda (x) (rec x subst mutated)) args))]
      [(~toplevel-define src mod name val)
        (make-toplevel-define src mod name (rec val subst mutated))]
      [(~toplevel-set src mod name val)
        (make-toplevel-set src mod name (rec val subst mutated))]
      [(~module-set src module name public? val)
        (make-module-set src module name public? (rec val subst mutated))]
      [(~if src test then else)
        (make-if src
          (rec test subst mutated)
          (rec then subst mutated)
          (rec else subst mutated))]
      [(~sequence src head tail)
        (make-sequence src (rec head subst mutated) (rec tail subst mutated))]
      [(~values src vals)
        (make-values src (map (lambda (x) (rec x subst mutated)) vals))]
      [(~proc src args body meta ids)
        (make-proc src args (wrap-mutable args ids body subst mutated) meta ids)]
      [(~let src style ids lhs rhs body)
        (when (memq style '(letrec letrec*))
          (assertion-violation 'eliminate-assignments
            "letrec must be eliminated before assignment elimination"
            term))
        (if (eq? style 'let*)
          (rec (nest-let* src ids lhs rhs body) subst mutated)
          (make-let src
            style
            ids
            lhs
            (map (lambda (r) (rec r subst mutated)) rhs)
            (wrap-mutable lhs ids body subst mutated)))]
      [(~fix src ids lhs rhs body)
        (make-fix src
          ids
          lhs
          (map
            (lambda (p)
              (match p
                [(~proc p-src args p-body meta p-ids)
                  (make-proc p-src
                    args
                    (wrap-mutable args p-ids p-body subst mutated)
                    meta
                    p-ids)]
                [_ (assertion-violation 'eliminate-assignments
                     "fix rhs is not a proc"
                     p)]))
            rhs)
          (rec body subst mutated))]
      [(~receive src ids vars producer consumer)
        (make-receive src
          ids
          vars
          (rec producer subst mutated)
          (wrap-mutable vars ids consumer subst mutated))]
      [(~wcm src key mark result)
        (make-wcm src key (rec mark subst mutated) (rec result subst mutated))]
      [_ term]))

  (define (eliminate-assignments term)
    (define mutated (collect-mutated term))
    (rec term (make-eq-hashtable) mutated)))
