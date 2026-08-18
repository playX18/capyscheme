;; well-known procedure expansion: inline lexically bound procedures
;; used exactly once as operators.


(library (capy compiler tree-il well-known-procs)
  (export expand-well-known-procs)
  (import
    (rnrs)
    (rnrs hashtables)
    (capy)
    (srfi 257)
    (capy compiler tree-il fold)
    (capy compiler tree-il terms))

  (define *enable-well-known-expansion? #f)

  ;; A `proc' binder list may be improper: the dotted tail is the rest
  ;; (variadic) parameter.
  (define (split-formals args)
    (if (list? args)
      (values args #f)
      (let loop ([a args] [fixed '()])
        (if (pair? a)
          (loop (cdr a) (cons (car a) fixed))
          (values (reverse fixed) a)))))

  ;; For every let/fix-bound proc, count total references and
  ;; operator-position references.  A proc with exactly one reference,
  ;; that being in operator position, becomes well-known.
  (define (analyze-procs t)
    (define infos (make-eq-hashtable))   ; sym -> (refcount op-refcount proc)
    (define (record-let-bindings lhs rhs)
      (for-each
        (lambda (l r)
          (when (proc? r)
            (hashtable-set! infos l (list 0 0 r))))
        lhs
        rhs))
    ((make-tree-il-folder)
      t
      (lambda (node)
        (match node
          [(~lref _ _ sym)
            (match (hashtable-ref infos sym #f)
              [#f (values)]
              [(~list refs ops proc)
                (hashtable-set! infos sym (list (+ refs 1) ops proc))
                (values)])]
          [(~lset _ _ sym _)
            (hashtable-set! infos sym #f)
            (values)]
          [(~application _ operator operands)
            (match operator
              [(~lref _ _ sym)
                (match (hashtable-ref infos sym #f)
                  [#f (values)]
                  [(~list refs ops proc)
                    (hashtable-set! infos sym (list (+ refs 1) (+ ops 1) proc))
                    (values)])]
              [_ (values)])]
          [(~let _ _ _ lhs rhs _)
            (record-let-bindings lhs rhs)
            (values)]
          [(~fix _ _ lhs rhs _)
            (record-let-bindings lhs rhs)
            (values)]
          [_ (values)]))
      (lambda (node) (values)))
    (define to-expand (make-eq-hashtable))
    (for-each
      (lambda (sym)
        (match (hashtable-ref infos sym #f)
          [(~list refs ops proc)
            (when (and (not (= ops 0)) (= (- refs ops) 1))
              (hashtable-set! to-expand sym proc))]
          [_ #f]))
      (vector->list (hashtable-keys infos)))
    to-expand)

  ;; Replace a reference to a well-known proc with an inline copy of the
  ;; proc whose body calls the original binding.
  (define (expand-proc-ref lexical proc)
    (define src (term-src lexical))
    (define args (proc-args proc))
    (define ids (proc-ids proc))
    (receive (fixed-args variadic) (split-formals args)
      (receive (fixed-ids _) (split-formals ids)
        (define var-refs (map (lambda (s) (make-lref src 'arg s)) fixed-args))
        (define body
          (if variadic
            (make-primcall src
              'apply
              (cons lexical
                (append var-refs (list (make-lref src 'rest variadic)))))
            (make-application src lexical var-refs)))
        (make-proc src args body (proc-meta proc) ids))))

  ;; Collapse (lambda (a b) (f a b)) back to a reference to f: a proc
  ;; whose body is just a call of a lexical with exactly its own formals.
  (define (collapse-proc-wrapper proc)
    (define args (proc-args proc))
    (define body (proc-body proc))
    (define (arg-matches? formal arg)
      (match arg
        [(~lref _ _ sym) (eq? sym formal)]
        [_ #f]))
    (define (all-matching? formals args)
      (if (null? formals)
        (null? args)
        (and (pair? args)
          (arg-matches? (car formals) (car args))
          (all-matching? (cdr formals) (cdr args)))))
    (match body
      [(~application _ (~lref _ _ var) operands)
        (if (and (list? operands)
             (= (length operands) (length args))
             (all-matching? args operands))
          (make-lref (term-src body) 'proc var)
          #f)]
      [(~primcall _ 'apply rands)
        (receive (fixed variadic) (split-formals args)
          (if (and variadic
               (pair? rands)
               (lref? (car rands))
               (= (length rands) (+ (length fixed) 2))
               (all-matching? fixed (list-tail rands 1))
               (arg-matches? variadic (car (reverse rands))))
            (let ([var (lref-sym (car rands))])
              (make-lref (term-src body) 'proc var))
            #f))]
      [_ #f]))

  (define (expand-well-known-procs term)
    (if (not *enable-well-known-expansion?)
      term
      (let ([to-expand (analyze-procs term)])
        (post-order
          (lambda (x)
            (match x
              [(~lref src _ sym)
                (match (hashtable-ref to-expand sym #f)
                  [#f x]
                  [proc (expand-proc-ref x proc)])]
              [(~proc _ _ _ _ _)
                (or (collapse-proc-wrapper x) x)]
              [_ x]))
          term)))))
