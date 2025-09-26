(seq
 (define syntax->datum (const false))
 (define datum->syntax (const false))
 (define identifier? (const false))
 (define generate-temporaries (const false))
 (define free-identifier=? (const false))
 (define bound-identifier=? (const false))
 (define $sc-dispatch (const false))
 (define macroexpand (const false))
 (let
    ([syntax?
      (call (toplevel-ref module-ref) (#%current-module)
       (const syntax?))]
    [make-syntax
      (call (toplevel-ref module-ref) (#%current-module)
       (const make-syntax))]
    [syntax-expression
      (call (toplevel-ref module-ref) (#%current-module)
       (const syntax-expression))]
    [syntax-wrap
      (call (toplevel-ref module-ref) (#%current-module)
       (const syntax-wrap))]
    [syntax-module
      (call (toplevel-ref module-ref) (#%current-module)
       (const syntax-module))]
    [syntax-sourcev
      (call (toplevel-ref module-ref) (#%current-module)
       (const syntax-sourcev))])
  (fix
    ([top-level-eval (lambda (exp env)
      (call (toplevel-ref primitive-eval) (lref exp)))])
   (fix
     ([local-eval (lambda (exp env)
       (call (toplevel-ref primitive-eval) (lref exp)))])
    (fix
      ([global-extend (lambda (type sym val)
        (call (toplevel-ref module-define!) (#%current-module)
         (lref sym)
         (call (toplevel-ref make-syntax-transformer) (lref sym)
          (lref type)
          (lref val))))])
     (fix
       ([sourcev-filename (lambda (s)
         (#%vector-ref (lref s) (const 0)))])
      (fix
        ([sourcev-line (lambda (s)
          (#%vector-ref (lref s) (const 1)))])
       (fix
         ([sourcev-column (lambda (s)
           (#%vector-ref (lref s) (const 2)))])
        (seq
         (lambda (sourcev)
          (fix
            ([maybe-acons (lambda (k v tail)
              (if
                (lref v)
                (#%cons (#%cons (lref k) (lref v)) (lref tail))
                (lref tail)))])
           (let
              ([and-tmp
                (lref sourcev)])
            (if
              (lref and-tmp)
              (call (lref maybe-acons) (const filename)
               (call (lref sourcev-filename) (lref sourcev))
               (#%cons (#%cons (const line)
                 (call (lref sourcev-line) (lref sourcev)))
                (#%cons (#%cons (const column)
                  (call (lref sourcev-column) (lref sourcev)))
                 (const '()))))
              (const false)))))
         (fix
           ([maybe-name-value (lambda (name val)
             (if
               (call (toplevel-ref proc?) (lref val))
               (let
                  ([meta
                    (call (toplevel-ref proc-meta) (lref val))])
                (if
                  (call (toplevel-ref assq) (const name) (lref meta))
                  (lref val)
                  (call (toplevel-ref make-proc) (call (toplevel-ref term-src) (lref val))
                    (call (toplevel-ref proc-args) (lref val))
                    (call (toplevel-ref proc-body) (lref val))
                    (#%cons (#%cons (const name) (lref name))
                     (lref meta))
                    (call (toplevel-ref proc-ids) (lref val)))))
               (lref val)))])
          (fix
            ([build-primref (lambda (src name)
              (call (toplevel-ref make-primref) (lref src)
               (lref name)))])
           (fix
             ([build-void (lambda (s)
               (call (toplevel-ref make-void) (lref s)))])
            (fix
              ([build-global-definition (lambda (src mod var exp)
                (call (toplevel-ref make-toplevel-define) (lref src)
                 (let
                    ([and-tmp
                      (lref mod)])
                  (if
                    (lref and-tmp)
                    (#%cdr (lref mod))
                    (const false)))
                 (lref var)
                 (call (lref maybe-name-value) (lref var)
                  (lref exp))))])
             (fix
               ([build-global-reference (lambda (src var mod)
                 (call (toplevel-ref make-toplevel-ref) (lref src)
                  (lref mod)
                  (lref var)))])
              (fix
                ([build-global-assignment (lambda (src var val mod)
                  (call (toplevel-ref make-toplevel-set) (lref src)
                   (lref mod)
                   (lref var)
                   (lref val)))])
               (fix
                 ([build-primcall (lambda (src name args)
                   (call (toplevel-ref make-primcall) (lref src)
                    (lref name)
                    (lref args)))])
                (fix
                  ([build-simple-lambda
                  (lambda (src ids vars meta exp)
                    (call (toplevel-ref make-proc) (lref src)
                     (lref vars)
                     (lref exp)
                     (lref meta)
                     (lref ids)))])
                 (fix
                   ([gen-lexical (lambda (id)
                     (call (toplevel-ref module-gensym) (#%symbol->string (lref id))))])
                  (fix
                    ([gen-var (lambda (id)
                      (let
                         ([id
                           (if
                             (call (lref syntax?) (lref id))
                             (call (lref syntax-expression) (lref id))
                             (lref id))])
                       (call (lref gen-lexical) (lref id))))])
                   (let
                      ([empty-wrap
                        (const ('()))])
                    (fix
                      ([strip (lambda (x)
                        (let
                           ([cond-test-tmp
                             (call (lref syntax?) (lref x))])
                         (if
                           (lref cond-test-tmp)
                           (call (lref strip) (call (lref syntax-expression) (lref x)))
                           (let
                               ([cond-test-tmp
                                 (#%pair? (lref x))])
                             (if
                               (lref cond-test-tmp)
                               (#%cons (call (lref strip) (#%car (lref x)))
                                (call (lref strip) (#%cdr (lref x))))
                               (let
                                   ([cond-test-tmp
                                     (#%vector? (lref x))])
                                 (if
                                   (lref cond-test-tmp)
                                   (call (toplevel-ref list->vector) (call (lref strip) (call (toplevel-ref vector->list) (lref x))))
                                   (lref x))))))))])
                     (fix
                       ([build-call (lambda (src rator rands)
                         (call (toplevel-ref make-application) (lref src)
                          (lref rator)
                          (lref rands)))])
                      (fix
                        ([parse-when-list (lambda (e when-list)
                          (let
                             ([result
                               (call (lref strip) (lref when-list))])
                           (fix
                             ([lp (lambda (l)
                               (let
                                  ([v
                                    (lref l)])
                                (fix
                                  ([fk (lambda ()
                                    (fix
                                      ([fk (lambda ()
                                        (call (toplevel-ref error) (const value failed to match)
                                         (lref v)))])
                                     (if
                                       (#%pair? (lref v))
                                       (let
                                          ([vx
                                            (#%car (lref v))]
                                          [vy
                                            (#%cdr (lref v))])
                                        (let
                                           ([x
                                             (lref vx)])
                                         (let
                                            ([l
                                              (lref vy)])
                                          (let
                                             ([v
                                               (lref x)])
                                           (fix
                                             ([fk (lambda ()
                                               (seq
                                                (lambda ()
                                                 (call (toplevel-ref error) (const value failed to match)
                                                  (lref v)))
                                                (call (toplevel-ref syntax-violation) (const eval-when)
                                                 (const invalid situation)
                                                 (lref e)
                                                 (lref x))))])
                                            (fix
                                              ([tk (lambda ()
                                                (call (lref lp) (lref l)))])
                                             (if
                                               (#%eq? (lref v)
                                                 (const compile))
                                               (call (lref tk))
                                               (fix
                                                  ([tk (lambda ()
                                                    (call (lref tk)))])
                                                 (if
                                                   (#%eq? (lref v)
                                                     (const load))
                                                   (call (lref tk))
                                                   (fix
                                                      ([tk (lambda ()
                                                        (call (lref tk)))])
                                                     (if
                                                       (#%eq? (lref v)
                                                         (const eval))
                                                       (call (lref tk))
                                                       (fix
                                                          ([tk
                                                          (lambda ()
                                                            (call (lref tk)))])
                                                         (if
                                                           (#%eq? (lref v)
                                                             (const expand))
                                                           (call (lref tk))
                                                           (call (lref fk)))))))))))))))
                                       (call (lref fk)))))])
                                 (if
                                   (#%null? (lref v))
                                   (lref result)
                                   (call (lref fk))))))])
                            (call (lref lp) (lref result)))))])
                       (fix
                         ([eval-local-transformer
                         (lambda (expanded mod)
                           (let
                              ([p
                                (call (lref local-eval) (lref expanded)
                                 (lref mod))])
                            (seq
                             (if
                               (#%not (#%procedure? (lref p)))
                               (call (toplevel-ref syntax-violation) (const false)
                                (const nonprocedure transformer)
                                (lref p))
                               (const #<undefined>))
                             (lref p))))])
                        (fix
                          ([wrap-syntax (lambda (x w defmod)
                            (call (lref make-syntax) (call (lref syntax-expression) (lref x))
                             (lref w)
                             (let
                                ([or-tmp
                                  (call (lref syntax-module) (lref x))])
                              (if
                                (lref or-tmp)
                                (lref or-tmp)
                                (lref defmod)))
                             (call (lref syntax-sourcev) (lref x))))])
                         (fix
                           ([smart-append (lambda (m1 m2)
                             (if
                               (#%null? (lref m2))
                               (lref m1)
                               (#%append (lref m1) (lref m2))))])
                          (fix
                            ([wrap-subst (lambda (w)
                              (#%cdr (lref w)))])
                           (fix
                             ([wrap-marks (lambda (w)
                               (#%car (lref w)))])
                            (fix
                              ([make-wrap (lambda (marks subst)
                                (#%cons (lref marks) (lref subst)))])
                             (fix
                               ([join-wraps (lambda (w1 w2)
                                 (let
                                    ([m1
                                      (call (lref wrap-marks) (lref w1))]
                                    [s1
                                      (call (lref wrap-subst) (lref w1))])
                                  (if
                                    (#%null? (lref m1))
                                    (if
                                      (#%null? (lref s1))
                                      (lref w2)
                                      (call (lref make-wrap) (call (lref wrap-marks) (lref w2))
                                        (call (lref smart-append) (lref s1)
                                         (call (lref wrap-subst) (lref w2)))))
                                    (call (lref make-wrap) (call (lref smart-append) (lref m1)
                                       (call (lref wrap-marks) (lref w2)))
                                      (call (lref smart-append) (lref s1)
                                       (call (lref wrap-subst) (lref w2)))))))])
                              (fix
                                ([source-wrap (lambda (x w s defmod)
                                  (let
                                     ([cond-test-tmp
                                       (let
                                          ([and-tmp
                                            (#%null? (call (lref wrap-marks) (lref w)))])
                                        (if
                                          (lref and-tmp)
                                          (let
                                             ([and-tmp
                                               (#%null? (call (lref wrap-subst) (lref w)))])
                                           (if
                                             (lref and-tmp)
                                             (let
                                                ([and-tmp
                                                  (#%not (lref defmod))])
                                              (if
                                                (lref and-tmp)
                                                (#%not (lref s))
                                                (const false)))
                                             (const false)))
                                          (const false)))])
                                   (if
                                     (lref cond-test-tmp)
                                     (lref x)
                                     (let
                                         ([cond-test-tmp
                                           (call (lref syntax?) (lref x))])
                                       (if
                                         (lref cond-test-tmp)
                                         (call (lref wrap-syntax) (lref x)
                                          (call (lref join-wraps) (lref w)
                                           (call (lref syntax-wrap) (lref x)))
                                          (lref defmod))
                                         (let
                                             ([cond-test-tmp
                                               (#%null? (lref x))])
                                           (if
                                             (lref cond-test-tmp)
                                             (lref x)
                                             (call (lref make-syntax) (lref x)
                                               (lref w)
                                               (lref defmod)
                                               (lref s)))))))))])
                               (fix
                                 ([id? (lambda (x)
                                   (let
                                      ([cond-test-tmp
                                        (call (module-ref (capy)::symbol? #t) (lref x))])
                                    (if
                                      (lref cond-test-tmp)
                                      (const true)
                                      (let
                                          ([cond-test-tmp
                                            (call (lref syntax?) (lref x))])
                                        (if
                                          (lref cond-test-tmp)
                                          (call (module-ref (capy)::symbol? #t) (call (lref syntax-expression) (lref x)))
                                          (const false))))))])
                                (fix
                                  ([valid-bound-ids? (lambda (ids)
                                    (fix
                                      ([all-ids? (lambda (ids)
                                        (let
                                           ([cond-test-tmp
                                             (#%null? (lref ids))])
                                         (if
                                           (lref cond-test-tmp)
                                           (const true)
                                           (let
                                               ([cond-test-tmp
                                                 (#%pair? (lref ids))])
                                             (if
                                               (lref cond-test-tmp)
                                               (let
                                                  ([id
                                                    (#%car (lref ids))]
                                                  [ids
                                                    (#%cdr (lref ids))])
                                                (let
                                                   ([and-tmp
                                                     (call (lref id?) (lref id))])
                                                 (if
                                                   (lref and-tmp)
                                                   (call (lref all-ids?) (lref ids))
                                                   (const false))))
                                               (const false))))))])
                                     (call (lref all-ids?) (lref ids))))])
                                 (fix
                                   ([make-ribcage
                                   (lambda (symnames marks labels)
                                     (let
                                        ([vec
                                          (#%make-vector (const 4)
                                           (const #<undefined>))])
                                      (seq
                                       (#%vector-set! (lref vec)
                                        (const 0)
                                        (const ribcage))
                                       (#%vector-set! (lref vec)
                                        (const 1)
                                        (lref symnames))
                                       (#%vector-set! (lref vec)
                                        (const 2)
                                        (lref marks))
                                       (#%vector-set! (lref vec)
                                        (const 3)
                                        (lref labels))
                                       (lref vec))))])
                                  (fix
                                    ([join-marks (lambda (m1 m2)
                                      (call (lref smart-append) (lref m1)
                                       (lref m2)))])
                                   (fix
                                     ([id-sym-name&marks (lambda (x w)
                                       (if
                                         (call (lref syntax?) (lref x))
                                         (values (call (lref syntax-expression) (lref x)) (call (lref join-marks) (call (lref wrap-marks) (lref w))
                                          (call (lref wrap-marks) (call (lref syntax-wrap) (lref x)))))
                                         (values (lref x) (call (lref wrap-marks) (lref w)))))])
                                    (fix
                                      ([make-binding-wrap
                                      (lambda (ids labels w)
                                        (let
                                           ([cond-test-tmp
                                             (#%null? (lref ids))])
                                         (if
                                           (lref cond-test-tmp)
                                           (lref w)
                                           (call (lref make-wrap) (call (lref wrap-marks) (lref w))
                                             (#%cons (let
                                                 ([labelvec
                                                   (call (toplevel-ref list->vector) (lref labels))])
                                               (let
                                                  ([n
                                                    (#%vector-length (lref labelvec))])
                                                (let
                                                   ([symnamevec
                                                     (#%make-vector (lref n))])
                                                 (let
                                                    ([marksvec
                                                      (#%make-vector (lref n))])
                                                  (fix
                                                    ([f
                                                    (lambda (ids i)
                                                      (let
                                                         ([cond-test-tmp
                                                           (#%null? (lref ids))])
                                                       (if
                                                         (lref cond-test-tmp)
                                                         (call (lref make-ribcage) (lref symnamevec)
                                                          (lref marksvec)
                                                          (lref labelvec))
                                                         (let
                                                             ([id
                                                               (#%car (lref ids))]
                                                             [ids
                                                               (#%cdr (lref ids))])
                                                           (receive (symname marks)
                                                             (call (lref id-sym-name&marks) (lref id)
                                                               (lref w))
                                                             (seq
                                                               (#%vector-set! (lref symnamevec)
                                                                (lref i)
                                                                (lref symname))
                                                               (#%vector-set! (lref marksvec)
                                                                (lref i)
                                                                (lref marks))
                                                               (call (lref f) (lref ids)
                                                                (#%+ (lref i)
                                                                 (const 1)))))))))])
                                                   (call (lref f) (lref ids)
                                                    (const 0)))))))
                                              (call (lref wrap-subst) (lref w)))))))])
                                     (fix
                                       ([gen-unique (lambda args
                                         (let
                                            ([cond-test-tmp
                                              (#%null? (lref args))])
                                          (if
                                            (lref cond-test-tmp)
                                            (let
                                               ([vec
                                                 (#%make-vector (const 2)
                                                  (const #<undefined>))])
                                             (seq
                                              (#%vector-set! (lref vec)
                                               (const 0)
                                               (const (capy)))
                                              (#%vector-set! (lref vec)
                                               (const 1)
                                               (call (toplevel-ref gensym) (const id)))
                                              (lref vec)))
                                            (let
                                                ([vec
                                                  (#%make-vector (const 2)
                                                   (const #<undefined>))])
                                              (seq
                                               (#%vector-set! (lref vec)
                                                (const 0)
                                                (call (toplevel-ref module-name) (#%car (lref args))))
                                               (#%vector-set! (lref vec)
                                                (const 1)
                                                (call (toplevel-ref module-generate-unique-id!) (#%car (lref args))))
                                               (lref vec))))))])
                                      (fix
                                        ([gen-label (lambda ()
                                          (call (lref gen-unique)))])
                                       (fix
                                         ([gen-labels (lambda (ls)
                                           (let
                                              ([cond-test-tmp
                                                (#%null? (lref ls))])
                                            (if
                                              (lref cond-test-tmp)
                                              (const '())
                                              (#%cons (call (lref gen-label))
                                                (call (lref gen-labels) (#%cdr (lref ls)))))))])
                                        (fix
                                          ([macros-only-env
                                          (lambda (r)
                                            (let
                                               ([v
                                                 (lref r)])
                                             (fix
                                               ([fk (lambda ()
                                                 (fix
                                                   ([fk (lambda ()
                                                     (call (toplevel-ref error) (const value failed to match)
                                                      (lref v)))])
                                                  (if
                                                    (#%pair? (lref v))
                                                    (let
                                                       ([vx
                                                         (#%car (lref v))]
                                                       [vy
                                                         (#%cdr (lref v))])
                                                     (let
                                                        ([a
                                                          (lref vx)])
                                                      (let
                                                         ([r
                                                           (lref vy)])
                                                       (let
                                                          ([v
                                                            (lref a)])
                                                        (fix
                                                          ([fk
                                                          (lambda ()
                                                            (seq
                                                             (lambda ()
                                                              (call (toplevel-ref error) (const value failed to match)
                                                               (lref v)))
                                                             (call (lref macros-only-env) (lref r))))])
                                                         (if
                                                           (#%pair? (lref v))
                                                           (let
                                                              ([vx
                                                                (#%car (lref v))]
                                                              [vy
                                                                (#%cdr (lref v))])
                                                            (let
                                                               ([k
                                                                 (lref vx)])
                                                             (if
                                                               (#%pair? (lref vy))
                                                               (let
                                                                  ([vx
                                                                    (#%car (lref vy))]
                                                                  [vy
                                                                    (#%cdr (lref vy))])
                                                                (fix
                                                                  ([tk
                                                                  (lambda ()
                                                                    (#%cons (lref a)
                                                                     (call (lref macros-only-env) (lref r))))])
                                                                 (if
                                                                   (#%eq? (lref vx)
                                                                     (const macro))
                                                                   (call (lref tk))
                                                                   (fix
                                                                      ([tk
                                                                      (lambda ()
                                                                        (call (lref tk)))])
                                                                     (if
                                                                       (#%eq? (lref vx)
                                                                         (const syntax-parameter))
                                                                       (call (lref tk))
                                                                       (fix
                                                                          ([tk
                                                                          (lambda ()
                                                                            (call (lref tk)))])
                                                                         (if
                                                                           (#%eq? (lref vx)
                                                                             (const ellipsis))
                                                                           (call (lref tk))
                                                                           (call (lref fk)))))))))
                                                               (call (lref fk)))))
                                                           (call (lref fk))))))))
                                                    (call (lref fk)))))])
                                              (if
                                                (#%null? (lref v))
                                                (const '())
                                                (call (lref fk))))))])
                                         (fix
                                           ([extend-env
                                           (lambda (labels bindings r)
                                             (let
                                                ([cond-test-tmp
                                                  (#%null? (lref labels))])
                                              (if
                                                (lref cond-test-tmp)
                                                (lref r)
                                                (let
                                                    ([cond-test-tmp
                                                      (#%pair? (lref labels))])
                                                  (if
                                                    (lref cond-test-tmp)
                                                    (let
                                                       ([label
                                                         (#%car (lref labels))]
                                                       [labels
                                                         (#%cdr (lref labels))])
                                                     (let
                                                        ([binding
                                                          (#%car (lref bindings))]
                                                        [bindings
                                                          (#%cdr (lref bindings))])
                                                      (call (lref extend-env) (lref labels)
                                                       (lref bindings)
                                                       (#%cons (#%cons (lref label)
                                                         (lref binding))
                                                        (lref r)))))
                                                    (const #<undefined>))))))])
                                          (fix
                                            ([build-sequence
                                            (lambda (src exps)
                                              (let
                                                 ([v
                                                   (lref exps)])
                                               (fix
                                                 ([fk (lambda ()
                                                   (fix
                                                     ([fk (lambda ()
                                                       (call (toplevel-ref error) (const value failed to match)
                                                        (lref v)))])
                                                    (if
                                                      (#%pair? (lref v))
                                                      (let
                                                         ([vx
                                                           (#%car (lref v))]
                                                         [vy
                                                           (#%cdr (lref v))])
                                                       (let
                                                          ([head
                                                            (lref vx)])
                                                        (let
                                                           ([tail
                                                             (lref vy)])
                                                         (call (toplevel-ref make-sequence) (lref src)
                                                          (lref head)
                                                          (call (lref build-sequence) (const false)
                                                           (lref tail))))))
                                                      (call (lref fk)))))])
                                                (if
                                                  (#%pair? (lref v))
                                                  (let
                                                     ([vx
                                                       (#%car (lref v))]
                                                     [vy
                                                       (#%cdr (lref v))])
                                                   (let
                                                      ([tail
                                                        (lref vx)])
                                                    (if
                                                      (#%null? (lref vy))
                                                      (lref tail)
                                                      (call (lref fk)))))
                                                  (call (lref fk))))))])
                                           (fix
                                             ([source-annotation
                                             (lambda (x)
                                               (if
                                                 (call (lref syntax?) (lref x))
                                                 (call (lref syntax-sourcev) (lref x))
                                                 (call (toplevel-ref datum-sourcev) (lref x))))])
                                            (fix
                                              ([build-lexical-reference
                                              (lambda (src name sym)
                                                (call (toplevel-ref make-lref) (lref src)
                                                 (lref name)
                                                 (lref sym)))])
                                             (fix
                                               ([wrap
                                               (lambda (x w defmod)
                                                 (call (lref source-wrap) (lref x)
                                                  (lref w)
                                                  (const false)
                                                  (lref defmod)))])
                                              (fix
                                                ([lambda-var-list
                                                (lambda (vars)
                                                  (fix
                                                    ([lvl
                                                    (lambda (vars ls w)
                                                      (let
                                                         ([cond-test-tmp
                                                           (#%pair? (lref vars))])
                                                       (if
                                                         (lref cond-test-tmp)
                                                         (call (lref lvl) (#%cdr (lref vars))
                                                          (#%cons (call (lref wrap) (#%car (lref vars))
                                                            (lref w)
                                                            (const false))
                                                           (lref ls))
                                                          (lref w))
                                                         (let
                                                             ([cond-test-tmp
                                                               (call (lref id?) (lref vars))])
                                                           (if
                                                             (lref cond-test-tmp)
                                                             (#%cons (call (lref wrap) (lref vars)
                                                               (lref w)
                                                               (const false))
                                                              (lref ls))
                                                             (let
                                                                 ([cond-test-tmp
                                                                   (#%null? (lref vars))])
                                                               (if
                                                                 (lref cond-test-tmp)
                                                                 (lref ls)
                                                                 (let
                                                                     ([cond-test-tmp
                                                                       (call (lref syntax?) (lref vars))])
                                                                   (if
                                                                     (lref cond-test-tmp)
                                                                     (call (lref lvl) (call (lref syntax-expression) (lref vars))
                                                                      (lref ls)
                                                                      (call (lref join-wraps) (lref w)
                                                                       (call (lref syntax-wrap) (lref vars))))
                                                                     (#%cons (lref vars)
                                                                       (lref ls)))))))))))])
                                                   (call (lref lvl) (lref vars)
                                                    (const '())
                                                    (lref empty-wrap))))])
                                               (let
                                                  ([transformer-environment
                                                    (call (toplevel-ref make-fluid) (lambda (k)
                                                      (call (toplevel-ref assertion-violation) (const transformer-environment)
                                                       (const called outside the dynamic extent of a syntax transformer))))])
                                                (let
                                                   ([the-anti-mark
                                                     (const false)])
                                                 (fix
                                                   ([new-mark
                                                   (lambda ()
                                                     (call (lref gen-unique)))])
                                                  (fix
                                                    ([anti-mark
                                                    (lambda (w)
                                                      (call (lref make-wrap) (#%cons (lref the-anti-mark)
                                                        (call (lref wrap-marks) (lref w)))
                                                       (#%cons (const shift)
                                                        (call (lref wrap-subst) (lref w)))))])
                                                   (fix
                                                     ([expand-macro
                                                     (lambda (p e r w s rib mod)
                                                       (fix
                                                         ([decorate-source
                                                         (lambda (x)
                                                           (call (lref source-wrap) (lref x)
                                                            (lref empty-wrap)
                                                            (lref s)
                                                            (const false)))])
                                                        (fix
                                                          ([map*
                                                          (lambda (f x)
                                                            (let
                                                               ([v
                                                                 (lref x)])
                                                             (fix
                                                               ([fk
                                                               (lambda ()
                                                                 (fix
                                                                   ([fk
                                                                   (lambda ()
                                                                     (seq
                                                                      (lambda ()
                                                                       (call (toplevel-ref error) (const value failed to match)
                                                                        (lref v)))
                                                                      (let
                                                                         ([x
                                                                           (lref v)])
                                                                       (call (lref f) (lref x)))))])
                                                                  (if
                                                                    (#%pair? (lref v))
                                                                    (let
                                                                       ([vx
                                                                         (#%car (lref v))]
                                                                       [vy
                                                                         (#%cdr (lref v))])
                                                                     (let
                                                                        ([x
                                                                          (lref vx)])
                                                                      (let
                                                                         ([x*
                                                                           (lref vy)])
                                                                       (#%cons (call (lref f) (lref x))
                                                                        (call (lref map*) (lref f)
                                                                         (lref x*))))))
                                                                    (call (lref fk)))))])
                                                              (if
                                                                (#%null? (lref v))
                                                                (const '())
                                                                (call (lref fk))))))])
                                                         (fix
                                                           ([rebuild-macro-output
                                                           (lambda (x m)
                                                             (let
                                                                ([cond-test-tmp
                                                                  (#%pair? (lref x))])
                                                              (if
                                                                (lref cond-test-tmp)
                                                                (call (lref decorate-source) (call (lref map*) (lambda (x)
                                                                   (call (lref rebuild-macro-output) (lref x)
                                                                    (lref m)))
                                                                  (lref x)))
                                                                (let
                                                                    ([cond-test-tmp
                                                                      (call (lref syntax?) (lref x))])
                                                                  (if
                                                                    (lref cond-test-tmp)
                                                                    (let
                                                                       ([w
                                                                         (call (lref syntax-wrap) (lref x))])
                                                                     (let
                                                                        ([ms
                                                                          (call (lref wrap-marks) (lref w))]
                                                                        [ss
                                                                          (call (lref wrap-subst) (lref w))])
                                                                      (if
                                                                        (let
                                                                            ([and-tmp
                                                                              (#%pair? (lref ms))])
                                                                          (if
                                                                            (lref and-tmp)
                                                                            (#%eq? (#%car (lref ms))
                                                                             (lref the-anti-mark))
                                                                            (const false)))
                                                                        (call (lref wrap-syntax) (lref x)
                                                                         (call (lref make-wrap) (#%cdr (lref ms))
                                                                          (if
                                                                            (lref rib)
                                                                            (#%cons (lref rib)
                                                                             (#%cdr (lref ss)))
                                                                            (#%cdr (lref ss))))
                                                                         (lref mod))
                                                                        (call (lref wrap-syntax) (lref x)
                                                                          (call (lref make-wrap) (#%cons (lref m)
                                                                            (lref ms))
                                                                           (if
                                                                             (lref rib)
                                                                             (#%cons (lref rib)
                                                                              (#%cons (const shift)
                                                                               (lref ss)))
                                                                             (#%cons (const shift)
                                                                               (lref ss))))
                                                                          (lref mod)))))
                                                                    (let
                                                                        ([cond-test-tmp
                                                                          (#%vector? (lref x))])
                                                                      (if
                                                                        (lref cond-test-tmp)
                                                                        (let
                                                                           ([n
                                                                             (#%vector-length (lref x))])
                                                                         (let
                                                                            ([v
                                                                              (#%make-vector (lref n))])
                                                                          (seq
                                                                           (fix
                                                                             ([loop
                                                                             (lambda (i)
                                                                               (if
                                                                                 (#%= (lref i)
                                                                                   (lref n))
                                                                                 (seq
                                                                                  (if
                                                                                    (const false)
                                                                                    (const false)
                                                                                    (const #<undefined>))
                                                                                  (lref v))
                                                                                 (seq
                                                                                   (#%vector-set! (lref v)
                                                                                    (lref i)
                                                                                    (call (lref rebuild-macro-output) (#%vector-ref (lref x)
                                                                                      (lref i))
                                                                                     (lref m)))
                                                                                   (call (lref loop) (#%+ (lref i)
                                                                                     (const 1))))))])
                                                                            (call (lref loop) (const 0)))
                                                                           (call (lref decorate-source) (lref v)))))
                                                                        (let
                                                                            ([cond-test-tmp
                                                                              (call (module-ref (capy)::symbol? #t) (lref x))])
                                                                          (if
                                                                            (lref cond-test-tmp)
                                                                            (call (toplevel-ref syntax-violation) (const false)
                                                                             (const encountered raw symbol in macro output)
                                                                             (call (lref source-wrap) (lref e)
                                                                              (lref w)
                                                                              (call (lref wrap-subst) (lref w))
                                                                              (lref mod))
                                                                             (lref x))
                                                                            (call (lref decorate-source) (lref x)))))))))))])
                                                          (let
                                                             ([old
                                                               (call (toplevel-ref fluid-ref) (lref transformer-environment))])
                                                           (call (toplevel-ref dynamic-wind) (lambda ()
                                                             (call (toplevel-ref fluid-set!) (lref transformer-environment)
                                                              (lambda (k)
                                                               (call (lref k) (lref e)
                                                                (lref r)
                                                                (lref w)
                                                                (lref s)
                                                                (lref rib)
                                                                (lref mod)))))
                                                            (lambda ()
                                                             (call (lref rebuild-macro-output) (call (lref p) (call (lref source-wrap) (lref e)
                                                                (call (lref anti-mark) (lref w))
                                                                (lref s)
                                                                (lref mod)))
                                                              (call (lref new-mark))))
                                                            (lambda ()
                                                             (call (toplevel-ref fluid-set!) (lref transformer-environment)
                                                              (lref old)))))))))])
                                                    (fix
                                                      ([same-marks?
                                                      (lambda (x y)
                                                        (let
                                                           ([or-tmp
                                                             (#%eq? (lref x)
                                                              (lref y))])
                                                         (if
                                                           (lref or-tmp)
                                                           (lref or-tmp)
                                                           (let
                                                               ([and-tmp
                                                                 (#%not (#%null? (lref x)))])
                                                             (if
                                                               (lref and-tmp)
                                                               (let
                                                                  ([and-tmp
                                                                    (#%not (#%null? (lref y)))])
                                                                (if
                                                                  (lref and-tmp)
                                                                  (let
                                                                     ([and-tmp
                                                                       (#%eq? (#%car (lref x))
                                                                        (#%car (lref y)))])
                                                                   (if
                                                                     (lref and-tmp)
                                                                     (call (lref same-marks?) (#%cdr (lref x))
                                                                      (#%cdr (lref y)))
                                                                     (const false)))
                                                                  (const false)))
                                                               (const false))))))])
                                                     (fix
                                                       ([id-var-name
                                                       (lambda (id w mod)
                                                         (fix
                                                           ([search
                                                           (lambda (sym subst marks)
                                                             (let
                                                                ([v
                                                                  (lref subst)])
                                                              (fix
                                                                ([fk
                                                                (lambda ()
                                                                  (fix
                                                                    ([fk
                                                                    (lambda ()
                                                                      (fix
                                                                        ([fk
                                                                        (lambda ()
                                                                          (call (toplevel-ref error) (const value failed to match)
                                                                           (lref v)))])
                                                                       (if
                                                                         (#%pair? (lref v))
                                                                         (let
                                                                            ([vx
                                                                              (#%car (lref v))]
                                                                            [vy
                                                                              (#%cdr (lref v))])
                                                                          (if
                                                                            (let
                                                                                ([and-tmp
                                                                                  (#%vector? (lref vx))])
                                                                              (if
                                                                                (lref and-tmp)
                                                                                (#%eq? (#%vector-length (lref vx))
                                                                                 (#%length (const ((quote ribcage) rsymnames rmarks rlabels))))
                                                                                (const false)))
                                                                            (if
                                                                              (#%eq? (#%vector-ref (lref vx)
                                                                                 (const 0))
                                                                                (const ribcage))
                                                                              (let
                                                                                 ([rsymnames
                                                                                   (#%vector-ref (lref vx)
                                                                                    (#%+ (const 1)
                                                                                     (const 0)))])
                                                                               (let
                                                                                  ([rmarks
                                                                                    (#%vector-ref (lref vx)
                                                                                     (#%+ (const 1)
                                                                                      (#%+ (const 1)
                                                                                       (const 0))))])
                                                                                (let
                                                                                   ([rlabels
                                                                                     (#%vector-ref (lref vx)
                                                                                      (#%+ (const 1)
                                                                                       (#%+ (const 1)
                                                                                        (#%+ (const 1)
                                                                                         (const 0)))))])
                                                                                 (let
                                                                                    ([subst
                                                                                      (lref vy)])
                                                                                  (fix
                                                                                    ([search-list-rib
                                                                                    (lambda ()
                                                                                      (fix
                                                                                        ([lp
                                                                                        (lambda (rsymnames rmarks rlabels)
                                                                                          (let
                                                                                             ([v
                                                                                               (lref rsymnames)])
                                                                                           (fix
                                                                                             ([fk
                                                                                             (lambda ()
                                                                                               (fix
                                                                                                 ([fk
                                                                                                 (lambda ()
                                                                                                   (call (toplevel-ref error) (const value failed to match)
                                                                                                    (lref v)))])
                                                                                                (if
                                                                                                  (#%pair? (lref v))
                                                                                                  (let
                                                                                                     ([vx
                                                                                                       (#%car (lref v))]
                                                                                                     [vy
                                                                                                       (#%cdr (lref v))])
                                                                                                   (let
                                                                                                      ([rsym
                                                                                                        (lref vx)])
                                                                                                    (let
                                                                                                       ([rsymnames
                                                                                                         (lref vy)])
                                                                                                     (let
                                                                                                        ([v
                                                                                                          (lref rmarks)])
                                                                                                      (fix
                                                                                                        ([fk
                                                                                                        (lambda ()
                                                                                                          (call (toplevel-ref error) (const value failed to match)
                                                                                                           (lref v)))])
                                                                                                       (if
                                                                                                         (#%pair? (lref v))
                                                                                                         (let
                                                                                                            ([vx
                                                                                                              (#%car (lref v))]
                                                                                                            [vy
                                                                                                              (#%cdr (lref v))])
                                                                                                          (let
                                                                                                             ([rmarks1
                                                                                                               (lref vx)])
                                                                                                           (let
                                                                                                              ([rmarks
                                                                                                                (lref vy)])
                                                                                                            (let
                                                                                                               ([v
                                                                                                                 (lref rlabels)])
                                                                                                             (fix
                                                                                                               ([fk
                                                                                                               (lambda ()
                                                                                                                 (call (toplevel-ref error) (const value failed to match)
                                                                                                                  (lref v)))])
                                                                                                              (if
                                                                                                                (#%pair? (lref v))
                                                                                                                (let
                                                                                                                   ([vx
                                                                                                                     (#%car (lref v))]
                                                                                                                   [vy
                                                                                                                     (#%cdr (lref v))])
                                                                                                                 (let
                                                                                                                    ([label
                                                                                                                      (lref vx)])
                                                                                                                  (let
                                                                                                                     ([rlabels
                                                                                                                       (lref vy)])
                                                                                                                   (if
                                                                                                                     (let
                                                                                                                         ([and-tmp
                                                                                                                           (#%eq? (lref sym)
                                                                                                                            (lref rsym))])
                                                                                                                       (if
                                                                                                                         (lref and-tmp)
                                                                                                                         (call (lref same-marks?) (lref marks)
                                                                                                                          (lref rmarks1))
                                                                                                                         (const false)))
                                                                                                                     (let
                                                                                                                        ([v
                                                                                                                          (lref label)])
                                                                                                                      (fix
                                                                                                                        ([fk
                                                                                                                        (lambda ()
                                                                                                                          (seq
                                                                                                                           (lambda ()
                                                                                                                            (call (toplevel-ref error) (const value failed to match)
                                                                                                                             (lref v)))
                                                                                                                           (lref label)))])
                                                                                                                       (if
                                                                                                                         (#%pair? (lref v))
                                                                                                                         (let
                                                                                                                            ([vx
                                                                                                                              (#%car (lref v))]
                                                                                                                            [vy
                                                                                                                              (#%cdr (lref v))])
                                                                                                                          (let
                                                                                                                             ([mod*
                                                                                                                               (lref vx)])
                                                                                                                           (let
                                                                                                                              ([label
                                                                                                                                (lref vy)])
                                                                                                                            (if
                                                                                                                              (#%equal? (lref mod*)
                                                                                                                                (lref mod))
                                                                                                                              (lref label)
                                                                                                                              (call (lref lp) (lref rsymnames)
                                                                                                                                (lref rmarks)
                                                                                                                                (lref rlabels))))))
                                                                                                                         (call (lref fk)))))
                                                                                                                     (call (lref lp) (lref rsymnames)
                                                                                                                       (lref rmarks)
                                                                                                                       (lref rlabels))))))
                                                                                                                (call (lref fk))))))))
                                                                                                         (call (lref fk))))))))
                                                                                                  (call (lref fk)))))])
                                                                                            (if
                                                                                              (#%null? (lref v))
                                                                                              (call (lref search) (lref sym)
                                                                                               (lref subst)
                                                                                               (lref marks))
                                                                                              (call (lref fk))))))])
                                                                                       (call (lref lp) (lref rsymnames)
                                                                                        (lref rmarks)
                                                                                        (lref rlabels))))])
                                                                                   (fix
                                                                                     ([search-vector-rib
                                                                                     (lambda ()
                                                                                       (let
                                                                                          ([n
                                                                                            (#%vector-length (lref rsymnames))])
                                                                                        (fix
                                                                                          ([lp
                                                                                          (lambda (i)
                                                                                            (let
                                                                                               ([cond-test-tmp
                                                                                                 (#%= (lref i)
                                                                                                  (lref n))])
                                                                                             (if
                                                                                               (lref cond-test-tmp)
                                                                                               (call (lref search) (lref sym)
                                                                                                (lref subst)
                                                                                                (lref marks))
                                                                                               (let
                                                                                                   ([cond-test-tmp
                                                                                                     (let
                                                                                                        ([and-tmp
                                                                                                          (#%eq? (#%vector-ref (lref rsymnames)
                                                                                                            (lref i))
                                                                                                           (lref sym))])
                                                                                                      (if
                                                                                                        (lref and-tmp)
                                                                                                        (call (lref same-marks?) (lref marks)
                                                                                                         (#%vector-ref (lref rmarks)
                                                                                                          (lref i)))
                                                                                                        (const false)))])
                                                                                                 (if
                                                                                                   (lref cond-test-tmp)
                                                                                                   (let
                                                                                                      ([v
                                                                                                        (#%vector-ref (lref rlabels)
                                                                                                         (lref i))])
                                                                                                    (fix
                                                                                                      ([fk
                                                                                                      (lambda ()
                                                                                                        (seq
                                                                                                         (lambda ()
                                                                                                          (call (toplevel-ref error) (const value failed to match)
                                                                                                           (lref v)))
                                                                                                         (let
                                                                                                            ([label
                                                                                                              (lref v)])
                                                                                                          (lref label))))])
                                                                                                     (if
                                                                                                       (#%pair? (lref v))
                                                                                                       (let
                                                                                                          ([vx
                                                                                                            (#%car (lref v))]
                                                                                                          [vy
                                                                                                            (#%cdr (lref v))])
                                                                                                        (let
                                                                                                           ([mod*
                                                                                                             (lref vx)])
                                                                                                         (let
                                                                                                            ([label
                                                                                                              (lref vy)])
                                                                                                          (if
                                                                                                            (#%equal? (lref mod*)
                                                                                                              (lref mod))
                                                                                                            (lref label)
                                                                                                            (call (lref lp) (#%+ (const 1)
                                                                                                               (lref i)))))))
                                                                                                       (call (lref fk)))))
                                                                                                   (call (lref lp) (#%+ (const 1)
                                                                                                      (lref i))))))))])
                                                                                         (call (lref lp) (const 0)))))])
                                                                                    (if
                                                                                      (#%vector? (lref rsymnames))
                                                                                      (call (lref search-vector-rib))
                                                                                      (call (lref search-list-rib)))))))))
                                                                              (call (lref fk)))
                                                                            (call (lref fk))))
                                                                         (call (lref fk)))))])
                                                                   (if
                                                                     (#%pair? (lref v))
                                                                     (let
                                                                        ([vx
                                                                          (#%car (lref v))]
                                                                        [vy
                                                                          (#%cdr (lref v))])
                                                                      (if
                                                                        (#%eq? (lref vx)
                                                                          (const shift))
                                                                        (let
                                                                           ([subst
                                                                             (lref vy)])
                                                                         (let
                                                                            ([v
                                                                              (lref marks)])
                                                                          (fix
                                                                            ([fk
                                                                            (lambda ()
                                                                              (call (toplevel-ref error) (const value failed to match)
                                                                               (lref v)))])
                                                                           (if
                                                                             (#%pair? (lref v))
                                                                             (let
                                                                                ([vx
                                                                                  (#%car (lref v))]
                                                                                [vy
                                                                                  (#%cdr (lref v))])
                                                                              (let
                                                                                 ([marks
                                                                                   (lref vy)])
                                                                               (call (lref search) (lref sym)
                                                                                (lref subst)
                                                                                (lref marks))))
                                                                             (call (lref fk))))))
                                                                        (call (lref fk))))
                                                                     (call (lref fk)))))])
                                                               (if
                                                                 (#%null? (lref v))
                                                                 (const false)
                                                                 (call (lref fk))))))])
                                                          (let
                                                             ([cond-test-tmp
                                                               (call (module-ref (capy)::symbol? #t) (lref id))])
                                                           (if
                                                             (lref cond-test-tmp)
                                                             (let
                                                                ([or-tmp
                                                                  (call (lref search) (lref id)
                                                                   (call (lref wrap-subst) (lref w))
                                                                   (call (lref wrap-marks) (lref w)))])
                                                              (if
                                                                (lref or-tmp)
                                                                (lref or-tmp)
                                                                (lref id)))
                                                             (let
                                                                 ([cond-test-tmp
                                                                   (call (lref syntax?) (lref id))])
                                                               (if
                                                                 (lref cond-test-tmp)
                                                                 (let
                                                                    ([id
                                                                      (call (lref syntax-expression) (lref id))]
                                                                    [w1
                                                                      (call (lref syntax-wrap) (lref id))]
                                                                    [mod
                                                                      (let
                                                                         ([or-tmp
                                                                           (call (lref syntax-module) (lref id))])
                                                                       (if
                                                                         (lref or-tmp)
                                                                         (lref or-tmp)
                                                                         (lref mod)))])
                                                                  (let
                                                                     ([marks
                                                                       (call (lref join-marks) (call (lref wrap-marks) (lref w))
                                                                        (call (lref wrap-marks) (lref w1)))])
                                                                   (let
                                                                      ([or-tmp
                                                                        (call (lref search) (lref id)
                                                                         (call (lref wrap-subst) (lref w))
                                                                         (lref marks))])
                                                                    (if
                                                                      (lref or-tmp)
                                                                      (lref or-tmp)
                                                                      (let
                                                                          ([or-tmp
                                                                            (call (lref search) (lref id)
                                                                             (call (lref wrap-subst) (lref w1))
                                                                             (lref marks))])
                                                                        (if
                                                                          (lref or-tmp)
                                                                          (lref or-tmp)
                                                                          (lref id)))))))
                                                                 (call (toplevel-ref syntax-violation) (const id-var-name)
                                                                   (const invalid id)
                                                                   (lref id))))))))])
                                                      (fix
                                                        ([binding-value
                                                        (lambda (x)
                                                          (#%cdr (lref x)))])
                                                       (fix
                                                         ([binding-type
                                                         (lambda (x)
                                                           (#%car (lref x)))])
                                                        (fix
                                                          ([resolve-identifier
                                                          (lambda (id w r mod resolve-syntax-parameters?)
                                                            (fix
                                                              ([resolve-global
                                                              (lambda (var mod)
                                                                (let
                                                                   ([v
                                                                     (let
                                                                        ([and-tmp
                                                                          (#%not (#%equal? (lref mod)
                                                                            (const (primitive))))])
                                                                      (if
                                                                        (lref and-tmp)
                                                                        (call (toplevel-ref module-variable) (if
                                                                           (lref mod)
                                                                           (call (toplevel-ref resolve-module) (#%cdr (lref mod))
                                                                            (const true)
                                                                            (const true))
                                                                           (#%current-module))
                                                                         (lref var))
                                                                        (const false)))])
                                                                 (if
                                                                   (let
                                                                       ([and-tmp
                                                                         (lref v)])
                                                                     (if
                                                                       (lref and-tmp)
                                                                       (let
                                                                          ([and-tmp
                                                                            (call (module-ref (capy)::variable-bound? #t) (lref v))])
                                                                        (if
                                                                          (lref and-tmp)
                                                                          (call (toplevel-ref macro?) (call (module-ref (capy)::variable-ref #t) (lref v)))
                                                                          (const false)))
                                                                       (const false)))
                                                                   (let
                                                                      ([m
                                                                        (call (module-ref (capy)::variable-ref #t) (lref v))])
                                                                    (let
                                                                       ([type
                                                                         (call (toplevel-ref macro-type) (lref m))])
                                                                     (let
                                                                        ([trans
                                                                          (call (toplevel-ref macro-binding) (lref m))])
                                                                      (if
                                                                        (#%eq? (lref type)
                                                                          (const syntax-parameter))
                                                                        (if
                                                                          (lref resolve-syntax-parameters?)
                                                                          (let
                                                                             ([lexical
                                                                               (call (toplevel-ref assq-ref) (lref r)
                                                                                (lref v))])
                                                                           (values (const macro) (if
                                                                             (lref lexical)
                                                                             (call (lref binding-value) (lref lexical))
                                                                             (lref trans)) (lref mod)))
                                                                          (values (lref type) (lref v) (lref mod)))
                                                                        (values (lref type) (lref trans) (lref mod))))))
                                                                   (values (const global) (lref var) (lref mod)))))])
                                                             (fix
                                                               ([resolve-lexical
                                                               (lambda (label mod)
                                                                 (let
                                                                    ([b
                                                                      (call (toplevel-ref assq-ref) (lref r)
                                                                       (lref label))])
                                                                  (if
                                                                    (lref b)
                                                                    (let
                                                                       ([type
                                                                         (call (lref binding-type) (lref b))]
                                                                       [value
                                                                         (call (lref binding-value) (lref b))])
                                                                     (if
                                                                       (#%eq? (lref type)
                                                                         (const syntax-parameter))
                                                                       (if
                                                                         (lref resolve-syntax-parameters?)
                                                                         (values (const macro) (lref value) (lref mod))
                                                                         (values (lref type) (lref label) (lref mod)))
                                                                       (values (lref type) (lref value) (lref mod))))
                                                                    (values (const displaced-lexical) (const false) (const false)))))])
                                                              (let
                                                                 ([n
                                                                   (call (lref id-var-name) (lref id)
                                                                    (lref w)
                                                                    (lref mod))])
                                                               (let
                                                                  ([cond-test-tmp
                                                                    (call (lref syntax?) (lref n))])
                                                                (if
                                                                  (lref cond-test-tmp)
                                                                  (let
                                                                     ([cond-test-tmp
                                                                       (#%not (#%eq? (lref n)
                                                                         (lref id)))])
                                                                   (if
                                                                     (lref cond-test-tmp)
                                                                     (call (lref resolve-identifier) (lref n)
                                                                      (lref w)
                                                                      (lref r)
                                                                      (lref mod)
                                                                      (lref resolve-syntax-parameters?))
                                                                     (call (lref resolve-identifier) (call (lref syntax-expression) (lref n))
                                                                       (call (lref syntax-wrap) (lref n))
                                                                       (lref r)
                                                                       (let
                                                                          ([or-tmp
                                                                            (call (lref syntax-module) (lref n))])
                                                                        (if
                                                                          (lref or-tmp)
                                                                          (lref or-tmp)
                                                                          (lref mod)))
                                                                       (lref resolve-syntax-parameters?))))
                                                                  (let
                                                                      ([cond-test-tmp
                                                                        (call (module-ref (capy)::symbol? #t) (lref n))])
                                                                    (if
                                                                      (lref cond-test-tmp)
                                                                      (call (lref resolve-global) (lref n)
                                                                       (let
                                                                          ([or-tmp
                                                                            (let
                                                                               ([and-tmp
                                                                                 (call (lref syntax?) (lref id))])
                                                                             (if
                                                                               (lref and-tmp)
                                                                               (call (lref syntax-module) (lref id))
                                                                               (const false)))])
                                                                        (if
                                                                          (lref or-tmp)
                                                                          (lref or-tmp)
                                                                          (lref mod))))
                                                                      (call (lref resolve-lexical) (lref n)
                                                                        (let
                                                                           ([or-tmp
                                                                             (let
                                                                                ([and-tmp
                                                                                  (call (lref syntax?) (lref id))])
                                                                              (if
                                                                                (lref and-tmp)
                                                                                (call (lref syntax-module) (lref id))
                                                                                (const false)))])
                                                                         (if
                                                                           (lref or-tmp)
                                                                           (lref or-tmp)
                                                                           (lref mod))))))))))))])
                                                         (fix
                                                           ([syntax-type
                                                           (lambda (e r w s rib mod for-car?)
                                                             (let
                                                                ([cond-test-tmp
                                                                  (call (module-ref (capy)::symbol? #t) (lref e))])
                                                              (if
                                                                (lref cond-test-tmp)
                                                                (receive (type value mod*)
                                                                  (call (lref resolve-identifier) (lref e)
                                                                    (lref w)
                                                                    (lref r)
                                                                    (lref mod)
                                                                    (const true))
                                                                  (let
                                                                      ([cond-test-tmp
                                                                        (#%eq? (lref type)
                                                                         (const macro))])
                                                                    (if
                                                                      (lref cond-test-tmp)
                                                                      (if
                                                                        (lref for-car?)
                                                                        (values (lref type) (lref value) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                        (call (lref syntax-type) (call (lref expand-macro) (lref value)
                                                                           (lref e)
                                                                           (lref r)
                                                                           (lref w)
                                                                           (lref s)
                                                                           (lref rib)
                                                                           (lref mod))
                                                                          (lref r)
                                                                          (lref empty-wrap)
                                                                          (lref s)
                                                                          (lref rib)
                                                                          (lref mod)
                                                                          (const false)))
                                                                      (let
                                                                          ([cond-test-tmp
                                                                            (#%eq? (lref type)
                                                                             (const global))])
                                                                        (if
                                                                          (lref cond-test-tmp)
                                                                          (values (lref type) (lref value) (lref e) (lref value) (lref w) (lref s) (lref mod*))
                                                                          (values (lref type) (lref value) (lref e) (lref e) (lref w) (lref s) (lref mod*)))))))
                                                                (let
                                                                    ([cond-test-tmp
                                                                      (#%pair? (lref e))])
                                                                  (if
                                                                    (lref cond-test-tmp)
                                                                    (let
                                                                       ([first
                                                                         (#%car (lref e))])
                                                                     (receive (ftype fval fform fe fw fs fmod)
                                                                       (call (lref syntax-type) (lref first)
                                                                         (lref r)
                                                                         (lref w)
                                                                         (lref s)
                                                                         (lref rib)
                                                                         (lref mod)
                                                                         (const true))
                                                                       (let
                                                                           ([cond-test-tmp
                                                                             (#%eq? (lref ftype)
                                                                              (const lexical))])
                                                                         (if
                                                                           (lref cond-test-tmp)
                                                                           (values (const lexical-call) (lref fval) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                           (let
                                                                               ([cond-test-tmp
                                                                                 (#%eq? (lref ftype)
                                                                                  (const global))])
                                                                             (if
                                                                               (lref cond-test-tmp)
                                                                               (if
                                                                                 (#%equal? (lref fmod)
                                                                                   (const (primitive)))
                                                                                 (values (const primitive-call) (lref fval) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                                 (values (const global-call) (call (lref make-syntax) (lref fval)
                                                                                   (lref w)
                                                                                   (lref fmod)
                                                                                   (lref fs)) (lref e) (lref e) (lref w) (lref s) (lref mod)))
                                                                               (let
                                                                                   ([cond-test-tmp
                                                                                     (#%eq? (lref ftype)
                                                                                      (const macro))])
                                                                                 (if
                                                                                   (lref cond-test-tmp)
                                                                                   (call (lref syntax-type) (call (lref expand-macro) (lref fval)
                                                                                     (lref e)
                                                                                     (lref r)
                                                                                     (lref w)
                                                                                     (lref s)
                                                                                     (lref rib)
                                                                                     (lref mod))
                                                                                    (lref r)
                                                                                    (lref empty-wrap)
                                                                                    (lref s)
                                                                                    (lref rib)
                                                                                    (lref mod)
                                                                                    (lref for-car?))
                                                                                   (let
                                                                                       ([cond-test-tmp
                                                                                         (#%eq? (lref ftype)
                                                                                          (const module-ref))])
                                                                                     (if
                                                                                       (lref cond-test-tmp)
                                                                                       (receive (e r w s mod)
                                                                                         (call (lref fval) (lref e)
                                                                                           (lref r)
                                                                                           (lref w)
                                                                                           (lref mod))
                                                                                         (call (lref syntax-type) (lref e)
                                                                                           (lref r)
                                                                                           (lref w)
                                                                                           (lref s)
                                                                                           (lref rib)
                                                                                           (lref mod)
                                                                                           (lref for-car?)))
                                                                                       (let
                                                                                           ([cond-test-tmp
                                                                                             (#%eq? (lref ftype)
                                                                                              (const core))])
                                                                                         (if
                                                                                           (lref cond-test-tmp)
                                                                                           (values (const core-form) (lref fval) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                                           (let
                                                                                               ([cond-test-tmp
                                                                                                 (#%eq? (lref ftype)
                                                                                                  (const local-syntax))])
                                                                                             (if
                                                                                               (lref cond-test-tmp)
                                                                                               (values (const local-syntax-form) (lref fval) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                                               (let
                                                                                                   ([cond-test-tmp
                                                                                                     (#%eq? (lref ftype)
                                                                                                      (const begin))])
                                                                                                 (if
                                                                                                   (lref cond-test-tmp)
                                                                                                   (values (const begin-form) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                                                   (let
                                                                                                       ([cond-test-tmp
                                                                                                         (#%eq? (lref ftype)
                                                                                                          (const eval-when))])
                                                                                                     (if
                                                                                                       (lref cond-test-tmp)
                                                                                                       (values (const eval-when-form) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                                                       (let
                                                                                                           ([cond-test-tmp
                                                                                                             (#%eq? (lref ftype)
                                                                                                              (const define-syntax))])
                                                                                                         (if
                                                                                                           (lref cond-test-tmp)
                                                                                                           (let
                                                                                                              ([tmp-1
                                                                                                                (lref e)])
                                                                                                            (let
                                                                                                               ([tmp
                                                                                                                 (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                  (const (_ any any)))])
                                                                                                             (if
                                                                                                               (let
                                                                                                                   ([and-tmp
                                                                                                                     (lref tmp)])
                                                                                                                 (if
                                                                                                                   (lref and-tmp)
                                                                                                                   (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                     (call (lref id?) (lref name)))
                                                                                                                    (lref tmp))
                                                                                                                   (const false)))
                                                                                                               (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                 (values (const define-syntax-form) (lref name) (lref e) (lref val) (lref w) (lref s) (lref mod)))
                                                                                                                (lref tmp))
                                                                                                               (call (toplevel-ref syntax-violation) (const false)
                                                                                                                 (const source expression failed to match any pattern)
                                                                                                                 (lref tmp-1)))))
                                                                                                           (let
                                                                                                               ([cond-test-tmp
                                                                                                                 (#%eq? (lref ftype)
                                                                                                                  (const define-syntax-parameter))])
                                                                                                             (if
                                                                                                               (lref cond-test-tmp)
                                                                                                               (let
                                                                                                                  ([tmp-1
                                                                                                                    (lref e)])
                                                                                                                (let
                                                                                                                   ([tmp
                                                                                                                     (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                      (const (_ any any)))])
                                                                                                                 (if
                                                                                                                   (let
                                                                                                                       ([and-tmp
                                                                                                                         (lref tmp)])
                                                                                                                     (if
                                                                                                                       (lref and-tmp)
                                                                                                                       (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                         (call (lref id?) (lref name)))
                                                                                                                        (lref tmp))
                                                                                                                       (const false)))
                                                                                                                   (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                     (values (const define-syntax-parameter-form) (lref name) (lref e) (lref val) (lref w) (lref s) (lref mod)))
                                                                                                                    (lref tmp))
                                                                                                                   (call (toplevel-ref syntax-violation) (const false)
                                                                                                                     (const source expression failed to match any pattern)
                                                                                                                     (lref tmp-1)))))
                                                                                                               (let
                                                                                                                   ([cond-test-tmp
                                                                                                                     (#%eq? (lref ftype)
                                                                                                                      (const define))])
                                                                                                                 (if
                                                                                                                   (lref cond-test-tmp)
                                                                                                                   (let
                                                                                                                      ([tmp
                                                                                                                        (lref e)])
                                                                                                                    (let
                                                                                                                       ([tmp-1
                                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                          (const (_ any any)))])
                                                                                                                     (if
                                                                                                                       (let
                                                                                                                           ([and-tmp
                                                                                                                             (lref tmp-1)])
                                                                                                                         (if
                                                                                                                           (lref and-tmp)
                                                                                                                           (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                             (call (lref id?) (lref name)))
                                                                                                                            (lref tmp-1))
                                                                                                                           (const false)))
                                                                                                                       (call (module-ref (capy)::apply #t) (lambda (name val)
                                                                                                                         (values (const define-form) (lref name) (lref e) (lref val) (lref w) (lref s) (lref mod)))
                                                                                                                        (lref tmp-1))
                                                                                                                       (let
                                                                                                                           ([tmp-1
                                                                                                                             (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                              (const (_ (any . any) any . each-any)))])
                                                                                                                         (if
                                                                                                                           (let
                                                                                                                               ([and-tmp
                                                                                                                                 (lref tmp-1)])
                                                                                                                             (if
                                                                                                                               (lref and-tmp)
                                                                                                                               (call (module-ref (capy)::apply #t) (lambda (name args e1 e2)
                                                                                                                                 (let
                                                                                                                                    ([and-tmp
                                                                                                                                      (call (lref id?) (lref name))])
                                                                                                                                  (if
                                                                                                                                    (lref and-tmp)
                                                                                                                                    (call (lref valid-bound-ids?) (call (lref lambda-var-list) (lref args)))
                                                                                                                                    (const false))))
                                                                                                                                (lref tmp-1))
                                                                                                                               (const false)))
                                                                                                                           (call (module-ref (capy)::apply #t) (lambda (name args e1 e2)
                                                                                                                             (values (const define-form) (call (lref wrap) (lref name)
                                                                                                                              (lref w)
                                                                                                                              (lref mod)) (call (lref wrap) (lref e)
                                                                                                                              (lref w)
                                                                                                                              (lref mod)) (call (lref source-wrap) (#%cons (call (lref make-syntax) (const lambda)
                                                                                                                                (const ((top)))
                                                                                                                                (const (hygiene capy)))
                                                                                                                               (call (lref wrap) (#%cons (lref args)
                                                                                                                                 (#%cons (lref e1)
                                                                                                                                  (lref e2)))
                                                                                                                                (lref w)
                                                                                                                                (lref mod)))
                                                                                                                              (lref empty-wrap)
                                                                                                                              (lref s)
                                                                                                                              (const false)) (lref empty-wrap) (lref s) (lref mod)))
                                                                                                                            (lref tmp-1))
                                                                                                                           (let
                                                                                                                               ([tmp-1
                                                                                                                                 (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                  (const (_ any)))])
                                                                                                                             (if
                                                                                                                               (let
                                                                                                                                   ([and-tmp
                                                                                                                                     (lref tmp-1)])
                                                                                                                                 (if
                                                                                                                                   (lref and-tmp)
                                                                                                                                   (call (module-ref (capy)::apply #t) (lambda (name)
                                                                                                                                     (call (lref id?) (lref name)))
                                                                                                                                    (lref tmp-1))
                                                                                                                                   (const false)))
                                                                                                                               (call (module-ref (capy)::apply #t) (lambda (name)
                                                                                                                                 (values (const define-form) (call (lref wrap) (lref name)
                                                                                                                                  (lref w)
                                                                                                                                  (lref mod)) (call (lref wrap) (lref e)
                                                                                                                                  (lref w)
                                                                                                                                  (lref mod)) (#%cons (call (lref make-syntax) (const if)
                                                                                                                                   (const ((top)))
                                                                                                                                   (const (hygiene capy)))
                                                                                                                                  (#%cons (const false)
                                                                                                                                   (#%cons (const false)
                                                                                                                                    (const '())))) (lref empty-wrap) (lref s) (lref mod)))
                                                                                                                                (lref tmp-1))
                                                                                                                               (call (toplevel-ref syntax-violation) (const false)
                                                                                                                                 (const source expression failed to match any pattern for 'define')
                                                                                                                                 (call (lref strip) (lref tmp))))))))))
                                                                                                                   (values (const call) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod))))))))))))))))))))))))))
                                                                    (let
                                                                        ([cond-test-tmp
                                                                          (call (lref syntax?) (lref e))])
                                                                      (if
                                                                        (lref cond-test-tmp)
                                                                        (call (lref syntax-type) (call (lref syntax-expression) (lref e))
                                                                         (lref r)
                                                                         (call (lref join-wraps) (lref w)
                                                                          (call (lref syntax-wrap) (lref e)))
                                                                         (let
                                                                            ([or-tmp
                                                                              (call (lref source-annotation) (lref e))])
                                                                          (if
                                                                            (lref or-tmp)
                                                                            (lref or-tmp)
                                                                            (lref s)))
                                                                         (lref rib)
                                                                         (let
                                                                            ([or-tmp
                                                                              (call (lref syntax-module) (lref e))])
                                                                          (if
                                                                            (lref or-tmp)
                                                                            (lref or-tmp)
                                                                            (lref mod)))
                                                                         (lref for-car?))
                                                                        (let
                                                                            ([cond-test-tmp
                                                                              (call (toplevel-ref self-evaluating?) (lref e))])
                                                                          (if
                                                                            (lref cond-test-tmp)
                                                                            (values (const constant) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod))
                                                                            (values (const other) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod)))))))))))])
                                                          (fix
                                                            ([expand-call
                                                            (lambda (x e r w s mod)
                                                              (let
                                                                 ([tmp-1
                                                                   (lref e)])
                                                               (let
                                                                  ([tmp
                                                                    (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                     (const (any . each-any)))])
                                                                (if
                                                                  (lref tmp)
                                                                  (call (module-ref (capy)::apply #t) (lambda (e0 e1)
                                                                    (call (lref build-call) (lref s)
                                                                     (lref x)
                                                                     (call (toplevel-ref map) (lambda (e)
                                                                       (call (lref expand) (lref e)
                                                                        (lref r)
                                                                        (lref w)
                                                                        (lref mod)))
                                                                      (lref e1))))
                                                                   (lref tmp))
                                                                  (call (toplevel-ref syntax-violation) (const false)
                                                                    (const source expression failed to match any pattern)
                                                                    (lref tmp-1))))))]
                                                            [expand-local-syntax
                                                            (lambda (rec? e r w s mod k)
                                                              (let
                                                                 ([tmp
                                                                   (lref e)])
                                                               (let
                                                                  ([tmp
                                                                    (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                     (const (_ #(each (any any)) any . each-any)))])
                                                                (if
                                                                  (lref tmp)
                                                                  (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                    (let
                                                                       ([ids
                                                                         (lref id)])
                                                                     (if
                                                                       (#%not (call (lref valid-bound-ids?) (lref ids)))
                                                                       (call (toplevel-ref syntax-violation) (const false)
                                                                        (const duplicate bound keyword)
                                                                        (lref e))
                                                                       (let
                                                                           ([labels
                                                                             (call (lref gen-labels) (lref ids))])
                                                                         (let
                                                                            ([new-w
                                                                              (call (lref make-binding-wrap) (lref ids)
                                                                               (lref labels)
                                                                               (lref w))])
                                                                          (call (lref k) (#%cons (lref e1)
                                                                            (lref e2))
                                                                           (call (lref extend-env) (lref labels)
                                                                            (let
                                                                               ([w
                                                                                 (if
                                                                                   (lref rec?)
                                                                                   (lref new-w)
                                                                                   (lref w))]
                                                                               [trans-r
                                                                                 (call (lref macros-only-env) (lref r))])
                                                                             (call (toplevel-ref map) (lambda (x)
                                                                               (#%cons (const macro)
                                                                                (call (lref eval-local-transformer) (call (lref expand) (lref x)
                                                                                  (lref trans-r)
                                                                                  (lref w)
                                                                                  (lref mod))
                                                                                 (lref mod))))
                                                                              (lref val)))
                                                                            (lref r))
                                                                           (lref new-w)
                                                                           (lref s)
                                                                           (lref mod)))))))
                                                                   (lref tmp))
                                                                  (call (toplevel-ref syntax-violation) (const false)
                                                                    (const bad local syntax definition)
                                                                    (call (lref source-wrap) (lref e)
                                                                     (lref w)
                                                                     (lref s)
                                                                     (lref mod)))))))]
                                                            [expand-sequence
                                                            (lambda (body r w s mod)
                                                              (call (lref build-sequence) (lref s)
                                                               (fix
                                                                 ([lp
                                                                 (lambda (body)
                                                                   (if
                                                                     (#%null? (lref body))
                                                                     (const '())
                                                                     (let
                                                                         ([head
                                                                           (#%car (lref body))]
                                                                         [tail
                                                                           (#%cdr (lref body))])
                                                                       (let
                                                                          ([expr
                                                                            (call (lref expand) (lref head)
                                                                             (lref r)
                                                                             (lref w)
                                                                             (lref mod))])
                                                                        (#%cons (lref expr)
                                                                         (call (lref lp) (lref tail)))))))])
                                                                (call (lref lp) (lref body)))))]
                                                            [expand-expr
                                                            (lambda (type value form e r w s mod)
                                                              (let
                                                                 ([cond-test-tmp
                                                                   (#%eq? (lref type)
                                                                    (const lexical))])
                                                               (if
                                                                 (lref cond-test-tmp)
                                                                 (call (lref build-lexical-reference) (lref s)
                                                                  (lref e)
                                                                  (lref value))
                                                                 (let
                                                                     ([cond-test-tmp
                                                                       (let
                                                                          ([or-tmp
                                                                            (#%eq? (lref type)
                                                                             (const core))])
                                                                        (if
                                                                          (lref or-tmp)
                                                                          (lref or-tmp)
                                                                          (#%eq? (lref type)
                                                                            (const core-form))))])
                                                                   (if
                                                                     (lref cond-test-tmp)
                                                                     (call (lref value) (lref e)
                                                                      (lref r)
                                                                      (lref w)
                                                                      (lref s)
                                                                      (lref mod))
                                                                     (let
                                                                         ([cond-test-tmp
                                                                           (#%eq? (lref type)
                                                                            (const module-ref))])
                                                                       (if
                                                                         (lref cond-test-tmp)
                                                                         (receive (e r w s mod)
                                                                           (call (lref value) (lref e)
                                                                             (lref r)
                                                                             (lref w)
                                                                             (lref mod))
                                                                           (call (lref expand) (lref e)
                                                                             (lref r)
                                                                             (lref w)
                                                                             (lref mod)))
                                                                         (let
                                                                             ([cond-test-tmp
                                                                               (#%eq? (lref type)
                                                                                (const lexical-call))])
                                                                           (if
                                                                             (lref cond-test-tmp)
                                                                             (call (lref expand-call) (let
                                                                                 ([id
                                                                                   (#%car (lref e))])
                                                                               (call (lref build-lexical-reference) (call (lref source-annotation) (lref id))
                                                                                (if
                                                                                  (call (lref syntax?) (lref id))
                                                                                  (call (toplevel-ref syntax->datum) (lref id))
                                                                                  (lref id))
                                                                                (lref value)))
                                                                              (lref e)
                                                                              (lref r)
                                                                              (lref w)
                                                                              (lref s)
                                                                              (lref mod))
                                                                             (let
                                                                                 ([cond-test-tmp
                                                                                   (#%eq? (lref type)
                                                                                    (const global-call))])
                                                                               (if
                                                                                 (lref cond-test-tmp)
                                                                                 (call (lref expand-call) (call (lref build-global-reference) (let
                                                                                      ([or-tmp
                                                                                        (call (lref source-annotation) (#%car (lref e)))])
                                                                                    (if
                                                                                      (lref or-tmp)
                                                                                      (lref or-tmp)
                                                                                      (lref s)))
                                                                                   (if
                                                                                     (call (lref syntax?) (lref value))
                                                                                     (call (lref syntax-expression) (lref value))
                                                                                     (lref value))
                                                                                   (let
                                                                                      ([or-tmp
                                                                                        (let
                                                                                           ([and-tmp
                                                                                             (call (lref syntax?) (lref value))])
                                                                                         (if
                                                                                           (lref and-tmp)
                                                                                           (call (lref syntax-module) (lref value))
                                                                                           (const false)))])
                                                                                    (if
                                                                                      (lref or-tmp)
                                                                                      (lref or-tmp)
                                                                                      (lref mod))))
                                                                                  (lref e)
                                                                                  (lref r)
                                                                                  (lref w)
                                                                                  (lref s)
                                                                                  (lref mod))
                                                                                 (let
                                                                                     ([cond-test-tmp
                                                                                       (#%eq? (lref type)
                                                                                        (const primitive-call))])
                                                                                   (if
                                                                                     (lref cond-test-tmp)
                                                                                     (call (lref build-primcall) (lref s)
                                                                                      (lref value)
                                                                                      (call (toplevel-ref map) (lambda (e)
                                                                                        (call (lref expand) (lref e)
                                                                                         (lref r)
                                                                                         (lref w)
                                                                                         (lref mod)))
                                                                                       (#%cdr (lref e))))
                                                                                     (let
                                                                                         ([cond-test-tmp
                                                                                           (#%eq? (lref type)
                                                                                            (const global))])
                                                                                       (if
                                                                                         (lref cond-test-tmp)
                                                                                         (call (lref build-global-reference) (lref s)
                                                                                          (lref value)
                                                                                          (lref mod))
                                                                                         (let
                                                                                             ([cond-test-tmp
                                                                                               (#%eq? (lref type)
                                                                                                (const constant))])
                                                                                           (if
                                                                                             (lref cond-test-tmp)
                                                                                             (call (toplevel-ref make-constant) (lref s)
                                                                                              (call (lref strip) (lref e)))
                                                                                             (let
                                                                                                 ([cond-test-tmp
                                                                                                   (#%eq? (lref type)
                                                                                                    (const call))])
                                                                                               (if
                                                                                                 (lref cond-test-tmp)
                                                                                                 (call (lref expand-call) (call (lref expand) (#%car (lref e))
                                                                                                   (lref r)
                                                                                                   (lref w)
                                                                                                   (lref mod))
                                                                                                  (lref e)
                                                                                                  (lref r)
                                                                                                  (lref w)
                                                                                                  (lref s)
                                                                                                  (lref mod))
                                                                                                 (let
                                                                                                     ([cond-test-tmp
                                                                                                       (#%eq? (lref type)
                                                                                                        (const eval-when-form))])
                                                                                                   (if
                                                                                                     (lref cond-test-tmp)
                                                                                                     (let
                                                                                                        ([tmp-1
                                                                                                          (lref e)])
                                                                                                      (let
                                                                                                         ([tmp
                                                                                                           (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                            (const (_ each-any any . each-any)))])
                                                                                                       (if
                                                                                                         (lref tmp)
                                                                                                         (call (module-ref (capy)::apply #t) (lambda (x e1 e2)
                                                                                                           (let
                                                                                                              ([when-list
                                                                                                                (call (lref parse-when-list) (lref e)
                                                                                                                 (lref x))])
                                                                                                            (if
                                                                                                              (call (module-ref (capy)::memq #t) (const eval)
                                                                                                                (lref when-list))
                                                                                                              (call (lref expand-sequence) (#%cons (lref e1)
                                                                                                                (lref e2))
                                                                                                               (lref r)
                                                                                                               (lref w)
                                                                                                               (lref s)
                                                                                                               (lref mod))
                                                                                                              (call (toplevel-ref expand-void)))))
                                                                                                          (lref tmp))
                                                                                                         (call (toplevel-ref syntax-violation) (const false)
                                                                                                           (const source expression failed to match any pattern)
                                                                                                           (lref tmp-1)))))
                                                                                                     (let
                                                                                                         ([cond-test-tmp
                                                                                                           (#%eq? (lref type)
                                                                                                            (const begin-form))])
                                                                                                       (if
                                                                                                         (lref cond-test-tmp)
                                                                                                         (let
                                                                                                            ([tmp
                                                                                                              (lref e)])
                                                                                                          (let
                                                                                                             ([tmp-1
                                                                                                               (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                (const (_ any . each-any)))])
                                                                                                           (if
                                                                                                             (lref tmp-1)
                                                                                                             (call (module-ref (capy)::apply #t) (lambda (e1 e2)
                                                                                                               (call (lref expand-sequence) (#%cons (lref e1)
                                                                                                                 (lref e2))
                                                                                                                (lref r)
                                                                                                                (lref w)
                                                                                                                (lref s)
                                                                                                                (lref mod)))
                                                                                                              (lref tmp-1))
                                                                                                             (let
                                                                                                                 ([tmp-1
                                                                                                                   (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                    (const (_)))])
                                                                                                               (if
                                                                                                                 (lref tmp-1)
                                                                                                                 (call (module-ref (capy)::apply #t) (lambda ()
                                                                                                                   (call (toplevel-ref syntax-violation) (const false)
                                                                                                                    (const sequence of zero expressions)
                                                                                                                    (call (lref source-wrap) (lref e)
                                                                                                                     (lref w)
                                                                                                                     (lref s)
                                                                                                                     (lref mod))))
                                                                                                                  (lref tmp-1))
                                                                                                                 (call (toplevel-ref syntax-violation) (const false)
                                                                                                                   (const source expression failed to match any pattern)
                                                                                                                   (lref tmp)))))))
                                                                                                         (let
                                                                                                             ([cond-test-tmp
                                                                                                               (let
                                                                                                                  ([or-tmp
                                                                                                                    (#%eq? (lref type)
                                                                                                                     (const define-form))])
                                                                                                                (if
                                                                                                                  (lref or-tmp)
                                                                                                                  (lref or-tmp)
                                                                                                                  (let
                                                                                                                      ([or-tmp
                                                                                                                        (#%eq? (lref type)
                                                                                                                         (const define-syntax-form))])
                                                                                                                    (if
                                                                                                                      (lref or-tmp)
                                                                                                                      (lref or-tmp)
                                                                                                                      (#%eq? (lref type)
                                                                                                                        (const define-syntax-parameter-form))))))])
                                                                                                           (if
                                                                                                             (lref cond-test-tmp)
                                                                                                             (call (toplevel-ref syntax-violation) (const false)
                                                                                                              (const definition in expression context, where definitions are not allowed)
                                                                                                              (call (lref source-wrap) (lref e)
                                                                                                               (lref w)
                                                                                                               (lref s)
                                                                                                               (lref mod)))
                                                                                                             (let
                                                                                                                 ([cond-test-tmp
                                                                                                                   (#%eq? (lref type)
                                                                                                                    (const local-syntax-form))])
                                                                                                               (if
                                                                                                                 (lref cond-test-tmp)
                                                                                                                 (call (lref expand-local-syntax) (lref value)
                                                                                                                  (lref e)
                                                                                                                  (lref r)
                                                                                                                  (lref w)
                                                                                                                  (lref s)
                                                                                                                  (lref mod)
                                                                                                                  (lref expand-sequence))
                                                                                                                 (let
                                                                                                                     ([cond-test-tmp
                                                                                                                       (#%eq? (lref type)
                                                                                                                        (const syntax))])
                                                                                                                   (if
                                                                                                                     (lref cond-test-tmp)
                                                                                                                     (call (toplevel-ref syntax-violation) (const false)
                                                                                                                      (const reference to pattern variable outside syntax form)
                                                                                                                      (call (lref source-wrap) (lref e)
                                                                                                                       (lref w)
                                                                                                                       (lref s)
                                                                                                                       (lref mod)))
                                                                                                                     (let
                                                                                                                         ([cond-test-tmp
                                                                                                                           (#%eq? (lref type)
                                                                                                                            (const displaced-lexical))])
                                                                                                                       (if
                                                                                                                         (lref cond-test-tmp)
                                                                                                                         (call (toplevel-ref syntax-violation) (const false)
                                                                                                                          (const reference to identifier outside its scope)
                                                                                                                          (call (lref source-wrap) (lref e)
                                                                                                                           (lref w)
                                                                                                                           (lref s)
                                                                                                                           (lref mod)))
                                                                                                                         (call (toplevel-ref syntax-violation) (const false)
                                                                                                                           (const unexpected syntax)
                                                                                                                           (lref type)
                                                                                                                           (call (lref source-wrap) (lref e)
                                                                                                                            (lref w)
                                                                                                                            (lref s)
                                                                                                                            (lref mod))))))))))))))))))))))))))))))))))]
                                                            [expand
                                                            (lambda (e r w mod)
                                                              (receive (type value form e w s mod)
                                                                (call (lref syntax-type) (lref e)
                                                                  (lref r)
                                                                  (lref w)
                                                                  (call (lref source-annotation) (lref e))
                                                                  (const false)
                                                                  (lref mod)
                                                                  (const false))
                                                                (call (lref expand-expr) (lref type)
                                                                  (lref value)
                                                                  (lref form)
                                                                  (lref e)
                                                                  (lref r)
                                                                  (lref w)
                                                                  (lref s)
                                                                  (lref mod))))])
                                                           (fix
                                                             ([set-ribcage-labels!
                                                             (lambda (r v)
                                                               (#%vector-set! (lref r)
                                                                (const 3)
                                                                (lref v)))])
                                                            (fix
                                                              ([set-ribcage-marks!
                                                              (lambda (r v)
                                                                (#%vector-set! (lref r)
                                                                 (const 2)
                                                                 (lref v)))])
                                                             (fix
                                                               ([set-ribcage-symnames!
                                                               (lambda (r v)
                                                                 (#%vector-set! (lref r)
                                                                  (const 1)
                                                                  (lref v)))])
                                                              (fix
                                                                ([ribcage-labels
                                                                (lambda (r)
                                                                  (#%vector-ref (lref r)
                                                                   (const 3)))])
                                                               (fix
                                                                 ([ribcage-marks
                                                                 (lambda (r)
                                                                   (#%vector-ref (lref r)
                                                                    (const 2)))])
                                                                (fix
                                                                  ([ribcage-symnames
                                                                  (lambda (r)
                                                                    (#%vector-ref (lref r)
                                                                     (const 1)))])
                                                                 (fix
                                                                   ([extend-ribcage!
                                                                   (lambda (ribcage id label)
                                                                     (seq
                                                                      (call (lref set-ribcage-symnames!) (lref ribcage)
                                                                       (#%cons (call (lref syntax-expression) (lref id))
                                                                        (call (lref ribcage-symnames) (lref ribcage))))
                                                                      (call (lref set-ribcage-marks!) (lref ribcage)
                                                                       (#%cons (call (lref wrap-marks) (call (lref syntax-wrap) (lref id)))
                                                                        (call (lref ribcage-marks) (lref ribcage))))
                                                                      (call (lref set-ribcage-labels!) (lref ribcage)
                                                                       (#%cons (lref label)
                                                                        (call (lref ribcage-labels) (lref ribcage))))))])
                                                                  (fix
                                                                    ([make-empty-ribcage
                                                                    (lambda ()
                                                                      (call (lref make-ribcage) (const '())
                                                                       (const '())
                                                                       (const '())))])
                                                                   (fix
                                                                     ([build-letrec*
                                                                     (lambda (src ids vars val-exps body-exp)
                                                                       (let
                                                                          ([v
                                                                            (call (toplevel-ref map) (lref maybe-name-value)
                                                                             (lref ids)
                                                                             (lref val-exps))])
                                                                        (let
                                                                           ([cond-test-tmp
                                                                             (#%null? (lref v))])
                                                                         (if
                                                                           (lref cond-test-tmp)
                                                                           (lref body-exp)
                                                                           (call (toplevel-ref make-let) (lref src)
                                                                             (const letrec*)
                                                                             (lref ids)
                                                                             (lref vars)
                                                                             (lref v)
                                                                             (lref body-exp))))))])
                                                                    (fix
                                                                      ([expand-body
                                                                      (lambda (body outer-form r w mod)
                                                                        (let
                                                                           ([r
                                                                             (#%cons (const (placeholder placeholder))
                                                                              (lref r))])
                                                                         (let
                                                                            ([ribcage
                                                                              (call (lref make-empty-ribcage))])
                                                                          (let
                                                                             ([w
                                                                               (call (lref make-wrap) (call (lref wrap-marks) (lref w))
                                                                                (#%cons (lref ribcage)
                                                                                 (call (lref wrap-subst) (lref w))))])
                                                                           (fix
                                                                             ([parse
                                                                             (lambda (body ids labels var-ids vars vals bindings expand-tail-expr)
                                                                               (let
                                                                                  ([cond-test-tmp
                                                                                    (#%null? (lref body))])
                                                                                (if
                                                                                  (lref cond-test-tmp)
                                                                                  (seq
                                                                                   (if
                                                                                     (#%not (lref expand-tail-expr))
                                                                                     (if
                                                                                       (#%null? (lref ids))
                                                                                       (call (toplevel-ref syntax-violation) (const false)
                                                                                        (const empty body)
                                                                                        (lref outer-form))
                                                                                       (call (toplevel-ref syntax-violation) (const false)
                                                                                         (const body should end with an expression)
                                                                                         (lref outer-form)))
                                                                                     (const #<undefined>))
                                                                                   (if
                                                                                     (#%not (call (lref valid-bound-ids?) (lref ids)))
                                                                                     (call (toplevel-ref syntax-violation) (const false)
                                                                                      (const invalid or duplicate identifier in definition)
                                                                                      (lref outer-form))
                                                                                     (const #<undefined>))
                                                                                   (#%set-cdr! (lref r)
                                                                                    (call (lref extend-env) (lref labels)
                                                                                     (lref bindings)
                                                                                     (#%cdr (lref r))))
                                                                                   (let
                                                                                      ([src
                                                                                        (call (lref source-annotation) (lref outer-form))])
                                                                                    (fix
                                                                                      ([lp
                                                                                      (lambda (var-ids vars vals tail)
                                                                                        (let
                                                                                           ([cond-test-tmp
                                                                                             (#%null? (lref var-ids))])
                                                                                         (if
                                                                                           (lref cond-test-tmp)
                                                                                           (lref tail)
                                                                                           (let
                                                                                               ([cond-test-tmp
                                                                                                 (#%not (#%car (lref var-ids)))])
                                                                                             (if
                                                                                               (lref cond-test-tmp)
                                                                                               (call (lref lp) (#%cdr (lref var-ids))
                                                                                                (#%cdr (lref vars))
                                                                                                (#%cdr (lref vals))
                                                                                                (call (toplevel-ref make-sequence) (lref src)
                                                                                                 (call (#%car (lref vals)))
                                                                                                 (lref tail)))
                                                                                               (let
                                                                                                   ([var-ids
                                                                                                     (call (toplevel-ref map) (lambda (id)
                                                                                                       (if
                                                                                                         (lref id)
                                                                                                         (call (toplevel-ref syntax->datum) (lref id))
                                                                                                         (const _)))
                                                                                                      (call (toplevel-ref reverse) (lref var-ids)))]
                                                                                                   [vars
                                                                                                     (call (toplevel-ref map) (lambda (var)
                                                                                                       (let
                                                                                                          ([or-tmp
                                                                                                            (lref var)])
                                                                                                        (if
                                                                                                          (lref or-tmp)
                                                                                                          (lref or-tmp)
                                                                                                          (call (lref gen-lexical) (const _)))))
                                                                                                      (call (toplevel-ref reverse) (lref vars)))]
                                                                                                   [vals
                                                                                                     (call (toplevel-ref map) (lambda (expand-expr id)
                                                                                                       (if
                                                                                                         (lref id)
                                                                                                         (call (lref expand-expr))
                                                                                                         (call (toplevel-ref make-sequence) (lref src)
                                                                                                           (call (lref expand-expr))
                                                                                                           (call (toplevel-ref make-void) (const false)))))
                                                                                                      (call (toplevel-ref reverse) (lref vals))
                                                                                                      (call (toplevel-ref reverse) (lref var-ids)))])
                                                                                                 (call (lref build-letrec*) (lref src)
                                                                                                  (lref var-ids)
                                                                                                  (lref vars)
                                                                                                  (lref vals)
                                                                                                  (lref tail))))))))])
                                                                                     (call (lref lp) (lref var-ids)
                                                                                      (lref vars)
                                                                                      (lref vals)
                                                                                      (call (lref expand-tail-expr))))))
                                                                                  (let
                                                                                      ([cond-test-tmp
                                                                                        (lref expand-tail-expr)])
                                                                                    (if
                                                                                      (lref cond-test-tmp)
                                                                                      (call (lref parse) (lref body)
                                                                                       (lref ids)
                                                                                       (lref labels)
                                                                                       (#%cons (const false)
                                                                                        (lref var-ids))
                                                                                       (#%cons (const false)
                                                                                        (lref vars))
                                                                                       (#%cons (lref expand-tail-expr)
                                                                                        (lref vals))
                                                                                       (lref bindings)
                                                                                       (const false))
                                                                                      (let
                                                                                          ([e
                                                                                            (#%cdr (#%car (lref body)))]
                                                                                          [er
                                                                                            (#%car (#%car (lref body)))]
                                                                                          [body
                                                                                            (#%cdr (lref body))])
                                                                                        (receive (type value form e w s mod)
                                                                                          (call (lref syntax-type) (lref e)
                                                                                            (lref er)
                                                                                            (lref empty-wrap)
                                                                                            (call (lref source-annotation) (lref e))
                                                                                            (lref ribcage)
                                                                                            (lref mod)
                                                                                            (const false))
                                                                                          (let
                                                                                              ([cond-test-tmp
                                                                                                (#%eq? (lref type)
                                                                                                 (const define-form))])
                                                                                            (if
                                                                                              (lref cond-test-tmp)
                                                                                              (let
                                                                                                 ([id
                                                                                                   (call (lref wrap) (lref value)
                                                                                                    (lref w)
                                                                                                    (lref mod))]
                                                                                                 [label
                                                                                                   (call (lref gen-label))])
                                                                                               (let
                                                                                                  ([var
                                                                                                    (call (lref gen-var) (lref id))])
                                                                                                (seq
                                                                                                 (call (lref extend-ribcage!) (lref ribcage)
                                                                                                  (lref id)
                                                                                                  (lref label))
                                                                                                 (call (lref parse) (lref body)
                                                                                                  (#%cons (lref id)
                                                                                                   (lref ids))
                                                                                                  (#%cons (lref label)
                                                                                                   (lref labels))
                                                                                                  (#%cons (lref id)
                                                                                                   (lref var-ids))
                                                                                                  (#%cons (lref var)
                                                                                                   (lref vars))
                                                                                                  (#%cons (let
                                                                                                      ([wrapped
                                                                                                        (call (lref source-wrap) (lref e)
                                                                                                         (lref w)
                                                                                                         (lref s)
                                                                                                         (lref mod))])
                                                                                                    (lambda ()
                                                                                                     (call (lref expand) (lref wrapped)
                                                                                                      (lref er)
                                                                                                      (lref empty-wrap)
                                                                                                      (lref mod))))
                                                                                                   (lref vals))
                                                                                                  (#%cons (#%cons (const lexical)
                                                                                                    (lref var))
                                                                                                   (lref bindings))
                                                                                                  (const false)))))
                                                                                              (let
                                                                                                  ([cond-test-tmp
                                                                                                    (#%eq? (lref type)
                                                                                                     (const begin-form))])
                                                                                                (if
                                                                                                  (lref cond-test-tmp)
                                                                                                  (let
                                                                                                     ([tmp-1
                                                                                                       (lref e)])
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                         (const (_ . each-any)))])
                                                                                                    (if
                                                                                                      (lref tmp)
                                                                                                      (call (module-ref (capy)::apply #t) (lambda (e1)
                                                                                                        (call (lref parse) (fix
                                                                                                           ([f
                                                                                                           (lambda (forms)
                                                                                                             (if
                                                                                                               (#%null? (lref forms))
                                                                                                               (lref body)
                                                                                                               (#%cons (#%cons (lref er)
                                                                                                                  (call (lref wrap) (#%car (lref forms))
                                                                                                                   (lref w)
                                                                                                                   (lref mod)))
                                                                                                                 (call (lref f) (#%cdr (lref forms))))))])
                                                                                                          (call (lref f) (lref e1)))
                                                                                                         (lref ids)
                                                                                                         (lref labels)
                                                                                                         (lref var-ids)
                                                                                                         (lref vars)
                                                                                                         (lref vals)
                                                                                                         (lref bindings)
                                                                                                         (const false)))
                                                                                                       (lref tmp))
                                                                                                      (call (toplevel-ref syntax-violation) (const false)
                                                                                                        (const source expression failed to match any pattern)
                                                                                                        (lref tmp-1)))))
                                                                                                  (let
                                                                                                      ([cond-test-tmp
                                                                                                        (#%eq? (lref type)
                                                                                                         (const define-syntax-form))])
                                                                                                    (if
                                                                                                      (lref cond-test-tmp)
                                                                                                      (let
                                                                                                         ([id
                                                                                                           (call (lref wrap) (lref value)
                                                                                                            (lref w)
                                                                                                            (lref mod))]
                                                                                                         [label
                                                                                                           (call (lref gen-label))]
                                                                                                         [trans-r
                                                                                                           (call (lref macros-only-env) (lref er))])
                                                                                                       (seq
                                                                                                        (call (lref extend-ribcage!) (lref ribcage)
                                                                                                         (lref id)
                                                                                                         (lref label))
                                                                                                        (#%set-cdr! (lref r)
                                                                                                         (call (lref extend-env) (#%cons (lref label)
                                                                                                           (const '()))
                                                                                                          (#%cons (#%cons (const macro)
                                                                                                            (call (lref eval-local-transformer) (call (lref expand) (lref e)
                                                                                                              (lref trans-r)
                                                                                                              (lref w)
                                                                                                              (lref mod))
                                                                                                             (lref mod)))
                                                                                                           (const '()))
                                                                                                          (#%cdr (lref r))))
                                                                                                        (call (lref parse) (lref body)
                                                                                                         (#%cons (lref id)
                                                                                                          (lref ids))
                                                                                                         (lref labels)
                                                                                                         (lref var-ids)
                                                                                                         (lref vars)
                                                                                                         (lref vals)
                                                                                                         (lref bindings)
                                                                                                         (const false))))
                                                                                                      (let
                                                                                                          ([cond-test-tmp
                                                                                                            (#%eq? (lref type)
                                                                                                             (const define-syntax-parameter-form))])
                                                                                                        (if
                                                                                                          (lref cond-test-tmp)
                                                                                                          (let
                                                                                                             ([id
                                                                                                               (call (lref wrap) (lref value)
                                                                                                                (lref w)
                                                                                                                (lref mod))]
                                                                                                             [label
                                                                                                               (call (lref gen-label))]
                                                                                                             [trans-r
                                                                                                               (call (lref macros-only-env) (lref er))])
                                                                                                           (seq
                                                                                                            (call (lref extend-ribcage!) (lref ribcage)
                                                                                                             (lref id)
                                                                                                             (lref label))
                                                                                                            (#%set-cdr! (lref r)
                                                                                                             (call (lref extend-env) (#%cons (lref label)
                                                                                                               (const '()))
                                                                                                              (#%cons (#%cons (const syntax-parameter)
                                                                                                                (call (lref eval-local-transformer) (call (lref expand) (lref e)
                                                                                                                  (lref trans-r)
                                                                                                                  (lref w)
                                                                                                                  (lref mod))
                                                                                                                 (lref mod)))
                                                                                                               (const '()))
                                                                                                              (#%cdr (lref r))))
                                                                                                            (call (lref parse) (lref body)
                                                                                                             (#%cons (lref id)
                                                                                                              (lref ids))
                                                                                                             (lref labels)
                                                                                                             (lref var-ids)
                                                                                                             (lref vars)
                                                                                                             (lref vals)
                                                                                                             (lref bindings)
                                                                                                             (const false))))
                                                                                                          (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%eq? (lref type)
                                                                                                                 (const local-syntax-form))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (call (lref expand-local-syntax) (lref value)
                                                                                                               (lref e)
                                                                                                               (lref er)
                                                                                                               (lref w)
                                                                                                               (lref s)
                                                                                                               (lref mod)
                                                                                                               (lambda (forms er w s mod)
                                                                                                                (call (lref parse) (fix
                                                                                                                   ([f
                                                                                                                   (lambda (forms)
                                                                                                                     (if
                                                                                                                       (#%null? (lref forms))
                                                                                                                       (lref body)
                                                                                                                       (#%cons (#%cons (lref er)
                                                                                                                          (call (lref wrap) (#%car (lref forms))
                                                                                                                           (lref w)
                                                                                                                           (lref mod)))
                                                                                                                         (call (lref f) (#%cdr (lref forms))))))])
                                                                                                                  (call (lref f) (lref forms)))
                                                                                                                 (lref ids)
                                                                                                                 (lref labels)
                                                                                                                 (lref var-ids)
                                                                                                                 (lref vars)
                                                                                                                 (lref vals)
                                                                                                                 (lref bindings)
                                                                                                                 (const false))))
                                                                                                              (let
                                                                                                                  ([wrapped
                                                                                                                    (call (lref source-wrap) (lref e)
                                                                                                                     (lref w)
                                                                                                                     (lref s)
                                                                                                                     (lref mod))])
                                                                                                                (call (lref parse) (lref body)
                                                                                                                 (lref ids)
                                                                                                                 (lref labels)
                                                                                                                 (lref var-ids)
                                                                                                                 (lref vars)
                                                                                                                 (lref vals)
                                                                                                                 (lref bindings)
                                                                                                                 (lambda ()
                                                                                                                  (call (lref expand) (lref wrapped)
                                                                                                                   (lref er)
                                                                                                                   (lref empty-wrap)
                                                                                                                   (lref mod))))))))))))))))))))))])
                                                                            (call (lref parse) (call (toplevel-ref map) (lambda (x)
                                                                               (#%cons (lref r)
                                                                                (call (lref wrap) (lref x)
                                                                                 (lref w)
                                                                                 (lref mod))))
                                                                              (lref body))
                                                                             (const '())
                                                                             (const '())
                                                                             (const '())
                                                                             (const '())
                                                                             (const '())
                                                                             (const '())
                                                                             (const false)))))))])
                                                                     (fix
                                                                       ([extend-var-env
                                                                       (lambda (labels vars r)
                                                                         (let
                                                                            ([cond-test-tmp
                                                                              (#%null? (lref labels))])
                                                                          (if
                                                                            (lref cond-test-tmp)
                                                                            (lref r)
                                                                            (let
                                                                                ([cond-test-tmp
                                                                                  (#%pair? (lref labels))])
                                                                              (if
                                                                                (lref cond-test-tmp)
                                                                                (let
                                                                                   ([label
                                                                                     (#%car (lref labels))]
                                                                                   [labels
                                                                                     (#%cdr (lref labels))])
                                                                                 (let
                                                                                    ([var
                                                                                      (#%car (lref vars))]
                                                                                    [vars
                                                                                      (#%cdr (lref vars))])
                                                                                  (call (lref extend-var-env) (lref labels)
                                                                                   (lref vars)
                                                                                   (#%cons (#%cons (lref label)
                                                                                     (#%cons (const lexical)
                                                                                      (lref var)))
                                                                                    (lref r)))))
                                                                                (const #<undefined>))))))])
                                                                      (fix
                                                                        ([expand-simple-lambda
                                                                        (lambda (e r w s mod req rest meta body)
                                                                          (let
                                                                             ([ids
                                                                               (if
                                                                                 (lref rest)
                                                                                 (#%append (lref req)
                                                                                  (#%cons (lref rest)
                                                                                   (const '())))
                                                                                 (lref req))])
                                                                           (let
                                                                              ([vars
                                                                                (call (toplevel-ref map) (lref gen-var)
                                                                                 (lref ids))])
                                                                            (let
                                                                               ([labels
                                                                                 (call (lref gen-labels) (lref ids))])
                                                                             (call (lref build-simple-lambda) (lref s)
                                                                              (lref ids)
                                                                              (lref vars)
                                                                              (lref meta)
                                                                              (call (lref expand-body) (lref body)
                                                                               (call (lref source-wrap) (lref e)
                                                                                (lref w)
                                                                                (lref s)
                                                                                (lref mod))
                                                                               (call (lref extend-var-env) (lref labels)
                                                                                (lref vars)
                                                                                (lref r))
                                                                               (call (lref make-binding-wrap) (lref ids)
                                                                                (lref labels)
                                                                                (lref w))
                                                                               (lref mod)))))))])
                                                                       (fix
                                                                         ([build-let
                                                                         (lambda (src ids vars val-exps body-exp)
                                                                           (let
                                                                              ([v
                                                                                (call (toplevel-ref map) (lref maybe-name-value)
                                                                                 (lref ids)
                                                                                 (lref val-exps))])
                                                                            (let
                                                                               ([cond-test-tmp
                                                                                 (#%null? (lref v))])
                                                                             (if
                                                                               (lref cond-test-tmp)
                                                                               (lref body-exp)
                                                                               (call (toplevel-ref make-let) (lref src)
                                                                                 (const let)
                                                                                 (lref ids)
                                                                                 (lref vars)
                                                                                 (lref v)
                                                                                 (lref body-exp))))))])
                                                                        (seq
                                                                         (lambda (src ids vars val-exps body-exp)
                                                                          (let
                                                                             ([v
                                                                               (call (toplevel-ref map) (lref maybe-name-value)
                                                                                (lref ids)
                                                                                (lref val-exps))])
                                                                           (let
                                                                              ([cond-test-tmp
                                                                                (#%null? (lref v))])
                                                                            (if
                                                                              (lref cond-test-tmp)
                                                                              (lref body-exp)
                                                                              (call (toplevel-ref make-let) (lref src)
                                                                                (const let*)
                                                                                (lref ids)
                                                                                (lref vars)
                                                                                (lref v)
                                                                                (lref body-exp))))))
                                                                         (fix
                                                                           ([build-letrec
                                                                           (lambda (src ids vars val-exps body-exp)
                                                                             (let
                                                                                ([v
                                                                                  (call (toplevel-ref map) (lref maybe-name-value)
                                                                                   (lref ids)
                                                                                   (lref val-exps))])
                                                                              (let
                                                                                 ([cond-test-tmp
                                                                                   (#%null? (lref v))])
                                                                               (if
                                                                                 (lref cond-test-tmp)
                                                                                 (lref body-exp)
                                                                                 (call (toplevel-ref make-let) (lref src)
                                                                                   (const letrec)
                                                                                   (lref ids)
                                                                                   (lref vars)
                                                                                   (lref v)
                                                                                   (lref body-exp))))))])
                                                                          (fix
                                                                            ([build-lexical-assignment
                                                                            (lambda (src name sym value)
                                                                              (call (toplevel-ref make-lset) (lref src)
                                                                               (lref name)
                                                                               (lref sym)
                                                                               (lref value)))])
                                                                           (fix
                                                                             ([build-conditional
                                                                             (lambda (src test-exp then-exp else-exp)
                                                                               (call (toplevel-ref make-if) (lref src)
                                                                                (lref test-exp)
                                                                                (lref then-exp)
                                                                                (lref else-exp)))])
                                                                            (fix
                                                                              ([build-data
                                                                              (lambda (src val)
                                                                                (call (toplevel-ref make-constant) (lref src)
                                                                                 (lref val)))])
                                                                             (fix
                                                                               ([build-named-let
                                                                               (lambda (src ids vars val-exps body-exp)
                                                                                 (let
                                                                                    ([v
                                                                                      (lref vars)])
                                                                                  (fix
                                                                                    ([fk
                                                                                    (lambda ()
                                                                                      (call (toplevel-ref error) (const value failed to match)
                                                                                       (lref v)))])
                                                                                   (if
                                                                                     (#%pair? (lref v))
                                                                                     (let
                                                                                        ([vx
                                                                                          (#%car (lref v))]
                                                                                        [vy
                                                                                          (#%cdr (lref v))])
                                                                                      (let
                                                                                         ([f
                                                                                           (lref vx)])
                                                                                       (let
                                                                                          ([vars
                                                                                            (lref vy)])
                                                                                        (let
                                                                                           ([v
                                                                                             (lref ids)])
                                                                                         (fix
                                                                                           ([fk
                                                                                           (lambda ()
                                                                                             (call (toplevel-ref error) (const value failed to match)
                                                                                              (lref v)))])
                                                                                          (if
                                                                                            (#%pair? (lref v))
                                                                                            (let
                                                                                               ([vx
                                                                                                 (#%car (lref v))]
                                                                                               [vy
                                                                                                 (#%cdr (lref v))])
                                                                                             (let
                                                                                                ([f-name
                                                                                                  (lref vx)])
                                                                                              (let
                                                                                                 ([ids
                                                                                                   (lref vy)])
                                                                                               (let
                                                                                                  ([proc
                                                                                                    (call (lref build-simple-lambda) (lref src)
                                                                                                     (lref ids)
                                                                                                     (lref vars)
                                                                                                     (const '())
                                                                                                     (lref body-exp))])
                                                                                                (call (toplevel-ref make-let) (lref src)
                                                                                                 (const letrec)
                                                                                                 (#%cons (lref f-name)
                                                                                                  (const '()))
                                                                                                 (#%cons (lref f)
                                                                                                  (const '()))
                                                                                                 (#%cons (call (lref maybe-name-value) (lref f-name)
                                                                                                   (lref proc))
                                                                                                  (const '()))
                                                                                                 (call (lref build-call) (lref src)
                                                                                                  (call (lref build-lexical-reference) (lref src)
                                                                                                   (lref f-name)
                                                                                                   (lref f))
                                                                                                  (call (toplevel-ref map) (lref maybe-name-value)
                                                                                                   (lref ids)
                                                                                                   (lref val-exps))))))))
                                                                                            (call (lref fk))))))))
                                                                                     (call (lref fk))))))])
                                                                              (fix
                                                                                ([nonsymbol-id?
                                                                                (lambda (x)
                                                                                  (let
                                                                                     ([and-tmp
                                                                                       (call (lref syntax?) (lref x))])
                                                                                   (if
                                                                                     (lref and-tmp)
                                                                                     (call (module-ref (capy)::symbol? #t) (call (lref syntax-expression) (lref x)))
                                                                                     (const false))))])
                                                                               (fix
                                                                                 ([id-sym-name
                                                                                 (lambda (x)
                                                                                   (if
                                                                                     (call (lref syntax?) (lref x))
                                                                                     (call (lref syntax-expression) (lref x))
                                                                                     (lref x)))])
                                                                                (seq
                                                                                 (lambda (w mod)
                                                                                  (fix
                                                                                    ([scan
                                                                                    (lambda (subst results)
                                                                                      (let
                                                                                         ([cond-test-tmp
                                                                                           (#%null? (lref subst))])
                                                                                       (if
                                                                                         (lref cond-test-tmp)
                                                                                         (lref results)
                                                                                         (let
                                                                                             ([cond-test-tmp
                                                                                               (let
                                                                                                  ([and-tmp
                                                                                                    (#%pair? (lref subst))])
                                                                                                (if
                                                                                                  (lref and-tmp)
                                                                                                  (#%eq? (#%car (lref subst))
                                                                                                   (const shift))
                                                                                                  (const false)))])
                                                                                           (if
                                                                                             (lref cond-test-tmp)
                                                                                             (call (lref scan) (#%cdr (lref subst))
                                                                                              (lref results))
                                                                                             (let
                                                                                                 ([cond-test-tmp
                                                                                                   (let
                                                                                                      ([and-tmp
                                                                                                        (#%pair? (lref subst))])
                                                                                                    (if
                                                                                                      (lref and-tmp)
                                                                                                      (let
                                                                                                         ([and-tmp
                                                                                                           (#%vector? (#%car (lref subst)))])
                                                                                                       (if
                                                                                                         (lref and-tmp)
                                                                                                         (#%eq? (#%vector-ref (#%car (lref subst))
                                                                                                           (const 0))
                                                                                                          (const ribcage))
                                                                                                         (const false)))
                                                                                                      (const false)))])
                                                                                               (if
                                                                                                 (lref cond-test-tmp)
                                                                                                 (let
                                                                                                    ([ribcage
                                                                                                      (#%car (lref subst))])
                                                                                                  (let
                                                                                                     ([symnames
                                                                                                       (call (lref ribcage-symnames) (lref ribcage))])
                                                                                                   (let
                                                                                                      ([marks
                                                                                                        (call (lref ribcage-marks) (lref ribcage))])
                                                                                                    (let
                                                                                                       ([labels
                                                                                                         (call (lref ribcage-labels) (lref ribcage))])
                                                                                                     (let
                                                                                                        ([subst*
                                                                                                          (#%cdr (lref subst))])
                                                                                                      (fix
                                                                                                        ([scan-list-rib
                                                                                                        (lambda ()
                                                                                                          (fix
                                                                                                            ([lp
                                                                                                            (lambda (symnames marks results)
                                                                                                              (let
                                                                                                                 ([cond-test-tmp
                                                                                                                   (#%null? (lref symnames))])
                                                                                                               (if
                                                                                                                 (lref cond-test-tmp)
                                                                                                                 (call (lref scan) (lref subst*)
                                                                                                                  (lref results))
                                                                                                                 (let
                                                                                                                     ([sym
                                                                                                                       (#%car (lref symnames))]
                                                                                                                     [symnames
                                                                                                                       (#%cdr (lref symnames))]
                                                                                                                     [m
                                                                                                                       (#%car (lref marks))]
                                                                                                                     [marks
                                                                                                                       (#%cdr (lref marks))])
                                                                                                                   (call (lref lp) (lref symnames)
                                                                                                                    (lref marks)
                                                                                                                    (#%cons (call (lref wrap) (lref sym)
                                                                                                                      (call (lref anti-mark) (call (lref make-wrap) (lref m)
                                                                                                                        (lref subst)))
                                                                                                                      (lref mod))
                                                                                                                     (toplevel-ref resulsts)))))))])
                                                                                                           (call (lref lp) (lref symnames)
                                                                                                            (lref marks)
                                                                                                            (lref results))))])
                                                                                                       (fix
                                                                                                         ([scan-vector-rib
                                                                                                         (lambda ()
                                                                                                           (let
                                                                                                              ([n
                                                                                                                (#%vector-length (lref symnames))])
                                                                                                            (fix
                                                                                                              ([lp
                                                                                                              (lambda (i results)
                                                                                                                (let
                                                                                                                   ([cond-test-tmp
                                                                                                                     (#%= (lref i)
                                                                                                                      (lref n))])
                                                                                                                 (if
                                                                                                                   (lref cond-test-tmp)
                                                                                                                   (call (lref scan) (lref subst*)
                                                                                                                    (lref results))
                                                                                                                   (let
                                                                                                                       ([sym
                                                                                                                         (#%vector-ref (lref symnames)
                                                                                                                          (lref i))]
                                                                                                                       [m
                                                                                                                         (#%vector-ref (lref marks)
                                                                                                                          (lref i))])
                                                                                                                     (call (lref lp) (#%+ (lref i)
                                                                                                                       (const 1))
                                                                                                                      (#%cons (call (lref wrap) (lref sym)
                                                                                                                        (call (lref anti-mark) (call (lref make-wrap) (lref m)
                                                                                                                          (lref subst)))
                                                                                                                        (lref mod))
                                                                                                                       (lref results)))))))])
                                                                                                             (call (lref lp) (const 0)
                                                                                                              (lref results)))))])
                                                                                                        (if
                                                                                                          (#%vector? (lref symnames))
                                                                                                          (call (lref scan-vector-rib))
                                                                                                          (call (lref scan-list-rib))))))))))
                                                                                                 (const #<undefined>))))))))])
                                                                                   (call (lref scan) (call (lref wrap-subst) (lref w))
                                                                                    (const '()))))
                                                                                 (seq
                                                                                  (lambda (k)
                                                                                   (call (call (toplevel-ref fluid-ref) (lref transformer-environment)) (lref k)))
                                                                                  (fix
                                                                                    ([free-id=?
                                                                                    (lambda (i j)
                                                                                      (let
                                                                                         ([mi
                                                                                           (let
                                                                                              ([and-tmp
                                                                                                (call (lref syntax?) (lref i))])
                                                                                            (if
                                                                                              (lref and-tmp)
                                                                                              (call (lref syntax-module) (lref i))
                                                                                              (const false)))])
                                                                                       (let
                                                                                          ([mj
                                                                                            (let
                                                                                               ([and-tmp
                                                                                                 (call (lref syntax?) (lref j))])
                                                                                             (if
                                                                                               (lref and-tmp)
                                                                                               (call (lref syntax-module) (lref j))
                                                                                               (const false)))])
                                                                                        (let
                                                                                           ([ni
                                                                                             (call (lref id-var-name) (lref i)
                                                                                              (lref empty-wrap)
                                                                                              (lref mi))])
                                                                                         (let
                                                                                            ([nj
                                                                                              (call (lref id-var-name) (lref j)
                                                                                               (lref empty-wrap)
                                                                                               (lref mj))])
                                                                                          (fix
                                                                                            ([id-module-binding
                                                                                            (lambda (id mod)
                                                                                              (call (toplevel-ref module-variable) (if
                                                                                                 (lref mod)
                                                                                                 (call (toplevel-ref resolve-module) (#%cdr (lref mod))
                                                                                                  (const true)
                                                                                                  (const true))
                                                                                                 (#%current-module))
                                                                                               (call (lref id-sym-name) (lref id))))])
                                                                                           (let
                                                                                              ([cond-test-tmp
                                                                                                (call (lref syntax?) (lref ni))])
                                                                                            (if
                                                                                              (lref cond-test-tmp)
                                                                                              (call (lref free-id=?) (lref ni)
                                                                                               (lref j))
                                                                                              (let
                                                                                                  ([cond-test-tmp
                                                                                                    (call (lref syntax?) (lref nj))])
                                                                                                (if
                                                                                                  (lref cond-test-tmp)
                                                                                                  (call (lref free-id=?) (lref i)
                                                                                                   (lref nj))
                                                                                                  (let
                                                                                                      ([cond-test-tmp
                                                                                                        (call (module-ref (capy)::symbol? #t) (lref ni))])
                                                                                                    (if
                                                                                                      (lref cond-test-tmp)
                                                                                                      (let
                                                                                                         ([and-tmp
                                                                                                           (#%eq? (lref nj)
                                                                                                            (call (lref id-sym-name) (lref j)))])
                                                                                                       (if
                                                                                                         (lref and-tmp)
                                                                                                         (let
                                                                                                            ([bi
                                                                                                              (call (lref id-module-binding) (lref i)
                                                                                                               (lref mi))]
                                                                                                            [bj
                                                                                                              (call (lref id-module-binding) (lref j)
                                                                                                               (lref mj))])
                                                                                                          (let
                                                                                                             ([and-tmp
                                                                                                               (#%eq? (lref bi)
                                                                                                                (lref bj))])
                                                                                                           (if
                                                                                                             (lref and-tmp)
                                                                                                             (let
                                                                                                                ([or-tmp
                                                                                                                  (lref bi)])
                                                                                                              (if
                                                                                                                (lref or-tmp)
                                                                                                                (lref or-tmp)
                                                                                                                (#%eq? (lref ni)
                                                                                                                  (lref nj))))
                                                                                                             (const false))))
                                                                                                         (const false)))
                                                                                                      (#%equal? (lref ni)
                                                                                                        (lref nj))))))))))))))])
                                                                                   (fix
                                                                                     ([bound-id=?
                                                                                     (lambda (i j)
                                                                                       (if
                                                                                         (let
                                                                                             ([and-tmp
                                                                                               (call (lref syntax?) (lref i))])
                                                                                           (if
                                                                                             (lref and-tmp)
                                                                                             (call (lref syntax?) (lref j))
                                                                                             (const false)))
                                                                                         (let
                                                                                            ([and-tmp
                                                                                              (#%eq? (call (lref syntax-expression) (lref i))
                                                                                               (call (lref syntax-expression) (lref j)))])
                                                                                          (if
                                                                                            (lref and-tmp)
                                                                                            (call (lref same-marks?) (call (lref wrap-marks) (call (lref syntax-wrap) (lref i)))
                                                                                             (call (lref wrap-marks) (call (lref syntax-wrap) (lref j))))
                                                                                            (const false)))
                                                                                         (const #<undefined>)))])
                                                                                    (fix
                                                                                      ([bound-id-member?
                                                                                      (lambda (x ids)
                                                                                        (if
                                                                                          (#%null? (lref ids))
                                                                                          (const false)
                                                                                          (let
                                                                                              ([or-tmp
                                                                                                (call (lref bound-id=?) (lref x)
                                                                                                 (#%car (lref ids)))])
                                                                                            (if
                                                                                              (lref or-tmp)
                                                                                              (lref or-tmp)
                                                                                              (call (lref bound-id-member?) (lref x)
                                                                                                (#%cdr (lref ids)))))))])
                                                                                     (fix
                                                                                       ([distinct-bound-ids?
                                                                                       (lambda (ids)
                                                                                         (fix
                                                                                           ([distinct?
                                                                                           (lambda (ids)
                                                                                             (let
                                                                                                ([cond-test-tmp
                                                                                                  (#%null? (lref ids))])
                                                                                              (if
                                                                                                (lref cond-test-tmp)
                                                                                                (const true)
                                                                                                (let
                                                                                                    ([cond-test-tmp
                                                                                                      (#%pair? (lref ids))])
                                                                                                  (if
                                                                                                    (lref cond-test-tmp)
                                                                                                    (let
                                                                                                       ([id
                                                                                                         (#%car (lref ids))]
                                                                                                       [ids
                                                                                                         (#%cdr (lref ids))])
                                                                                                     (let
                                                                                                        ([and-tmp
                                                                                                          (#%not (call (lref bound-id-member?) (lref id)
                                                                                                            (lref ids)))])
                                                                                                      (if
                                                                                                        (lref and-tmp)
                                                                                                        (call (lref distinct?) (lref ids))
                                                                                                        (const false))))
                                                                                                    (const #<undefined>))))))])
                                                                                          (call (lref distinct?) (lref ids))))])
                                                                                      (let
                                                                                         ([top-wrap
                                                                                           (const ((top)))])
                                                                                       (fix
                                                                                         ([expand-install-global
                                                                                         (lambda (mod name type e)
                                                                                           (call (lref build-global-definition) (const false)
                                                                                            (lref mod)
                                                                                            (lref name)
                                                                                            (call (lref build-primcall) (const false)
                                                                                             (const make-syntax-transformer)
                                                                                             (#%cons (call (toplevel-ref make-constant) (const false)
                                                                                               (lref name))
                                                                                              (#%cons (call (toplevel-ref make-constant) (const false)
                                                                                                (if
                                                                                                  (#%eq? (lref type)
                                                                                                    (const define-syntax-parameter-form))
                                                                                                  (const syntax-parameter)
                                                                                                  (const macro)))
                                                                                               (#%cons (lref e)
                                                                                                (const '())))))))])
                                                                                        (fix
                                                                                          ([expand-top-sequence
                                                                                          (lambda (body r w s m essew mod)
                                                                                            (let
                                                                                               ([r
                                                                                                 (#%cons (const (placeholder placeholder))
                                                                                                  (lref r))])
                                                                                             (let
                                                                                                ([ribcage
                                                                                                  (call (lref make-empty-ribcage))])
                                                                                              (let
                                                                                                 ([w
                                                                                                   (call (lref make-wrap) (call (lref wrap-marks) (lref w))
                                                                                                    (#%cons (lref ribcage)
                                                                                                     (call (lref wrap-subst) (lref w))))])
                                                                                               (fix
                                                                                                 ([record-definition!
                                                                                                 (lambda (id var)
                                                                                                   (let
                                                                                                      ([mod
                                                                                                        (#%cons (const hygiene)
                                                                                                         (call (toplevel-ref module-name) (#%current-module)))])
                                                                                                    (call (lref extend-ribcage!) (lref ribcage)
                                                                                                     (lref id)
                                                                                                     (#%cons (let
                                                                                                         ([or-tmp
                                                                                                           (call (lref syntax-module) (lref id))])
                                                                                                       (if
                                                                                                         (lref or-tmp)
                                                                                                         (lref or-tmp)
                                                                                                         (lref mod)))
                                                                                                      (call (lref wrap) (lref var)
                                                                                                       (lref top-wrap)
                                                                                                       (lref mod))))))])
                                                                                                (fix
                                                                                                  ([macro-introduced-identifier?
                                                                                                  (lambda (id)
                                                                                                    (#%not (#%equal? (call (lref wrap-marks) (call (lref syntax-wrap) (lref id)))
                                                                                                      (const (top)))))])
                                                                                                 (fix
                                                                                                   ([ensure-fresh-name
                                                                                                   (lambda (var)
                                                                                                     (fix
                                                                                                       ([ribcage-has-var?
                                                                                                       (lambda (var)
                                                                                                         (fix
                                                                                                           ([loop
                                                                                                           (lambda (labels)
                                                                                                             (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%null? (lref labels))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (const false)
                                                                                                                (let
                                                                                                                    ([wrapped
                                                                                                                      (#%cdr (#%car (lref labels)))]
                                                                                                                    [labels
                                                                                                                      (#%cdr (lref labels))])
                                                                                                                  (let
                                                                                                                     ([or-tmp
                                                                                                                       (#%eq? (call (lref syntax-expression) (lref wrapped))
                                                                                                                        (lref var))])
                                                                                                                   (if
                                                                                                                     (lref or-tmp)
                                                                                                                     (lref or-tmp)
                                                                                                                     (call (lref loop) (lref labels))))))))])
                                                                                                          (call (lref loop) (call (lref ribcage-labels) (lref ribcage)))))])
                                                                                                      (fix
                                                                                                        ([loop
                                                                                                        (lambda (unique n)
                                                                                                          (if
                                                                                                            (call (lref ribcage-has-var?) (lref unique))
                                                                                                            (let
                                                                                                               ([tail
                                                                                                                 (#%string->symbol (#%number->string (lref n)))])
                                                                                                             (call (lref loop) (call (toplevel-ref symbol-append) (lref var)
                                                                                                               (const -)
                                                                                                               (lref tail))
                                                                                                              (#%+ (const 1)
                                                                                                               (lref n))))
                                                                                                            (lref unique)))])
                                                                                                       (call (lref loop) (lref var)
                                                                                                        (const 1)))))])
                                                                                                  (fix
                                                                                                    ([fresh-derived-name
                                                                                                    (lambda (id orig-form)
                                                                                                      (call (lref ensure-fresh-name) (call (toplevel-ref symbol-append) (call (lref syntax-expression) (lref id))
                                                                                                        (const -)
                                                                                                        (call (module-ref (capy)::string->symbol #t) (#%number->string (call (toplevel-ref hash) (call (toplevel-ref syntax->datum) (lref orig-form))))
                                                                                                         (const 16)))))])
                                                                                                   (fix
                                                                                                     ([parse1
                                                                                                     (lambda (x r w s m esew mod)
                                                                                                       (fix
                                                                                                         ([current-module-for-expansion
                                                                                                         (lambda (mod)
                                                                                                           (let
                                                                                                              ([cond-test-tmp
                                                                                                                (let
                                                                                                                   ([and-tmp
                                                                                                                     (#%pair? (lref mod))])
                                                                                                                 (if
                                                                                                                   (lref and-tmp)
                                                                                                                   (#%eq? (#%car (lref mod))
                                                                                                                    (const hygiene))
                                                                                                                   (const false)))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (#%cons (const hygiene)
                                                                                                               (call (toplevel-ref module-name) (#%current-module)))
                                                                                                              (lref mod))))])
                                                                                                        (receive (type value form e w s mod)
                                                                                                          (let
                                                                                                              ([mod
                                                                                                                (call (lref current-module-for-expansion) (lref mod))])
                                                                                                            (call (lref syntax-type) (lref x)
                                                                                                             (lref r)
                                                                                                             (lref w)
                                                                                                             (call (lref source-annotation) (lref x))
                                                                                                             (lref ribcage)
                                                                                                             (lref mod)
                                                                                                             (const false)))
                                                                                                          (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%eq? (lref type)
                                                                                                                 (const define-form))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (let
                                                                                                                 ([id
                                                                                                                   (call (lref wrap) (lref value)
                                                                                                                    (lref w)
                                                                                                                    (lref mod))])
                                                                                                               (let
                                                                                                                  ([var
                                                                                                                    (if
                                                                                                                      (call (lref macro-introduced-identifier?) (lref id))
                                                                                                                      (call (lref fresh-derived-name) (lref id)
                                                                                                                       (lref x))
                                                                                                                      (call (lref syntax-expression) (lref id)))])
                                                                                                                (seq
                                                                                                                 (call (lref record-definition!) (lref id)
                                                                                                                  (lref var))
                                                                                                                 (#%cons (if
                                                                                                                    (#%eq? (lref m)
                                                                                                                      (const c&e))
                                                                                                                    (let
                                                                                                                       ([x
                                                                                                                         (call (lref build-global-definition) (lref s)
                                                                                                                          (lref mod)
                                                                                                                          (lref var)
                                                                                                                          (call (lref expand) (lref e)
                                                                                                                           (lref r)
                                                                                                                           (lref w)
                                                                                                                           (lref mod)))])
                                                                                                                     (seq
                                                                                                                      (call (lref top-level-eval) (lref x)
                                                                                                                       (lref mod))
                                                                                                                      (lambda ()
                                                                                                                       (lref x))))
                                                                                                                    (receive (type* value* mod*)
                                                                                                                       (call (lref resolve-identifier) (lref id)
                                                                                                                         (lref empty-wrap)
                                                                                                                         (lref r)
                                                                                                                         (lref mod)
                                                                                                                         (const true))
                                                                                                                       (seq
                                                                                                                         (if
                                                                                                                           (#%eq? (lref type*)
                                                                                                                             (const macro))
                                                                                                                           (call (lref top-level-eval) (call (lref build-global-definition) (lref s)
                                                                                                                             (lref mod)
                                                                                                                             (lref var)
                                                                                                                             (call (lref build-void) (lref s)))
                                                                                                                            (lref mod))
                                                                                                                           (const #<undefined>))
                                                                                                                         (lambda ()
                                                                                                                          (call (lref build-global-definition) (lref s)
                                                                                                                           (lref mod)
                                                                                                                           (lref var)
                                                                                                                           (call (lref expand) (lref e)
                                                                                                                            (lref r)
                                                                                                                            (lref w)
                                                                                                                            (lref mod)))))))
                                                                                                                  (const '())))))
                                                                                                              (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (#%eq? (lref type)
                                                                                                                     (const begin-form))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (let
                                                                                                                     ([tmp
                                                                                                                       (call (toplevel-ref $sc-dispatch) (lref e)
                                                                                                                        (const (_ . each-any)))])
                                                                                                                   (if
                                                                                                                     (lref tmp)
                                                                                                                     (call (module-ref (capy)::apply #t) (lambda (e1)
                                                                                                                       (call (lref parse) (lref e1)
                                                                                                                        (lref r)
                                                                                                                        (lref w)
                                                                                                                        (lref s)
                                                                                                                        (lref m)
                                                                                                                        (lref essew)
                                                                                                                        (lref mod)))
                                                                                                                      (lref tmp))
                                                                                                                     (call (toplevel-ref syntax-violation) (const false)
                                                                                                                       (const source expansion failed to match any pattern)
                                                                                                                       (lref e))))
                                                                                                                  (let
                                                                                                                      ([cond-test-tmp
                                                                                                                        (#%eq? (lref type)
                                                                                                                         (const eval-when-form))])
                                                                                                                    (if
                                                                                                                      (lref cond-test-tmp)
                                                                                                                      (let
                                                                                                                         ([tmp-1
                                                                                                                           (lref e)])
                                                                                                                       (let
                                                                                                                          ([tmp
                                                                                                                            (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                             (const (_ each-any any . each-any)))])
                                                                                                                        (if
                                                                                                                          (lref tmp)
                                                                                                                          (call (module-ref (capy)::apply #t) (lambda (x e1 e2)
                                                                                                                            (let
                                                                                                                               ([when-list
                                                                                                                                 (call (lref parse-when-list) (lref e)
                                                                                                                                  (lref x))]
                                                                                                                               [body
                                                                                                                                 (#%cons (lref e1)
                                                                                                                                  (lref e2))])
                                                                                                                             (fix
                                                                                                                               ([recurse
                                                                                                                               (lambda (m esew)
                                                                                                                                 (call (lref parse) (lref body)
                                                                                                                                  (lref r)
                                                                                                                                  (lref w)
                                                                                                                                  (lref s)
                                                                                                                                  (lref m)
                                                                                                                                  (lref esew)
                                                                                                                                  (lref mod)))])
                                                                                                                              (let
                                                                                                                                 ([cond-test-tmp
                                                                                                                                   (#%eq? (lref m)
                                                                                                                                    (const e))])
                                                                                                                               (if
                                                                                                                                 (lref cond-test-tmp)
                                                                                                                                 (if
                                                                                                                                   (call (module-ref (capy)::memq #t) (const eval)
                                                                                                                                     (lref when-list))
                                                                                                                                   (call (lref recurse) (if
                                                                                                                                      (call (module-ref (capy)::memq #t) (const expand)
                                                                                                                                        (lref when-list))
                                                                                                                                      (const c&e)
                                                                                                                                      (const e))
                                                                                                                                    (const (eval)))
                                                                                                                                   (seq
                                                                                                                                     (if
                                                                                                                                       (call (module-ref (capy)::memq #t) (const expand)
                                                                                                                                         (lref when-list))
                                                                                                                                       (call (lref top-level-eval) (call (lref expand-top-sequence) (lref body)
                                                                                                                                         (lref r)
                                                                                                                                         (lref w)
                                                                                                                                         (lref s)
                                                                                                                                         (const e)
                                                                                                                                         (const (eval))
                                                                                                                                         (lref mod))
                                                                                                                                        (lref mod))
                                                                                                                                       (const #<undefined>))
                                                                                                                                     (const '())))
                                                                                                                                 (let
                                                                                                                                     ([cond-test-tmp
                                                                                                                                       (call (module-ref (capy)::memq #t) (const load)
                                                                                                                                        (lref when-list))])
                                                                                                                                   (if
                                                                                                                                     (lref cond-test-tmp)
                                                                                                                                     (let
                                                                                                                                        ([cond-test-tmp
                                                                                                                                          (let
                                                                                                                                             ([or-tmp
                                                                                                                                               (call (module-ref (capy)::memq #t) (const compile)
                                                                                                                                                (lref when-list))])
                                                                                                                                           (if
                                                                                                                                             (lref or-tmp)
                                                                                                                                             (lref or-tmp)
                                                                                                                                             (let
                                                                                                                                                 ([or-tmp
                                                                                                                                                   (call (module-ref (capy)::memq #t) (const expand)
                                                                                                                                                    (lref when-list))])
                                                                                                                                               (if
                                                                                                                                                 (lref or-tmp)
                                                                                                                                                 (lref or-tmp)
                                                                                                                                                 (let
                                                                                                                                                     ([and-tmp
                                                                                                                                                       (#%eq? (lref m)
                                                                                                                                                        (const c&e))])
                                                                                                                                                   (if
                                                                                                                                                     (lref and-tmp)
                                                                                                                                                     (call (module-ref (capy)::memq #t) (const eval)
                                                                                                                                                      (lref when-list))
                                                                                                                                                     (const false)))))))])
                                                                                                                                      (if
                                                                                                                                        (lref cond-test-tmp)
                                                                                                                                        (call (lref recurse) (const c&e)
                                                                                                                                         (const (compile load)))
                                                                                                                                        (let
                                                                                                                                            ([cond-test-tmp
                                                                                                                                              (let
                                                                                                                                                 ([memq-tmp
                                                                                                                                                   (#%eq? (lref m)
                                                                                                                                                    (const c))])
                                                                                                                                               (if
                                                                                                                                                 (lref memq-tmp)
                                                                                                                                                 (lref memq-tmp)
                                                                                                                                                 (#%eq? (lref m)
                                                                                                                                                   (const c&e))))])
                                                                                                                                          (if
                                                                                                                                            (lref cond-test-tmp)
                                                                                                                                            (call (lref recurse) (const c)
                                                                                                                                             (const (load)))
                                                                                                                                            (const '())))))
                                                                                                                                     (let
                                                                                                                                         ([cond-test-tmp
                                                                                                                                           (let
                                                                                                                                              ([or-tmp
                                                                                                                                                (call (module-ref (capy)::memq #t) (const compile)
                                                                                                                                                 (lref when-list))])
                                                                                                                                            (if
                                                                                                                                              (lref or-tmp)
                                                                                                                                              (lref or-tmp)
                                                                                                                                              (let
                                                                                                                                                  ([or-tmp
                                                                                                                                                    (call (module-ref (capy)::memq #t) (const expand)
                                                                                                                                                     (lref when-list))])
                                                                                                                                                (if
                                                                                                                                                  (lref or-tmp)
                                                                                                                                                  (lref or-tmp)
                                                                                                                                                  (let
                                                                                                                                                      ([and-tmp
                                                                                                                                                        (#%eq? (lref m)
                                                                                                                                                         (const c&e))])
                                                                                                                                                    (if
                                                                                                                                                      (lref and-tmp)
                                                                                                                                                      (call (module-ref (capy)::memq #t) (const eval)
                                                                                                                                                       (lref when-list))
                                                                                                                                                      (const false)))))))])
                                                                                                                                       (if
                                                                                                                                         (lref cond-test-tmp)
                                                                                                                                         (seq
                                                                                                                                          (call (lref top-level-eval) (call (lref expand-top-sequence) (lref body)
                                                                                                                                            (lref r)
                                                                                                                                            (lref w)
                                                                                                                                            (lref s)
                                                                                                                                            (const e)
                                                                                                                                            (const (eval))
                                                                                                                                            (lref mod))
                                                                                                                                           (lref mod))
                                                                                                                                          (const '()))
                                                                                                                                         (const '()))))))))))
                                                                                                                           (lref tmp))
                                                                                                                          (call (toplevel-ref syntax-violation) (const false)
                                                                                                                            (const source expression failed to match any pattern)
                                                                                                                            (lref tmp-1)))))
                                                                                                                      (let
                                                                                                                          ([cond-test-tmp
                                                                                                                            (let
                                                                                                                               ([or-tmp
                                                                                                                                 (#%eq? (lref type)
                                                                                                                                  (const define-syntax-form))])
                                                                                                                             (if
                                                                                                                               (lref or-tmp)
                                                                                                                               (lref or-tmp)
                                                                                                                               (#%eq? (lref type)
                                                                                                                                 (const define-syntax-parameter-form))))])
                                                                                                                        (if
                                                                                                                          (lref cond-test-tmp)
                                                                                                                          (let
                                                                                                                             ([id
                                                                                                                               (call (lref wrap) (lref value)
                                                                                                                                (lref w)
                                                                                                                                (lref mod))])
                                                                                                                           (let
                                                                                                                              ([var
                                                                                                                                (if
                                                                                                                                  (call (lref macro-introduced-identifier?) (lref id))
                                                                                                                                  (call (lref fresh-derived-name) (lref id)
                                                                                                                                   (lref x))
                                                                                                                                  (call (lref syntax-expression) (lref id)))])
                                                                                                                            (seq
                                                                                                                             (call (lref record-definition!) (lref id)
                                                                                                                              (lref var))
                                                                                                                             (let
                                                                                                                                ([key
                                                                                                                                  (lref m)])
                                                                                                                              (let
                                                                                                                                 ([cond-test-tmp
                                                                                                                                   (#%eqv? (lref key)
                                                                                                                                    (const c))])
                                                                                                                               (if
                                                                                                                                 (lref cond-test-tmp)
                                                                                                                                 (let
                                                                                                                                    ([cond-test-tmp
                                                                                                                                      (call (module-ref (capy)::memq #t) (const compile)
                                                                                                                                       (lref esew))])
                                                                                                                                  (if
                                                                                                                                    (lref cond-test-tmp)
                                                                                                                                    (let
                                                                                                                                       ([e
                                                                                                                                         (call (lref expand-install-global) (lref mod)
                                                                                                                                          (lref var)
                                                                                                                                          (lref type)
                                                                                                                                          (call (lref expand) (lref e)
                                                                                                                                           (lref r)
                                                                                                                                           (lref w)
                                                                                                                                           (lref mod)))])
                                                                                                                                     (seq
                                                                                                                                      (call (lref top-level-eval) (lref e)
                                                                                                                                       (lref mod))
                                                                                                                                      (if
                                                                                                                                        (call (module-ref (capy)::memq #t) (const load)
                                                                                                                                          (lref esew))
                                                                                                                                        (#%cons (lambda ()
                                                                                                                                          (lref e))
                                                                                                                                         (const '()))
                                                                                                                                        (const '()))))
                                                                                                                                    (let
                                                                                                                                        ([cond-test-tmp
                                                                                                                                          (call (module-ref (capy)::memq #t) (const load)
                                                                                                                                           (lref esew))])
                                                                                                                                      (if
                                                                                                                                        (lref cond-test-tmp)
                                                                                                                                        (#%cons (lambda ()
                                                                                                                                          (call (lref expand-install-global) (lref mod)
                                                                                                                                           (lref var)
                                                                                                                                           (lref type)
                                                                                                                                           (call (lref expand) (lref e)
                                                                                                                                            (lref r)
                                                                                                                                            (lref w)
                                                                                                                                            (lref mod))))
                                                                                                                                         (const '()))
                                                                                                                                        (const '())))))
                                                                                                                                 (let
                                                                                                                                     ([cond-test-tmp
                                                                                                                                       (#%eqv? (lref key)
                                                                                                                                        (const c&e))])
                                                                                                                                   (if
                                                                                                                                     (lref cond-test-tmp)
                                                                                                                                     (let
                                                                                                                                        ([e
                                                                                                                                          (call (lref expand-install-global) (lref mod)
                                                                                                                                           (lref var)
                                                                                                                                           (lref type)
                                                                                                                                           (call (lref expand) (lref e)
                                                                                                                                            (lref r)
                                                                                                                                            (lref w)
                                                                                                                                            (lref mod)))])
                                                                                                                                      (seq
                                                                                                                                       (call (lref top-level-eval) (lref e)
                                                                                                                                        (lref mod))
                                                                                                                                       (#%cons (lambda ()
                                                                                                                                         (lref e))
                                                                                                                                        (const '()))))
                                                                                                                                     (seq
                                                                                                                                       (if
                                                                                                                                         (call (module-ref (capy)::memq #t) (const eval)
                                                                                                                                           (lref esew))
                                                                                                                                         (call (lref top-level-eval) (call (lref expand-install-global) (lref mod)
                                                                                                                                           (lref var)
                                                                                                                                           (lref type)
                                                                                                                                           (call (lref expand) (lref e)
                                                                                                                                            (lref r)
                                                                                                                                            (lref w)
                                                                                                                                            (lref mod)))
                                                                                                                                          (lref mod))
                                                                                                                                         (const #<undefined>))
                                                                                                                                       (const '()))))))))))
                                                                                                                          (#%cons (if
                                                                                                                              (#%eq? (lref m)
                                                                                                                                (const c&e))
                                                                                                                              (let
                                                                                                                                 ([x
                                                                                                                                   (call (lref expand-expr) (lref type)
                                                                                                                                    (lref value)
                                                                                                                                    (lref form)
                                                                                                                                    (lref e)
                                                                                                                                    (lref r)
                                                                                                                                    (lref w)
                                                                                                                                    (lref s)
                                                                                                                                    (lref mod))])
                                                                                                                               (seq
                                                                                                                                (call (lref top-level-eval) (lref x)
                                                                                                                                 (lref mod))
                                                                                                                                (lambda ()
                                                                                                                                 (lref x))))
                                                                                                                              (lambda ()
                                                                                                                                (call (lref expand-expr) (lref type)
                                                                                                                                 (lref value)
                                                                                                                                 (lref form)
                                                                                                                                 (lref e)
                                                                                                                                 (lref r)
                                                                                                                                 (lref w)
                                                                                                                                 (lref s)
                                                                                                                                 (lref mod))))
                                                                                                                            (const '())))))))))))))]
                                                                                                     [parse
                                                                                                     (lambda (body r w s m esew mod)
                                                                                                       (fix
                                                                                                         ([loop
                                                                                                         (lambda (body)
                                                                                                           (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%null? (lref body))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (const '())
                                                                                                              (let
                                                                                                                  ([head
                                                                                                                    (#%car (lref body))]
                                                                                                                  [tail
                                                                                                                    (#%cdr (lref body))])
                                                                                                                (let
                                                                                                                   ([thunks
                                                                                                                     (call (lref parse1) (lref head)
                                                                                                                      (lref r)
                                                                                                                      (lref w)
                                                                                                                      (lref s)
                                                                                                                      (lref m)
                                                                                                                      (lref essew)
                                                                                                                      (lref mod))])
                                                                                                                 (#%append (lref thunks)
                                                                                                                  (call (lref loop) (lref tail))))))))])
                                                                                                        (call (lref loop) (lref body))))])
                                                                                                    (let
                                                                                                       ([res
                                                                                                         (fix
                                                                                                           ([lp
                                                                                                           (lambda (thunks)
                                                                                                             (if
                                                                                                               (#%null? (lref thunks))
                                                                                                               (const '())
                                                                                                               (#%cons (call (#%car (lref thunks)))
                                                                                                                 (call (lref lp) (#%cdr (lref thunks))))))])
                                                                                                          (call (lref lp) (call (lref parse) (lref body)
                                                                                                            (lref r)
                                                                                                            (lref w)
                                                                                                            (lref s)
                                                                                                            (lref m)
                                                                                                            (lref essew)
                                                                                                            (lref mod))))])
                                                                                                     (if
                                                                                                       (#%null? (lref res))
                                                                                                       (call (lref build-void) (lref s))
                                                                                                       (call (lref build-sequence) (lref s)
                                                                                                         (lref res)))))))))))))])
                                                                                         (fix
                                                                                           ([ellipsis?
                                                                                           (lambda (e r mod)
                                                                                             (let
                                                                                                ([and-tmp
                                                                                                  (call (lref nonsymbol-id?) (lref e))])
                                                                                              (if
                                                                                                (lref and-tmp)
                                                                                                (receive (type value mod)
                                                                                                  (call (lref resolve-identifier) (call (lref make-syntax) (const $sc-ellipsis)
                                                                                                     (call (lref syntax-wrap) (lref e))
                                                                                                     (let
                                                                                                        ([or-tmp
                                                                                                          (call (lref syntax-module) (lref e))])
                                                                                                      (if
                                                                                                        (lref or-tmp)
                                                                                                        (lref or-tmp)
                                                                                                        (lref mod)))
                                                                                                     (const false))
                                                                                                    (lref empty-wrap)
                                                                                                    (lref r)
                                                                                                    (lref mod)
                                                                                                    (const false))
                                                                                                  (if
                                                                                                     (#%eq? (lref type)
                                                                                                       (const ellipsis))
                                                                                                     (call (lref bound-id=?) (lref e)
                                                                                                      (lref value))
                                                                                                     (call (lref free-id=?) (lref e)
                                                                                                       (call (lref make-syntax) (const ...)
                                                                                                        (const ((top)))
                                                                                                        (const (hygiene capy))))))
                                                                                                (const false))))])
                                                                                          (fix
                                                                                            ([lambda-formals
                                                                                            (lambda (orig-args)
                                                                                              (fix
                                                                                                ([check
                                                                                                (lambda (req rest)
                                                                                                  (if
                                                                                                    (call (lref distinct-bound-ids?) (if
                                                                                                        (lref rest)
                                                                                                        (#%cons (lref rest)
                                                                                                         (lref req))
                                                                                                        (lref req)))
                                                                                                    (values (lref req) (const false) (lref rest) (const false))
                                                                                                    (call (toplevel-ref syntax-violation) (const lambda)
                                                                                                      (const duplicate identifier in argument list)
                                                                                                      (lref orig-args))))])
                                                                                               (fix
                                                                                                 ([req
                                                                                                 (lambda (args rreq)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (lref args)])
                                                                                                    (let
                                                                                                       ([tmp-1
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                          (const '()))])
                                                                                                     (if
                                                                                                       (lref tmp-1)
                                                                                                       (call (module-ref (capy)::apply #t) (lambda ()
                                                                                                         (call (lref check) (call (toplevel-ref reverse) (lref rreq))
                                                                                                          (const false)))
                                                                                                        (lref tmp-1))
                                                                                                       (let
                                                                                                           ([tmp-1
                                                                                                             (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                              (const (any . any)))])
                                                                                                         (if
                                                                                                           (let
                                                                                                               ([and-tmp
                                                                                                                 (lref tmp-1)])
                                                                                                             (if
                                                                                                               (lref and-tmp)
                                                                                                               (call (module-ref (capy)::apply #t) (lambda (a b)
                                                                                                                 (call (lref id?) (lref a)))
                                                                                                                (lref tmp-1))
                                                                                                               (const false)))
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (a b)
                                                                                                             (call (lref req) (lref b)
                                                                                                              (#%cons (lref a)
                                                                                                               (lref rreq))))
                                                                                                            (lref tmp-1))
                                                                                                           (let
                                                                                                               ([tmp-1
                                                                                                                 (#%cons (lref tmp)
                                                                                                                  (const '()))])
                                                                                                             (if
                                                                                                               (let
                                                                                                                   ([and-tmp
                                                                                                                     (lref tmp-1)])
                                                                                                                 (if
                                                                                                                   (lref and-tmp)
                                                                                                                   (call (module-ref (capy)::apply #t) (lambda (r)
                                                                                                                     (call (lref id?) (lref r)))
                                                                                                                    (lref tmp-1))
                                                                                                                   (const false)))
                                                                                                               (call (module-ref (capy)::apply #t) (lambda (r)
                                                                                                                 (call (lref check) (call (toplevel-ref reverse) (lref rreq))
                                                                                                                  (lref r)))
                                                                                                                (lref tmp-1))
                                                                                                               (let
                                                                                                                   ([else
                                                                                                                     (lref tmp)])
                                                                                                                 (call (toplevel-ref syntax-violation) (const lambda)
                                                                                                                  (const invalid argument list)
                                                                                                                  (lref orig-args)
                                                                                                                  (lref args)))))))))))])
                                                                                                (call (lref req) (lref orig-args)
                                                                                                 (const '())))))])
                                                                                           (let
                                                                                              ([nil
                                                                                                (#%cons (const nil)
                                                                                                 (const '()))])
                                                                                            (let
                                                                                               ([no-source
                                                                                                 (const false)])
                                                                                             (let
                                                                                                ([null-env
                                                                                                  (const '())])
                                                                                              (let
                                                                                                 ([expand-syntax
                                                                                                   (fix
                                                                                                     ([gen-vector
                                                                                                     (lambda (x)
                                                                                                       (let
                                                                                                          ([cond-test-tmp
                                                                                                            (#%eq? (#%car (lref x))
                                                                                                             (const list))])
                                                                                                        (if
                                                                                                          (lref cond-test-tmp)
                                                                                                          (#%cons (const vector)
                                                                                                           (#%cdr (lref x)))
                                                                                                          (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%eq? (#%car (lref x))
                                                                                                                 (const quote))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (#%cons (const quote)
                                                                                                               (#%cons (call (toplevel-ref list->vector) (#%car (#%cdr (lref x))))
                                                                                                                (const '())))
                                                                                                              (#%cons (const list->vector)
                                                                                                                (#%cons (lref x)
                                                                                                                 (const '()))))))))])
                                                                                                    (fix
                                                                                                      ([gen-append
                                                                                                      (lambda (x y)
                                                                                                        (if
                                                                                                          (#%equal? (lref y)
                                                                                                            (const (quote '())))
                                                                                                          (lref x)
                                                                                                          (#%cons (const append)
                                                                                                            (#%cons (lref x)
                                                                                                             (#%cons (lref y)
                                                                                                              (const '()))))))])
                                                                                                     (fix
                                                                                                       ([gen-cons
                                                                                                       (lambda (x y)
                                                                                                         (let
                                                                                                            ([key
                                                                                                              (#%car (lref y))])
                                                                                                          (let
                                                                                                             ([cond-test-tmp
                                                                                                               (#%eqv? (lref key)
                                                                                                                (const quote))])
                                                                                                           (if
                                                                                                             (lref cond-test-tmp)
                                                                                                             (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%eq? (#%car (lref x))
                                                                                                                   (const quote))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (#%cons (const quote)
                                                                                                                 (#%cons (#%cons (#%car (#%cdr (lref x)))
                                                                                                                   (#%car (#%cdr (lref y))))
                                                                                                                  (const '())))
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (#%eq? (#%car (#%cdr (lref y)))
                                                                                                                       (const '()))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (#%cons (const list)
                                                                                                                     (#%cons (lref x)
                                                                                                                      (const '())))
                                                                                                                    (#%cons (const cons)
                                                                                                                      (#%cons (lref x)
                                                                                                                       (#%cons (lref y)
                                                                                                                        (const '()))))))))
                                                                                                             (let
                                                                                                                 ([cond-test-tmp
                                                                                                                   (#%eqv? (lref key)
                                                                                                                    (const list))])
                                                                                                               (if
                                                                                                                 (lref cond-test-tmp)
                                                                                                                 (#%cons (const list)
                                                                                                                  (#%cons (lref x)
                                                                                                                   (#%cdr (lref y))))
                                                                                                                 (#%cons (const cons)
                                                                                                                   (#%cons (lref x)
                                                                                                                    (#%cons (lref y)
                                                                                                                     (const '()))))))))))])
                                                                                                      (fix
                                                                                                        ([gen-map
                                                                                                        (lambda (e map-env)
                                                                                                          (let
                                                                                                             ([formals
                                                                                                               (call (toplevel-ref map) #%cdr
                                                                                                                (lref map-env))]
                                                                                                             [actuals
                                                                                                               (call (toplevel-ref map) (lambda (x)
                                                                                                                 (#%cons (const ref)
                                                                                                                  (#%cons (#%car (lref x))
                                                                                                                   (const '()))))
                                                                                                                (lref map-env))])
                                                                                                           (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%eq? (#%car (lref e))
                                                                                                                 (const ref))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (#%car (lref actuals))
                                                                                                              (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (call (toplevel-ref and-map) (lambda (x)
                                                                                                                      (let
                                                                                                                         ([and-tmp
                                                                                                                           (#%eq? (#%car (lref x))
                                                                                                                            (const ref))])
                                                                                                                       (if
                                                                                                                         (lref and-tmp)
                                                                                                                         (call (module-ref (capy)::memq #t) (#%car (#%cdr (lref x)))
                                                                                                                          (lref formals))
                                                                                                                         (const false))))
                                                                                                                     (#%cdr (lref e)))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (#%cons (const map)
                                                                                                                   (#%cons (#%cons (const primitive)
                                                                                                                     (#%cons (#%car (lref e))
                                                                                                                      (const '())))
                                                                                                                    (call (toplevel-ref map) (let
                                                                                                                        ([r
                                                                                                                          (call (toplevel-ref map) #%cons
                                                                                                                           (lref formals)
                                                                                                                           (lref actuals))])
                                                                                                                      (lambda (x)
                                                                                                                       (#%cdr (call (toplevel-ref assq) (#%car (#%cdr (lref x)))
                                                                                                                         (lref r)))))
                                                                                                                     (#%cdr (lref e)))))
                                                                                                                  (#%cons (const map)
                                                                                                                    (#%cons (#%cons (const lambda)
                                                                                                                      (#%cons (lref formals)
                                                                                                                       (#%cons (lref e)
                                                                                                                        (const '()))))
                                                                                                                     (lref actuals)))))))))])
                                                                                                       (fix
                                                                                                         ([gen-mappend
                                                                                                         (lambda (e map-env)
                                                                                                           (#%cons (const apply)
                                                                                                            (#%cons (const (primitive append))
                                                                                                             (#%cons (call (lref gen-map) (lref e)
                                                                                                               (lref map-env))
                                                                                                              (const '())))))])
                                                                                                        (fix
                                                                                                          ([gen-ref
                                                                                                          (lambda (src var level maps)
                                                                                                            (let
                                                                                                               ([cond-test-tmp
                                                                                                                 (#%= (lref level)
                                                                                                                  (const 0))])
                                                                                                             (if
                                                                                                               (lref cond-test-tmp)
                                                                                                               (values (lref var) (lref maps))
                                                                                                               (let
                                                                                                                   ([cond-test-tmp
                                                                                                                     (#%null? (lref maps))])
                                                                                                                 (if
                                                                                                                   (lref cond-test-tmp)
                                                                                                                   (call (toplevel-ref syntax-violation) (const syntax)
                                                                                                                    (const missing ellipsis)
                                                                                                                    (lref src))
                                                                                                                   (receive (outer-var outer-maps)
                                                                                                                      (call (lref gen-ref) (lref src)
                                                                                                                        (lref var)
                                                                                                                        (#%- (lref level)
                                                                                                                         (const 1))
                                                                                                                        (#%cdr (lref maps)))
                                                                                                                      (let
                                                                                                                          ([b
                                                                                                                            (call (toplevel-ref assq) (lref outer-var)
                                                                                                                             (#%car (lref maps)))])
                                                                                                                        (if
                                                                                                                          (lref b)
                                                                                                                          (values (#%cdr (lref b)) (lref maps))
                                                                                                                          (let
                                                                                                                              ([inner-var
                                                                                                                                (call (lref gen-var) (const tmp))])
                                                                                                                            (values (lref inner-var) (#%cons (#%cons (#%cons (lref outer-var)
                                                                                                                               (lref inner-var))
                                                                                                                              (#%car (lref maps)))
                                                                                                                             (lref outer-maps))))))))))))])
                                                                                                         (fix
                                                                                                           ([gen-syntax
                                                                                                           (lambda (src e r maps ellipsis? mod)
                                                                                                             (if
                                                                                                               (call (lref id?) (lref e))
                                                                                                               (receive (type value mod)
                                                                                                                 (call (lref resolve-identifier) (lref e)
                                                                                                                   (lref empty-wrap)
                                                                                                                   (lref r)
                                                                                                                   (lref mod)
                                                                                                                   (const false))
                                                                                                                 (let
                                                                                                                     ([key
                                                                                                                       (lref type)])
                                                                                                                   (let
                                                                                                                      ([cond-test-tmp
                                                                                                                        (#%eqv? (lref key)
                                                                                                                         (const syntax))])
                                                                                                                    (if
                                                                                                                      (lref cond-test-tmp)
                                                                                                                      (receive (var maps)
                                                                                                                        (call (lref gen-ref) (lref src)
                                                                                                                          (#%car (lref value))
                                                                                                                          (#%cdr (lref value))
                                                                                                                          (lref maps))
                                                                                                                        (values (#%cons (const ref)
                                                                                                                          (#%cons (lref var)
                                                                                                                           (const '()))) (lref maps)))
                                                                                                                      (let
                                                                                                                          ([cond-test-tmp
                                                                                                                            (call (lref ellipsis?) (lref e)
                                                                                                                             (lref r)
                                                                                                                             (lref mod))])
                                                                                                                        (if
                                                                                                                          (lref cond-test-tmp)
                                                                                                                          (call (toplevel-ref syntax-violation) (const syntax)
                                                                                                                           (const misplaced ellipsis)
                                                                                                                           (lref src))
                                                                                                                          (values (#%cons (const quote)
                                                                                                                            (#%cons (lref e)
                                                                                                                             (const '()))) (lref maps))))))))
                                                                                                               (let
                                                                                                                   ([tmp
                                                                                                                     (lref e)])
                                                                                                                 (let
                                                                                                                    ([tmp-1
                                                                                                                      (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                       (const (any any)))])
                                                                                                                  (if
                                                                                                                    (let
                                                                                                                        ([and-tmp
                                                                                                                          (lref tmp-1)])
                                                                                                                      (if
                                                                                                                        (lref and-tmp)
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (dots e)
                                                                                                                          (call (lref ellipsis?) (lref dots)
                                                                                                                           (lref r)
                                                                                                                           (lref mod)))
                                                                                                                         (lref tmp-1))
                                                                                                                        (const false)))
                                                                                                                    (call (module-ref (capy)::apply #t) (lambda (dots e)
                                                                                                                      (call (lref gen-syntax) (lref src)
                                                                                                                       (lref e)
                                                                                                                       (lref r)
                                                                                                                       (lref maps)
                                                                                                                       (lambda (e r mod)
                                                                                                                        (const false))
                                                                                                                       (lref mod)))
                                                                                                                     (lref tmp-1))
                                                                                                                    (let
                                                                                                                        ([tmp-1
                                                                                                                          (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                           (const (any any . any)))])
                                                                                                                      (if
                                                                                                                        (let
                                                                                                                            ([and-tmp
                                                                                                                              (lref tmp-1)])
                                                                                                                          (if
                                                                                                                            (lref and-tmp)
                                                                                                                            (call (module-ref (capy)::apply #t) (lambda (x dots y)
                                                                                                                              (call (lref ellipsis?) (lref dots)
                                                                                                                               (lref r)
                                                                                                                               (lref mod)))
                                                                                                                             (lref tmp-1))
                                                                                                                            (const false)))
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (x dots y)
                                                                                                                          (fix
                                                                                                                            ([f
                                                                                                                            (lambda (y k)
                                                                                                                              (let
                                                                                                                                 ([tmp
                                                                                                                                   (lref y)])
                                                                                                                               (let
                                                                                                                                  ([tmp
                                                                                                                                    (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                     (const (any . any)))])
                                                                                                                                (if
                                                                                                                                  (let
                                                                                                                                      ([and-tmp
                                                                                                                                        (lref tmp)])
                                                                                                                                    (if
                                                                                                                                      (lref and-tmp)
                                                                                                                                      (call (module-ref (capy)::apply #t) (lambda (dots y)
                                                                                                                                        (call (lref ellipsis?) (lref dots)
                                                                                                                                         (lref r)
                                                                                                                                         (lref mod)))
                                                                                                                                       (lref tmp))
                                                                                                                                      (const false)))
                                                                                                                                  (call (module-ref (capy)::apply #t) (lambda (dots y)
                                                                                                                                    (call (lref f) (lref y)
                                                                                                                                     (lambda (maps)
                                                                                                                                      (receive (x maps)
                                                                                                                                        (call (lref k) (#%cons (const '())
                                                                                                                                           (lref maps)))
                                                                                                                                        (if
                                                                                                                                           (#%null? (#%car (lref maps)))
                                                                                                                                           (call (toplevel-ref syntax-violation) (const syntax)
                                                                                                                                            (const extra ellipsis)
                                                                                                                                            (lref src))
                                                                                                                                           (values (call (lref gen-mappend) (lref x)
                                                                                                                                             (#%car (lref maps))) (#%cdr (lref maps))))))))
                                                                                                                                   (lref tmp))
                                                                                                                                  (receive (y maps)
                                                                                                                                     (call (lref gen-syntax) (lref src)
                                                                                                                                       (lref y)
                                                                                                                                       (lref r)
                                                                                                                                       (lref maps)
                                                                                                                                       (lref ellipsis?)
                                                                                                                                       (lref mod))
                                                                                                                                     (receive (x maps)
                                                                                                                                        (call (lref k) (lref maps))
                                                                                                                                        (values (call (lref gen-append) (lref x)
                                                                                                                                          (lref y)) (lref maps))))))))])
                                                                                                                           (call (lref f) (lref y)
                                                                                                                            (lambda (maps)
                                                                                                                             (receive (x maps)
                                                                                                                               (call (lref gen-syntax) (lref src)
                                                                                                                                 (lref x)
                                                                                                                                 (lref r)
                                                                                                                                 (#%cons (const '())
                                                                                                                                  (lref maps))
                                                                                                                                 (lref ellipsis?)
                                                                                                                                 (lref mod))
                                                                                                                               (if
                                                                                                                                  (#%null? (#%car (lref maps)))
                                                                                                                                  (call (toplevel-ref syntax-violation) (const syntax)
                                                                                                                                   (const extra ellipsis)
                                                                                                                                   (lref src))
                                                                                                                                  (values (call (lref gen-map) (lref x)
                                                                                                                                    (#%car (lref maps))) (#%cdr (lref maps)))))))))
                                                                                                                         (lref tmp-1))
                                                                                                                        (let
                                                                                                                            ([tmp-1
                                                                                                                              (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                               (const (any . any)))])
                                                                                                                          (if
                                                                                                                            (lref tmp-1)
                                                                                                                            (call (module-ref (capy)::apply #t) (lambda (x y)
                                                                                                                              (receive (x maps)
                                                                                                                                (call (lref gen-syntax) (lref src)
                                                                                                                                  (lref x)
                                                                                                                                  (lref r)
                                                                                                                                  (lref maps)
                                                                                                                                  (lref ellipsis?)
                                                                                                                                  (lref mod))
                                                                                                                                (receive (y maps)
                                                                                                                                   (call (lref gen-syntax) (lref src)
                                                                                                                                     (lref y)
                                                                                                                                     (lref r)
                                                                                                                                     (lref maps)
                                                                                                                                     (lref ellipsis?)
                                                                                                                                     (lref mod))
                                                                                                                                   (values (call (lref gen-cons) (lref x)
                                                                                                                                     (lref y)) (lref maps)))))
                                                                                                                             (lref tmp-1))
                                                                                                                            (let
                                                                                                                                ([tmp-1
                                                                                                                                  (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                   (const #(vector (any . each-any))))])
                                                                                                                              (if
                                                                                                                                (lref tmp-1)
                                                                                                                                (call (module-ref (capy)::apply #t) (lambda (e1 e2)
                                                                                                                                  (receive (e maps)
                                                                                                                                    (call (lref gen-syntax) (lref src)
                                                                                                                                      (#%cons (lref e1)
                                                                                                                                       (lref e2))
                                                                                                                                      (lref r)
                                                                                                                                      (lref maps)
                                                                                                                                      (lref ellipsis?)
                                                                                                                                      (lref mod))
                                                                                                                                    (values (call (lref gen-vector) (lref e)) (lref maps))))
                                                                                                                                 (lref tmp-1))
                                                                                                                                (let
                                                                                                                                    ([tmp-1
                                                                                                                                      (#%cons (lref tmp)
                                                                                                                                       (const '()))])
                                                                                                                                  (if
                                                                                                                                    (let
                                                                                                                                        ([and-tmp
                                                                                                                                          (lref tmp-1)])
                                                                                                                                      (if
                                                                                                                                        (lref and-tmp)
                                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (x)
                                                                                                                                          (#%eq? (call (toplevel-ref syntax->datum) (lref x))
                                                                                                                                           (lref nil)))
                                                                                                                                         (lref tmp-1))
                                                                                                                                        (const false)))
                                                                                                                                    (call (module-ref (capy)::apply #t) (lambda (x)
                                                                                                                                      (values (lref nil) (lref maps)))
                                                                                                                                     (lref tmp-1))
                                                                                                                                    (let
                                                                                                                                        ([tmp
                                                                                                                                          (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                           (const '()))])
                                                                                                                                      (if
                                                                                                                                        (lref tmp)
                                                                                                                                        (call (module-ref (capy)::apply #t) (lambda ()
                                                                                                                                          (values (const (quote '())) (lref maps)))
                                                                                                                                         (lref tmp))
                                                                                                                                        (values (#%cons (const quote)
                                                                                                                                          (#%cons (lref e)
                                                                                                                                           (const '()))) (lref maps)))))))))))))))))])
                                                                                                          (fix
                                                                                                            ([regen
                                                                                                            (lambda (x)
                                                                                                              (let
                                                                                                                 ([key
                                                                                                                   (#%car (lref x))])
                                                                                                               (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (#%eqv? (lref key)
                                                                                                                     (const ref))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (call (lref build-lexical-reference) (lref no-source)
                                                                                                                   (#%car (#%cdr (lref x)))
                                                                                                                   (#%car (#%cdr (lref x))))
                                                                                                                  (let
                                                                                                                      ([cond-test-tmp
                                                                                                                        (#%eqv? (lref key)
                                                                                                                         (const primitive))])
                                                                                                                    (if
                                                                                                                      (lref cond-test-tmp)
                                                                                                                      (call (lref build-primref) (lref no-source)
                                                                                                                       (#%car (#%cdr (lref x))))
                                                                                                                      (let
                                                                                                                          ([cond-test-tmp
                                                                                                                            (#%eqv? (lref key)
                                                                                                                             (const quote))])
                                                                                                                        (if
                                                                                                                          (lref cond-test-tmp)
                                                                                                                          (call (lref build-data) (lref no-source)
                                                                                                                           (#%car (#%cdr (lref x))))
                                                                                                                          (let
                                                                                                                              ([cond-test-tmp
                                                                                                                                (#%eqv? (lref key)
                                                                                                                                 (const lambda))])
                                                                                                                            (if
                                                                                                                              (lref cond-test-tmp)
                                                                                                                              (if
                                                                                                                                (#%list? (#%car (#%cdr (lref x))))
                                                                                                                                (call (lref build-simple-lambda) (lref no-source)
                                                                                                                                 (#%car (#%cdr (lref x)))
                                                                                                                                 (#%car (#%cdr (lref x)))
                                                                                                                                 (const '())
                                                                                                                                 (call (lref regen) (#%car (#%cdr (#%cdr (lref x))))))
                                                                                                                                (call (toplevel-ref error) (const how did we get here)
                                                                                                                                  (lref x)))
                                                                                                                              (call (lref build-primcall) (lref no-source)
                                                                                                                                (#%car (lref x))
                                                                                                                                (call (toplevel-ref map) (lref regen)
                                                                                                                                 (#%cdr (lref x))))))))))))))])
                                                                                                           (lambda (e r w s mod)
                                                                                                            (let
                                                                                                               ([e
                                                                                                                 (call (lref source-wrap) (lref e)
                                                                                                                  (lref w)
                                                                                                                  (lref s)
                                                                                                                  (lref mod))])
                                                                                                             (let
                                                                                                                ([tmp
                                                                                                                  (lref e)])
                                                                                                              (let
                                                                                                                 ([tmp
                                                                                                                   (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                    (const (_ any)))])
                                                                                                               (if
                                                                                                                 (lref tmp)
                                                                                                                 (call (module-ref (capy)::apply #t) (lambda (x)
                                                                                                                   (receive (e maps)
                                                                                                                     (call (lref gen-syntax) (lref e)
                                                                                                                       (lref x)
                                                                                                                       (lref r)
                                                                                                                       (const '())
                                                                                                                       (lref ellipsis?)
                                                                                                                       (lref mod))
                                                                                                                     (call (lref regen) (lref e))))
                                                                                                                  (lref tmp))
                                                                                                                 (call (toplevel-ref syntax-violation) (const syntax)
                                                                                                                   (const bad `syntax' form)
                                                                                                                   (lref e)))))))))))))))])
                                                                                               (let
                                                                                                  ([expand-syntax-case
                                                                                                    (fix
                                                                                                      ([convert-pattern
                                                                                                      (lambda (pattern keys ellipsis?)
                                                                                                        (fix
                                                                                                          ([v-reverse
                                                                                                          (lambda (x)
                                                                                                            (fix
                                                                                                              ([loop
                                                                                                              (lambda (r x)
                                                                                                                (if
                                                                                                                  (#%not (#%pair? (lref x)))
                                                                                                                  (values (lref r) (lref x))
                                                                                                                  (call (lref loop) (#%cons (#%car (lref x))
                                                                                                                     (lref r))
                                                                                                                    (#%cdr (lref x)))))])
                                                                                                             (call (lref loop) (const '())
                                                                                                              (lref x))))])
                                                                                                         (fix
                                                                                                           ([cvt
                                                                                                           (lambda (p n ids)
                                                                                                             (if
                                                                                                               (call (lref id?) (lref p))
                                                                                                               (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (call (lref bound-id-member?) (lref p)
                                                                                                                     (lref keys))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (values (let
                                                                                                                     ([vec
                                                                                                                       (#%make-vector (const 2)
                                                                                                                        (const #<undefined>))])
                                                                                                                   (seq
                                                                                                                    (#%vector-set! (lref vec)
                                                                                                                     (const 0)
                                                                                                                     (const free-id))
                                                                                                                    (#%vector-set! (lref vec)
                                                                                                                     (const 1)
                                                                                                                     (lref p))
                                                                                                                    (lref vec))) (lref ids))
                                                                                                                  (let
                                                                                                                      ([cond-test-tmp
                                                                                                                        (call (lref free-id=?) (lref p)
                                                                                                                         (call (lref make-syntax) (const _)
                                                                                                                          (const ((top)))
                                                                                                                          (const (hygiene capy))))])
                                                                                                                    (if
                                                                                                                      (lref cond-test-tmp)
                                                                                                                      (values (const _) (lref ids))
                                                                                                                      (values (const any) (#%cons (#%cons (lref p)
                                                                                                                         (lref n))
                                                                                                                        (lref ids)))))))
                                                                                                               (let
                                                                                                                   ([tmp
                                                                                                                     (lref p)])
                                                                                                                 (let
                                                                                                                    ([tmp-1
                                                                                                                      (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                       (const (any any)))])
                                                                                                                  (if
                                                                                                                    (let
                                                                                                                        ([and-tmp
                                                                                                                          (lref tmp-1)])
                                                                                                                      (if
                                                                                                                        (lref and-tmp)
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (x dots)
                                                                                                                          (call (lref ellipsis?) (lref dots)))
                                                                                                                         (lref tmp-1))
                                                                                                                        (const false)))
                                                                                                                    (call (module-ref (capy)::apply #t) (lambda (x dots)
                                                                                                                      (receive (p ids)
                                                                                                                        (call (lref cvt) (lref x)
                                                                                                                          (#%+ (const 1)
                                                                                                                           (lref n))
                                                                                                                          (lref ids))
                                                                                                                        (values (if
                                                                                                                           (#%eq? (lref p)
                                                                                                                             (const any))
                                                                                                                           (const each-any)
                                                                                                                           (let
                                                                                                                               ([vec
                                                                                                                                 (#%make-vector (const 2)
                                                                                                                                  (const #<undefined>))])
                                                                                                                             (seq
                                                                                                                              (#%vector-set! (lref vec)
                                                                                                                               (const 0)
                                                                                                                               (const each))
                                                                                                                              (#%vector-set! (lref vec)
                                                                                                                               (const 1)
                                                                                                                               (lref p))
                                                                                                                              (lref vec)))) (lref ids))))
                                                                                                                     (lref tmp-1))
                                                                                                                    (let
                                                                                                                        ([tmp-1
                                                                                                                          (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                           (const (any any . any)))])
                                                                                                                      (if
                                                                                                                        (let
                                                                                                                            ([and-tmp
                                                                                                                              (lref tmp-1)])
                                                                                                                          (if
                                                                                                                            (lref and-tmp)
                                                                                                                            (call (module-ref (capy)::apply #t) (lambda (x dots ys)
                                                                                                                              (call (lref ellipsis?) (lref dots)))
                                                                                                                             (lref tmp-1))
                                                                                                                            (const false)))
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (x dots ys)
                                                                                                                          (receive (ys ids)
                                                                                                                            (call (lref cvt*) (lref ys)
                                                                                                                              (lref n)
                                                                                                                              (lref ids))
                                                                                                                            (receive (x ids)
                                                                                                                               (call (lref cvt) (lref x)
                                                                                                                                 (#%+ (lref n)
                                                                                                                                  (const 1))
                                                                                                                                 (lref ids))
                                                                                                                               (receive (ys e)
                                                                                                                                  (call (lref v-reverse) (lref ys))
                                                                                                                                  (values (let
                                                                                                                                      ([vec
                                                                                                                                        (#%make-vector (const 4)
                                                                                                                                         (const #<undefined>))])
                                                                                                                                    (seq
                                                                                                                                     (#%vector-set! (lref vec)
                                                                                                                                      (const 0)
                                                                                                                                      (const each+))
                                                                                                                                     (#%vector-set! (lref vec)
                                                                                                                                      (const 1)
                                                                                                                                      (lref x))
                                                                                                                                     (#%vector-set! (lref vec)
                                                                                                                                      (const 2)
                                                                                                                                      (lref ys))
                                                                                                                                     (#%vector-set! (lref vec)
                                                                                                                                      (const 3)
                                                                                                                                      (lref e))
                                                                                                                                     (lref vec))) (lref ids))))))
                                                                                                                         (lref tmp-1))
                                                                                                                        (let
                                                                                                                            ([tmp-1
                                                                                                                              (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                               (const (any . any)))])
                                                                                                                          (if
                                                                                                                            (lref tmp-1)
                                                                                                                            (call (module-ref (capy)::apply #t) (lambda (x y)
                                                                                                                              (receive (y ids)
                                                                                                                                (call (lref cvt) (lref y)
                                                                                                                                  (lref n)
                                                                                                                                  (lref ids))
                                                                                                                                (receive (x ids)
                                                                                                                                   (call (lref cvt) (lref x)
                                                                                                                                     (lref n)
                                                                                                                                     (lref ids))
                                                                                                                                   (values (#%cons (lref x)
                                                                                                                                     (lref y)) (lref ids)))))
                                                                                                                             (lref tmp-1))
                                                                                                                            (let
                                                                                                                                ([tmp-1
                                                                                                                                  (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                   (const '()))])
                                                                                                                              (if
                                                                                                                                (lref tmp-1)
                                                                                                                                (call (module-ref (capy)::apply #t) (lambda ()
                                                                                                                                  (values (const '()) (lref ids)))
                                                                                                                                 (lref tmp-1))
                                                                                                                                (let
                                                                                                                                    ([tmp-1
                                                                                                                                      (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                       (const #(vector each-any)))])
                                                                                                                                  (if
                                                                                                                                    (lref tmp-1)
                                                                                                                                    (call (module-ref (capy)::apply #t) (lambda (x)
                                                                                                                                      (receive (p ids)
                                                                                                                                        (call (lref cvt) (lref x)
                                                                                                                                          (lref n)
                                                                                                                                          (lref ids))
                                                                                                                                        (values (let
                                                                                                                                            ([vec
                                                                                                                                              (#%make-vector (const 2)
                                                                                                                                               (const #<undefined>))])
                                                                                                                                          (seq
                                                                                                                                           (#%vector-set! (lref vec)
                                                                                                                                            (const 0)
                                                                                                                                            (const vector))
                                                                                                                                           (#%vector-set! (lref vec)
                                                                                                                                            (const 1)
                                                                                                                                            (lref p))
                                                                                                                                           (lref vec))) (lref ids))))
                                                                                                                                     (lref tmp-1))
                                                                                                                                    (let
                                                                                                                                        ([x
                                                                                                                                          (lref tmp)])
                                                                                                                                      (values (let
                                                                                                                                         ([vec
                                                                                                                                           (#%make-vector (const 2)
                                                                                                                                            (const #<undefined>))])
                                                                                                                                       (seq
                                                                                                                                        (#%vector-set! (lref vec)
                                                                                                                                         (const 0)
                                                                                                                                         (const atom))
                                                                                                                                        (#%vector-set! (lref vec)
                                                                                                                                         (const 1)
                                                                                                                                         (call (lref strip) (lref p)))
                                                                                                                                        (lref vec))) (lref ids))))))))))))))))]
                                                                                                           [cvt*
                                                                                                           (lambda (p* n ids)
                                                                                                             (let
                                                                                                                ([tmp
                                                                                                                  (lref p*)])
                                                                                                              (let
                                                                                                                 ([tmp
                                                                                                                   (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                    (const (any . any)))])
                                                                                                               (if
                                                                                                                 (lref tmp)
                                                                                                                 (call (module-ref (capy)::apply #t) (lambda (x y)
                                                                                                                   (receive (y ids)
                                                                                                                     (call (lref cvt*) (lref y)
                                                                                                                       (lref n)
                                                                                                                       (lref ids))
                                                                                                                     (receive (x ids)
                                                                                                                        (call (lref cvt) (lref x)
                                                                                                                          (lref n)
                                                                                                                          (lref ids))
                                                                                                                        (values (#%cons (lref x)
                                                                                                                          (lref y)) (lref ids)))))
                                                                                                                  (lref tmp))
                                                                                                                 (call (lref cvt) (lref p*)
                                                                                                                   (lref n)
                                                                                                                   (lref ids))))))])
                                                                                                          (call (lref cvt) (lref pattern)
                                                                                                           (const 0)
                                                                                                           (const '())))))])
                                                                                                     (fix
                                                                                                       ([build-dispatch-call
                                                                                                       (lambda (pvars exp y r mod)
                                                                                                         (let
                                                                                                            ([ids
                                                                                                              (call (toplevel-ref map) #%car
                                                                                                               (lref pvars))]
                                                                                                            [levels
                                                                                                              (call (toplevel-ref map) #%cdr
                                                                                                               (lref pvars))])
                                                                                                          (let
                                                                                                             ([labels
                                                                                                               (call (lref gen-labels) (lref ids))]
                                                                                                             [new-vars
                                                                                                               (call (toplevel-ref map) (lref gen-var)
                                                                                                                (lref ids))])
                                                                                                           (call (lref build-primcall) (lref no-source)
                                                                                                            (const apply)
                                                                                                            (#%cons (call (lref build-simple-lambda) (lref no-source)
                                                                                                              (call (toplevel-ref map) (toplevel-ref syntax->datum)
                                                                                                               (lref ids))
                                                                                                              (lref new-vars)
                                                                                                              (const '())
                                                                                                              (call (lref expand) (lref exp)
                                                                                                               (call (lref extend-env) (lref labels)
                                                                                                                (call (toplevel-ref map) (lambda (var level)
                                                                                                                  (#%cons (const syntax)
                                                                                                                   (#%cons (lref var)
                                                                                                                    (lref level))))
                                                                                                                 (lref new-vars)
                                                                                                                 (call (toplevel-ref map) #%cdr
                                                                                                                  (lref pvars)))
                                                                                                                (lref r))
                                                                                                               (call (lref make-binding-wrap) (lref ids)
                                                                                                                (lref labels)
                                                                                                                (lref empty-wrap))
                                                                                                               (lref mod)))
                                                                                                             (#%cons (lref y)
                                                                                                              (const '())))))))])
                                                                                                      (fix
                                                                                                        ([gen-syntax-case
                                                                                                        (lambda (x keys clauses r mod)
                                                                                                          (if
                                                                                                            (#%null? (lref clauses))
                                                                                                            (call (lref build-primcall) (lref no-source)
                                                                                                             (const syntax-violation)
                                                                                                             (#%cons (call (lref build-data) (lref no-source)
                                                                                                               (const false))
                                                                                                              (#%cons (call (lref build-data) (lref no-source)
                                                                                                                (const source expression failed to match any pattern))
                                                                                                               (#%cons (lref x)
                                                                                                                (const '())))))
                                                                                                            (let
                                                                                                                ([tmp-1
                                                                                                                  (#%car (lref clauses))])
                                                                                                              (let
                                                                                                                 ([tmp
                                                                                                                   (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                    (const (any any)))])
                                                                                                               (if
                                                                                                                 (lref tmp)
                                                                                                                 (call (module-ref (capy)::apply #t) (lambda (pat exp)
                                                                                                                   (if
                                                                                                                     (let
                                                                                                                         ([and-tmp
                                                                                                                           (call (lref id?) (lref pat))])
                                                                                                                       (if
                                                                                                                         (lref and-tmp)
                                                                                                                         (call (toplevel-ref and-map) (lambda (x)
                                                                                                                           (#%not (call (lref free-id=?) (lref pat)
                                                                                                                             (lref x))))
                                                                                                                          (#%cons (call (lref make-syntax) (const ...)
                                                                                                                            (const ((top)))
                                                                                                                            (const (hygiene capy)))
                                                                                                                           (lref keys)))
                                                                                                                         (const false)))
                                                                                                                     (if
                                                                                                                       (call (lref free-id=?) (lref pat)
                                                                                                                         (call (lref make-syntax) (const _)
                                                                                                                          (const ((top)))
                                                                                                                          (const (hygiene capy))))
                                                                                                                       (call (lref expand) (lref exp)
                                                                                                                        (lref r)
                                                                                                                        (lref empty-wrap)
                                                                                                                        (lref mod))
                                                                                                                       (let
                                                                                                                           ([labels
                                                                                                                             (#%cons (call (lref gen-label))
                                                                                                                              (const '()))]
                                                                                                                           [var
                                                                                                                             (call (lref gen-var) (lref pat))])
                                                                                                                         (call (lref build-call) (lref no-source)
                                                                                                                          (call (lref build-simple-lambda) (lref no-source)
                                                                                                                           (#%cons (call (toplevel-ref syntax->datum) (lref pat))
                                                                                                                            (const '()))
                                                                                                                           (#%cons (lref var)
                                                                                                                            (const '()))
                                                                                                                           (const '())
                                                                                                                           (call (lref expand) (lref exp)
                                                                                                                            (call (lref extend-env) (lref labels)
                                                                                                                             (#%cons (#%cons (const syntax)
                                                                                                                               (#%cons (lref var)
                                                                                                                                (const 0)))
                                                                                                                              (const '()))
                                                                                                                             (lref r))
                                                                                                                            (call (lref make-binding-wrap) (#%cons (lref pat)
                                                                                                                              (const '()))
                                                                                                                             (lref labels)
                                                                                                                             (lref empty-wrap))
                                                                                                                            (lref mod)))
                                                                                                                          (#%cons (lref x)
                                                                                                                           (const '())))))
                                                                                                                     (call (lref gen-clause) (lref x)
                                                                                                                       (lref keys)
                                                                                                                       (#%cdr (lref clauses))
                                                                                                                       (lref r)
                                                                                                                       (lref pat)
                                                                                                                       (const true)
                                                                                                                       (lref exp)
                                                                                                                       (lref mod))))
                                                                                                                  (lref tmp))
                                                                                                                 (let
                                                                                                                     ([tmp
                                                                                                                       (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                        (const (any any any)))])
                                                                                                                   (if
                                                                                                                     (lref tmp)
                                                                                                                     (call (module-ref (capy)::apply #t) (lambda (pat fender exp)
                                                                                                                       (call (lref gen-clause) (lref x)
                                                                                                                        (lref keys)
                                                                                                                        (#%cdr (lref clauses))
                                                                                                                        (lref r)
                                                                                                                        (lref pat)
                                                                                                                        (lref fender)
                                                                                                                        (lref exp)
                                                                                                                        (lref mod)))
                                                                                                                      (lref tmp))
                                                                                                                     (call (toplevel-ref syntax-violation) (const syntax-case)
                                                                                                                       (const invalid clause)
                                                                                                                       (#%car (lref clauses))))))))))]
                                                                                                        [gen-clause
                                                                                                        (lambda (x keys clauses r pat fender exp mod)
                                                                                                          (receive (p pvars)
                                                                                                            (call (lref convert-pattern) (lref pat)
                                                                                                              (lref keys)
                                                                                                              (lambda (e)
                                                                                                               (call (lref ellipsis?) (lref e)
                                                                                                                (lref r)
                                                                                                                (lref mod))))
                                                                                                            (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%not (call (toplevel-ref and-map) (lambda (x)
                                                                                                                     (#%not (call (lref ellipsis?) (#%car (lref x))
                                                                                                                       (lref r)
                                                                                                                       (lref mod))))
                                                                                                                    (lref pvars)))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (call (toplevel-ref syntax-violation) (const syntax-case)
                                                                                                                 (const misplaced ellipsis)
                                                                                                                 (lref pat))
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (#%not (call (lref distinct-bound-ids?) (call (toplevel-ref map) #%car
                                                                                                                         (lref pvars))))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (call (toplevel-ref syntax-violation) (const syntax-case)
                                                                                                                     (const duplicate pattern variable)
                                                                                                                     (lref pat))
                                                                                                                    (let
                                                                                                                        ([y
                                                                                                                          (call (lref gen-var) (const tmp))])
                                                                                                                      (call (lref build-call) (lref no-source)
                                                                                                                       (call (lref build-simple-lambda) (lref no-source)
                                                                                                                        (#%cons (const tmp)
                                                                                                                         (const '()))
                                                                                                                        (#%cons (lref y)
                                                                                                                         (const '()))
                                                                                                                        (const '())
                                                                                                                        (let
                                                                                                                           ([y
                                                                                                                             (call (lref build-lexical-reference) (lref no-source)
                                                                                                                              (const tmp)
                                                                                                                              (lref y))])
                                                                                                                         (call (lref build-conditional) (lref no-source)
                                                                                                                          (let
                                                                                                                             ([tmp
                                                                                                                               (lref fender)])
                                                                                                                           (let
                                                                                                                              ([tmp
                                                                                                                                (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                                                 (const #(atom true)))])
                                                                                                                            (if
                                                                                                                              (lref tmp)
                                                                                                                              (call (module-ref (capy)::apply #t) (lambda ()
                                                                                                                                (lref y))
                                                                                                                               (lref tmp))
                                                                                                                              (call (lref build-conditional) (lref no-source)
                                                                                                                                (lref y)
                                                                                                                                (call (lref build-dispatch-call) (lref pvars)
                                                                                                                                 (lref fender)
                                                                                                                                 (lref y)
                                                                                                                                 (lref r)
                                                                                                                                 (lref mod))
                                                                                                                                (call (lref build-data) (lref no-source)
                                                                                                                                 (const false))))))
                                                                                                                          (call (lref build-dispatch-call) (lref pvars)
                                                                                                                           (lref exp)
                                                                                                                           (lref y)
                                                                                                                           (lref r)
                                                                                                                           (lref mod))
                                                                                                                          (call (lref gen-syntax-case) (lref x)
                                                                                                                           (lref keys)
                                                                                                                           (lref clauses)
                                                                                                                           (lref r)
                                                                                                                           (lref mod)))))
                                                                                                                       (#%cons (if
                                                                                                                          (#%eq? (lref p)
                                                                                                                            (const any))
                                                                                                                          (call (lref build-primcall) (lref no-source)
                                                                                                                           (const list)
                                                                                                                           (#%cons (lref x)
                                                                                                                            (const '())))
                                                                                                                          (call (lref build-primcall) (lref no-source)
                                                                                                                            (const $sc-dispatch)
                                                                                                                            (#%cons (lref x)
                                                                                                                             (#%cons (call (lref build-data) (lref no-source)
                                                                                                                               (lref p))
                                                                                                                              (const '())))))
                                                                                                                        (const '()))))))))))])
                                                                                                       (lambda (e r w s mod)
                                                                                                        (let
                                                                                                           ([e
                                                                                                             (call (lref source-wrap) (lref e)
                                                                                                              (lref w)
                                                                                                              (lref s)
                                                                                                              (lref mod))])
                                                                                                         (let
                                                                                                            ([tmp-1
                                                                                                              (lref e)])
                                                                                                          (let
                                                                                                             ([tmp
                                                                                                               (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                (const (_ any each-any . each-any)))])
                                                                                                           (if
                                                                                                             (lref tmp)
                                                                                                             (call (module-ref (capy)::apply #t) (lambda (val key m)
                                                                                                               (if
                                                                                                                 (call (toplevel-ref and-map) (lambda (x)
                                                                                                                    (let
                                                                                                                       ([and-tmp
                                                                                                                         (call (lref id?) (lref x))])
                                                                                                                     (if
                                                                                                                       (lref and-tmp)
                                                                                                                       (#%not (call (lref ellipsis?) (lref x)
                                                                                                                         (lref r)
                                                                                                                         (lref mod)))
                                                                                                                       (const false))))
                                                                                                                   (lref key))
                                                                                                                 (let
                                                                                                                    ([x
                                                                                                                      (call (lref gen-var) (const tmp))])
                                                                                                                  (call (lref build-call) (lref s)
                                                                                                                   (call (lref build-simple-lambda) (lref no-source)
                                                                                                                    (#%cons (const tmp)
                                                                                                                     (const '()))
                                                                                                                    (#%cons (lref x)
                                                                                                                     (const '()))
                                                                                                                    (const '())
                                                                                                                    (call (lref gen-syntax-case) (call (lref build-lexical-reference) (lref no-source)
                                                                                                                      (const tmp)
                                                                                                                      (lref x))
                                                                                                                     (lref key)
                                                                                                                     (lref m)
                                                                                                                     (lref r)
                                                                                                                     (lref mod)))
                                                                                                                   (#%cons (call (lref expand) (lref val)
                                                                                                                     (lref r)
                                                                                                                     (lref empty-wrap)
                                                                                                                     (lref mod))
                                                                                                                    (const '()))))
                                                                                                                 (call (toplevel-ref syntax-violation) (const syntax-case)
                                                                                                                   (const invalid literals list)
                                                                                                                   (lref e))))
                                                                                                              (lref tmp))
                                                                                                             (call (toplevel-ref syntax-violation) (const false)
                                                                                                               (const source expression failed to match any pattern)
                                                                                                               (lref tmp-1))))))))))])
                                                                                                (seq
                                                                                                 (toplevel-set syntax->datum (lambda (x)
                                                                                                  (call (lref strip) (lref x))))
                                                                                                 (toplevel-set $sc-dispatch (lambda (e p)
                                                                                                  (fix
                                                                                                    ([combine
                                                                                                    (lambda (r* r)
                                                                                                      (if
                                                                                                        (#%null? (#%car (lref r*)))
                                                                                                        (lref r)
                                                                                                        (#%cons (call (toplevel-ref map) #%car
                                                                                                           (lref r*))
                                                                                                          (call (lref combine) (call (toplevel-ref map) #%cdr
                                                                                                            (lref r*))
                                                                                                           (lref r)))))])
                                                                                                   (fix
                                                                                                     ([match-empty
                                                                                                     (lambda (p r)
                                                                                                       (let
                                                                                                          ([cond-test-tmp
                                                                                                            (#%null? (lref p))])
                                                                                                        (if
                                                                                                          (lref cond-test-tmp)
                                                                                                          (lref r)
                                                                                                          (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%eq? (lref p)
                                                                                                                 (const _))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (lref r)
                                                                                                              (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (#%eq? (lref p)
                                                                                                                     (const any))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (#%cons (const '())
                                                                                                                   (lref r))
                                                                                                                  (let
                                                                                                                      ([cond-test-tmp
                                                                                                                        (#%pair? (lref p))])
                                                                                                                    (if
                                                                                                                      (lref cond-test-tmp)
                                                                                                                      (call (lref match-empty) (#%car (lref p))
                                                                                                                       (call (lref match-empty) (#%cdr (lref p))
                                                                                                                        (lref r)))
                                                                                                                      (let
                                                                                                                          ([cond-test-tmp
                                                                                                                            (#%eq? (lref p)
                                                                                                                             (const each-any))])
                                                                                                                        (if
                                                                                                                          (lref cond-test-tmp)
                                                                                                                          (#%cons (const '())
                                                                                                                           (lref r))
                                                                                                                          (let
                                                                                                                              ([key
                                                                                                                                (#%vector-ref (lref p)
                                                                                                                                 (const 0))])
                                                                                                                            (let
                                                                                                                               ([cond-test-tmp
                                                                                                                                 (#%eqv? (lref key)
                                                                                                                                  (const each))])
                                                                                                                             (if
                                                                                                                               (lref cond-test-tmp)
                                                                                                                               (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                                 (const 1))
                                                                                                                                (lref r))
                                                                                                                               (let
                                                                                                                                   ([cond-test-tmp
                                                                                                                                     (#%eqv? (lref key)
                                                                                                                                      (const each+))])
                                                                                                                                 (if
                                                                                                                                   (lref cond-test-tmp)
                                                                                                                                   (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                                     (const 1))
                                                                                                                                    (call (lref match-empty) (call (toplevel-ref reverse) (#%vector-ref (lref p)
                                                                                                                                       (const 2)))
                                                                                                                                     (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                                       (const 3))
                                                                                                                                      (lref r))))
                                                                                                                                   (let
                                                                                                                                       ([cond-test-tmp
                                                                                                                                         (let
                                                                                                                                            ([memv-tmp
                                                                                                                                              (#%eqv? (lref key)
                                                                                                                                               (const free-id))])
                                                                                                                                          (if
                                                                                                                                            (lref memv-tmp)
                                                                                                                                            (lref memv-tmp)
                                                                                                                                            (#%eqv? (lref key)
                                                                                                                                              (const atom))))])
                                                                                                                                     (if
                                                                                                                                       (lref cond-test-tmp)
                                                                                                                                       (lref r)
                                                                                                                                       (let
                                                                                                                                           ([cond-test-tmp
                                                                                                                                             (#%eqv? (lref key)
                                                                                                                                              (const vector))])
                                                                                                                                         (if
                                                                                                                                           (lref cond-test-tmp)
                                                                                                                                           (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                                             (const 1))
                                                                                                                                            (lref r))
                                                                                                                                           (const #<undefined>)))))))))))))))))))))])
                                                                                                    (fix
                                                                                                      ([match-each-any
                                                                                                      (lambda (e w mod)
                                                                                                        (let
                                                                                                           ([cond-test-tmp
                                                                                                             (#%pair? (lref e))])
                                                                                                         (if
                                                                                                           (lref cond-test-tmp)
                                                                                                           (let
                                                                                                              ([l
                                                                                                                (call (lref match-each-any) (#%cdr (lref e))
                                                                                                                 (lref w)
                                                                                                                 (lref mod))])
                                                                                                            (let
                                                                                                               ([and-tmp
                                                                                                                 (lref l)])
                                                                                                             (if
                                                                                                               (lref and-tmp)
                                                                                                               (#%cons (call (lref wrap) (#%car (lref e))
                                                                                                                 (lref w)
                                                                                                                 (lref mod))
                                                                                                                (lref l))
                                                                                                               (const false))))
                                                                                                           (let
                                                                                                               ([cond-test-tmp
                                                                                                                 (#%null? (lref e))])
                                                                                                             (if
                                                                                                               (lref cond-test-tmp)
                                                                                                               (const '())
                                                                                                               (let
                                                                                                                   ([cond-test-tmp
                                                                                                                     (call (lref syntax?) (lref e))])
                                                                                                                 (if
                                                                                                                   (lref cond-test-tmp)
                                                                                                                   (call (lref match-each-any) (call (lref syntax-expression) (lref e))
                                                                                                                    (call (lref join-wraps) (lref w)
                                                                                                                     (call (lref syntax-wrap) (lref e)))
                                                                                                                    (lref mod))
                                                                                                                   (const false))))))))])
                                                                                                     (fix
                                                                                                       ([match-each+
                                                                                                       (lambda (e x-pat y-pat z-pat w r mod)
                                                                                                         (fix
                                                                                                           ([f
                                                                                                           (lambda (e w)
                                                                                                             (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%pair? (lref e))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (receive (xr* y-pat r)
                                                                                                                  (call (lref f) (#%cdr (lref e))
                                                                                                                    (lref w))
                                                                                                                  (if
                                                                                                                     (lref r)
                                                                                                                     (if
                                                                                                                       (#%null? (lref y-pat))
                                                                                                                       (let
                                                                                                                          ([xr
                                                                                                                            (call (lref match) (#%car (lref e))
                                                                                                                             (lref x-pat)
                                                                                                                             (lref w)
                                                                                                                             (const '())
                                                                                                                             (lref mod))])
                                                                                                                        (if
                                                                                                                          (lref xr)
                                                                                                                          (values (#%cons (lref xr)
                                                                                                                           (lref xr*)) (lref y-pat) (lref r))
                                                                                                                          (values (const false) (const false) (const false))))
                                                                                                                       (values (const '()) (#%cdr (lref y-pat)) (call (lref match) (#%car (lref e))
                                                                                                                         (#%car (lref y-pat))
                                                                                                                         (lref w)
                                                                                                                         (lref r)
                                                                                                                         (lref mod))))
                                                                                                                     (values (const false) (const false) (const false))))
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (call (lref syntax?) (lref e))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (call (lref f) (call (lref syntax-expression) (lref e))
                                                                                                                     (call (lref join-wraps) (lref w)
                                                                                                                      (call (lref syntax-wrap) (lref e))))
                                                                                                                    (values (const '()) (lref y-pat) (call (lref match) (lref e)
                                                                                                                      (lref z-pat)
                                                                                                                      (lref w)
                                                                                                                      (lref r)
                                                                                                                      (lref mod))))))))])
                                                                                                          (call (lref f) (lref e)
                                                                                                           (lref w))))]
                                                                                                       [match*
                                                                                                       (lambda (e p w r mod)
                                                                                                         (let
                                                                                                            ([cond-test-tmp
                                                                                                              (#%null? (lref p))])
                                                                                                          (if
                                                                                                            (lref cond-test-tmp)
                                                                                                            (let
                                                                                                               ([and-tmp
                                                                                                                 (#%null? (lref e))])
                                                                                                             (if
                                                                                                               (lref and-tmp)
                                                                                                               (lref r)
                                                                                                               (const false)))
                                                                                                            (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%pair? (lref p))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (let
                                                                                                                   ([and-tmp
                                                                                                                     (#%pair? (lref e))])
                                                                                                                 (if
                                                                                                                   (lref and-tmp)
                                                                                                                   (call (lref match) (#%car (lref e))
                                                                                                                    (#%car (lref p))
                                                                                                                    (lref w)
                                                                                                                    (call (lref match) (#%cdr (lref e))
                                                                                                                     (#%cdr (lref p))
                                                                                                                     (lref w)
                                                                                                                     (lref r)
                                                                                                                     (lref mod))
                                                                                                                    (lref mod))
                                                                                                                   (const false)))
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (#%eq? (lref p)
                                                                                                                       (const each-any))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (let
                                                                                                                       ([l
                                                                                                                         (call (lref match-each-any) (lref e)
                                                                                                                          (lref w)
                                                                                                                          (lref mod))])
                                                                                                                     (let
                                                                                                                        ([and-tmp
                                                                                                                          (lref l)])
                                                                                                                      (if
                                                                                                                        (lref and-tmp)
                                                                                                                        (#%cons (lref l)
                                                                                                                         (lref r))
                                                                                                                        (const false))))
                                                                                                                    (let
                                                                                                                        ([key
                                                                                                                          (#%vector-ref (lref p)
                                                                                                                           (const 0))])
                                                                                                                      (let
                                                                                                                         ([cond-test-tmp
                                                                                                                           (#%eqv? (lref key)
                                                                                                                            (const each))])
                                                                                                                       (if
                                                                                                                         (lref cond-test-tmp)
                                                                                                                         (if
                                                                                                                           (#%null? (lref e))
                                                                                                                           (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                             (const 1))
                                                                                                                            (lref r))
                                                                                                                           (let
                                                                                                                               ([l
                                                                                                                                 (call (lref match-each) (lref e)
                                                                                                                                  (#%vector-ref (lref p)
                                                                                                                                   (const 1))
                                                                                                                                  (lref w)
                                                                                                                                  (lref mod))])
                                                                                                                             (let
                                                                                                                                ([and-tmp
                                                                                                                                  (lref l)])
                                                                                                                              (if
                                                                                                                                (lref and-tmp)
                                                                                                                                (fix
                                                                                                                                  ([collect
                                                                                                                                  (lambda (l)
                                                                                                                                    (if
                                                                                                                                      (#%null? (#%car (lref l)))
                                                                                                                                      (lref r)
                                                                                                                                      (#%cons (call (toplevel-ref map) #%car
                                                                                                                                         (lref l))
                                                                                                                                        (call (lref collect) (call (toplevel-ref map) #%cdr
                                                                                                                                          (lref l))))))])
                                                                                                                                 (call (lref collect) (lref l)))
                                                                                                                                (const false)))))
                                                                                                                         (let
                                                                                                                             ([cond-test-tmp
                                                                                                                               (#%eqv? (lref key)
                                                                                                                                (const each+))])
                                                                                                                           (if
                                                                                                                             (lref cond-test-tmp)
                                                                                                                             (receive (xr* y-pat r)
                                                                                                                               (call (lref match-each+) (lref e)
                                                                                                                                 (#%vector-ref (lref p)
                                                                                                                                  (const 1))
                                                                                                                                 (#%vector-ref (lref p)
                                                                                                                                  (const 2))
                                                                                                                                 (#%vector-ref (lref p)
                                                                                                                                  (const 3))
                                                                                                                                 (lref w)
                                                                                                                                 (lref r)
                                                                                                                                 (lref mod))
                                                                                                                               (let
                                                                                                                                   ([and-tmp
                                                                                                                                     (lref r)])
                                                                                                                                 (if
                                                                                                                                   (lref and-tmp)
                                                                                                                                   (let
                                                                                                                                      ([and-tmp
                                                                                                                                        (#%null? (lref y-pat))])
                                                                                                                                    (if
                                                                                                                                      (lref and-tmp)
                                                                                                                                      (if
                                                                                                                                        (#%null? (lref xr*))
                                                                                                                                        (call (lref match-empty) (#%vector-ref (lref p)
                                                                                                                                          (const 1))
                                                                                                                                         (lref r))
                                                                                                                                        (call (lref combine) (lref xr*)
                                                                                                                                          (lref r)))
                                                                                                                                      (const false)))
                                                                                                                                   (const false))))
                                                                                                                             (let
                                                                                                                                 ([cond-test-tmp
                                                                                                                                   (#%eqv? (lref key)
                                                                                                                                    (const free-id))])
                                                                                                                               (if
                                                                                                                                 (lref cond-test-tmp)
                                                                                                                                 (let
                                                                                                                                    ([and-tmp
                                                                                                                                      (call (lref id?) (lref e))])
                                                                                                                                  (if
                                                                                                                                    (lref and-tmp)
                                                                                                                                    (let
                                                                                                                                       ([and-tmp
                                                                                                                                         (call (lref free-id=?) (call (lref wrap) (lref e)
                                                                                                                                           (lref w)
                                                                                                                                           (lref mod))
                                                                                                                                          (#%vector-ref (lref p)
                                                                                                                                           (const 1)))])
                                                                                                                                     (if
                                                                                                                                       (lref and-tmp)
                                                                                                                                       (lref r)
                                                                                                                                       (const false)))
                                                                                                                                    (const false)))
                                                                                                                                 (let
                                                                                                                                     ([cond-test-tmp
                                                                                                                                       (#%eqv? (lref key)
                                                                                                                                        (const atom))])
                                                                                                                                   (if
                                                                                                                                     (lref cond-test-tmp)
                                                                                                                                     (let
                                                                                                                                        ([and-tmp
                                                                                                                                          (#%equal? (#%vector-ref (lref p)
                                                                                                                                            (const 1))
                                                                                                                                           (call (lref strip) (lref e)))])
                                                                                                                                      (if
                                                                                                                                        (lref and-tmp)
                                                                                                                                        (lref r)
                                                                                                                                        (const false)))
                                                                                                                                     (let
                                                                                                                                         ([cond-test-tmp
                                                                                                                                           (#%eqv? (lref key)
                                                                                                                                            (const vector))])
                                                                                                                                       (if
                                                                                                                                         (lref cond-test-tmp)
                                                                                                                                         (let
                                                                                                                                            ([and-tmp
                                                                                                                                              (#%vector? (lref e))])
                                                                                                                                          (if
                                                                                                                                            (lref and-tmp)
                                                                                                                                            (call (lref match) (call (toplevel-ref vector->list) (lref e))
                                                                                                                                             (#%vector-ref (lref p)
                                                                                                                                              (const 1))
                                                                                                                                             (lref w)
                                                                                                                                             (lref r)
                                                                                                                                             (lref mod))
                                                                                                                                            (const false)))
                                                                                                                                         (const #<undefined>)))))))))))))))))))]
                                                                                                       [match
                                                                                                       (lambda (e p w r mod)
                                                                                                         (let
                                                                                                            ([cond-test-tmp
                                                                                                              (#%not (lref r))])
                                                                                                          (if
                                                                                                            (lref cond-test-tmp)
                                                                                                            (const false)
                                                                                                            (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%eq? (lref p)
                                                                                                                   (const _))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (lref r)
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (#%eq? (lref p)
                                                                                                                       (const any))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (#%cons (call (lref wrap) (lref e)
                                                                                                                      (lref w)
                                                                                                                      (lref mod))
                                                                                                                     (lref r))
                                                                                                                    (let
                                                                                                                        ([cond-test-tmp
                                                                                                                          (call (lref syntax?) (lref e))])
                                                                                                                      (if
                                                                                                                        (lref cond-test-tmp)
                                                                                                                        (call (lref match*) (call (lref syntax-expression) (lref e))
                                                                                                                         (lref p)
                                                                                                                         (call (lref join-wraps) (lref w)
                                                                                                                          (call (lref syntax-wrap) (lref e)))
                                                                                                                         (lref r)
                                                                                                                         (let
                                                                                                                            ([or-tmp
                                                                                                                              (call (lref syntax-module) (lref e))])
                                                                                                                          (if
                                                                                                                            (lref or-tmp)
                                                                                                                            (lref or-tmp)
                                                                                                                            (lref mod))))
                                                                                                                        (call (lref match*) (lref e)
                                                                                                                          (lref p)
                                                                                                                          (lref w)
                                                                                                                          (lref r)
                                                                                                                          (lref mod)))))))))))]
                                                                                                       [match-each
                                                                                                       (lambda (e p w mod)
                                                                                                         (let
                                                                                                            ([cond-test-tmp
                                                                                                              (#%pair? (lref e))])
                                                                                                          (if
                                                                                                            (lref cond-test-tmp)
                                                                                                            (let
                                                                                                               ([first
                                                                                                                 (call (lref match) (#%car (lref e))
                                                                                                                  (lref p)
                                                                                                                  (lref w)
                                                                                                                  (const '())
                                                                                                                  (lref mod))])
                                                                                                             (let
                                                                                                                ([and-tmp
                                                                                                                  (lref first)])
                                                                                                              (if
                                                                                                                (lref and-tmp)
                                                                                                                (let
                                                                                                                   ([rest
                                                                                                                     (call (lref match-each) (#%cdr (lref e))
                                                                                                                      (lref p)
                                                                                                                      (lref w)
                                                                                                                      (lref mod))])
                                                                                                                 (let
                                                                                                                    ([and-tmp
                                                                                                                      (lref rest)])
                                                                                                                  (if
                                                                                                                    (lref and-tmp)
                                                                                                                    (#%cons (lref first)
                                                                                                                     (lref rest))
                                                                                                                    (const false))))
                                                                                                                (const false))))
                                                                                                            (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%null? (lref e))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (const '())
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (call (lref syntax?) (lref e))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (call (lref match-each) (call (lref syntax-expression) (lref e))
                                                                                                                     (lref p)
                                                                                                                     (call (lref join-wraps) (lref w)
                                                                                                                      (call (lref syntax-wrap) (lref e)))
                                                                                                                     (let
                                                                                                                        ([or-tmp
                                                                                                                          (call (lref syntax-module) (lref e))])
                                                                                                                      (if
                                                                                                                        (lref or-tmp)
                                                                                                                        (lref or-tmp)
                                                                                                                        (lref mod))))
                                                                                                                    (const false))))))))])
                                                                                                      (let
                                                                                                         ([cond-test-tmp
                                                                                                           (#%eq? (lref p)
                                                                                                            (const any))])
                                                                                                       (if
                                                                                                         (lref cond-test-tmp)
                                                                                                         (#%cons (lref e)
                                                                                                          (const '()))
                                                                                                         (let
                                                                                                             ([cond-test-tmp
                                                                                                               (#%eq? (lref p)
                                                                                                                (const _))])
                                                                                                           (if
                                                                                                             (lref cond-test-tmp)
                                                                                                             (const '())
                                                                                                             (let
                                                                                                                 ([cond-test-tmp
                                                                                                                   (call (lref syntax?) (lref e))])
                                                                                                               (if
                                                                                                                 (lref cond-test-tmp)
                                                                                                                 (call (lref match*) (call (lref syntax-expression) (lref e))
                                                                                                                  (lref p)
                                                                                                                  (call (lref syntax-wrap) (lref e))
                                                                                                                  (const '())
                                                                                                                  (call (lref syntax-module) (lref e)))
                                                                                                                 (call (lref match*) (lref e)
                                                                                                                   (lref p)
                                                                                                                   (lref empty-wrap)
                                                                                                                   (const '())
                                                                                                                   (const false))))))))))))))
                                                                                                 (toplevel-set identifier? (lambda (x)
                                                                                                  (call (lref nonsymbol-id?) (lref x))))
                                                                                                 (toplevel-set datum->syntax (lambda (id datum source)
                                                                                                  (fix
                                                                                                    ([props->sourcev
                                                                                                    (lambda (alist)
                                                                                                      (let
                                                                                                         ([and-tmp
                                                                                                           (#%pair? (lref alist))])
                                                                                                       (if
                                                                                                         (lref and-tmp)
                                                                                                         (let
                                                                                                            ([vec
                                                                                                              (#%make-vector (const 3)
                                                                                                               (const #<undefined>))])
                                                                                                          (seq
                                                                                                           (#%vector-set! (lref vec)
                                                                                                            (const 0)
                                                                                                            (call (toplevel-ref assq-ref) (lref alist)
                                                                                                             (const filename)))
                                                                                                           (#%vector-set! (lref vec)
                                                                                                            (const 1)
                                                                                                            (call (toplevel-ref assq-ref) (lref alist)
                                                                                                             (const line)))
                                                                                                           (#%vector-set! (lref vec)
                                                                                                            (const 2)
                                                                                                            (call (toplevel-ref assq-ref) (lref alist)
                                                                                                             (const column)))
                                                                                                           (lref vec)))
                                                                                                         (const false))))])
                                                                                                   (call (lref make-syntax) (lref datum)
                                                                                                    (if
                                                                                                      (lref id)
                                                                                                      (call (lref syntax-wrap) (lref id))
                                                                                                      (lref empty-wrap))
                                                                                                    (if
                                                                                                      (lref id)
                                                                                                      (call (lref syntax-module) (lref id))
                                                                                                      (const false))
                                                                                                    (let
                                                                                                       ([cond-test-tmp
                                                                                                         (#%not (lref source))])
                                                                                                     (if
                                                                                                       (lref cond-test-tmp)
                                                                                                       (call (lref props->sourcev) (call (toplevel-ref source-properties) (lref datum)))
                                                                                                       (let
                                                                                                           ([cond-test-tmp
                                                                                                             (call (toplevel-ref alist?) (lref source))])
                                                                                                         (if
                                                                                                           (lref cond-test-tmp)
                                                                                                           (call (lref props->sourcev) (lref source))
                                                                                                           (let
                                                                                                               ([cond-test-tmp
                                                                                                                 (let
                                                                                                                    ([and-tmp
                                                                                                                      (#%vector? (lref source))])
                                                                                                                  (if
                                                                                                                    (lref and-tmp)
                                                                                                                    (#%= (#%vector-length (lref source))
                                                                                                                     (const 3))
                                                                                                                    (const false)))])
                                                                                                             (if
                                                                                                               (lref cond-test-tmp)
                                                                                                               (lref source)
                                                                                                               (call (lref syntax-sourcev) (lref source))))))))))))
                                                                                                 (toplevel-set free-identifier=? (lambda (x y)
                                                                                                  (seq
                                                                                                   (if
                                                                                                     (#%not (call (lref nonsymbol-id?) (lref x)))
                                                                                                     (call (toplevel-ref assertion-violation) (const free-identifier=?)
                                                                                                      (const Expected syntax identifier)
                                                                                                      (lref x))
                                                                                                     (const #<undefined>))
                                                                                                   (if
                                                                                                     (#%not (call (lref nonsymbol-id?) (lref y)))
                                                                                                     (call (toplevel-ref assertion-violation) (const free-identifier=?)
                                                                                                      (const Expected syntax identifier)
                                                                                                      (lref y))
                                                                                                     (const #<undefined>))
                                                                                                   (call (lref free-id=?) (lref x)
                                                                                                    (lref y)))))
                                                                                                 (toplevel-set bound-identifier=? (lambda (x y)
                                                                                                  (seq
                                                                                                   (if
                                                                                                     (#%not (call (lref nonsymbol-id?) (lref x)))
                                                                                                     (call (toplevel-ref assertion-violation) (const bound-identifier=?)
                                                                                                      (const Expected syntax identifier)
                                                                                                      (lref x))
                                                                                                     (const #<undefined>))
                                                                                                   (if
                                                                                                     (#%not (call (lref nonsymbol-id?) (lref y)))
                                                                                                     (call (toplevel-ref assertion-violation) (const bound-identifier=?)
                                                                                                      (const Expected syntax identifier)
                                                                                                      (lref y))
                                                                                                     (const #<undefined>))
                                                                                                   (call (lref bound-id=?) (lref x)
                                                                                                    (lref y)))))
                                                                                                 (toplevel-set generate-temporaries (lambda (ls)
                                                                                                  (seq
                                                                                                   (let
                                                                                                      ([x
                                                                                                        (lref ls)])
                                                                                                    (if
                                                                                                      (#%not (#%list? (lref x)))
                                                                                                      (call (toplevel-ref syntax-violation) (const generate-temporaries)
                                                                                                       (const invalid argument)
                                                                                                       (lref x))
                                                                                                      (const #<undefined>)))
                                                                                                   (let
                                                                                                      ([mod
                                                                                                        (#%cons (const hygiene)
                                                                                                         (call (toplevel-ref module-name) (#%current-module)))])
                                                                                                    (call (toplevel-ref map) (lambda (x)
                                                                                                      (call (lref wrap) (call (lref gen-var) (const t))
                                                                                                       (lref top-wrap)
                                                                                                       (lref mod)))
                                                                                                     (lref ls))))))
                                                                                                 (toplevel-set macroexpand (lambda (x rest)
                                                                                                  (fix
                                                                                                    ([unstrip
                                                                                                    (lambda (x)
                                                                                                      (fix
                                                                                                        ([annotate
                                                                                                        (lambda (result)
                                                                                                          (let
                                                                                                             ([props
                                                                                                               (call (toplevel-ref source-properties) (lref x))])
                                                                                                           (if
                                                                                                             (#%pair? (lref props))
                                                                                                             (call (toplevel-ref datum->syntax) (const false)
                                                                                                              (lref result)
                                                                                                              (lref props))
                                                                                                             (lref result))))])
                                                                                                       (let
                                                                                                          ([cond-test-tmp
                                                                                                            (#%pair? (lref x))])
                                                                                                        (if
                                                                                                          (lref cond-test-tmp)
                                                                                                          (call (lref annotate) (#%cons (call (lref unstrip) (#%car (lref x)))
                                                                                                            (call (lref unstrip) (#%cdr (lref x)))))
                                                                                                          (let
                                                                                                              ([cond-test-tmp
                                                                                                                (#%vector? (lref x))])
                                                                                                            (if
                                                                                                              (lref cond-test-tmp)
                                                                                                              (call (lref annotate) (call (toplevel-ref list->vector) (call (toplevel-ref map) (lref unstrip)
                                                                                                                 (call (toplevel-ref vector->list) (lref x)))))
                                                                                                              (let
                                                                                                                  ([cond-test-tmp
                                                                                                                    (call (lref syntax?) (lref x))])
                                                                                                                (if
                                                                                                                  (lref cond-test-tmp)
                                                                                                                  (lref x)
                                                                                                                  (call (lref annotate) (lref x))))))))))])
                                                                                                   (let
                                                                                                      ([m
                                                                                                        (if
                                                                                                          (#%null? (lref rest))
                                                                                                          (const e)
                                                                                                          (#%car (lref rest)))]
                                                                                                      [essew
                                                                                                        (if
                                                                                                          (#%= (#%length (lref rest))
                                                                                                            (const 2))
                                                                                                          (#%car (#%cdr (lref rest)))
                                                                                                          (const (eval)))])
                                                                                                    (call (lref expand-top-sequence) (#%cons (call (lref unstrip) (lref x))
                                                                                                      (const '()))
                                                                                                     (lref null-env)
                                                                                                     (lref top-wrap)
                                                                                                     (const false)
                                                                                                     (lref m)
                                                                                                     (lref essew)
                                                                                                     (#%cons (const hygiene)
                                                                                                      (call (toplevel-ref module-name) (#%current-module))))))))
                                                                                                 (call (lref global-extend) (const define)
                                                                                                  (const define)
                                                                                                  (const '()))
                                                                                                 (call (lref global-extend) (const begin)
                                                                                                  (const begin)
                                                                                                  (const '()))
                                                                                                 (call (lref global-extend) (const eval-when)
                                                                                                  (const eval-when)
                                                                                                  (const '()))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const if)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (lref e)])
                                                                                                    (let
                                                                                                       ([tmp-1
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                          (const (_ any any)))])
                                                                                                     (if
                                                                                                       (lref tmp-1)
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (test then)
                                                                                                         (call (lref build-conditional) (lref s)
                                                                                                          (call (lref expand) (lref test)
                                                                                                           (lref r)
                                                                                                           (lref w)
                                                                                                           (lref mod))
                                                                                                          (call (lref expand) (lref then)
                                                                                                           (lref r)
                                                                                                           (lref w)
                                                                                                           (lref mod))
                                                                                                          (call (lref build-void) (lref no-source))))
                                                                                                        (lref tmp-1))
                                                                                                       (let
                                                                                                           ([tmp-1
                                                                                                             (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                              (const (_ any any any)))])
                                                                                                         (if
                                                                                                           (lref tmp-1)
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (test then else)
                                                                                                             (call (lref build-conditional) (lref s)
                                                                                                              (call (lref expand) (lref test)
                                                                                                               (lref r)
                                                                                                               (lref w)
                                                                                                               (lref mod))
                                                                                                              (call (lref expand) (lref then)
                                                                                                               (lref r)
                                                                                                               (lref w)
                                                                                                               (lref mod))
                                                                                                              (call (lref expand) (lref else)
                                                                                                               (lref r)
                                                                                                               (lref w)
                                                                                                               (lref mod))))
                                                                                                            (lref tmp-1))
                                                                                                           (call (toplevel-ref syntax-violation) (const false)
                                                                                                             (const source expression failed to match any pattern)
                                                                                                             (lref tmp)))))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const quote)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (call (toplevel-ref $sc-dispatch) (lref e)
                                                                                                         (const (_ any)))])
                                                                                                    (if
                                                                                                      (lref tmp)
                                                                                                      (call (module-ref (capy)::apply #t) (lambda (e)
                                                                                                        (call (lref build-data) (lref s)
                                                                                                         (call (lref strip) (lref e))))
                                                                                                       (lref tmp))
                                                                                                      (call (toplevel-ref syntax-violation) (const false)
                                                                                                        (const source expression failed to match any pattern)
                                                                                                        (lref e))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const quote-syntax)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp-1
                                                                                                        (call (lref source-wrap) (lref e)
                                                                                                         (lref w)
                                                                                                         (lref s)
                                                                                                         (lref mod))])
                                                                                                    (let
                                                                                                       ([tmp
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                          (const (_ any)))])
                                                                                                     (if
                                                                                                       (lref tmp)
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (e)
                                                                                                         (call (lref build-data) (lref s)
                                                                                                          (lref e)))
                                                                                                        (lref tmp))
                                                                                                       (let
                                                                                                           ([e
                                                                                                             (lref tmp-1)])
                                                                                                         (call (toplevel-ref syntax-violation) (const quote)
                                                                                                          (const bad syntax)
                                                                                                          (lref e))))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const lambda)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (lref e)])
                                                                                                    (let
                                                                                                       ([tmp
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                          (const (_ any any . each-any)))])
                                                                                                     (if
                                                                                                       (lref tmp)
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (args e1 e2)
                                                                                                         (receive (req opt rest kw)
                                                                                                           (call (lref lambda-formals) (lref args))
                                                                                                           (fix
                                                                                                              ([lp
                                                                                                              (lambda (body meta)
                                                                                                                (let
                                                                                                                   ([tmp-1
                                                                                                                     (lref body)])
                                                                                                                 (let
                                                                                                                    ([tmp
                                                                                                                      (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                       (const (any any . each-any)))])
                                                                                                                  (if
                                                                                                                    (let
                                                                                                                        ([and-tmp
                                                                                                                          (lref tmp)])
                                                                                                                      (if
                                                                                                                        (lref and-tmp)
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (docstring e1 e2)
                                                                                                                          (#%string? (call (toplevel-ref syntax->datum) (lref docstring))))
                                                                                                                         (lref tmp))
                                                                                                                        (const false)))
                                                                                                                    (call (module-ref (capy)::apply #t) (lambda (docstring e1 e2)
                                                                                                                      (call (lref lp) (#%cons (lref e1)
                                                                                                                        (lref e2))
                                                                                                                       (#%append (lref meta)
                                                                                                                        (#%cons (#%cons (const documentation)
                                                                                                                          (call (toplevel-ref syntax->datum) (lref docstring)))
                                                                                                                         (const '())))))
                                                                                                                     (lref tmp))
                                                                                                                    (let
                                                                                                                        ([tmp
                                                                                                                          (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                           (const (#(vector #(each (any . any))) any . each-any)))])
                                                                                                                      (if
                                                                                                                        (lref tmp)
                                                                                                                        (call (module-ref (capy)::apply #t) (lambda (k v e1 e2)
                                                                                                                          (call (lref lp) (#%cons (lref e1)
                                                                                                                            (lref e2))
                                                                                                                           (#%append (lref meta)
                                                                                                                            (call (toplevel-ref syntax->datum) (call (toplevel-ref map) #%cons
                                                                                                                              (lref k)
                                                                                                                              (lref v))))))
                                                                                                                         (lref tmp))
                                                                                                                        (call (lref expand-simple-lambda) (lref e)
                                                                                                                          (lref r)
                                                                                                                          (lref w)
                                                                                                                          (lref s)
                                                                                                                          (lref mod)
                                                                                                                          (lref req)
                                                                                                                          (lref rest)
                                                                                                                          (lref meta)
                                                                                                                          (lref body))))))))])
                                                                                                             (call (lref lp) (#%cons (lref e1)
                                                                                                               (lref e2))
                                                                                                              (const '())))))
                                                                                                        (lref tmp))
                                                                                                       (call (toplevel-ref syntax-violation) (const lambda)
                                                                                                         (const bad lambda)
                                                                                                         (lref e)))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const let)
                                                                                                  (let
                                                                                                     ()
                                                                                                   (fix
                                                                                                     ([expand-let
                                                                                                     (lambda (e r w s mod constructor ids vals exps)
                                                                                                       (if
                                                                                                         (#%not (call (lref valid-bound-ids?) (lref ids)))
                                                                                                         (call (toplevel-ref syntax-violation) (const let)
                                                                                                          (const duplicate bound variable)
                                                                                                          (lref e))
                                                                                                         (let
                                                                                                             ([labels
                                                                                                               (call (lref gen-labels) (lref ids))]
                                                                                                             [new-vars
                                                                                                               (call (toplevel-ref map) (lref gen-var)
                                                                                                                (lref ids))])
                                                                                                           (let
                                                                                                              ([nw
                                                                                                                (call (lref make-binding-wrap) (lref ids)
                                                                                                                 (lref labels)
                                                                                                                 (lref w))]
                                                                                                              [nr
                                                                                                                (call (lref extend-var-env) (lref labels)
                                                                                                                 (lref new-vars)
                                                                                                                 (lref r))])
                                                                                                            (call (lref constructor) (lref s)
                                                                                                             (call (toplevel-ref map) (toplevel-ref syntax->datum)
                                                                                                              (lref ids))
                                                                                                             (lref new-vars)
                                                                                                             (call (toplevel-ref map) (lambda (x)
                                                                                                               (call (lref expand) (lref x)
                                                                                                                (lref r)
                                                                                                                (lref w)
                                                                                                                (lref mod)))
                                                                                                              (lref vals))
                                                                                                             (call (lref expand-body) (lref exps)
                                                                                                              (call (lref source-wrap) (lref e)
                                                                                                               (lref nw)
                                                                                                               (lref s)
                                                                                                               (lref mod))
                                                                                                              (lref nr)
                                                                                                              (lref nw)
                                                                                                              (lref mod)))))))])
                                                                                                    (lambda (e r w s mod)
                                                                                                     (let
                                                                                                        ([tmp-1
                                                                                                          (lref e)])
                                                                                                      (let
                                                                                                         ([tmp
                                                                                                           (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                            (const (_ #(each (any any)) any . each-any)))])
                                                                                                       (if
                                                                                                         (let
                                                                                                             ([and-tmp
                                                                                                               (lref tmp)])
                                                                                                           (if
                                                                                                             (lref and-tmp)
                                                                                                             (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                               (call (toplevel-ref and-map) (lref id?)
                                                                                                                (lref id)))
                                                                                                              (lref tmp))
                                                                                                             (const false)))
                                                                                                         (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                           (call (lref expand-let) (lref e)
                                                                                                            (lref r)
                                                                                                            (lref w)
                                                                                                            (lref s)
                                                                                                            (lref mod)
                                                                                                            (lref build-let)
                                                                                                            (lref id)
                                                                                                            (lref val)
                                                                                                            (#%cons (lref e1)
                                                                                                             (lref e2))))
                                                                                                          (lref tmp))
                                                                                                         (let
                                                                                                             ([tmp
                                                                                                               (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                                (const (_ any #(each (any any)) any . each-any)))])
                                                                                                           (if
                                                                                                             (let
                                                                                                                 ([and-tmp
                                                                                                                   (lref tmp)])
                                                                                                               (if
                                                                                                                 (lref and-tmp)
                                                                                                                 (call (module-ref (capy)::apply #t) (lambda (f id val e1 e2)
                                                                                                                   (let
                                                                                                                      ([and-tmp
                                                                                                                        (call (lref id?) (lref f))])
                                                                                                                    (if
                                                                                                                      (lref and-tmp)
                                                                                                                      (call (toplevel-ref and-map) (lref id?)
                                                                                                                       (lref id))
                                                                                                                      (const false))))
                                                                                                                  (lref tmp))
                                                                                                                 (const false)))
                                                                                                             (call (module-ref (capy)::apply #t) (lambda (f id val e1 e2)
                                                                                                               (call (lref expand-let) (lref e)
                                                                                                                (lref r)
                                                                                                                (lref w)
                                                                                                                (lref s)
                                                                                                                (lref mod)
                                                                                                                (lref build-named-let)
                                                                                                                (#%cons (lref f)
                                                                                                                 (lref id))
                                                                                                                (lref val)
                                                                                                                (#%cons (lref e1)
                                                                                                                 (lref e2))))
                                                                                                              (lref tmp))
                                                                                                             (call (toplevel-ref syntax-violation) (const let)
                                                                                                               (const bad let)
                                                                                                               (call (lref source-wrap) (lref e)
                                                                                                                (lref w)
                                                                                                                (lref s)
                                                                                                                (lref mod))))))))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const letrec)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (lref e)])
                                                                                                    (let
                                                                                                       ([tmp
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                          (const (_ #(each (any any)) any . each-any)))])
                                                                                                     (if
                                                                                                       (let
                                                                                                           ([and-tmp
                                                                                                             (lref tmp)])
                                                                                                         (if
                                                                                                           (lref and-tmp)
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                             (call (toplevel-ref and-map) (lref id?)
                                                                                                              (lref id)))
                                                                                                            (lref tmp))
                                                                                                           (const false)))
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                         (let
                                                                                                            ([ids
                                                                                                              (lref id)])
                                                                                                          (if
                                                                                                            (#%not (call (lref valid-bound-ids?) (lref ids)))
                                                                                                            (call (toplevel-ref syntax-violation) (const letrec)
                                                                                                             (const duplicate bound variable)
                                                                                                             (lref e))
                                                                                                            (let
                                                                                                                ([labels
                                                                                                                  (call (lref gen-labels) (lref ids))]
                                                                                                                [new-vars
                                                                                                                  (call (toplevel-ref map) (lref gen-var)
                                                                                                                   (lref ids))])
                                                                                                              (let
                                                                                                                 ([w
                                                                                                                   (call (lref make-binding-wrap) (lref ids)
                                                                                                                    (lref labels)
                                                                                                                    (lref w))]
                                                                                                                 [r
                                                                                                                   (call (lref extend-var-env) (lref labels)
                                                                                                                    (lref new-vars)
                                                                                                                    (lref r))])
                                                                                                               (call (lref build-letrec) (lref s)
                                                                                                                (call (toplevel-ref map) (toplevel-ref syntax->datum)
                                                                                                                 (lref ids))
                                                                                                                (lref new-vars)
                                                                                                                (call (toplevel-ref map) (lambda (x)
                                                                                                                  (call (lref expand) (lref x)
                                                                                                                   (lref r)
                                                                                                                   (lref w)
                                                                                                                   (lref mod)))
                                                                                                                 (lref val))
                                                                                                                (call (lref expand-body) (#%cons (lref e1)
                                                                                                                  (lref e2))
                                                                                                                 (call (lref source-wrap) (lref e)
                                                                                                                  (lref w)
                                                                                                                  (lref s)
                                                                                                                  (lref mod))
                                                                                                                 (lref r)
                                                                                                                 (lref w)
                                                                                                                 (lref mod))))))))
                                                                                                        (lref tmp))
                                                                                                       (call (toplevel-ref syntax-violation) (const letrec)
                                                                                                         (const bad letrec)
                                                                                                         (call (lref source-wrap) (lref e)
                                                                                                          (lref w)
                                                                                                          (lref s)
                                                                                                          (lref mod))))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const letrec*)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp
                                                                                                        (lref e)])
                                                                                                    (let
                                                                                                       ([tmp
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                                                                          (const (_ #(each (any any)) any . each-any)))])
                                                                                                     (if
                                                                                                       (let
                                                                                                           ([and-tmp
                                                                                                             (lref tmp)])
                                                                                                         (if
                                                                                                           (lref and-tmp)
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                             (call (toplevel-ref and-map) (lref id?)
                                                                                                              (lref id)))
                                                                                                            (lref tmp))
                                                                                                           (const false)))
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (id val e1 e2)
                                                                                                         (let
                                                                                                            ([ids
                                                                                                              (lref id)])
                                                                                                          (if
                                                                                                            (#%not (call (lref valid-bound-ids?) (lref ids)))
                                                                                                            (call (toplevel-ref syntax-violation) (const letrec)
                                                                                                             (const duplicate bound variable)
                                                                                                             (lref e))
                                                                                                            (let
                                                                                                                ([labels
                                                                                                                  (call (lref gen-labels) (lref ids))]
                                                                                                                [new-vars
                                                                                                                  (call (toplevel-ref map) (lref gen-var)
                                                                                                                   (lref ids))])
                                                                                                              (let
                                                                                                                 ([w
                                                                                                                   (call (lref make-binding-wrap) (lref ids)
                                                                                                                    (lref labels)
                                                                                                                    (lref w))]
                                                                                                                 [r
                                                                                                                   (call (lref extend-var-env) (lref labels)
                                                                                                                    (lref new-vars)
                                                                                                                    (lref r))])
                                                                                                               (call (lref build-letrec*) (lref s)
                                                                                                                (call (toplevel-ref map) (toplevel-ref syntax->datum)
                                                                                                                 (lref ids))
                                                                                                                (lref new-vars)
                                                                                                                (call (toplevel-ref map) (lambda (x)
                                                                                                                  (call (lref expand) (lref x)
                                                                                                                   (lref r)
                                                                                                                   (lref w)
                                                                                                                   (lref mod)))
                                                                                                                 (lref val))
                                                                                                                (call (lref expand-body) (#%cons (lref e1)
                                                                                                                  (lref e2))
                                                                                                                 (call (lref source-wrap) (lref e)
                                                                                                                  (lref w)
                                                                                                                  (lref s)
                                                                                                                  (lref mod))
                                                                                                                 (lref r)
                                                                                                                 (lref w)
                                                                                                                 (lref mod))))))))
                                                                                                        (lref tmp))
                                                                                                       (call (toplevel-ref syntax-violation) (const letrec)
                                                                                                         (const bad letrec)
                                                                                                         (call (lref source-wrap) (lref e)
                                                                                                          (lref w)
                                                                                                          (lref s)
                                                                                                          (lref mod))))))))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const set!)
                                                                                                  (lambda (e r w s mod)
                                                                                                   (let
                                                                                                      ([tmp-1
                                                                                                        (lref e)])
                                                                                                    (let
                                                                                                       ([tmp
                                                                                                         (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                          (const (_ any any)))])
                                                                                                     (if
                                                                                                       (let
                                                                                                           ([and-tmp
                                                                                                             (lref tmp)])
                                                                                                         (if
                                                                                                           (lref and-tmp)
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (id val)
                                                                                                             (call (lref id?) (lref id)))
                                                                                                            (lref tmp))
                                                                                                           (const false)))
                                                                                                       (call (module-ref (capy)::apply #t) (lambda (id val)
                                                                                                         (receive (type value id-mod)
                                                                                                           (call (lref resolve-identifier) (lref id)
                                                                                                             (lref w)
                                                                                                             (lref r)
                                                                                                             (lref mod)
                                                                                                             (const true))
                                                                                                           (let
                                                                                                               ([key
                                                                                                                 (lref type)])
                                                                                                             (let
                                                                                                                ([cond-test-tmp
                                                                                                                  (#%eq? (lref key)
                                                                                                                   (const lexical))])
                                                                                                              (if
                                                                                                                (lref cond-test-tmp)
                                                                                                                (call (lref build-lexical-assignment) (lref s)
                                                                                                                 (call (toplevel-ref syntax->datum) (lref id))
                                                                                                                 (lref value)
                                                                                                                 (call (lref expand) (lref val)
                                                                                                                  (lref r)
                                                                                                                  (lref w)
                                                                                                                  (lref mod)))
                                                                                                                (let
                                                                                                                    ([cond-test-tmp
                                                                                                                      (#%eq? (lref key)
                                                                                                                       (const global))])
                                                                                                                  (if
                                                                                                                    (lref cond-test-tmp)
                                                                                                                    (call (lref build-global-assignment) (lref s)
                                                                                                                     (lref value)
                                                                                                                     (call (lref expand) (lref val)
                                                                                                                      (lref r)
                                                                                                                      (lref w)
                                                                                                                      (lref mod))
                                                                                                                     (lref id-mod))
                                                                                                                    (let
                                                                                                                        ([cond-test-tmp
                                                                                                                          (#%eq? (lref key)
                                                                                                                           (const macro))])
                                                                                                                      (if
                                                                                                                        (lref cond-test-tmp)
                                                                                                                        (if
                                                                                                                          (call (toplevel-ref procedure-property) (lref value)
                                                                                                                            (const variable-transformer))
                                                                                                                          (call (lref expand) (call (lref expand-macro) (lref value)
                                                                                                                            (lref e)
                                                                                                                            (lref r)
                                                                                                                            (lref w)
                                                                                                                            (lref s)
                                                                                                                            (const false)
                                                                                                                            (lref mod))
                                                                                                                           (lref r)
                                                                                                                           (lref empty-wrap)
                                                                                                                           (lref mod))
                                                                                                                          (call (toplevel-ref syntax-violation) (const set!)
                                                                                                                            (const not a variable transformer)
                                                                                                                            (call (lref wrap) (lref e)
                                                                                                                             (lref w)
                                                                                                                             (lref mod))
                                                                                                                            (call (lref wrap) (lref id)
                                                                                                                             (lref w)
                                                                                                                             (lref id-mod))))
                                                                                                                        (let
                                                                                                                            ([cond-test-tmp
                                                                                                                              (#%eq? (lref key)
                                                                                                                               (const displaced-lexical))])
                                                                                                                          (if
                                                                                                                            (lref cond-test-tmp)
                                                                                                                            (call (toplevel-ref syntax-violation) (const set!)
                                                                                                                             (const identifier out of context)
                                                                                                                             (call (lref wrap) (lref id)
                                                                                                                              (lref w)
                                                                                                                              (lref mod)))
                                                                                                                            (call (toplevel-ref syntax-violation) (const set!)
                                                                                                                              (const bad set!)
                                                                                                                              (call (lref source-wrap) (lref e)
                                                                                                                               (lref w)
                                                                                                                               (lref s)
                                                                                                                               (lref mod))))))))))))))
                                                                                                        (lref tmp))
                                                                                                       (let
                                                                                                           ([tmp
                                                                                                             (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                                                                                              (const (_ (any . each-any) any)))])
                                                                                                         (if
                                                                                                           (lref tmp)
                                                                                                           (call (module-ref (capy)::apply #t) (lambda (head tail val)
                                                                                                             (receive (type value ee* ee ww ss modmod)
                                                                                                               (call (lref syntax-type) (lref head)
                                                                                                                 (lref r)
                                                                                                                 (lref empty-wrap)
                                                                                                                 (lref no-source)
                                                                                                                 (const false)
                                                                                                                 (lref mod)
                                                                                                                 (const true))
                                                                                                               (let
                                                                                                                   ([key
                                                                                                                     (lref type)])
                                                                                                                 (if
                                                                                                                   (#%eq? (lref key)
                                                                                                                     (const module-ref))
                                                                                                                   (let
                                                                                                                      ([val
                                                                                                                        (call (lref expand) (lref val)
                                                                                                                         (lref r)
                                                                                                                         (lref w)
                                                                                                                         (lref mod))])
                                                                                                                    (receive (e r w s* mod)
                                                                                                                      (call (lref value) (#%cons (lref head)
                                                                                                                         (lref tail))
                                                                                                                        (lref r)
                                                                                                                        (lref w)
                                                                                                                        (lref mod))
                                                                                                                      (let
                                                                                                                          ([tmp-1
                                                                                                                            (lref e)])
                                                                                                                        (let
                                                                                                                           ([tmp
                                                                                                                             (#%cons (lref tmp-1)
                                                                                                                              (const '()))])
                                                                                                                         (if
                                                                                                                           (let
                                                                                                                               ([and-tmp
                                                                                                                                 (lref tmp)])
                                                                                                                             (if
                                                                                                                               (lref and-tmp)
                                                                                                                               (call (module-ref (capy)::apply #t) (lambda (e)
                                                                                                                                 (call (lref id?) (lref e)))
                                                                                                                                (lref tmp))
                                                                                                                               (const false)))
                                                                                                                           (call (module-ref (capy)::apply #t) (lambda (e)
                                                                                                                             (call (lref build-global-assignment) (lref s)
                                                                                                                              (call (toplevel-ref syntax->datum) (lref e))
                                                                                                                              (lref val)
                                                                                                                              (lref mod)))
                                                                                                                            (lref tmp))
                                                                                                                           (call (toplevel-ref syntax-violation) (const false)
                                                                                                                             (const source expression failed to match any pattern)
                                                                                                                             (lref tmp-1)))))))
                                                                                                                   (call (lref build-call) (lref s)
                                                                                                                     (call (lref expand) (#%cons (call (lref make-syntax) (const setter)
                                                                                                                        (const ((top)))
                                                                                                                        (const (hygiene capy)))
                                                                                                                       (#%cons (lref head)
                                                                                                                        (const '())))
                                                                                                                      (lref r)
                                                                                                                      (lref w)
                                                                                                                      (lref mod))
                                                                                                                     (call (toplevel-ref map) (lambda (e)
                                                                                                                       (call (lref expand) (lref e)
                                                                                                                        (lref r)
                                                                                                                        (lref w)
                                                                                                                        (lref mod)))
                                                                                                                      (#%append (lref tail)
                                                                                                                       (#%cons (lref val)
                                                                                                                        (const '())))))))))
                                                                                                            (lref tmp))
                                                                                                           (call (toplevel-ref syntax-violation) (const set!)
                                                                                                             (const bad set!)
                                                                                                             (call (lref source-wrap) (lref e)
                                                                                                              (lref w)
                                                                                                              (lref s)
                                                                                                              (lref mod))))))))))
                                                                                                 (call (lref global-extend) (const local-syntax)
                                                                                                  (const letrec-syntax)
                                                                                                  (const true))
                                                                                                 (call (lref global-extend) (const local-syntax)
                                                                                                  (const let-syntax)
                                                                                                  (const false))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const syntax-case)
                                                                                                  (lref expand-syntax-case))
                                                                                                 (call (lref global-extend) (const core)
                                                                                                  (const syntax)
                                                                                                  (lref expand-syntax))
                                                                                                 (call (lref global-extend) (const define-syntax)
                                                                                                  (const define-syntax)
                                                                                                  (const '()))
                                                                                                 (call (lref global-extend) (const define-syntax-parameter)
                                                                                                  (const define-syntax-parameter)
                                                                                                  (const '()))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
 (define with-syntax (let
    ([make-syntax
      (toplevel-ref make-syntax)])
  (call (toplevel-ref make-syntax-transformer) (const with-syntax)
   (const macro)
   (lambda (x)
    (let
       ([tmp
         (lref x)])
     (let
        ([tmp-1
          (call (toplevel-ref $sc-dispatch) (lref tmp)
           (const (_ '() any . each-any)))])
      (if
        (lref tmp-1)
        (call (module-ref (capy)::apply #t) (lambda (e1 e2)
          (#%cons (call (lref make-syntax) (const let)
            (const ((top)))
            (const (hygiene capy)))
           (#%cons (const '()) (#%cons (lref e1) (lref e2)))))
         (lref tmp-1))
        (let
            ([tmp-1
              (call (toplevel-ref $sc-dispatch) (lref tmp)
               (const (_ ((any any)) any . each-any)))])
          (if
            (lref tmp-1)
            (call (module-ref (capy)::apply #t) (lambda (out in e1 e2)
              (#%cons (call (lref make-syntax) (const syntax-case)
                (const ((top)))
                (const (hygiene capy)))
               (#%cons (lref in)
                (#%cons (const '())
                 (#%cons (#%cons (lref out)
                   (#%cons (#%cons (call (lref make-syntax) (const let)
                      (const ((top)))
                      (const (hygiene capy)))
                     (#%cons (const '())
                      (#%cons (lref e1) (lref e2))))
                    (const '())))
                  (const '()))))))
             (lref tmp-1))
            (let
                ([tmp-1
                  (call (toplevel-ref $sc-dispatch) (lref tmp)
                   (const (_ #(each (any any)) any . each-any)))])
              (if
                (lref tmp-1)
                (call (module-ref (capy)::apply #t) (lambda (out in e1 e2)
                  (#%cons (call (lref make-syntax) (const syntax-case)
                    (const ((top)))
                    (const (hygiene capy)))
                   (#%cons (#%cons (call (lref make-syntax) (const list)
                      (const ((top)))
                      (const (hygiene capy)))
                     (lref in))
                    (#%cons (const '())
                     (#%cons (#%cons (lref out)
                       (#%cons (#%cons (call (lref make-syntax) (const let)
                          (const ((top)))
                          (const (hygiene capy)))
                         (#%cons (const '())
                          (#%cons (lref e1) (lref e2))))
                        (const '())))
                      (const '()))))))
                 (lref tmp-1))
                (call (toplevel-ref syntax-violation) (const false)
                  (const source expression failed to match any pattern)
                  (lref tmp)))))))))))))
 (define syntax-error (let
    ([make-syntax
      (toplevel-ref make-syntax)])
  (call (toplevel-ref make-syntax-transformer) (const syntax-error)
   (const macro)
   (lambda (x)
    (let
       ([tmp-1
         (lref x)])
     (let
        ([tmp
          (call (toplevel-ref $sc-dispatch) (lref tmp-1)
           (const (_ (any . any) any . each-any)))])
      (if
        (if
           (lref tmp)
           (call (module-ref (capy)::apply #t) (lambda (keyword operands message arg)
             (#%string? (call (toplevel-ref syntax->datum) (lref message))))
            (lref tmp))
           (const false))
        (call (module-ref (capy)::apply #t) (lambda (keyword operands message arg)
          (call (toplevel-ref syntax-violation) (call (toplevel-ref syntax->datum) (lref keyword))
           (call (toplevel-ref string-join) (#%cons (call (toplevel-ref syntax->datum) (lref message))
             (call (toplevel-ref map) (lambda (x)
               (call (toplevel-ref object->string) (call (toplevel-ref syntax->datum) (lref x))))
              (lref arg))))
           (if
             (call (toplevel-ref syntax->datum) (lref keyword))
             (#%cons (lref keyword) (lref operands))
             (const false))))
         (lref tmp))
        (let
            ([tmp
              (call (toplevel-ref $sc-dispatch) (lref tmp-1)
               (const (_ any . each-any)))])
          (if
            (if
               (lref tmp)
               (call (module-ref (capy)::apply #t) (lambda (message arg)
                 (#%string? (call (toplevel-ref syntax->datum) (lref message))))
                (lref tmp))
               (const false))
            (call (module-ref (capy)::apply #t) (lambda (message arg)
              (#%cons (call (lref make-syntax) (const syntax-error)
                (#%cons (const (top))
                 (#%cons (let
                     ([vec
                       (#%make-vector (const 4)
                        (const #<undefined>))])
                   (seq
                    (#%vector-set! (lref vec)
                     (const 0)
                     (const ribcage))
                    (#%vector-set! (lref vec)
                     (const 1)
                     (const #(syntax-error)))
                    (#%vector-set! (lref vec)
                     (const 2)
                     (const #((top))))
                    (#%vector-set! (lref vec)
                     (const 3)
                     (let
                        ([vec
                          (#%make-vector (const 1)
                           (const #<undefined>))])
                      (seq
                       (#%vector-set! (lref vec)
                        (const 0)
                        (#%cons (const (hygiene capy))
                         (call (lref make-syntax) (const syntax-error)
                          (const ((top)))
                          (const (hygiene capy)))))
                       (lref vec))))
                    (lref vec)))
                  (const '())))
                (const (hygiene capy)))
               (#%cons (const (false))
                (#%cons (lref message) (lref arg)))))
             (lref tmp))
            (call (toplevel-ref syntax-violation) (const false)
              (const source expression failed to match any pattern)
              (lref tmp-1)))))))))))
 (define syntax-rules (let
    ([make-syntax
      (toplevel-ref make-syntax)])
  (call (toplevel-ref make-syntax-transformer) (const syntax-rules)
   (const macro)
   (lambda (xx)
    (fix
      ([expand-clause (lambda (clause)
        (let
           ([tmp-1
             (lref clause)])
         (let
            ([tmp
              (call (toplevel-ref $sc-dispatch) (lref tmp-1)
               (#%cons (const (any . any))
                (#%cons (#%cons (let
                     ([vec
                       (#%make-vector (const 2)
                        (const #<undefined>))])
                   (seq
                    (#%vector-set! (lref vec)
                     (const 0)
                     (const free-id))
                    (#%vector-set! (lref vec)
                     (const 1)
                     (call (lref make-syntax) (const syntax-error)
                      (const ((top)))
                      (const (hygiene capy))))
                    (lref vec)))
                  (const (any . each-any)))
                 (const '()))))])
          (if
            (if
               (lref tmp)
               (call (module-ref (capy)::apply #t) (lambda (keyword pattern message arg)
                 (#%string? (call (toplevel-ref syntax->datum) (lref message))))
                (lref tmp))
               (const false))
            (call (module-ref (capy)::apply #t) (lambda (keyword pattern message arg)
              (#%cons (#%cons (call (lref make-syntax) (const dummy)
                 (const ((top)))
                 (const (hygiene capy)))
                (lref pattern))
               (#%cons (#%cons (call (lref make-syntax) (const syntax)
                  (const ((top)))
                  (const (hygiene capy)))
                 (#%cons (#%cons (call (lref make-syntax) (const syntax-error)
                    (const ((top)))
                    (const (hygiene capy)))
                   (#%cons (#%cons (call (lref make-syntax) (const dummy)
                      (const ((top)))
                      (const (hygiene capy)))
                     (lref pattern))
                    (#%cons (lref message) (lref arg))))
                  (const '())))
                (const '()))))
             (lref tmp))
            (const (quote (let ((tmp ($sc-dispatch tmp-1 (quote ((any . any) any))))) (if tmp (apply (lambda (keyword pattern template) (list (cons (make-syntax (quote dummy) (quote ((top))) (quote (hygiene capy))) pattern) (list (make-syntax (quote syntax) (quote ((top))) (quote (hygiene capy))) template))) tmp) (syntax-violation false source expression failed to match any pattern tmp-1)))))))))])
     (fix
       ([expand-syntax-rules (lambda (dots keys docstrings clauses)
         (let
            ([tmp-1
              (#%cons (lref keys)
               (#%cons (lref docstrings)
                (#%cons (lref clauses)
                 (#%cons (call (toplevel-ref map) (lref expand-clause)
                   (lref clauses))
                  (const '())))))])
          (let
             ([tmp
               (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                (const (each-any each-any #(each ((any . any) any)) each-any)))])
           (if
             (lref tmp)
             (call (module-ref (capy)::apply #t) (lambda (k docstring keyword pattern template clause)
               (let
                  ([tmp
                    (#%cons (call (lref make-syntax) (const lambda)
                      (const ((top)))
                      (const (hygiene capy)))
                     (#%cons (#%cons (call (lref make-syntax) (const x)
                        (const ((top)))
                        (const (hygiene capy)))
                       (const '()))
                      (#%append (lref docstring)
                       (#%cons (let
                           ([vec
                             (#%make-vector (const 2)
                              (const #<undefined>))])
                         (seq
                          (#%vector-set! (lref vec)
                           (const 0)
                           (#%cons (call (lref make-syntax) (const macro-type)
                             (const ((top)))
                             (const (hygiene capy)))
                            (call (lref make-syntax) (const syntax-rules)
                             (#%cons (const (top))
                              (#%cons (let
                                  ([vec
                                    (#%make-vector (const 4)
                                     (const #<undefined>))])
                                (seq
                                 (#%vector-set! (lref vec)
                                  (const 0)
                                  (const ribcage))
                                 (#%vector-set! (lref vec)
                                  (const 1)
                                  (const #(syntax-rules)))
                                 (#%vector-set! (lref vec)
                                  (const 2)
                                  (const #((top))))
                                 (#%vector-set! (lref vec)
                                  (const 3)
                                  (let
                                     ([vec
                                       (#%make-vector (const 1)
                                        (const #<undefined>))])
                                   (seq
                                    (#%vector-set! (lref vec)
                                     (const 0)
                                     (#%cons (const (hygiene capy))
                                      (call (lref make-syntax) (const syntax-rules)
                                       (const ((top)))
                                       (const (hygiene capy)))))
                                    (lref vec))))
                                 (lref vec)))
                               (const '())))
                             (const (hygiene capy)))))
                          (#%vector-set! (lref vec)
                           (const 1)
                           (#%cons (call (lref make-syntax) (const patterns)
                             (const ((top)))
                             (const (hygiene capy)))
                            (lref pattern)))
                          (lref vec)))
                        (#%cons (#%cons (call (lref make-syntax) (const syntax-case)
                           (const ((top)))
                           (const (hygiene capy)))
                          (#%cons (call (lref make-syntax) (const x)
                            (const ((top)))
                            (const (hygiene capy)))
                           (#%cons (lref k) (lref clause))))
                         (const '()))))))])
                (let
                   ([form
                     (lref tmp)])
                 (if
                   (lref dots)
                   (let
                      ([tmp
                        (lref dots)])
                    (let
                       ([dots
                         (lref tmp)])
                     (#%cons (call (lref make-syntax) (const with-ellipsis)
                       (const ((top)))
                       (const (hygiene capy)))
                      (#%cons (lref dots)
                       (#%cons (lref form) (const '()))))))
                   (lref form)))))
              (lref tmp))
             (call (toplevel-ref syntax-violation) (const false)
               (const source expression failed to match any pattern)
               (lref tmp-1))))))])
      (let
         ([tmp
           (lref xx)])
       (let
          ([tmp-1
            (call (toplevel-ref $sc-dispatch) (lref tmp)
             (const (_ each-any . #(each ((any . any) any)))))])
        (if
          (lref tmp-1)
          (call (module-ref (capy)::apply #t) (lambda (k keyword pattern template)
            (call (lref expand-syntax-rules) (const false)
             (lref k)
             (const '())
             (call (toplevel-ref map) (lambda (tmp-680b775fb37a463-145d tmp-680b775fb37a463-145c tmp-680b775fb37a463-145b)
               (#%cons (#%cons (lref tmp-680b775fb37a463-145b)
                 (lref tmp-680b775fb37a463-145c))
                (#%cons (lref tmp-680b775fb37a463-145d) (const '()))))
              (lref template)
              (lref pattern)
              (lref keyword))))
           (lref tmp-1))
          (let
              ([tmp-1
                (call (toplevel-ref $sc-dispatch) (lref tmp)
                 (const (_ each-any any . #(each ((any . any) any)))))])
            (if
              (if
                 (lref tmp-1)
                 (call (module-ref (capy)::apply #t) (lambda (k docstring keyword pattern template)
                   (#%string? (call (toplevel-ref syntax->datum) (lref docstring))))
                  (lref tmp-1))
                 (const false))
              (call (module-ref (capy)::apply #t) (lambda (k docstring keyword pattern template)
                (call (lref expand-syntax-rules) (const false)
                 (lref k)
                 (#%cons (lref docstring) (const '()))
                 (call (toplevel-ref map) (lambda (tmp-680b775fb37a463-2 tmp-680b775fb37a463-1 tmp-680b775fb37a463)
                   (#%cons (#%cons (lref tmp-680b775fb37a463)
                     (lref tmp-680b775fb37a463-1))
                    (#%cons (lref tmp-680b775fb37a463-2)
                     (const '()))))
                  (lref template)
                  (lref pattern)
                  (lref keyword))))
               (lref tmp-1))
              (let
                  ([tmp-1
                    (call (toplevel-ref $sc-dispatch) (lref tmp)
                     (const (_ any each-any . #(each ((any . any) any)))))])
                (if
                  (if
                     (lref tmp-1)
                     (call (module-ref (capy)::apply #t) (lambda (dots k keyword pattern template)
                       (call (toplevel-ref identifier?) (lref dots)))
                      (lref tmp-1))
                     (const false))
                  (call (module-ref (capy)::apply #t) (lambda (dots k keyword pattern template)
                    (call (lref expand-syntax-rules) (lref dots)
                     (lref k)
                     (const '())
                     (call (toplevel-ref map) (lambda (tmp-680b775fb37a463-148f tmp-680b775fb37a463-148e tmp-680b775fb37a463-148d)
                       (#%cons (#%cons (lref tmp-680b775fb37a463-148d)
                         (lref tmp-680b775fb37a463-148e))
                        (#%cons (lref tmp-680b775fb37a463-148f)
                         (const '()))))
                      (lref template)
                      (lref pattern)
                      (lref keyword))))
                   (lref tmp-1))
                  (let
                      ([tmp-1
                        (call (toplevel-ref $sc-dispatch) (lref tmp)
                         (const (_ any each-any any . #(each ((any . any) any)))))])
                    (if
                      (if
                         (lref tmp-1)
                         (call (module-ref (capy)::apply #t) (lambda (dots k docstring keyword pattern template)
                           (if
                             (call (toplevel-ref identifier?) (lref dots))
                             (#%string? (call (toplevel-ref syntax->datum) (lref docstring)))
                             (const false)))
                          (lref tmp-1))
                         (const false))
                      (call (module-ref (capy)::apply #t) (lambda (dots k docstring keyword pattern template)
                        (call (lref expand-syntax-rules) (lref dots)
                         (lref k)
                         (#%cons (lref docstring) (const '()))
                         (call (toplevel-ref map) (lambda (tmp-680b775fb37a463-14ae tmp-680b775fb37a463-14ad tmp-680b775fb37a463-14ac)
                           (#%cons (#%cons (lref tmp-680b775fb37a463-14ac)
                             (lref tmp-680b775fb37a463-14ad))
                            (#%cons (lref tmp-680b775fb37a463-14ae)
                             (const '()))))
                          (lref template)
                          (lref pattern)
                          (lref keyword))))
                       (lref tmp-1))
                      (call (toplevel-ref syntax-violation) (const false)
                        (const source expression failed to match any pattern)
                        (lref tmp)))))))))))))))))
 (define define-syntax-rule (let
    ([make-syntax
      (toplevel-ref make-syntax)])
  (call (toplevel-ref make-syntax-transformer) (const define-syntax-rule)
   (const macro)
   (lambda (x)
    (let
       ([tmp-1
         (lref x)])
     (let
        ([tmp
          (call (toplevel-ref $sc-dispatch) (lref tmp-1)
           (const (_ (any . any) any)))])
      (if
        (lref tmp)
        (call (module-ref (capy)::apply #t) (lambda (name pattern template)
          (#%cons (call (lref make-syntax) (const define-syntax)
            (const ((top)))
            (const (hygiene capy)))
           (#%cons (lref name)
            (#%cons (#%cons (call (lref make-syntax) (const syntax-rules)
               (const ((top)))
               (const (hygiene capy)))
              (#%cons (const '())
               (#%cons (#%cons (#%cons (call (lref make-syntax) (const _)
                   (const ((top)))
                   (const (hygiene capy)))
                  (lref pattern))
                 (#%cons (lref template) (const '())))
                (const '()))))
             (const '())))))
         (lref tmp))
        (let
            ([tmp
              (call (toplevel-ref $sc-dispatch) (lref tmp-1)
               (const (_ (any . any) any any)))])
          (if
            (if
               (lref tmp)
               (call (module-ref (capy)::apply #t) (lambda (name pattern docstring template)
                 (#%string? (call (toplevel-ref syntax->datum) (lref docstring))))
                (lref tmp))
               (const false))
            (call (module-ref (capy)::apply #t) (lambda (name pattern docstring template)
              (#%cons (call (lref make-syntax) (const define-syntax)
                (const ((top)))
                (const (hygiene capy)))
               (#%cons (lref name)
                (#%cons (#%cons (call (lref make-syntax) (const syntax-rules)
                   (const ((top)))
                   (const (hygiene capy)))
                  (#%cons (const '())
                   (#%cons (lref docstring)
                    (#%cons (#%cons (#%cons (call (lref make-syntax) (const _)
                        (const ((top)))
                        (const (hygiene capy)))
                       (lref pattern))
                      (#%cons (lref template) (const '())))
                     (const '())))))
                 (const '())))))
             (lref tmp))
            (call (toplevel-ref syntax-violation) (const false)
              (const source expression failed to match any pattern)
              (lref tmp-1))))))))))))
