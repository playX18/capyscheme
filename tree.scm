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
    ([wrap-syntax (lambda (x w defmod)
      (call (lref make-syntax) (call (lref syntax-expression) (lref x))
       (lref w)
       (let
          ([or-tmp
            (call (lref syntax-module) (lref x))])
        (if  (lref or-tmp)  (lref or-tmp)  (lref defmod)))
       (call (lref syntax-sourcev) (lref x))))]
    [strip (lambda (x)
      (let
         ([cond-test-tmp
           (call (lref syntax?) (lref x))])
       (if
         (lref cond-test-tmp)
         (call (lref strip) (call (lref syntax-expression) (lref x)))
         (let
             ([cond-test-tmp
               (call (toplevel-ref pair?) (lref x))])
           (if
             (lref cond-test-tmp)
             (#%cons (call (lref strip) (call (toplevel-ref car) (lref x)))
              (call (lref strip) (call (toplevel-ref cdr) (lref x))))
             (let
                 ([cond-test-tmp
                   (call (toplevel-ref vector?) (lref x))])
               (if
                 (lref cond-test-tmp)
                 (call (toplevel-ref list->vector) (call (lref strip) (call (toplevel-ref vector->list) (lref x))))
                 (lref x))))))))])
   (let
      ([transformer-environment
        (const #<undefined>)])
    (let
       ([&transformer-environment
         (#%box (lref transformer-environment))])
     (fix
       ([join-marks (lambda (m1 m2)
         (call (lref smart-append) (lref m1) (lref m2)))]
       [smart-append (lambda (m1 m2)
         (if
           (#%null? (lref m2))
           (lref m1)
           (call (toplevel-ref append) (lref m1) (lref m2))))]
       [same-marks? (lambda (x y)
         (let
            ([or-tmp
              (#%eq? (lref x) (lref y))])
          (if
            (lref or-tmp)
            (lref or-tmp)
            (let
                ([and-tmp
                  (call (toplevel-ref not) (#%null? (lref x)))])
              (if
                (lref and-tmp)
                (let
                   ([and-tmp
                     (call (toplevel-ref not) (#%null? (lref y)))])
                 (if
                   (lref and-tmp)
                   (let
                      ([and-tmp
                        (#%eq? (call (toplevel-ref car) (lref x))
                         (call (toplevel-ref car) (lref y)))])
                    (if
                      (lref and-tmp)
                      (call (lref same-marks?) (call (toplevel-ref cdr) (lref x))
                       (call (toplevel-ref cdr) (lref y)))
                      (const false)))
                   (const false)))
                (const false))))))]
       [with-transformer-environment (lambda (k)
         (call (call (toplevel-ref fluid-ref) (#%box-ref (lref &transformer-environment))) (lref k)))])
      (seq
       (#%set-box! (lref &transformer-environment)
        (call (toplevel-ref make-fluid) (lambda (k)
          (call (toplevel-ref assertion-violation) (const transformer-environment)
           (const called outside the dynamic extent of a syntax transformer)))))
       (let
          ([the-anti-mark
            (const false)])
        (let
           ([top-wrap
             (const ((top)))])
         (let
            ([empty-wrap
              (const ('()))])
          (fix
            ([extend-env (lambda (labels bindings r)
              (let
                 ([cond-test-tmp
                   (#%null? (lref labels))])
               (if
                 (lref cond-test-tmp)
                 (lref r)
                 (let
                     ([cond-test-tmp
                       (call (toplevel-ref pair?) (lref labels))])
                   (if
                     (lref cond-test-tmp)
                     (let
                        ([label
                          (call (toplevel-ref car) (lref labels))]
                        [labels
                          (call (toplevel-ref cdr) (lref labels))])
                      (let
                         ([binding
                           (call (toplevel-ref car) (lref bindings))]
                         [bindings
                           (call (toplevel-ref cdr) (lref bindings))])
                       (call (lref extend-env) (lref labels)
                        (lref bindings)
                        (call (toplevel-ref acons) (lref label)
                         (lref binding)
                         (lref r)))))
                     (const #<undefined>))))))]
            [extend-var-env (lambda (labels vars r)
              (let
                 ([cond-test-tmp
                   (#%null? (lref labels))])
               (if
                 (lref cond-test-tmp)
                 (lref r)
                 (let
                     ([cond-test-tmp
                       (call (toplevel-ref pair?) (lref labels))])
                   (if
                     (lref cond-test-tmp)
                     (let
                        ([label
                          (call (toplevel-ref car) (lref labels))]
                        [labels
                          (call (toplevel-ref cdr) (lref labels))])
                      (let
                         ([var
                           (call (toplevel-ref car) (lref vars))]
                         [vars
                           (call (toplevel-ref cdr) (lref vars))])
                       (call (lref extend-var-env) (lref labels)
                        (lref vars)
                        (call (toplevel-ref acons) (lref label)
                         (#%cons (const lexical) (lref var))
                         (lref r)))))
                     (const #<undefined>))))))]
            [macros-only-env (lambda (r)
              (let*
                 ([v
                   (lref r)]
                 [fk
                   (lambda ()
                    (let
                       ([fk
                         (lambda ()
                          (call (toplevel-ref error) (const value failed to match)
                           (lref v)))])
                     (if
                       (call (toplevel-ref pair?) (lref v))
                       (let
                          ([vx
                            (call (toplevel-ref car) (lref v))]
                          [vy
                            (call (toplevel-ref cdr) (lref v))])
                        (let*
                           ([a
                             (lref vx)]
                           [r
                             (lref vy)]
                           [v
                             (lref a)]
                           [fk
                             (lambda ()
                              (let
                                 ([fk
                                   (lambda ()
                                    (call (toplevel-ref error) (const value failed to match)
                                     (lref v)))])
                               (call (lref macros-only-env) (lref r))))])
                         (if
                           (call (toplevel-ref pair?) (lref v))
                           (let
                              ([vx
                                (call (toplevel-ref car) (lref v))]
                              [vy
                                (call (toplevel-ref cdr) (lref v))])
                            (let
                               ([k
                                 (lref vx)])
                             (if
                               (call (toplevel-ref pair?) (lref vy))
                               (let
                                  ([vx
                                    (call (toplevel-ref car) (lref vy))]
                                  [vy
                                    (call (toplevel-ref cdr) (lref vy))])
                                (let
                                   ([tk
                                     (lambda ()
                                      (#%cons (lref a)
                                       (call (lref macros-only-env) (lref r))))])
                                 (if
                                   (#%eq? (lref vx) (const macro))
                                   (call (lref tk))
                                   (let
                                       ([tk
                                         (lambda ()
                                          (call (lref tk)))])
                                     (if
                                       (#%eq? (lref vx)
                                         (const syntax-parameter))
                                       (call (lref tk))
                                       (let
                                           ([tk
                                             (lambda ()
                                              (call (lref tk)))])
                                         (if
                                           (#%eq? (lref vx)
                                             (const ellipsis))
                                           (call (lref tk))
                                           (call (lref fk)))))))))
                               (call (lref fk)))))
                           (call (lref fk)))))
                       (call (lref fk)))))])
               (if
                 (#%null? (lref v))
                 (const '())
                 (call (lref fk)))))]
            [nonsymbol-id? (lambda (x)
              (let
                 ([and-tmp
                   (call (lref syntax?) (lref x))])
               (if
                 (lref and-tmp)
                 (#%symbol? (call (lref syntax-expression) (lref x)))
                 (const false))))]
            [valid-bound-ids? (lambda (ids)
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
                           (call (toplevel-ref pair?) (lref ids))])
                       (if
                         (lref cond-test-tmp)
                         (let
                            ([id
                              (call (toplevel-ref car) (lref ids))]
                            [ids
                              (call (toplevel-ref cdr) (lref ids))])
                          (let
                             ([and-tmp
                               (call (lref id?) (lref id))])
                           (if
                             (lref and-tmp)
                             (call (lref all-ids?) (lref ids))
                             (const false))))
                         (const false))))))])
               (call (lref all-ids?) (lref ids))))]
            [lambda-var-list (lambda (vars)
              (fix
                ([lvl (lambda (vars ls w)
                  (let
                     ([cond-test-tmp
                       (call (toplevel-ref pair?) (lref vars))])
                   (if
                     (lref cond-test-tmp)
                     (call (lref lvl) (call (toplevel-ref cdr) (lref vars))
                      (#%cons (call (lref wrap) (call (toplevel-ref car) (lref vars))
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
                (lref empty-wrap))))]
            [id? (lambda (x)
              (let
                 ([or-tmp
                   (#%symbol? (lref x))])
               (if
                 (lref or-tmp)
                 (lref or-tmp)
                 (let
                     ([and-tmp
                       (call (lref syntax?) (lref x))])
                   (if
                     (lref and-tmp)
                     (#%symbol? (call (lref syntax-expression) (lref x)))
                     (const false))))))]
            [free-id=? (lambda (i j)
              (let*
                 ([mi
                   (let
                      ([and-tmp
                        (call (lref syntax?) (lref i))])
                    (if
                      (lref and-tmp)
                      (call (lref syntax-module) (lref i))
                      (const false)))]
                 [mj
                   (let
                      ([and-tmp
                        (call (lref syntax?) (lref j))])
                    (if
                      (lref and-tmp)
                      (call (lref syntax-module) (lref j))
                      (const false)))]
                 [ni
                   (call (lref id-var-name) (lref i)
                    (lref empty-wrap)
                    (lref mi))]
                 [nj
                   (call (lref id-var-name) (lref j)
                    (lref empty-wrap)
                    (lref mj))])
               (fix
                 ([id-module-binding (lambda (id mod)
                   (call (toplevel-ref module-variable) (if
                      (lref mod)
                      (call (toplevel-ref resolve-module) (call (toplevel-ref cdr) (lref mod))
                       (const true)
                       (const true))
                      (#%current-module))
                    (call (lref id-sym-name) (lref id))))])
                (let
                   ([cond-test-tmp
                     (call (lref syntax?) (lref ni))])
                 (if
                   (lref cond-test-tmp)
                   (call (lref free-id=?) (lref ni) (lref j))
                   (let
                       ([cond-test-tmp
                         (call (lref syntax?) (lref nj))])
                     (if
                       (lref cond-test-tmp)
                       (call (lref free-id=?) (lref i) (lref nj))
                       (let
                           ([cond-test-tmp
                             (#%symbol? (lref ni))])
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
                                    (#%eq? (lref bi) (lref bj))])
                                (if
                                  (lref and-tmp)
                                  (let
                                     ([or-tmp
                                       (lref bi)])
                                   (if
                                     (lref or-tmp)
                                     (lref or-tmp)
                                     (#%eq? (lref ni) (lref nj))))
                                  (const false))))
                              (const false)))
                           (#%equal? (lref ni) (lref nj)))))))))))]
            [id-sym-name (lambda (x)
              (if
                (call (lref syntax?) (lref x))
                (call (lref syntax-expression) (lref x))
                (lref x)))]
            [make-binding-wrap (lambda (ids labels w)
              (let
                 ([cond-test-tmp
                   (#%null? (lref ids))])
               (if
                 (lref cond-test-tmp)
                 (lref w)
                 (call (lref make-wrap) (call (lref wrap-marks) (lref w))
                   (#%cons (let*
                       ([labelvec
                         (call (toplevel-ref list->vector) (lref labels))]
                       [n
                         (call (toplevel-ref vector-length) (lref labelvec))]
                       [symnamevec
                         (call (toplevel-ref make-vector) (lref n))]
                       [marksvec
                         (call (toplevel-ref make-vector) (lref n))])
                     (fix
                       ([f (lambda (ids i)
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
                                  (call (toplevel-ref car) (lref ids))]
                                [ids
                                  (call (toplevel-ref cdr) (lref ids))])
                              (call (toplevel-ref call-with-values) (lambda ()
                                (call (lref id-sym-name&marks) (lref id)
                                 (lref w)))
                               (lambda (symname marks)
                                (seq
                                 (call (toplevel-ref vector-set!) (lref symnamevec)
                                  (lref i)
                                  (lref symname))
                                 (call (toplevel-ref vector-set!) (lref marksvec)
                                  (lref i)
                                  (lref marks))
                                 (call (lref f) (lref ids)
                                  (#%+ (lref i) (const 1))))))))))])
                      (call (lref f) (lref ids) (const 0)))))))))]
            [id-sym-name&marks (lambda (x w)
              (if
                (call (lref syntax?) (lref x))
                (values (call (lref syntax-expression) (lref x)) (call (lref join-marks) (call (lref wrap-marks) (lref w))
                 (call (lref wrap-marks) (call (lref syntax-wrap) (lref x)))))
                (values (lref x) (lref w))))]
            [locally-bound-identifiers (lambda (w mod)
              (fix
                ([scan (lambda (subst results)
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
                                (call (toplevel-ref pair?) (lref subst))])
                            (if
                              (lref and-tmp)
                              (#%eq? (call (toplevel-ref car) (lref subst))
                               (const shift))
                              (const false)))])
                       (if
                         (lref cond-test-tmp)
                         (call (lref scan) (call (toplevel-ref cdr) (lref subst))
                          (lref results))
                         (let
                             ([cond-test-tmp
                               (let
                                  ([and-tmp
                                    (call (toplevel-ref pair?) (lref subst))])
                                (if
                                  (lref and-tmp)
                                  (let
                                     ([and-tmp
                                       (call (toplevel-ref vector?) (call (toplevel-ref car) (lref subst)))])
                                   (if
                                     (lref and-tmp)
                                     (#%eq? (call (toplevel-ref vector-ref) (call (toplevel-ref car) (lref subst))
                                       (const 0))
                                      (const ribcage))
                                     (const false)))
                                  (const false)))])
                           (if
                             (lref cond-test-tmp)
                             (let*
                                ([ribcage
                                  (call (toplevel-ref car) (lref subst))]
                                [symnames
                                  (call (lref ribcage-symnames) (lref ribcage))]
                                [marks
                                  (call (lref ribcage-marks) (lref ribcage))]
                                [labels
                                  (call (lref ribcage-labels) (lref ribcage))]
                                [subst*
                                  (call (toplevel-ref cdr) (lref subst))])
                              (fix
                                ([scan-list-rib (lambda ()
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
                                               (call (toplevel-ref car) (lref symnames))]
                                             [symnames
                                               (call (toplevel-ref cdr) (lref symnames))]
                                             [m
                                               (call (toplevel-ref car) (lref marks))]
                                             [marks
                                               (call (toplevel-ref cdr) (lref marks))])
                                           (call (lref lp) (lref symnames)
                                            (lref marks)
                                            (#%cons (call (lref wrap) (lref sym)
                                              (call (lref anti-mark) (call (lref make-wrap) (lref m)
                                                (lref subst)))
                                              (lref mod))
                                             (toplevel-ref resulsts)))))))])
                                   (call (lref lp) (lref symnames)
                                    (lref marks)
                                    (lref results))))]
                                [scan-vector-rib (lambda ()
                                  (let
                                     ([n
                                       (call (toplevel-ref vector-length) (lref symnames))])
                                   (fix
                                     ([lp (lambda (i results)
                                       (let
                                          ([cond-test-tmp
                                            (call (toplevel-ref =) (lref i)
                                             (lref n))])
                                        (if
                                          (lref cond-test-tmp)
                                          (call (lref scan) (lref subst*)
                                           (lref results))
                                          (let
                                              ([sym
                                                (call (toplevel-ref vector-ref) (lref symnames)
                                                 (lref i))]
                                              [m
                                                (call (toplevel-ref vector-ref) (lref marks)
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
                                 (call (toplevel-ref vector?) (lref symnames))
                                 (call (lref scan-vector-rib))
                                 (call (lref scan-list-rib)))))
                             (const #<undefined>))))))))])
               (call (lref scan) (call (lref wrap-subst) (lref w))
                (const '()))))]
            [anti-mark (lambda (w)
              (call (lref make-wrap) (#%cons (lref the-anti-mark)
                (call (lref wrap-marks) (lref w)))
               (#%cons (const shift)
                (call (lref wrap-subst) (lref w)))))]
            [wrap (lambda (x w defmod)
              (call (lref source-wrap) (lref x)
               (lref w)
               (const false)
               (lref defmod)))]
            [source-wrap (lambda (x w s defmod)
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
                              (call (toplevel-ref not) (lref defmod))])
                          (if
                            (lref and-tmp)
                            (call (toplevel-ref not) (lref s))
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
                           (lref s)))))))))]
            [join-wraps (lambda (w1 w2)
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
                    (call (lref wrap-subst) (lref w2)))))))]
            [make-wrap (lambda (marks subst)
              (#%cons (lref marks) (lref subst)))]
            [extend-ribcage! (lambda (ribcage id label)
              (seq
               (call (lref set-ribcage-symnames!) (lref ribcage)
                (#%cons (call (lref syntax-expression) (lref id))
                 (call (lref ribcage-symnames) (lref ribcage))))
               (call (lref set-ribcage-marks!) (lref ribcage)
                (#%cons (call (lref wrap-marks) (call (lref syntax-wrap) (lref id)))
                 (call (lref ribcage-marks) (lref ribcage))))
               (call (lref set-ribcage-labels!) (lref ribcage)
                (#%cons (lref label)
                 (call (lref ribcage-labels) (lref ribcage))))))]
            [id-var-name (lambda (id w mod)
              (fix
                ([search (lambda (sym subst marks)
                  (let
                     ([cond-test-tmp
                       (#%null? (lref subst))])
                   (if
                     (lref cond-test-tmp)
                     (const false)
                     (let
                         ([cond-test-tmp
                           (let
                              ([and-tmp
                                (call (toplevel-ref pair?) (lref subst))])
                            (if
                              (lref and-tmp)
                              (#%eq? (call (toplevel-ref car) (lref subst))
                               (const shift))
                              (const false)))])
                       (if
                         (lref cond-test-tmp)
                         (call (lref search) (lref sym)
                          (call (toplevel-ref cdr) (lref subst))
                          (call (toplevel-ref cdr) (lref marks)))
                         (let
                             ([cond-test-tmp
                               (let
                                  ([and-tmp
                                    (call (toplevel-ref pair?) (lref subst))])
                                (if
                                  (lref and-tmp)
                                  (let
                                     ([and-tmp
                                       (call (toplevel-ref vector?) (call (toplevel-ref car) (lref subst)))])
                                   (if
                                     (lref and-tmp)
                                     (#%eq? (call (toplevel-ref vector-ref) (call (toplevel-ref car) (lref subst))
                                       (const 0))
                                      (const ribcage))
                                     (const false)))
                                  (const false)))])
                           (if
                             (lref cond-test-tmp)
                             (let*
                                ([ribcage
                                  (call (toplevel-ref car) (lref subst))]
                                [rsymnames
                                  (call (lref ribcage-symnames) (lref ribcage))]
                                [rmarks
                                  (call (lref ribcage-marks) (lref ribcage))]
                                [rlabels
                                  (call (lref ribcage-labels) (lref ribcage))])
                              (let
                                 ([subst
                                   (call (toplevel-ref cdr) (lref subst))])
                               (fix
                                 ([search-vector-rib (lambda ()
                                   (let
                                      ([n
                                        (call (toplevel-ref vector-length) (lref rsymnames))])
                                    (fix
                                      ([lp (lambda (i)
                                        (let
                                           ([cond-test-tmp
                                             (call (toplevel-ref =) (lref i)
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
                                                      (#%eq? (call (toplevel-ref vector-ref) (lref rsymnames)
                                                        (lref i))
                                                       (lref sym))])
                                                  (if
                                                    (lref and-tmp)
                                                    (call (lref same-marks?) (lref marks)
                                                     (call (toplevel-ref vector-ref) (lref rmarks)
                                                      (lref i)))
                                                    (const false)))])
                                             (if
                                               (lref cond-test-tmp)
                                               (let
                                                  ([lbl
                                                    (call (toplevel-ref vector-ref) (lref rlabels)
                                                     (lref i))])
                                                (let
                                                   ([cond-test-tmp
                                                     (call (toplevel-ref pair?) (lref lbl))])
                                                 (if
                                                   (lref cond-test-tmp)
                                                   (if
                                                     (#%equal? (call (toplevel-ref car) (lref lbl))
                                                       (lref mod))
                                                     (toplevel-ref label)
                                                     (call (lref lp) (#%+ (lref i)
                                                        (const 1))))
                                                   (lref lbl))))
                                               (call (lref lp) (#%+ (lref i)
                                                  (const 1))))))))])
                                     (call (lref lp) (const 0)))))]
                                 [search-list-rib (lambda ()
                                   (fix
                                     ([lp
                                     (lambda (rsymnames rmarks rlabels)
                                       (let
                                          ([cond-test-tmp
                                            (#%null? (lref rsymnames))])
                                        (if
                                          (lref cond-test-tmp)
                                          (call (lref search) (lref sym)
                                           (lref subst)
                                           (lref marks))
                                          (let
                                              ([rsym
                                                (call (toplevel-ref car) (lref rsymnames))]
                                              [rsymnames
                                                (call (toplevel-ref cdr) (lref rsymnames))]
                                              [rmarks1
                                                (call (toplevel-ref car) (lref rmarks))]
                                              [rmarks
                                                (call (toplevel-ref cdr) (lref rmarks))]
                                              [label
                                                (call (toplevel-ref car) (lref rlabels))]
                                              [rlabels
                                                (call (toplevel-ref cdr) (lref rlabels))])
                                            (if
                                              (let
                                                  ([and-tmp
                                                    (#%eq? (lref rsym)
                                                     (lref sym))])
                                                (if
                                                  (lref and-tmp)
                                                  (call (lref same-marks?) (lref marks)
                                                   (lref rmarks1))
                                                  (const false)))
                                              (let
                                                 ([cond-test-tmp
                                                   (call (toplevel-ref pair?) (lref label))])
                                               (if
                                                 (lref cond-test-tmp)
                                                 (if
                                                   (#%equal? (call (toplevel-ref car) (lref label))
                                                     (lref mod))
                                                   (lref label)
                                                   (call (lref lp) (lref rsymnames)
                                                     (lref rmarks)
                                                     (lref rlabels)))
                                                 (lref label)))
                                              (const #<undefined>))))))])
                                    (call (lref lp) (lref rsymnames)
                                     (lref rmarks)
                                     (lref rlabels))))])
                                (if
                                  (call (toplevel-ref vector?) (lref rsymnames))
                                  (call (lref search-vector-rib))
                                  (call (lref search-list-rib))))))
                             (const #<undefined>))))))))])
               (let
                  ([cond-test-tmp
                    (#%symbol? (lref id))])
                (if
                  (lref cond-test-tmp)
                  (let
                     ([or-tmp
                       (call (lref search) (lref id)
                        (call (lref wrap-subst) (lref w))
                        (call (lref wrap-marks) (lref w)))])
                   (if  (lref or-tmp)  (lref or-tmp)  (lref id)))
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
                           (call (lref syntax-module) (lref id))])
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
                        (lref id))))))))]
            [distinct-bound-ids? (lambda (ids)
              (fix
                ([distinct? (lambda (ids)
                  (let
                     ([cond-test-tmp
                       (#%null? (lref ids))])
                   (if
                     (lref cond-test-tmp)
                     (const true)
                     (let
                         ([cond-test-tmp
                           (call (toplevel-ref pair?) (lref ids))])
                       (if
                         (lref cond-test-tmp)
                         (let
                            ([id
                              (call (toplevel-ref car) (lref ids))]
                            [ids
                              (call (toplevel-ref cdr) (lref ids))])
                          (let
                             ([and-tmp
                               (call (toplevel-ref not) (call (lref bound-id-member?) (lref id)
                                 (lref ids)))])
                           (if
                             (lref and-tmp)
                             (call (lref distinct?) (lref ids))
                             (const false))))
                         (const #<undefined>))))))])
               (call (lref distinct?) (lref ids))))]
            [bound-id-member? (lambda (x ids)
              (if
                (#%null? (lref ids))
                (const false)
                (let
                    ([or-tmp
                      (call (lref bound-id=?) (lref x)
                       (call (toplevel-ref car) (lref ids)))])
                  (if
                    (lref or-tmp)
                    (lref or-tmp)
                    (call (lref bound-id-member?) (lref x)
                      (call (toplevel-ref cdr) (lref ids)))))))]
            [bound-id=? (lambda (i j)
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
                (const #<undefined>)))]
            [wrap-marks (lambda (w)
              (call (toplevel-ref car) (lref w)))]
            [wrap-subst (lambda (w)
              (call (toplevel-ref cdr) (lref w)))]
            [gen-labels (lambda (ls)
              (let
                 ([cond-test-tmp
                   (#%null? (lref ls))])
               (if
                 (lref cond-test-tmp)
                 (const '())
                 (#%cons (call (lref gen-label))
                   (call (lref gen-labels) (call (toplevel-ref cdr) (lref ls)))))))]
            [gen-label (lambda () (call (lref gen-unique)))]
            [new-mark (lambda () (call (lref gen-unique)))]
            [gen-unique (lambda args
              (let
                 ([cond-test-tmp
                   (#%null? (lref args))])
               (if
                 (lref cond-test-tmp)
                 (#%vector (const (capy))
                  (call (toplevel-ref gensym) (const id)))
                 (#%vector (call (toplevel-ref module-name) (call (toplevel-ref car) (lref args)))
                   (call (toplevel-ref module-generate-unique-id!) (call (toplevel-ref car) (lref args)))))))]
            [make-empty-ribcage (lambda ()
              (call (lref make-ribcage) (const '())
               (const '())
               (const '())))]
            [make-ribcage (lambda (symnames marks labels)
              (#%vector (const ribcage)
               (lref symnames)
               (lref marks)
               (lref labels)))]
            [ribcage-symnames (lambda (r)
              (call (toplevel-ref vector-ref) (lref r) (const 1)))]
            [ribcage-marks (lambda (r)
              (call (toplevel-ref vector-ref) (lref r) (const 2)))]
            [ribcage-labels (lambda (r)
              (call (toplevel-ref vector-ref) (lref r) (const 3)))]
            [set-ribcage-symnames! (lambda (r v)
              (call (toplevel-ref vector-set!) (lref r)
               (const 1)
               (lref v)))]
            [set-ribcage-marks! (lambda (r v)
              (call (toplevel-ref vector-set!) (lref r)
               (const 2)
               (lref v)))]
            [set-ribcage-labels! (lambda (r v)
              (call (toplevel-ref vector-set!) (lref r)
               (const 3)
               (lref v)))])
           (let
              ([null-env
                (const '())])
            (fix
              ([toplevel-eval (lambda (exp env)
                (call (toplevel-ref primitive-eval) (lref exp)))]
              [local-eval (lambda (exp env)
                (call (toplevel-ref primitive) (toplevel-ref eval)
                 (lref exp)))]
              [global-extend (lambda (type sym val)
                (call (toplevel-ref module-define!) (#%current-module)
                 (lref sym)
                 (call (toplevel-ref make-syntax-transformer) (lref sym)
                  (lref type)
                  (lref val))))]
              [sourcev->alist (lambda (sourcev)
                (fix
                  ([maybe-acons (lambda (k v tail)
                    (if
                      (lref v)
                      (call (toplevel-ref acons) (lref k)
                       (lref v)
                       (lref tail))
                      (lref tail)))])
                 (let
                    ([and-tmp
                      (lref sourcev)])
                  (if
                    (lref and-tmp)
                    (call (lref maybe-acons) (const filename)
                     (call (lref sourcev-filename) (lref sourcev))
                     (#%list (#%cons (const line)
                       (call (lref sourcev-line) (lref sourcev)))
                      (#%cons (const column)
                       (call (lref sourcev-column) (lref sourcev)))))
                    (const false)))))]
              [sourcev-filename (lambda (s)
                (call (toplevel-ref vector-ref) (lref s) (const 0)))]
              [sourcev-line (lambda (s)
                (call (toplevel-ref vector-ref) (lref s) (const 1)))]
              [sourcev-column (lambda (s)
                (call (toplevel-ref vector-ref) (lref s) (const 2)))]
              [expand-top-sequence (lambda (body r w s m essew mod)
                (let*
                   ([r
                     (#%cons (const (placeholder placeholder))
                      (lref r))]
                   [ribcage
                     (call (lref make-empty-ribcage))]
                   [w
                     (call (lref make-wrap) (call (lref wrap-marks) (lref w))
                      (#%cons (lref ribcage)
                       (call (lref wrap-subst) (lref w))))])
                 (fix
                   ([parse (lambda (body r w s m esew mod)
                     (fix
                       ([loop (lambda (body)
                         (let
                            ([cond-test-tmp
                              (#%null? (lref body))])
                          (if
                            (lref cond-test-tmp)
                            (const '())
                            (let
                                ([head
                                  (call (toplevel-ref car) (lref body))]
                                [tail
                                  (call (toplevel-ref cdr) (lref body))])
                              (let
                                 ([thunks
                                   (call (lref parse1) (lref head)
                                    (lref r)
                                    (lref w)
                                    (lref s)
                                    (lref m)
                                    (lref essew)
                                    (lref mod))])
                               (call (toplevel-ref append) (lref thunks)
                                (call (lref loop) (lref tail))))))))])
                      (call (lref loop) (lref body))))]
                   [parse1 (lambda (x r w s m essew mod)
                     (fix
                       ([current-module-for-expansion (lambda (mod)
                         (let
                            ([cond-test-tmp
                              (let
                                 ([and-tmp
                                   (call (toplevel-ref pair?) (lref mod))])
                               (if
                                 (lref and-tmp)
                                 (#%eq? (call (toplevel-ref car) (lref mod))
                                  (const hygiene))
                                 (const false)))])
                          (if
                            (lref cond-test-tmp)
                            (#%cons (const hygiene)
                             (call (toplevel-ref module-name) (#%current-module)))
                            (lref mod))))])
                      (call (toplevel-ref call-with-values) (lambda ()
                        (let
                           ([mod
                             (call (lref current-module-for-expansion) (lref mod))])
                         (call (lref syntax-type) (lref x)
                          (lref r)
                          (lref w)
                          (call (lref source-annotation) (lref x))
                          (lref ribcage)
                          (lref mod)
                          (const false))))
                       (lambda (type value form e w s mod)
                        (let
                           ([cond-test-tmp
                             (#%eq? (lref type) (const define-form))])
                         (if
                           (lref cond-test-tmp)
                           (let*
                              ([id
                                (call (lref wrap) (lref value)
                                 (lref w)
                                 (lref mod))]
                              [var
                                (if
                                  (call (lref macro-introduced-identifier?) (lref id))
                                  (call (lref fresh-derived-name) (lref id)
                                   (lref x))
                                  (call (lref syntax-expression) (lref id)))])
                            (seq
                             (call (lref record-definition!) (lref id)
                              (lref var))
                             (#%list (if
                                (#%eq? (lref m) (const c&e))
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
                                  (call (toplevel-ref top-level-eval) (lref x)
                                   (lref mod))
                                  (lambda () (lref x))))
                                (call (toplevel-ref call-with-values) (lambda ()
                                   (call (lref resolve-identifier) (lref id)
                                    (lref empty-wrap)
                                    (lref r)
                                    (lref mod)
                                    (const true)))
                                  (lambda (type* value* mod*)
                                   (seq
                                    (if
                                      (#%eq? (lref type*)
                                        (const macro))
                                      (call (toplevel-ref top-level-eval) (call (lref build-global-definition) (lref s)
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
                                       (lref mod)))))))))))
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
                                  (call (toplevel-ref apply) (lambda (e1)
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
                               (#%list (if
                                   (#%eq? (lref m) (const c&e))
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
                                     (call (toplevel-ref top-level-eval) (lref x)
                                      (lref mod))
                                     (lambda () (lref x))))
                                   (lambda ()
                                     (call (lref expand-expr) (lref type)
                                      (lref value)
                                      (lref form)
                                      (lref e)
                                      (lref r)
                                      (lref w)
                                      (lref s)
                                      (lref mod)))))))))))))]
                   [record-definition! (lambda (id var)
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
                         (lref mod))))))]
                   [macro-introduced-identifier? (lambda (id)
                     (call (toplevel-ref not) (#%equal? (call (lref wrap-marks) (call (lref syntax-wrap) (lref id)))
                       (const (top)))))]
                   [fresh-derived-name (lambda (id orig-form)
                     (call (lref ensure-fresh-name) (call (toplevel-ref symbol-append) (call (lref syntax-expression) (lref id))
                       (const -)
                       (call (toplevel-ref string->symbol) (call (toplevel-ref number->string) (call (toplevel-ref hash) (call (toplevel-ref syntax->datum) (lref orig-form))))
                        (const 16)))))]
                   [ensure-fresh-name (lambda (var)
                     (fix
                       ([ribcage-has-var? (lambda (var)
                         (fix
                           ([loop (lambda (labels)
                             (let
                                ([cond-test-tmp
                                  (#%null? (lref labels))])
                              (if
                                (lref cond-test-tmp)
                                (const false)
                                (let
                                    ([wrapped
                                      (call (toplevel-ref cdr) (call (toplevel-ref car) (lref labels)))]
                                    [labels
                                      (call (toplevel-ref cdr) (lref labels))])
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
                        ([loop (lambda (unique n)
                          (if
                            (call (lref ribcage-has-var?) (lref unique))
                            (let
                               ([tail
                                 (call (toplevel-ref string->symbol) (call (toplevel-ref number->string) (lref n)))])
                             (call (lref loop) (call (toplevel-ref symbol-append) (lref var)
                               (const -)
                               (lref tail))
                              (#%+ (const 1) (lref n))))
                            (lref unique)))])
                       (call (lref loop) (lref var) (const 1)))))])
                  (let
                     ([res
                       (fix
                         ([lp (lambda (thunks)
                           (if
                             (#%null? (lref thunks))
                             (const '())
                             (#%cons (call (call (toplevel-ref car) (lref thunks)))
                               (call (lref lp) (call (toplevel-ref cdr) (lref thunks))))))])
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
                       (lref res)))))))]
              [expand-install-global (lambda (mod name type e)
                (call (lref build-global-definition) (const false)
                 (lref mod)
                 (lref name)
                 (call (lref build-primcall) (const false)
                  (const make-syntax-transformer)
                  (#%list (call (toplevel-ref make-constant) (const false)
                    (lref name))
                   (call (toplevel-ref make-constant) (const false)
                    (if
                      (#%eq? (lref type)
                        (const define-syntax-parameter-form))
                      (const syntax-parameter)
                      (const macro)))
                   (lref e)))))]
              [build-global-definition (lambda (src mod var exp)
                (call (toplevel-ref make-toplevel-define) (lref src)
                 (let
                    ([and-tmp
                      (lref mod)])
                  (if
                    (lref and-tmp)
                    (call (toplevel-ref cdr) (lref mod))
                    (const false)))
                 (lref var)
                 (call (lref maybe-name-value) (lref var)
                  (lref exp))))]
              [build-let (lambda (src ids vars val-exps body-exp)
                (let*
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
                      (lref body-exp))))))]
              [build-let* (lambda (src ids vars val-exps body-exp)
                (let*
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
                      (lref body-exp))))))]
              [expand-body (lambda (body outer-form r w mod)
                (let*
                   ([r
                     (#%cons (const (placeholder placeholder))
                      (lref r))]
                   [ribcage
                     (call (lref make-empty-ribcage))]
                   [w
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
                           (call (toplevel-ref not) (lref expand-tail-expr))
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
                           (call (toplevel-ref not) (call (lref valid-bound-ids?) (lref ids)))
                           (call (toplevel-ref syntax-violation) (const false)
                            (const invalid or duplicate identifier in definition)
                            (lref outer-form))
                           (const #<undefined>))
                         (#%set-cdr! (lref r)
                          (call (lref extend-env) (lref labels)
                           (lref bindings)
                           (call (toplevel-ref cdr) (lref r))))
                         (let
                            ([src
                              (call (lref source-annotation) (lref outer-form))])
                          (fix
                            ([lp (lambda (var-ids vars vals tail)
                              (let
                                 ([cond-test-tmp
                                   (#%null? (lref var-ids))])
                               (if
                                 (lref cond-test-tmp)
                                 (lref tail)
                                 (let
                                     ([cond-test-tmp
                                       (call (toplevel-ref not) (call (toplevel-ref car) (lref var-ids)))])
                                   (if
                                     (lref cond-test-tmp)
                                     (call (lref lp) (call (toplevel-ref cdr) (lref var-ids))
                                      (call (toplevel-ref cdr) (lref vars))
                                      (call (toplevel-ref cdr) (lref vals))
                                      (call (toplevel-ref make-seq) (lref src)
                                       (call (call (toplevel-ref car) (lref vals)))
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
                                               (call (toplevel-ref make-seq) (lref src)
                                                 (call (lref expand-expr))
                                                 (call (toplevel-ref make-void) (const false)))))
                                            (call (toplevel-ref reverse) (lref vals)))])
                                       (call (lref build-letrec) (lref src)
                                        (const true)
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
                             (#%cons (const false) (lref var-ids))
                             (#%cons (const false) (lref vars))
                             (#%cons (lref expand-tail-expr)
                              (lref vals))
                             (lref bindings)
                             (const false))
                            (let
                                ([e
                                  (call (toplevel-ref cdar) (lref body))]
                                [er
                                  (call (toplevel-ref caar) (lref body))]
                                [body
                                  (call (toplevel-ref cdr) (lref body))])
                              (call (toplevel-ref call-with-values) (lambda ()
                                (call (toplevel-ref synta-type) (lref e)
                                 (lref er)
                                 (lref empty-wrap)
                                 (call (lref source-annotation) (lref e))
                                 (lref ribcage)
                                 (lref mod)
                                 (const false)))
                               (lambda (type value form e w s mod)
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
                                       (#%cons (lref id) (lref ids))
                                       (#%cons (lref label)
                                        (lref labels))
                                       (#%cons (lref id)
                                        (lref var-ids))
                                       (#%cons (lref var) (lref vars))
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
                                       (#%cons (call (toplevel-ref make-binding) (const lexical)
                                         (lref var))
                                        (lref bindings))
                                       (const false)))))
                                   (let
                                       ([cond-test-tmp
                                         (#%eq? (lref type)
                                          (const begin-form))])
                                     (if
                                       (lref cond-test-tmp)
                                       (let*
                                          ([tmp-1
                                            (lref e)]
                                          [tmp
                                            (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                                             (const (_ . each-any)))])
                                        (if
                                          (lref tmp)
                                          (call (toplevel-ref apply) (lambda (e1)
                                            (call (lref parse) (fix
                                               ([f (lambda (forms)
                                                 (if
                                                   (#%null? (lref forms))
                                                   (lref body)
                                                   (#%cons (#%cons (lref er)
                                                      (call (lref wrap) (call (toplevel-ref car) (lref forms))
                                                       (lref w)
                                                       (lref mod)))
                                                     (call (lref f) (call (toplevel-ref cdr) (lref forms))))))])
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
                                            (lref tmp-1))))
                                       (let
                                           ([cond-test-tmp
                                             (#%eq? (lref type)
                                              (const define-syntax-form))])
                                         (if
                                           (lref cond-test-tmp)
                                           (call (toplevel-ref error) (const expand-body)
                                            (const not yet implemented: define-syntax))
                                           (let
                                               ([cond-test-tmp
                                                 (#%eq? (lref type)
                                                  (const define-syntax-parameter-form))])
                                             (if
                                               (lref cond-test-tmp)
                                               (call (toplevel-ref error) (const expand-body)
                                                (const not yet implemented: define-syntax-parameter))
                                               (let
                                                   ([cond-test-tmp
                                                     (#%eq? (lref type)
                                                      (const local-syntax-form))])
                                                 (if
                                                   (lref cond-test-tmp)
                                                   (call (toplevel-ref error) (const expand-body)
                                                    (const not yet implemented: local-syntax))
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
                                                        (lref mod)))))))))))))))))))))))])
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
                   (const false)))))]
              [build-letrec (lambda (src ids vars val-exps body-exp)
                (let*
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
                      (lref body-exp))))))]
              [build-letrec* (lambda (src ids vars val-exps body-exp)
                (let*
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
                      (lref body-exp))))))]
              [build-named-let
              (lambda (src ids vars val-exps body-exp)
                (let*
                   ([v
                     (lref vars)]
                   [fk
                     (lambda ()
                      (call (toplevel-ref error) (const value failed to match)
                       (lref v)))])
                 (if
                   (call (toplevel-ref pair?) (lref v))
                   (let
                      ([vx
                        (call (toplevel-ref car) (lref v))]
                      [vy
                        (call (toplevel-ref cdr) (lref v))])
                    (let*
                       ([f
                         (lref vx)]
                       [vars
                         (lref vy)]
                       [v
                         (lref ids)]
                       [fk
                         (lambda ()
                          (call (toplevel-ref error) (const value failed to match)
                           (lref v)))])
                     (if
                       (call (toplevel-ref pair?) (lref v))
                       (let
                          ([vx
                            (call (toplevel-ref car) (lref v))]
                          [vy
                            (call (toplevel-ref cdr) (lref v))])
                        (let*
                           ([f-name
                             (lref vx)]
                           [ids
                             (lref vy)]
                           [proc
                             (call (lref build-simple-lambda) (lref src)
                              (lref ids)
                              (lref vars)
                              (const '())
                              (lref body-exp))])
                         (call (toplevel-ref make-letrec) (lref src)
                          (const false)
                          (#%list (lref f-name))
                          (#%list (lref f))
                          (#%list (call (lref maybe-name-value) (lref f-name)
                            (lref proc)))
                          (call (lref build-call) (lref src)
                           (call (lref build-lexical-reference) (lref src)
                            (lref f-name)
                            (lref f))
                           (call (toplevel-ref map) (lref maybe-name-value)
                            (lref ids)
                            (lref val-exps))))))
                       (call (lref fk)))))
                   (call (lref fk)))))]
              [maybe-name-value (lambda (name val)
                (if
                  (call (toplevel-ref proc?) (lref val))
                  (let
                     ([meta
                       (call (toplevel-ref proc-meta) (lref val))])
                   (if
                     (call (toplevel-ref assq) (const name)
                       (lref meta))
                     (lref val)
                     (call (toplevel-ref make-proc) (call (toplevel-ref term-src) (lref val))
                       (call (toplevel-ref proc-args) (lref val))
                       (call (toplevel-ref proc-body) (lref val))
                       (call (toplevel-ref acons) (const name)
                        (lref name)
                        (lref meta)))))
                  (lref val)))]
              [build-void (lambda (s)
                (call (toplevel-ref make-void) (lref s)))]
              [expand-call (lambda (x e r w s mod)
                (let*
                   ([tmp-1
                     (lref e)]
                   [tmp
                     (call (toplevel-ref $sc-dispatch) (lref tmp-1)
                      (const (any . each-any)))])
                 (if
                   (lref tmp)
                   (call (toplevel-ref apply) (lambda (e0 e1)
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
                     (lref tmp-1)))))]
              [expand-sequence (lambda (body r w s mod)
                (call (lref build-sequence) (lref s)
                 (fix
                   ([lp (lambda (body)
                     (if
                       (#%null? (lref body))
                       (const '())
                       (let
                           ([head
                             (call (toplevel-ref car) (lref body))]
                           [tail
                             (call (toplevel-ref cdr) (lref body))])
                         (let
                            ([expr
                              (call (lref expand) (toplevel-ref hed)
                               (lref r)
                               (lref w)
                               (lref s)
                               (lref mod))])
                          (#%cons (lref expr)
                           (call (lref lp) (lref tail)))))))])
                  (call (lref lp) (lref body)))))]
              [expand (lambda (e r w mod)
                (call (toplevel-ref call-with-values) (lambda ()
                  (call (lref syntax-type) (lref e)
                   (lref r)
                   (lref w)
                   (call (lref source-annotation) (lref e))
                   (const false)
                   (lref mod)
                   (const false)))
                 (lambda (type value form e w s mod)
                  (call (lref expand-expr) (lref type)
                   (lref value)
                   (lref form)
                   (lref e)
                   (lref r)
                   (lref w)
                   (lref s)
                   (lref mod)))))]
              [expand-expr (lambda (type value form e r w s mod)
                (let
                   ([cond-test-tmp
                     (#%eq? (lref type) (const lexical))])
                 (if
                   (lref cond-test-tmp)
                   (call (lref build-lexical-reference) (lref s)
                    (lref e)
                    (lref value))
                   (let
                       ([cond-test-tmp
                         (let
                            ([or-tmp
                              (#%eq? (lref type) (const core))])
                          (if
                            (lref or-tmp)
                            (lref or-tmp)
                            (#%eq? (lref type) (const core-form))))])
                     (if
                       (lref cond-test-tmp)
                       (call (lref value) (lref e)
                        (lref r)
                        (lref w)
                        (lref s)
                        (lref mod))
                       (let
                           ([cond-test-tmp
                             (#%eq? (lref type) (const module-ref))])
                         (if
                           (lref cond-test-tmp)
                           (call (toplevel-ref call-with-values) (lambda ()
                             (call (lref value) (lref e)
                              (lref r)
                              (lref w)
                              (lref mod)))
                            (lambda (e r w s mod)
                             (call (lref expand) (lref e)
                              (lref r)
                              (lref w)
                              (lref mod))))
                           (let
                               ([cond-test-tmp
                                 (#%eq? (lref type)
                                  (const lexical-call))])
                             (if
                               (lref cond-test-tmp)
                               (call (lref expand-call) (let
                                   ([id
                                     (call (toplevel-ref car) (lref e))])
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
                                          (call (lref source-annotation) (call (toplevel-ref car) (lref e)))])
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
                                         (call (toplevel-ref cdr) (lref e))))
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
                                               (call (lref expand-call) (call (lref expand) (call (toplevel-ref car) (lref e))
                                                 (lref r)
                                                 (lref w)
                                                 (lref mod))
                                                (lref r)
                                                (lref w)
                                                (lref s)
                                                (lref mod))
                                               (let
                                                   ([cond-test-tmp
                                                     (#%eq? (lref type)
                                                      (const begin-form))])
                                                 (if
                                                   (lref cond-test-tmp)
                                                   (let*
                                                      ([tmp
                                                        (lref e)]
                                                      [tmp-1
                                                        (call (toplevel-ref $sc-dispatch) (lref tmp)
                                                         (const (_ any . each-any)))])
                                                    (if
                                                      (lref tmp-1)
                                                      (call (toplevel-ref apply) (lambda (e1 e2)
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
                                                          (call (toplevel-ref apply) (lambda ()
                                                            (call (toplevel-ref syntax-violation) (const false)
                                                             (const sequence of zero expressions)
                                                             (call (lref source-wrap) (lref e)
                                                              (lref w)
                                                              (lref s)
                                                              (lref mod))))
                                                           (lref tmp-1))
                                                          (call (toplevel-ref syntax-violation) (const false)
                                                            (const source expression failed to match any pattern)
                                                            (lref tmp))))))
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
                                                       (call (toplevel-ref syntax-violation) (const false)
                                                         (const unexpected syntax)
                                                         (call (lref source-wrap) (lref e)
                                                          (lref w)
                                                          (lref s)
                                                          (lref mod))))))))))))))))))))))))]
              [build-global-reference (lambda (src var mod)
                (call (toplevel-ref make-toplevel-ref) (lref src)
                 (lref mod)
                 (lref var)))]
              [build-primcall (lambda (src name args)
                (call (toplevel-ref make-primcall) (lref src)
                 (lref name)
                 (lref args)))]
              [build-simple-lambda (lambda (src ids vars meta exp)
                (call (toplevel-ref make-proc) (lref src)
                 (lref vars)
                 (lref exp)
                 (lref meta)
                 (lref ids)))]
              [build-sequence (lambda (src exps)
                (let*
                   ([v
                     (lref exps)]
                   [fk
                     (lambda ()
                      (let
                         ([fk
                           (lambda ()
                            (call (toplevel-ref error) (const value failed to match)
                             (lref v)))])
                       (if
                         (call (toplevel-ref pair?) (lref v))
                         (let
                            ([vx
                              (call (toplevel-ref car) (lref v))]
                            [vy
                              (call (toplevel-ref cdr) (lref v))])
                          (let*
                             ([head
                               (lref vx)]
                             [tail
                               (lref vy)])
                           (call (toplevel-ref make-sequence) (lref src)
                            (lref head)
                            (call (lref build-sequence) (const false)
                             (lref tail)))))
                         (call (lref fk)))))])
                 (if
                   (call (toplevel-ref pair?) (lref v))
                   (let
                      ([vx
                        (call (toplevel-ref car) (lref v))]
                      [vy
                        (call (toplevel-ref cdr) (lref v))])
                    (let
                       ([tail
                         (lref vx)])
                     (if
                       (#%null? (lref vy))
                       (lref tail)
                       (call (lref fk)))))
                   (call (lref fk)))))]
              [build-lexical-reference (lambda (src name sym)
                (call (toplevel-ref make-lref) (lref src)
                 (lref name)
                 (lref sym)))]
              [build-lexical-assignment (lambda (src name sym value)
                (call (toplevel-ref make-lset) (lref src)
                 (lref name)
                 (lref sym)
                 (lref value)))]
              [build-call (lambda (src rator rands)
                (call (toplevel-ref make-application) (lref src)
                 (lref rator)
                 (lref rands)))]
              [build-conditional
              (lambda (src test-exp then-exp else-exp)
                (call (toplevel-ref make-if) (lref src)
                 (lref test-exp)
                 (lref then-exp)
                 (lref else-exp)))]
              [gen-var (lambda (id)
                (let
                   ([id
                     (if
                       (call (lref syntax?) (lref id))
                       (call (lref syntax-expression) (lref id))
                       (lref id))])
                 (call (lref gen-lexical) (lref id))))]
              [gen-lexical (lambda (id)
                (call (toplevel-ref module-gensym) (call (toplevel-ref symbol->string) (lref id))))]
              [syntax-type (lambda (e r w s rib mod for-car?)
                (let
                   ([cond-test-tmp
                     (#%symbol? (lref e))])
                 (if
                   (lref cond-test-tmp)
                   (call (toplevel-ref call-with-values) (lambda ()
                     (call (lref resolve-identifier) (lref e)
                      (lref w)
                      (lref r)
                      (lref mod)
                      (const true)))
                    (lambda (type value mod*)
                     (let
                        ([cond-test-tmp
                          (#%eq? (lref type) (const macro))])
                      (if
                        (lref cond-test-tmp)
                        (if
                          (lref for-car?)
                          (values (lref type) (lref value) (lref e) (lref e) (lref w) (lref s) (lref mod))
                          (call (lref syntax-type) (call (toplevel-ref expand-macro) (lref value)
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
                              (#%eq? (lref type) (const global))])
                          (if
                            (lref cond-test-tmp)
                            (values (lref type) (lref value) (lref e) (lref value) (lref w) (lref s) (lref mod*))
                            (values (lref type) (lref value) (lref e) (lref e) (lref w) (lref s) (lref mod*))))))))
                   (let
                       ([cond-test-tmp
                         (call (toplevel-ref pair?) (lref e))])
                     (if
                       (lref cond-test-tmp)
                       (let
                          ([first
                            (call (toplevel-ref car) (lref e))])
                        (call (toplevel-ref call-with-values) (lambda ()
                          (call (lref syntax-type) (lref first)
                           (lref r)
                           (lref w)
                           (lref s)
                           (lref rib)
                           (lref mod)
                           (const true)))
                         (lambda (ftype fval fform fe fw fs fmod)
                          (let
                             ([cond-test-tmp
                               (#%eq? (lref ftype) (const lexical))])
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
                                     (call (lref syntax-type) (call (toplevel-ref expand-macro) (lref fval)
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
                                         (call (toplevel-ref call-with-values) (lambda ()
                                           (call (lref fval) (lref e)
                                            (lref r)
                                            (lref w)
                                            (lref mod)))
                                          (lambda (e r w s mod)
                                           (call (lref syntax-type) (lref e)
                                            (lref r)
                                            (lref w)
                                            (lref s)
                                            (lref rib)
                                            (lref mod)
                                            (lref for-car?))))
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
                                                                (const define))])
                                                           (if
                                                             (lref cond-test-tmp)
                                                             (let
                                                                ([tmp
                                                                  (call (toplevel-ref $sc-dispatch) (lref e)
                                                                   (const (_ any any)))])
                                                              (if
                                                                (let
                                                                    ([and-tmp
                                                                      (lref tmp)])
                                                                  (if
                                                                    (lref and-tmp)
                                                                    (call (lref id?) (call (toplevel-ref car) (lref tmp)))
                                                                    (const false)))
                                                                (call (toplevel-ref apply) (lambda (name val)
                                                                  (values (const define-form) (lref name) (lref e) (lref val) (lref w) (lref s) (lref mod)))
                                                                 (lref tmp))
                                                                (let
                                                                    ([tmp
                                                                      (call (toplevel-ref $sc-dispatch) (lref e)
                                                                       (const (_ (any . any) any . each-any)))])
                                                                  (if
                                                                    (let
                                                                        ([and-tmp
                                                                          (lref tmp)])
                                                                      (if
                                                                        (lref and-tmp)
                                                                        (call (toplevel-ref apply) (lambda (name args e1 e2)
                                                                          (seq
                                                                           (let
                                                                              ([and-tmp
                                                                                (call (lref id?) (lref name))])
                                                                            (if
                                                                              (lref and-tmp)
                                                                              (call (lref valid-bound-ids?) (call (lref lambda-var-list) (lref args)))
                                                                              (const false)))
                                                                           (lref tmp))))
                                                                        (const false)))
                                                                    (call (toplevel-ref apply) (lambda (name args e1 e2)
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
                                                                     (lref tmp))
                                                                    (call (toplevel-ref syntax-violation) (const false)
                                                                      (const syntax expression failed to match any pattern)
                                                                      (lref e))))))
                                                             (values (const call) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod)))))))))))))))))))))))
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
                               (values (const other) (const false) (lref e) (lref e) (lref w) (lref s) (lref mod)))))))))))]
              [source-annotation (lambda (x)
                (if
                  (call (lref syntax?) (lref x))
                  (call (lref syntax-sourcev) (lref x))
                  (call (toplevel-ref datum-sourcev) (lref x))))]
              [resolve-identifier
              (lambda (id w r mod resolve-syntax-parameters?)
                (fix
                  ([resolve-global (lambda (var mod)
                    (let
                       ([v
                         (let
                            ([and-tmp
                              (call (toplevel-ref not) (#%equal? (lref mod)
                                (const (primitive))))])
                          (if
                            (lref and-tmp)
                            (call (toplevel-ref module-variable) (if
                               (lref mod)
                               (call (toplevel-ref resolve-module) (call (toplevel-ref cdr) (lref mod))
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
                                (call (toplevel-ref variable-bound?) (lref v))])
                            (if
                              (lref and-tmp)
                              (call (toplevel-ref macro?) (call (toplevel-ref variable-ref) (lref v)))
                              (const false)))
                           (const false)))
                       (let*
                          ([m
                            (call (toplevel-ref variable-ref) (lref v))]
                          [type
                            (call (toplevel-ref macro-type) (lref m))]
                          [trans
                            (call (toplevel-ref macro-binding) (lref m))])
                        (if
                          (#%eq? (lref type) (const syntax-parameter))
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
                          (values (lref type) (lref trans) (lref mod))))
                       (values (const global) (lref var) (lref mod)))))]
                  [resolve-lexical (lambda (label mod)
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
                          (#%eq? (lref type) (const syntax-parameter))
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
                          (call (toplevel-ref not) (#%eq? (lref n)
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
                           (#%symbol? (lref n))])
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
                              (lref mod)))))))))))]
              [binding-type (lambda (x)
                (call (toplevel-ref car) (lref x)))]
              [binding-value (lambda (x)
                (call (toplevel-ref cdr) (lref x)))])
             (seq
              (toplevel-set syntax->datum (lambda (x)
               (call (lref strip) (lref x))))
              (toplevel-set $sc-dispatch (lambda (e p)
               (fix
                 ([match-each+ (lambda (e x-pat y-pat z-pat w r mod)
                   (fix
                     ([f (lambda (e w)
                       (let
                          ([cond-test-tmp
                            (call (toplevel-ref pair?) (lref e))])
                        (if
                          (lref cond-test-tmp)
                          (call (toplevel-ref call-with-values) (lambda ()
                            (call (lref f) (call (toplevel-ref cdr) (lref e))
                             (lref w)))
                           (lambda (xr* y-pat r)
                            (if
                              (lref r)
                              (if
                                (#%null? (lref y-pat))
                                (let
                                   ([xr
                                     (call (lref match) (call (toplevel-ref car) (lref e))
                                      (lref x-pat)
                                      (lref w)
                                      (const '())
                                      (lref mod))])
                                 (if
                                   (lref xr)
                                   (values (#%cons (lref xr)
                                    (lref xr*)) (lref y-pat) (lref r))
                                   (values (const false) (const false) (const false))))
                                (values (const '()) (call (toplevel-ref cdr) (lref y-pat)) (call (lref match) (call (toplevel-ref car) (lref e))
                                  (call (toplevel-ref car) (lref y-pat))
                                  (lref w)
                                  (lref r)
                                  (lref mod))))
                              (values (const false) (const false) (const false)))))
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
                    (call (lref f) (lref e) (lref w))))]
                 [match (lambda (e p w r mod)
                   (let
                      ([cond-test-tmp
                        (call (toplevel-ref not) (lref r))])
                    (if
                      (lref cond-test-tmp)
                      (const false)
                      (let
                          ([cond-test-tmp
                            (#%eq? (lref p) (const _))])
                        (if
                          (lref cond-test-tmp)
                          (lref r)
                          (let
                              ([cond-test-tmp
                                (#%eq? (lref p) (const any))])
                            (if
                              (lref cond-test-tmp)
                              (#%cons (call (lref wrap) (lref e)
                                (lref w)
                                (lref mod))
                               (lref r))
                              (let
                                  ([cond-test-tmp
                                    (call (lref syntax?) (lref p))])
                                (if
                                  (lref cond-test-tmp)
                                  (call (lref match*) (call (lref syntax-expression) (lref e))
                                   (lref p)
                                   (call (lref join-wraps) (lref w)
                                    (call (lref syntax-wrap) (lref p)))
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
                 [match* (lambda (e p w r mod)
                   (let
                      ([cond-test-tmp
                        (#%null? (lref p))])
                    (if
                      (lref cond-test-tmp)
                      (let
                         ([and-tmp
                           (#%null? (lref e))])
                       (if  (lref and-tmp)  (lref r)  (const false)))
                      (let
                          ([cond-test-tmp
                            (call (toplevel-ref pair?) (lref p))])
                        (if
                          (lref cond-test-tmp)
                          (let
                             ([and-tmp
                               (call (toplevel-ref pair?) (lref e))])
                           (if
                             (lref and-tmp)
                             (call (lref match) (call (toplevel-ref car) (lref e))
                              (call (toplevel-ref car) (lref p))
                              (lref w)
                              (call (lref match) (call (toplevel-ref cdr) (lref e))
                               (call (toplevel-ref cdr) (lref p))
                               (lref w)
                               (lref r)
                               (lref mod))
                              (lref mod))
                             (const false)))
                          (let
                              ([cond-test-tmp
                                (#%eq? (lref p) (const each-any))])
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
                                  (#%cons (lref l) (lref r))
                                  (const false))))
                              (let
                                  ([v
                                    (call (toplevel-ref vector-ref) (lref p)
                                     (const 0))])
                                (let
                                   ([cond-test-tmp
                                     (#%eq? (lref v) (const each))])
                                 (if
                                   (lref cond-test-tmp)
                                   (if
                                     (#%null? (lref p))
                                     (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                       (const 1))
                                      (lref r))
                                     (let
                                         ([l
                                           (call (lref match-each) (lref e)
                                            (call (toplevel-ref vector-ref) (lref p)
                                             (const 1))
                                            (lref w)
                                            (lref mod))])
                                       (let
                                          ([and-tmp
                                            (lref l)])
                                        (if
                                          (lref and-tmp)
                                          (fix
                                            ([collect (lambda (l)
                                              (if
                                                (#%null? (call (toplevel-ref car) (lref l)))
                                                (lref r)
                                                (#%cons (call (toplevel-ref map) (toplevel-ref car)
                                                   (lref l))
                                                  (call (lref collect) (call (toplevel-ref map) (toplevel-ref cdr)
                                                    (lref l))))))])
                                           (call (lref collect) (lref l)))
                                          (const false)))))
                                   (let
                                       ([cond-test-tmp
                                         (#%eq? (lref v)
                                          (const each+))])
                                     (if
                                       (lref cond-test-tmp)
                                       (call (toplevel-ref call-with-values) (lambda ()
                                         (call (lref match-each+) (lref e)
                                          (call (toplevel-ref vector-ref) (lref p)
                                           (const 1))
                                          (call (toplevel-ref vector-ref) (lref p)
                                           (const 2))
                                          (call (toplevel-ref vector-ref) (lref p)
                                           (const 3))
                                          (lref w)
                                          (lref r)
                                          (lref mod)))
                                        (lambda (xr* y-pat r)
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
                                                 (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                                   (const 1))
                                                  (lref r))
                                                 (call (lref combine) (lref xr*)
                                                   (lref r)))
                                               (const false)))
                                            (const false)))))
                                       (let
                                           ([cond-test-tmp
                                             (#%eq? (lref v)
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
                                                    (call (toplevel-ref vector-ref) (lref p)
                                                     (const 1)))])
                                               (if
                                                 (lref and-tmp)
                                                 (lref r)
                                                 (const false)))
                                              (const false)))
                                           (let
                                               ([cond-test-tmp
                                                 (#%eq? (lref v)
                                                  (const atom))])
                                             (if
                                               (lref cond-test-tmp)
                                               (let
                                                  ([and-tmp
                                                    (#%equal? (call (toplevel-ref vector-ref) (lref p)
                                                      (const 1))
                                                     (call (lref strip) (lref e)))])
                                                (if
                                                  (lref and-tmp)
                                                  (lref r)
                                                  (const false)))
                                               (let
                                                   ([cond-test-tmp
                                                     (#%eq? (lref v)
                                                      (const vector))])
                                                 (if
                                                   (lref cond-test-tmp)
                                                   (let
                                                      ([and-tmp
                                                        (call (toplevel-ref vector?) (lref e))])
                                                    (if
                                                      (lref and-tmp)
                                                      (call (lref match) (call (toplevel-ref vector->list) (lref e))
                                                       (call (toplevel-ref vector-ref) (lref p)
                                                        (const 1))
                                                       (lref w)
                                                       (lref r)
                                                       (lref mod))
                                                      (const false)))
                                                   (const #<undefined>)))))))))))))))))))]
                 [match-each (lambda (e p w mod)
                   (let
                      ([cond-test-tmp
                        (call (toplevel-ref pair?) (lref e))])
                    (if
                      (lref cond-test-tmp)
                      (let
                         ([first
                           (call (lref match) (call (toplevel-ref car) (lref e))
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
                               (call (lref match-each) (call (toplevel-ref cdr) (lref e))
                                (lref p)
                                (lref w)
                                (lref mod))])
                           (let
                              ([and-tmp
                                (lref rest)])
                            (if
                              (lref and-tmp)
                              (#%cons (lref first) (lref rest))
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
                              (const false))))))))]
                 [match-each-any (lambda (e w mod)
                   (let
                      ([cond-test-tmp
                        (call (toplevel-ref pair?) (lref e))])
                    (if
                      (lref cond-test-tmp)
                      (let
                         ([l
                           (call (lref match-each-any) (call (toplevel-ref cdr) (lref e))
                            (lref w)
                            (lref mod))])
                       (let
                          ([and-tmp
                            (lref l)])
                        (if
                          (lref and-tmp)
                          (#%cons (call (lref wrap) (call (toplevel-ref car) (lref e))
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
                              (const false))))))))]
                 [match-empty (lambda (p r)
                   (let
                      ([cond-test-tmp
                        (#%null? (lref p))])
                    (if
                      (lref cond-test-tmp)
                      (lref r)
                      (let
                          ([cond-test-tmp
                            (#%eq? (lref p) (const _))])
                        (if
                          (lref cond-test-tmp)
                          (lref r)
                          (let
                              ([cond-test-tmp
                                (#%eq? (lref p) (const any))])
                            (if
                              (lref cond-test-tmp)
                              (#%cons (const '()) (lref r))
                              (let
                                  ([cond-test-tmp
                                    (call (toplevel-ref pair?) (lref p))])
                                (if
                                  (lref cond-test-tmp)
                                  (call (lref match-empty) (call (toplevel-ref car) (lref p))
                                   (call (lref match-empty) (call (toplevel-ref cdr) (lref p))
                                    (lref r)))
                                  (let
                                      ([cond-test-tmp
                                        (#%eq? (lref p)
                                         (const each-any))])
                                    (if
                                      (lref cond-test-tmp)
                                      (#%cons (const '()) (lref r))
                                      (let
                                          ([v
                                            (call (toplevel-ref vector-ref) (lref p)
                                             (const 0))])
                                        (let
                                           ([cond-test-tmp
                                             (#%eq? (lref v)
                                              (const each))])
                                         (if
                                           (lref cond-test-tmp)
                                           (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                             (const 1))
                                            (lref r))
                                           (let
                                               ([cond-test-tmp
                                                 (#%eq? (lref v)
                                                  (const each+))])
                                             (if
                                               (lref cond-test-tmp)
                                               (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                                 (const 1))
                                                (call (lref match-empty) (call (toplevel-ref reverse) (call (toplevel-ref vector-ref) (lref p)
                                                   (const 2)))
                                                 (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                                   (const 3))
                                                  (lref r))))
                                               (let
                                                   ([cond-test-tmp
                                                     (let
                                                        ([or-tmp
                                                          (#%eq? (lref v)
                                                           (const free-id))])
                                                      (if
                                                        (lref or-tmp)
                                                        (lref or-tmp)
                                                        (#%eq? (lref v)
                                                          (const atom))))])
                                                 (if
                                                   (lref cond-test-tmp)
                                                   (lref r)
                                                   (let
                                                       ([cond-test-tmp
                                                         (#%eq? (lref v)
                                                          (const vector))])
                                                     (if
                                                       (lref cond-test-tmp)
                                                       (call (lref match-empty) (call (toplevel-ref vector-ref) (lref p)
                                                         (const 1))
                                                        (lref r))
                                                       (const #<undefined>)))))))))))))))))))))]
                 [combine (lambda (r* r)
                   (if
                     (#%null? (call (toplevel-ref car) (lref r*)))
                     (lref r)
                     (#%cons (call (toplevel-ref map) (toplevel-ref car)
                        (lref r*))
                       (call (lref combine) (call (toplevel-ref map) (toplevel-ref cdr)
                         (lref r)
                         #%*)
                        (lref r)))))])
                (let
                   ([cond-test-tmp
                     (#%eq? (lref p) (const any))])
                 (if
                   (lref cond-test-tmp)
                   (#%list (lref e))
                   (let
                       ([cond-test-tmp
                         (#%eq? (lref p) (const _))])
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
                             (const false)))))))))))
              (toplevel-set identifier? (lambda (x)
               (call (lref nonsymbol-id?) (lref x))))
              (toplevel-set datum->syntax (lambda (id datum source)
               (fix
                 ([props->sourcev (lambda (alist)
                   (let
                      ([and-tmp
                        (call (toplevel-ref pair?) (lref alist))])
                    (if
                      (lref and-tmp)
                      (#%vector (call (toplevel-ref assq-ref) (lref alist)
                        (const filename))
                       (call (toplevel-ref assq-ref) (lref alist)
                        (const line))
                       (call (toplevel-ref assq-ref) (lref alist)
                        (const column)))
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
                      (call (toplevel-ref not) (lref source))])
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
                                   (call (toplevel-ref vector?) (lref source))])
                               (if
                                 (lref and-tmp)
                                 (call (toplevel-ref =) (call (toplevel-ref vector-length) (lref source)
                                   (const 3)))
                                 (const false)))])
                          (if
                            (lref cond-test-tmp)
                            (lref source)
                            (call (lref syntax-sourcev) (lref source))))))))))))
              (toplevel-set free-identifier=? (lambda (x y)
               (seq
                (if
                  (call (toplevel-ref not) (call (lref nonsymbol-id?) (lref x)))
                  (call (toplevel-ref assertion-violation) (const free-identifier=?)
                   (const Expected syntax identifier)
                   (lref x))
                  (const #<undefined>))
                (if
                  (call (toplevel-ref not) (call (lref nonsymbol-id?) (lref y)))
                  (call (toplevel-ref assertion-violation) (const free-identifier=?)
                   (const Expected syntax identifier)
                   (lref y))
                  (const #<undefined>))
                (call (lref free-id=?) (lref x) (lref y)))))
              (toplevel-set bound-identifier=? (lambda (x y)
               (seq
                (if
                  (call (toplevel-ref not) (call (lref nonsymbol-id?) (lref x)))
                  (call (toplevel-ref assertion-violation) (const bound-identifier=?)
                   (const Expected syntax identifier)
                   (lref x))
                  (const #<undefined>))
                (if
                  (call (toplevel-ref not) (call (lref nonsymbol-id?) (lref y)))
                  (call (toplevel-ref assertion-violation) (const bound-identifier=?)
                   (const Expected syntax identifier)
                   (lref y))
                  (const #<undefined>))
                (call (lref bound-id=?) (lref x) (lref y)))))
              (toplevel-set macroexpand (lambda (x rest)
               (fix
                 ([unstrip (lambda (x)
                   (fix
                     ([annotate (lambda (result)
                       (let
                          ([props
                            (call (toplevel-ref source-properties) (lref x))])
                        (if
                          (call (toplevel-ref pair?) (lref props))
                          (call (toplevel-ref datum->syntax) (const false)
                           (lref result)
                           (lref props))
                          (lref result))))])
                    (let
                       ([cond-test-tmp
                         (call (toplevel-ref pair?) (lref x))])
                     (if
                       (lref cond-test-tmp)
                       (call (lref annotate) (#%cons (call (lref unstrip) (call (toplevel-ref car) (lref x)))
                         (call (lref unstrip) (call (toplevel-ref cdr) (lref x)))))
                       (let
                           ([cond-test-tmp
                             (call (toplevel-ref vector?) (lref x))])
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
                       (call (toplevel-ref car) (lref rest)))]
                   [essew
                     (if
                       (call (toplevel-ref =) (call (toplevel-ref length) (lref rest))
                         (const 2))
                       (call (toplevel-ref car) (call (toplevel-ref cdr) (lref rest)))
                       (const (eval)))])
                 (call (lref expand-top-sequence) (#%list (call (lref unstrip) (lref x)))
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
              (call (lref global-extend) (const core)
               (const if)
               (lambda (e r w s mod)
                (let*
                   ([tmp
                     (lref e)]
                   [tmp-1
                     (call (toplevel-ref $sc-dispatch) (lref tmp)
                      (const (_ any any)))])
                 (if
                   (lref tmp-1)
                   (call (toplevel-ref apply) (lambda (test then)
                     (call (lref build-conditional) (lref s)
                      (call (lref expand) (lref test)
                       (lref r)
                       (lref w)
                       (lref mod))
                      (call (lref expand) (lref then)
                       (lref r)
                       (lref w)
                       (lref mod))
                      (call (lref build-void) (toplevel-ref no-source))))
                    (lref tmp-1))
                   (let
                       ([tmp-1
                         (call (toplevel-ref $sc-dispatch) (lref tmp)
                          (const (_ any any any)))])
                     (if
                       (lref tmp-1)
                       (call (toplevel-ref apply) (lambda (test then else)
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
                         (lref tmp)))))))))))))))))))))
 (define tree (call (toplevel-ref macroexpand) (const (if 1 2 3))))
 (define closure (call (toplevel-ref interpret/preprocess) (toplevel-ref tree)
  (const '())))
 (call (toplevel-ref print) (call (toplevel-ref closure) (const '()))))
