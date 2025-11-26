(define :adjust-only 
    (let* ([rtd (make-record-type-descriptor 'adjust-only #f #f #f #f '#((immutable syms)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'adjust-only rtd rcd)))

(define adjust-only? (record-predicate (record-type-rtd :adjust-only)))
(define adjust-only-syms (record-accessor (record-type-rtd :adjust-only) 0))
(define make-adjust-only (record-constructor (record-type-rcd :adjust-only)))

(define :adjust-prefix 
    (let* ([rtd (make-record-type-descriptor 'adjust-prefix #f #f #f #f '#((immutable sym)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'adjust-prefix rtd rcd)))

(define adjust-prefix? (record-predicate (record-type-rtd :adjust-prefix)))
(define adjust-prefix-sym (record-accessor (record-type-rtd :adjust-prefix) 0))
(define make-adjust-prefix (record-constructor (record-type-rcd :adjust-prefix)))

(define :adjust-all-except 
    (let* ([rtd (make-record-type-descriptor 'adjust-all-except #f #f #f #f '#((immutable prefix-sym) (immutable syms)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'adjust-all-except rtd rcd)))

(define adjust-all-except? (record-predicate (record-type-rtd :adjust-all-except)))
(define adjust-all-except-prefix-sym (record-accessor (record-type-rtd :adjust-all-except) 0))
(define adjust-all-except-syms (record-accessor (record-type-rtd :adjust-all-except) 1))
(define make-adjust-all-except (record-constructor (record-type-rcd :adjust-all-except)))

(define :adjust-rename 
    (let* ([rtd (make-record-type-descriptor 'adjust-rename #f #f #f #f '#((immutable to-id) (immutable from-sym)))]
           [rcd (make-record-constructor-descriptor rtd #f #f)])
        (make-record-type 'adjust-rename rtd rcd)))

(define adjust-rename? (record-predicate (record-type-rtd :adjust-rename)))
(define adjust-rename-to-id (record-accessor (record-type-rtd :adjust-rename) 0))
(define adjust-rename-from-sym (record-accessor (record-type-rtd :adjust-rename) 1))
(define make-adjust-rename (record-constructor (record-type-rcd :adjust-rename)))

(define layers '(raw raw/no-just-meta phaseless path))

(define (parse-and-perform-require! reqs self m-ns phase-shift requires+provides . rest)
    (define run? (if (null? rest) #f (car rest)))
    
    (let loop ([reqs reqs]
               [top-req #f]
               [phase-shift phase-shift]
               [just-meta 'all]
               [adjust #f]
               [layer 'raw])
        (for-each 
            (lambda (req)
                (define (check-nested want-layer)
                    (unless (member want-layer (member layer layers))
                        (syntax-violation #f "invalid nesting" req)))
            
                (define fm (and (pair? (syntax-e req))
                                (identifier? (car (syntax-e req)))
                                (syntax-e (car (syntax-e req)))))
                
                (case fm 
                    [(for-meta)
                        (check-nested 'raw/no-just-meta)
                        (let* ([m (match-syntax req '(for-meta phase-level spec :::))]
                               [p (syntax-e (m 'phase-level))])
                            (unless (phase? p)
                                (syntax-violation #f "invalid phase level" req))
                            (loop (m 'spec)
                                  (or top-req req)
                                  (phase+ phase-shift p)
                                  just-meta
                                  adjust 
                                  'phaseless))]
                    [(for-template)
                        (check-nested 'raw/no-just-meta)
                        (let ([m (match-syntax req '(for-template spec :::))])
                            (loop (m 'spec)
                                (or top-req req)
                                (phase+ phase-shift -1)
                                just-meta
                                adjust
                                'phaseless))]
                    [(for-label)
                        (check-nested 'raw/no-just-meta)
                        (let ([m (match-syntax req '(for-label spec :::))])
                            (loop (m 'spec)
                                  (or top-req req)
                                  (phase+ phase-shift #f)
                                  just-meta
                                  adjust
                                  'phaseless))]
                    [(just-meta)
                        (check-nested 'raw)
                        (let* ([m (match-syntax req '(just-meta phase-level spec :::))]
                               [p (syntax-e (m 'phase-level))])
                            (unless (phase? p)
                                (syntax-violation #f "invalid phase level" req))
                            (loop (m 'spec)
                                  (or top-req req)
                                  phase-shift
                                  p
                                  adjust
                                  'raw/no-just-meta))]
                    [(only)
                        (check-nested 'phaseless)
                        (let ([m (match-syntax req '(only spec id :::))])
                            (loop (m 'spec)
                                  (or top-req req)
                                  phase-shift
                                  just-meta
                                  (make-adjust-only (ids->sym-set (m 'id)))
                                  'path))]
                    [(prefix)
                        (check-nested 'phaseless)
                        (let ([m (match-syntax req '(prefix id:prefix spec))])
                            (loop (list (m 'spec))
                                  (or top-req req)
                                  phase-shift
                                  just-meta
                                  (make-adjust-prefix (syntax-e (m 'id:prefix)))
                                  'path))]
                    
                    [(all-except)
                        (check-nested 'phaseless)
                        (let ([m (match-syntax req '(all-except spec id :::))])
                            (loop (list (m 'spec))
                                  (or top-req req)
                                  phase-shift
                                  just-meta
                                  (make-adjust-all-except
                                      (string->symbol "")
                                      (ids->sym-set (m 'id)))
                                  'path))]
                    [(rename)
                        (check-nested 'phaseless)
                        (let ([m (match-syntax req '(rename spec id:to id:from))])
                            (loop (list (m 'spec))
                                  (or top-req req)
                                  phase-shift
                                  just-meta
                                  (make-adjust-rename
                                      (m 'id:to)
                                      (syntax-e (m 'id:from)))
                                  'path))]

                    [(for-syntax)
                        (check-nested 'raw/no-just-meta)
                        (let* ([m (match-syntax req '(for-syntax spec :::))])
                            (loop (m 'spec)
                                  (or top-req req)
                                  (phase+ phase-shift 1)
                                  just-meta
                                  adjust 
                                  'phaseless))]
                    [else
                        (let ([mp (syntax->datum req)])
                            (perform-require!
                                mp 
                                self
                                (or req top-req)
                                m-ns phase-shift just-meta adjust
                                requires+provides 
                                run?
                                #f))]))
            reqs)))

(define (ids->sym-set ids)
    (foldl (lambda (s id)
        (set-add s (syntax-e id)))
        '()
        ids))

(define (perform-initial-require! mod-path self in-stx m-ns requires+provides)
    (perform-require!
        mod-path 
        self
        in-stx
        m-ns
        0
        'all
        #f
        requires+provides
        #f 
        #t))

(define (perform-require! mod-path self in-stx m-ns phase-shift just-meta adjust requires+provides run? can-shadow?)
    (define module-name (resolve-module-path mod-path self))
    (define bind-in-stx (if (adjust-rename? adjust)
                            (adjust-rename-to-id adjust)
                            in-stx))
    (define done-syms (make-core-hash-equal))
    (add-required-module! requires+provides module-name phase-shift)
    (bind-all-provides!
        bind-in-stx
        phase-shift 
        m-ns 
        module-name
        (lambda (binding)
            (define sym (module-binding-nominal-sym binding))
            (define provide-phase (module-binding-nominal-phase binding))
            (define adjusted-sym 
                (cond 
                    [(and (not (eq? just-meta 'all))
                          (not (equal? provide-phase just-meta)))
                        #f]
                    [(not adjust) sym]
                    [(adjust-only? adjust)
                        (and (set-member? (adjust-only-syms adjust) sym)
                             (core-hash-set! done-syms sym #t)
                             sym)]
                    [(adjust-prefix? adjust)
                        (string->symbol 
                            (string-append (adjust-prefix-sym adjust)
                                           (symbol->string sym)))]
                    [(adjust-all-except? adjust)
                        (and (not (and (set-member? (adjust-all-except-syms adjust) sym)
                                       (core-hash-set! done-syms sym #t)))
                             (string->symbol 
                                 (string-append (symbol->string (adjust-all-except-prefix-sym adjust))
                                                (symbol->string sym))))]
                    [(adjust-rename? adjust)
                        (and (eq? sym (adjust-rename-from-sym adjust))
                             (core-hash-set! done-syms sym #t)
                             (adjust-rename-to-id adjust))]
                    [else #f]))
                (when adjusted-sym
                    (let* ([s (datum->syntax bind-in-stx adjusted-sym)]
                           [bind-phase (phase+ phase-shift provide-phase)])
                        (add-defined-or-required-id! requires+provides
                            s bind-phase binding can-shadow?)))
                adjusted-sym))
    (namespace-module-visit! m-ns module-name phase-shift)
    (when run? 
        (namespace-module-instantiate! m-ns module-name phase-shift))
    (let ([need-syms (cond 
                        [(adjust-only? adjust) (adjust-only-syms adjust)]
                        [(adjust-all-except? adjust) (adjust-all-except-syms adjust)]
                        [(adjust-rename? adjust) (list (adjust-rename-from-sym adjust))]
                        [else #f])])
        (when (and need-syms 
                   (not (= (length need-syms) (core-hash-size done-syms))))
            (for-each (lambda (sym)
                (unless (core-hash-ref done-syms sym #f)
                    (syntax-violation #f "required symbol not provided" sym)))
                     need-syms))))

(define (bind-all-provides! in-stx phase-shift ns module-name filter)
    (define m (namespace->module ns module-name))
    (define self (mod-self-name m))
   
    (for-each 
        (lambda (entry)
            (define provide-phase-level (car entry))
            (define provides (cdr entry))
            (define phase (phase+ phase-shift provide-phase-level))
           
            (for-each 
                (lambda (entry)
                    (define sym (car entry))
                    (define binding (cdr entry))
                    (define from-mod (module-binding-module binding))
                    (define b (make-module-binding 
                        (if (eq? from-mod self)
                            module-name
                            from-mod)
                        (module-binding-phase binding)
                        (module-binding-sym binding)
                        module-name
                        provide-phase-level
                        sym
                        phase-shift))
                   
                    (let ([sym (filter b)])
                        (when sym 
                            (add-binding!
                                (datum->syntax in-stx sym) b phase))))
                (core-hash->list provides))
        )
        (core-hash->list (mod-provides m))))
