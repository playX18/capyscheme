
(let ([syntax? (module-ref (current-module) 'syntax?)]
      [make-syntax (module-ref (current-module) 'make-syntax)]
      [syntax-expression (module-ref (current-module) 'syntax-expression)]
      [syntax-wrap (module-ref (current-module) 'syntax-wrap)]
      [syntax-module (module-ref (current-module) 'syntax-module)]
      [syntax-sourcev (module-ref (current-module) 'syntax-sourcev)])
    (define (toplevel-eval exp env) (primitive-eval exp))
    (define (local-eval exp env) (primitive eval exp))

    (define (global-extend type sym val)
      (module-define! (current-module) sym (make-syntax-transformer sym type val)))
    
    (define (sourcev-filename s) (vector-ref s 0))
    (define (sourcev-line s) (vector-ref s 1))
    (define (sourcev-column s) (vector-ref s 2))
    (define sourcev->alist
             (lambda (sourcev)
               (letrec* ((maybe-acons (lambda (k v tail) (if v (acons k v tail) tail))))
                 (and sourcev
                      (maybe-acons
                       'filename
                       (sourcev-filename sourcev)
                       (list (cons 'line (sourcev-line sourcev)) (cons 'column (sourcev-column sourcev))))))))
    (define maybe-name-value
             (lambda (name val)
               (if (proc? val)
                   (let ((meta (proc-meta val)))
                     (if (assq 'name meta) val (make-proc (term-src val) (proc-args val) (proc-body val) (acons 'name name meta))))
                   val)))
    (define (build-simple-lambda src ids vars meta exp)
      (make-proc src vars exp meta ids))
    (define (build-sequence src exps)
               (let* ((v exps)
                      (fk (lambda ()
                            (let ((fk (lambda () (error "value failed to match" v))))
                              (if (pair? v)
                                  (let ((vx (car v)) (vy (cdr v)))
                                    (let* ((head vx) (tail vy)) (make-sequence src head (build-sequence #f tail))))
                                  (fk))))))
                 (if (pair? v) (let ((vx (car v)) (vy (cdr v))) (let ((tail vx)) (if (null? vy) tail (fk)))) (fk))))
    (define (build-let src ids vars val-exps body-exp)
      (let* ([v (map maybe-name-value ids val-exps)])
        (cond 
          [(null? v) body-exp]
          [else 
            (make-let src 'let ids vars v body-exp)])))
    (define (build-let* src ids vars val-exps body-exp)
      (let* ([v (map maybe-name-value ids val-exps)])
        (cond 
          [(null? v) body-exp]
          [else 
            (make-let src 'let* ids vars v body-exp)])))
    (define (build-letrec src ids vars val-exps body-exp)
      (let* ([v (map maybe-name-value ids val-exps)])
        (cond 
          [(null? v) body-exp]
          [else 
            (make-let src 'letrec ids vars v body-exp)])))
    (define (build-letrec* src ids vars val-exps body-exp)
      (let* ([v (map maybe-name-value ids val-exps)])
        (cond 
          [(null? v) body-exp]
          [else 
            (make-let src 'letrec* ids vars v body-exp)])))
    (define (build-lexical-reference src name sym)
      (make-lref src name sym))
    (define (build-call src rator rands)
      (make-application src rator rands))

    (define build-named-let
             (lambda (src ids vars val-exps body-exp)
               (let* ((v vars) (fk (lambda () (error "value failed to match" v))))
                 (if (pair? v)
                     (let ((vx (car v)) (vy (cdr v)))
                       (let* ((f vx) (vars vy) (v ids) (fk (lambda () (error "value failed to match" v))))
                         (if (pair? v)
                             (let ((vx (car v)) (vy (cdr v)))
                               (let* ((f-name vx) (ids vy) (proc (build-simple-lambda src ids vars '() body-exp)))
                                 (make-letrec
                                  src
                                  #f
                                  (list f-name)
                                  (list f)
                                  (list (maybe-name-value f-name proc))
                                  (build-call
                                   src
                                   (build-lexical-reference src f-name f)
                                   (map maybe-name-value ids val-exps)))))
                             (fk))))
                     (fk)))))
    (define (gen-lexical id) (module-gensym (symbol->string id)))
    (define (source-annotation x) (if (syntax? x) (syntax-sourcev x) (datum-sourcev x)))
    (define (binding-type x) (car x))
    (define (binding-value x) (cdr x))
    (define null-env '())
    (define (extend-env labels bindings r)
      (cond 
        [(null? labels) r]
        [(pair? labels)
          (let ([label (car labels)] [labels (cdr labels)])
            (let ([binding (car bindings)] [bindings (cdr bindings)])
              (extend-env labels bindings (acons label binding r))))]))
    (define (extend-var-env labels vars r)
      (cond 
        [(null? labels) r]
        [(pair? labels)
          (let ([label (car labels)] [labels (cdr labels)])
            (let ([var (car vars)] [vars (cdr vars)])
              (extend-var-env labels vars (acons label (cons 'lexical var) r))))]))
    (define macros-only-env
             (lambda (r)
               (let* ((v r)
                      (fk (lambda ()
                            (let ((fk (lambda () (error "value failed to match" v))))
                              (if (pair? v)
                                  (let ((vx (car v)) (vy (cdr v)))
                                    (let* ((a vx)
                                           (r vy)
                                           (v a)
                                           (fk (lambda ()
                                                 (let ((fk (lambda () (error "value failed to match" v))))
                                                   (macros-only-env r)))))
                                      (if (pair? v)
                                          (let ((vx (car v)) (vy (cdr v)))
                                            (let ((k vx))
                                              (if (pair? vy)
                                                  (let ((vx (car vy)) (vy (cdr vy)))
                                                    (let ((tk (lambda () (cons a (macros-only-env r)))))
                                                      (if (eq? vx 'macro)
                                                          (tk)
                                                          (let ((tk (lambda () (tk))))
                                                            (if (eq? vx 'syntax-parameter)
                                                                (tk)
                                                                (let ((tk (lambda () (tk))))
                                                                  (if (eq? vx 'ellipsis) (tk) (fk))))))))
                                                  (fk))))
                                          (fk))))
                                  (fk))))))
                 (if (null? v) '() (fk)))))
    (define (nonsymbol-id? x) (and (syntax? x) (symbol? (syntax-expression x))))
    (define (id? x) (or (symbol? x) (and (syntax? x) (symbol? (syntax-expression x)))))
    (define (id-sym-name x) (if (syntax? x) (syntax-expression x) x))
    (define (id-sym-name&marks x w)
      (if (syntax? x)
        (values (syntax-expression x) (join-marks (wrap-marks w) (wrap-marks (syntax-wrap x))))
        (values x w)))
    (define (make-wrap marks subst) (cons marks subst))
    (define (wrap-marks w) (car w))
    (define (wrap-subst w) (cdr w))

    (define (gen-unique . args)
      (cond 
        [(null? args) (vector '(capy) (gensym "id"))]
        [else (vector (module-name (car args)) (module-generate-unique-id! (car args)))]))
    
    (define (gen-label) (gen-unique))
    (define (gen-labels ls)
      (cond 
        [(null? ls) '()]
        [else (cons (gen-label) (gen-labels (cdr ls)))]))
    (define (make-ribcage symnames marks labels) (vector 'ribcage symnames marks labels))
    (define (ribcage-symnames r) (vector-ref r 1))
    (define (ribcage-marks r) (vector-ref r 2))
    (define (ribcage-labels r) (vector-ref r 3))
    (define (set-ribcage-symnames! r v) (vector-set! r 1 v))
    (define (set-ribcage-marks! r v) (vector-set! r 2 v))
    (define (set-ribcage-labels! r v) (vector-set! r 3 v))
    
    (define empty-wrap '(()))
    (define top-wrap '((top)))
    (define the-anti-mark #f)
    (define (anti-mark w) (make-wrap (cons the-anti-mark (wrap-marks w)) (cons 'shift (wrap-subst w))))
    (define (new-mark) (gen-unique))
    (define (make-empty-ribcage) (make-ribcage '() '() '()))
    (define (extend-ribcage! ribcage id label)
      (set-ribcage-symnames! ribcage (cons (syntax-expression id) (ribcage-symnames ribcage)))
      (set-ribcage-marks! ribcage (cons (wrap-marks (syntax-wrap id)) (ribcage-marks ribcage)))
      (set-ribcage-labels! ribcage (cons label (ribcage-labels ribcage))))
    (define (make-binding-wrap ids labels w)
      (cond 
        [(null? ids) w]
        [else 
          (make-wrap
            (wrap-marks w)
            (cons (let* ([labelvec (list->vector labels)]
                         [n (vector-length labelvec)]
                         [symnamevec (make-vector n)]
                         [marksvec (make-vector n)])
                    (let f ([ids ids] [i 0])
                      (cond 
                        [(null? ids) (make-ribcage symnamevec marksvec labelvec)]
                        [else 
                          (let ([id (car ids)] [ids (cdr ids)])
                            (call-with-values 
                              (lambda () (id-sym-name&marks id w))
                              (lambda (symname marks)
                                (vector-set! symnamevec i symname)
                                (vector-set! marksvec i marks)
                                (f ids (+ i 1)))))])))))]))
    (define (smart-append m1 m2) (if (null? m2) m1 (append m1 m2)))
    (define (join-wraps w1 w2)
      (let ([m1 (wrap-marks w1)] [s1 (wrap-subst w1)])
        (if (null? m1)
          (if (null? s1) w2 (make-wrap (wrap-marks w2) (smart-append s1 (wrap-subst w2))))
          (make-wrap (smart-append m1 (wrap-marks w2)) (smart-append s1 (wrap-subst w2))))))
    
    (define (join-marks m1 m2) (smart-append m1 m2))
    (define (same-marks? x y)
      (or (eq? x y) 
          (and (not (null? x)) 
               (not (null? y)) 
               (eq? (car x) (car y)) 
               (same-marks? (cdr x) (cdr y)))))
    (define (id-var-name id w mod)
      (define (search sym subst marks)
        (cond 
          [(null? subst) #f]
          [(and (pair? subst) (eq? (car subst) 'shift))
            (search sym (cdr subst) (cdr marks))])
          [(and (pair? subst)
                (vector? (car subst))
                (eq? (vector-ref (car subst) 0) 'ribcage))
            (let* ([ribcage (car subst)]
                   [rsymnames (ribcage-symnames ribcage)]
                   [rmarks (ribcage-marks ribcage)]
                   [rlabels (ribcage-labels ribcage)])
              (define (search-vector-rib)
                (let ([n (vector-length rsymnames)])
                  (let lp ([i 0])
                    (cond
                      [(= i n) (search sym subst marks)]
                      [(and (eq? (vector-ref rsymnames i) sym) (same-marks? marks (vector-ref rmarks i)))
                        (let ([lbl (vector-ref rlabels i)])
                          (cond 
                            [(pair? lbl) 
                              (if (equal? (car lbl) mod)
                                label 
                                (lp (+ i 1)))]
                            [else lbl]))]
                      [else (lp (+ i 1))]))))
              (define (search-list-rib)
                (let lp ([rsymnames rsymnames]
                         [rmarks rmarks]
                         [rlabels rlabels])
                  (cond
                    [(null? rsymnames) (search sym subst marks)]
                    [else 
                      (let ([rsym (car rsymnames)]
                            [rsymnames (cdr rsymnames)]
                            [rmarks1 (car rmarks)]
                            [rmarks (cdr rmarks)]
                            [label (car labels)]
                            [rlabels (cdr labels)])
                        (if (and (eq? rsym sym) (same-marks? marks rmarks1))
                          (cond 
                            [(pair? label) 
                              (if (equal? (car label) mod)
                                label 
                                (lp rsymnames rmarks rlabels))]
                            [else label])))])))
              (if (vector? rsymnames) (search-vector-rib) (search-list-rib)))])
      (cond 
        [(symbol? id) (or (search id (wrap-subst w) (wrap-marks w)) id)]
        [(syntax? id)
          (let ([id (syntax-expression id)]
                [w1 (syntax-wrap id)]
                [mod (syntax-module id)])
            (let ([marks (join-marks (wrap-marks w) (wrap-marks w1))])
              (or (search id (wrap-subst w) marks)
                  (search id (wrap-subst w1) marks)
                  id)))]
        [else (syntax-violation 'id-var-name "invalid id" id)]))

    (define (locally-bound-identifiers w mod)
      (define (scan subst results)
        (cond
          [(null? subst) results]
          [(and (pair? subst) (eq? (car subst) 'shift))
            (scan (cdr subst) results)]
          [(and (pair? subst)
                (vector? (car subst))
                (eq? (vector-ref (car subst) 0) 'ribcage))
            (let* ([ribcage (car subst)]
                   [symnames (ribcage-symnames ribcage)]
                   [marks (ribcage-marks ribcage)]
                   [labels (ribcage-labels ribcage)]
                   [subst* (cdr subst)])
              (define (scan-list-rib)
                (let lp ([symnames symnames] [marks marks] [results results])
                  (cond 
                    [(null? symnames) (scan subst* results)]
                    [else 
                      (let ([sym (car symnames)]
                            [symnames (cdr symnames)]
                            [m (car marks)]
                            [marks (cdr marks)])
                        (lp symnames marks
                          (cons (wrap sym (anti-mark (make-wrap m subst)) mod) resulsts)))])))
              (define (scan-vector-rib)
                (let ([n (vector-length symnames)])
                  (let lp ([i 0] [results results])
                    (cond
                      [(= i n) (scan subst* results)]
                      [else 
                        (let ([sym (vector-ref symnames i)]
                              [m (vector-ref marks i)])
                          (lp (+ i 1)
                            (cons (wrap sym (anti-mark (make-wrap m subst)) mod) results)))]))))
              (if (vector? symnames) (scan-vector-rib) (scan-list-rib)))]))
      (scan (wrap-subst w) '()))
    (define (resolve-identifier id w r mod resolve-syntax-parameters?)
      (define (resolve-global var mod)
        (let ([v (and (not (equal? mod '(primitive)))
                           (module-variable (if mod (resolve-module (cdr mod) #t #t) (current-module)) var))])
          (if (and v (variable-bound? v) (macro? (variable-ref v)))
            (let* ([m (variable-ref v)]
                   [type (macro-type m)]
                   [trans (macro-binding m)])
              (if (eq? type 'syntax-parameter)
                (if resolve-syntax-parameters?
                  (let ([lexical (assq-ref r v)])
                    (values 'macro 
                      (if lexical 
                        (binding-value lexical)
                        trans)
                      mod))
                  (values type v mod))
                (values type trans mod)))
            (values 'global var mod))))
      (define (resolve-lexical label mod)
        (let ([b (assq-ref r label)])
          (if b 
            (let ([type (binding-type b)]
                  [value (binding-value b)])
              (if (eq? type 'syntax-parameter)
                (if resolve-syntax-parameters?
                  (values 'macro value mod)
                  (values type label mod))
                (values type value mod)))
            (values 'displaced-lexical #f #f))))
      (let ([n (id-var-name id w mod)])
        (cond 
          [(syntax? n)
            (cond 
              [(not (eq? n id))
                (resolve-identifier n w r mod resolve-syntax-parameters?)]
              [else 
                (resolve-identifier (syntax-expression n) (syntax-wrap n) r (or (syntax-module n) mod) resolve-syntax-parameters?)])]
          [(symbol? n)
            (resolve-global n (or (and (syntax? id) (syntax-module id)) mod))]
          [else 
            (resolve-lexical n (or (and (syntax? id) (syntax-module id)) mod))])))
    (define transformer-environment 
      (make-fluid
        (lambda (k)
          (assertion-violation 'transformer-environment "called outside the dynamic extent of a syntax transformer"))))
    (define (with-transformer-environment k)
      ((fluid-ref transformer-environment) k))
    
    
    (define (free-id=? i j)
      (let* ([mi (and (syntax? i) (syntax-module i))]
             [mj (and (syntax? j) (syntax-module j))]
             [ni (id-var-name i empty-wrap mi)]
             [nj (id-var-name j empty-wrap mj)])
        (define (id-module-binding id mod)
          (module-variable 
            (if mod (resolve-module (cdr mod) #t #t) (current-module))
            (id-sym-name id)))
        
        (cond 
          [(syntax? ni) (free-id=? ni j)]
          [(syntax? nj) (free-id=? i nj)]
          [(symbol? ni)
            (and (eq? nj (id-sym-name j))
                 (let ([bi (id-module-binding i mi)]
                       [bj (id-module-binding j mj)])
                    (and (eq? bi bj)
                      (or bi (eq? ni nj)))))]
          [else (equal? ni nj)])))
    
    (define (bound-id=? i j)
      (if (and (syntax? i) (syntax? j))
        (and (eq? (syntax-expression i) (syntax-expression j))
             (same-marks? (wrap-marks (syntax-wrap i)) (wrap-marks (syntax-wrap j))))))
    
    (define (valid-bound-ids? ids)
      (and (let all-ids? ([ids ids])
        (cond
          [(null? ids) #t]
          [(pair? ids)
            (let ([id (car ids)] [ids (cdr ids)])
              (and (id? id) (all-ids? ids)))]
          [else #f]))))
    (define (distinct-bound-ids? ids)
      (let distinct? ([ids ids])
        (cond 
          [(null? ids) #t]
          [(pair? ids)
            (let ([id (car ids)] [ids (cdr ids)])
              (and (not (bound-id-member? id ids)) (distinct? ids)))])))

    (define (bound-id-member? x ids)
      (if (null? ids) 
        #f 
        (or (bound-id=? x (car ids))
            (bound-id-member? x (cdr ids)))))
    
    (define (wrap x w defmod) (source-wrap x w #f defmod))
    (define (wrap-syntax x w defmod)
      (make-syntax (syntax-expression x) w (or (syntax-module x) defmod) (syntax-sourcev x)))
    (define (source-wrap x w s defmod)
      (cond 
        [(and (null? (wrap-marks w))
              (null? (wrap-subst w))
              (not defmod)
              (not s)) 
          x]
        [(syntax? x) (wrap-syntax x (join-wraps w (syntax-wrap x)) defmod)]
        [(null? x) x]
        [else (make-syntax x w defmod s)]))
    (define (expand-sequence body r w s mod)
      (build-sequence s 
        (let lp ([body body])
          (if (null? body) '()
            (let ([head (car body)] [tail (cdr body)])
              (let ([expr (expand hed r w s mod)])
                (cons expr (lp tail))))))))
    
    (define (expand-top-sequence body r w s m essew mod)
      (let* ([r (cons '("placeholder" . (placeholder)) r)]
             [ribcage (make-empty-ribcage)]
             [w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))])
            42
        )
    )

    (define (strip x)
      (cond 
        [(syntax? x) (strip (syntax-expression x))]
        [(pair? x) (cons (strip (car x)) (strip (cdr x)))]
        [(vector? x) (list->vector (strip (vector->list x)))]
        [else x]))

    (define ($sc-dispatch e p)
      (define (match-each e p w mod)
        (cond 
          [(pair? e)
            (let ([first (match (car e) p w '() mod)])
              (and first
                (let ([rest (match-each (cdr e) p w mod)])
                  (and rest (cons first rest)))))]
          [(null? e) '()]
          [(syntax? e)
            (match-each 
              (syntax-expression e)
              p
              (join-wraps w (syntax-wrap e))
              (or (syntax-module e) mod))]
          [else #f]))
      (define (match-each+ e x-pat y-pat z-pat w r mod)
        (let f ((e e) (w w))
          (cond
          ((pair? e)
            (call-with-values (lambda () (f (cdr e) w))
              (lambda (xr* y-pat r)
                (if r
                    (if (null? y-pat)
                        (let ((xr (match (car e) x-pat w '() mod)))
                          (if xr
                              (values (cons xr xr*) y-pat r)
                              (values #f #f #f)))
                        (values
                        '()
                        (cdr y-pat)
                        (match (car e) (car y-pat) w r mod)))
                    (values #f #f #f)))))
          ((syntax? e)
            (f (syntax-expression e)
              (join-wraps w (syntax-wrap e))))
          (else
            (values '() y-pat (match e z-pat w r mod))))))
      (define (match-each-any e w mod)
        (cond
        ((pair? e)
          (let ((l (match-each-any (cdr e) w mod)))
            (and l (cons (wrap (car e) w mod) l))))
        ((null? e) '())
        ((syntax? e)
          (match-each-any (syntax-expression e)
                          (join-wraps w (syntax-wrap e))
                          mod))
        (else #f)))
      (define (match-empty p r)
        (cond 
          [(null? p) r]
          [(eq? p '_) r]
          [(eq? p 'any) (cons '() r)]
          [(pair? p) (match-empty (car p) (match-empty (cdr p) r))]
          [(eq? p 'each-any) (cons '() r)]
          [else 
            (let ([v (vector-ref p 0)])
              (cond 
                [(eq? v 'each) (match-empty (vector-ref p 1) r)]
                [(eq? v 'each+) (match-empty (vector-ref p 1) 
                                              (match-empty 
                                                (reverse (vector-ref p 2))
                                                (match-empty (vector-ref p 3) r)))]
                [(or (eq? v 'free-id) (eq? v 'atom)) r]
                [(eq? v 'vector) (match-empty (vector-ref p 1) r)]))]))
      (define (combine r* r)
        (if (null? (car r*))
          r 
          (cons (map car r*) (combine (map cdr r *) r))))
      (define (match* e p w r mod)
        (cond 
          [(null? p) (and (null? e) r)]
          [(pair? p)
            (and (pair? e) (match (car e) (car p) w 
                                  (match (cdr e) (cdr p) w r mod)
                                  mod))]
          [(eq? p 'each-any)
            (let ([l (match-each-any e w mod)])
              (and l (cons l r)))]
          [else 
            (let ([v (vector-ref p 0)])
              (cond 
                [(eq? v 'each)
                  (if (null? p)
                    (match-empty (vector-ref p 1) r)
                    (let ([l (match-each e (vector-ref p 1) w mod)])
                      (and l 
                        (let collect ([l l])
                          (if (null? (car l))
                            r
                            (cons (map car l) (collect (map cdr l))))))))]
                [(eq? v 'each+)
                  (call-with-values 
                    (lambda ()
                      (match-each+ e (vector-ref p 1) (vector-ref p 2) (vector-ref p 3) w r mod))
                    (lambda (xr* y-pat r)
                      (and r 
                        (null? y-pat)
                        (if (null? xr*)
                          (match-empty (vector-ref p 1) r)
                          (combine xr* r)))))]
                [(eq? v 'free-id)
                  (and (id? e) (free-id=? (wrap e w mod) (vector-ref p 1)) r)]
                [(eq? v 'atom) (and (equal? (vector-ref p 1) (strip e)) r)]
                [(eq? v 'vector)
                  (and (vector? e)
                       (match (vector->list e) (vector-ref p 1) w r mod))]))]))
      (define (match e p w r mod)
        (cond 
          [(not r) #f]
          [(eq? p '_) r]
          [(eq? p 'any) (cons (wrap e w mod) r)]
          [(syntax? p)
            (match* 
              (syntax-expression e)
              p
              (join-wraps w (syntax-wrap p))
              r
              (or (syntax-module e) mod))]
          [else (match* e p w r mod)]))
      (cond 
        [(eq? p 'any) (list e)]
        [(eq? p '_) '()]
        [(syntax? e)
          (match* (syntax-expression e) p (syntax-wrap e) '() (syntax-module e))]
        [else (match* e p empty-wrap '() #f)]))
    
    (print ($sc-dispatch '(1 2 3) '(any any #(atom 3)))))