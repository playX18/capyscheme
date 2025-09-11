
(define syntax->datum #f)
(define datum->syntax #f)
(define identifier? #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define $sc-dispatch #f)
(define macroexpand #f)

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

    (define (build-void s) (make-void s))
    (define (build-global-definition src mod var exp)
        (make-toplevel-define src (and mod (cdr mod)) var (maybe-name-value var exp)))
    (define (build-global-reference src var mod)
        (make-toplevel-ref src mod var))
    (define (build-primcall src name args)
        (make-primcall src name args))
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
    (define (build-lexical-assignment src name sym value)
      (make-lset src name sym value))
    (define (build-call src rator rands)
      (make-application src rator rands))
    (define (build-conditional src test-exp then-exp else-exp)
        (make-if src test-exp then-exp else-exp))


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
            (search sym (cdr subst) (cdr marks))]
          [(and (pair? subst)
                (vector? (car subst))
                (eq? (vector-ref (car subst) 0) 'ribcage))
            (let* ([ribcage (car subst)]
                   [rsymnames (ribcage-symnames ribcage)]
                   [rmarks (ribcage-marks ribcage)]
                   [rlabels (ribcage-labels ribcage)])
            (let ([subst (cdr subst)])
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
                            [label (car rlabels)]
                            [rlabels (cdr rlabels)])
                        (if (and (eq? rsym sym) (same-marks? marks rmarks1))
                          (cond
                            [(pair? label)
                              (if (equal? (car label) mod)
                                label
                                (lp rsymnames rmarks rlabels))]
                            [else label])))])))
              (if (vector? rsymnames) (search-vector-rib) (search-list-rib))))]))
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
        (define (record-definition! id var)
            (let ([mod (cons 'hygiene (module-name (current-module)))])
                (extend-ribcage! ribcage id
                                (cons (or (syntax-module id) mod)
                                        (wrap var top-wrap mod)))))
        (define (macro-introduced-identifier? id)
            (not (equal? (wrap-marks (syntax-wrap id)) '(top))))
        (define (ensure-fresh-name var)
            (define (ribcage-has-var? var)
                (let loop ([labels (ribcage-labels ribcage)])
                    (cond
                        [(null? labels) #f]
                        [else
                            (let ([wrapped (cdr (car labels))] [labels (cdr labels)])
                                (or (eq? (syntax-expression wrapped) var) (loop labels)))])))
            (let loop ([unique var] [n 1])
                (if (ribcage-has-var? unique)
                    (let ([tail (string->symbol (number->string n))])
                        (loop (symbol-append var '- tail) (+ 1 n)))
                    unique)))
        (define (fresh-derived-name id orig-form)
            (ensure-fresh-name
                (symbol-append
                    (syntax-expression id)
                    '-
                    (string->symbol
                        (number->string
                            (hash (syntax->datum orig-form))) 16))))
        (define (parse body r w s m esew mod)
            (let loop ([body body])

                (cond
                  [(null? body) '()]
                  [else
                      (let ([head (car body)] [tail (cdr body)])
                          (let ([thunks (parse1 head r w s m essew mod)])
                              (append thunks (loop tail))))])))

        (define (parse1 x r w s m essew mod)
            (define (current-module-for-expansion mod)
                (cond
                    [(and (pair? mod) (eq? (car mod) 'hygiene)) (cons 'hygiene (module-name (current-module)))]
                    [else mod]))

            (call-with-values
                (lambda ()
                    (let ([mod (current-module-for-expansion mod)])
                        (syntax-type x r w (source-annotation x) ribcage mod #f)))
                (lambda (type value form e w s mod)
                    (cond
                     [(eq? type 'define-form)
                         (let* ([id (wrap value w mod)]
                                [var (if (macro-introduced-identifier? id)
                                          (fresh-derived-name id x)
                                          (syntax-expression id))])
                            (record-definition! id var)
                            (list
                                (if (eq? m 'c&e)
                                    (let ([x (build-global-definition s mod var (expand e r w mod))])
                                        (top-level-eval x mod)
                                        (lambda () x))

                                (call-with-values
                                    (lambda () (resolve-identifier id empty-wrap r mod #t))
                                    (lambda (type* value* mod*)
                                        (if (eq? type* 'macro)
                                            (top-level-eval (build-global-definition s mod var (build-void s)) mod))
                                        (lambda ()
                                            (build-global-definition s mod var (expand e r w mod))))))))]
                      [(eq? type 'begin-form)
                          (let ([tmp ($sc-dispatch e '(_ . each-any))])
                              (if tmp
                                  (apply (lambda (e1) (parse e1 r w s m essew mod)) tmp)
                                  (syntax-violation
                                      #f
                                      "source expansion failed to match any pattern"
                                      e)))]
                      [else
                          (list
                              (if (eq? m 'c&e)
                                  (let ([x (expand-expr type value form e r w s mod)])
                                      (top-level-eval x mod)
                                      (lambda () x))
                                  (lambda ()
                                      (expand-expr type value form e r w s mod))))]
                    ))))
        (let ([res (let lp ([thunks (parse body r w s m essew mod)])
            (if (null? thunks)
                '()
                (cons ((car thunks)) (lp (cdr thunks)))))])
            (if (null? res) (build-void s) (build-sequence s res)))))
    (define (lambda-var-list vars)
       (let lvl ((vars vars) (ls '()) (w empty-wrap))
         (cond
           ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w #f) ls) w))
           ((id? vars) (cons (wrap vars w #f) ls))
           ((null? vars) ls)
           ((syntax? vars) (lvl (syntax-expression vars) ls (join-wraps w (syntax-wrap vars))))
           (else (cons vars ls)))))
    (define (syntax-type e r w s rib mod for-car?)
        (cond
         [(symbol? e)
              (call-with-values (lambda () (resolve-identifier e w r mod #t))
                  (lambda (type value mod*)
                      (cond
                          [(eq? type 'macro)
                              (if for-car?
                                  (values type value e e w s mod)
                                  (syntax-type (expand-macro value e r w s rib mod) r empty-wrap s rib mod #f))]
                          [(eq? type 'global)
                              (values type value e value w s mod*)]
                          [else (values type value e e w s mod*)])))]
         [(pair? e)
            (let ([first (car e)])
                (call-with-values (lambda () (syntax-type first r w s rib mod #t))
                    (lambda (ftype fval fform fe fw fs fmod)
                        (cond
                            [(eq? ftype 'lexical) (values 'lexical-call fval e e w s mod)]
                            [(eq? ftype 'global)
                                (if (equal? fmod '(primitive))
                                    (values 'primitive-call fval e e w s mod)
                                    (values 'global-call (make-syntax fval w fmod fs) e e w s mod))]
                            [(eq? ftype 'macro)
                                (syntax-type (expand-macro fval e r w s rib mod)
                                    r empty-wrap s rib mod for-car?)]
                            [(eq? ftype 'module-ref)
                                (call-with-values (lambda () (fval e r w mod))
                                    (lambda (e r w s mod) (syntax-type e r w s rib mod for-car?)))]
                            [(eq? ftype 'core) (values 'core-form fval e e w s mod)]
                            [(eq? ftype 'local-syntax) (values 'local-syntax-form fval e e w s mod)]
                            [(eq? ftype 'begin) (values 'begin-form #f e e w s mod)]
                            [(eq? ftype 'eval-when) (values 'eval-when-form #f e e w s mod)]
                            [(eq? ftype 'define)
                                (let ([tmp ($sc-dispatch e '(_ any any))])
                                    (if (and tmp (id? (car tmp)))
                                        (apply (lambda (name val) (values 'define-form name e val w s mod)) tmp)
                                        (let ([tmp ($sc-dispatch e '(_ (any . any) any . each-any))])
                                            (if (and tmp
                                                    (apply (lambda (name args e1 e2)
                                                        (and (id? name) (valid-bound-ids? (lambda-var-list args))) tmp)))
                                                (apply (lambda (name args e1 e2)
                                                    (values 'define-form
                                                            (wrap name w mod)
                                                            (wrap e w mod)
                                                            (source-wrap
                                                                (cons (make-syntax 'lambda '((top)) '(hygiene capy))
                                                                      (wrap (cons args (cons e1 e2)) w mod))
                                                                empty-wrap
                                                                s
                                                                #f)
                                                            empty-wrap
                                                            s
                                                            mod)) tmp)
                                                (syntax-violation #f "syntax expression failed to match any pattern" e)))))]
                            [else (values 'call #f e e w s mod)]
                        ))))]
         [(syntax? e)
             (syntax-type (syntax-expression e)
                          r
                          (join-wraps w (syntax-wrap e))
                          (or (source-annotation e) s) rib
                          (or (syntax-module e) mod) for-car?)]
         [(self-evaluating? e) (values 'constant #f e e w s mod)]
         [else (values 'other #f e e w s mod)]))

    (define (expand-install-global mod name type e)
        (build-global-definition
            #f
            mod
            name
            (build-primcall
                #f
                'make-syntax-transformer
                (list (make-constant #f name)
                      (make-constant #f
                          (if (eq? type 'define-syntax-parameter-form)
                              'syntax-parameter 'macro))
                      e))))
    (define (expand e r w mod)
        (call-with-values
            (lambda () (syntax-type e r w (source-annotation e) #f mod #f))
            (lambda (type value form e w s mod)
                (expand-expr type value form e r w s mod))))
    (define (expand-expr type value form e r w s mod)
        (cond
            [(eq? type 'lexical)
                (build-lexical-reference s e value)]
            [(or (eq? type 'core) (eq? type 'core-form))
                (value e r w s mod)]
            [(eq? type 'module-ref)
                (call-with-values (lambda () (value e r w mod))
                    (lambda (e r w s mod)
                        (expand e r w mod)))]
            [(eq? type 'lexical-call)
                (expand-call
                    (let ([id (car e)])
                        (build-lexical-reference (source-annotation id)
                                                 (if (syntax? id) (syntax->datum id) id)
                                                 value))
                    e r w s mod)]
            [(eq? type 'global-call)
                (expand-call
                 (build-global-reference (or (source-annotation (car e)) s)
                                         (if (syntax? value)
                                             (syntax-expression value)
                                             value)
                                         (or (and (syntax? value)
                                                  (syntax-module value))
                                             mod))
                 e r w s mod)]
            [(eq? type 'primitive-call)
                (build-primcall s
                                value
                                (map (lambda (e) (expand e r w mod)) (cdr e)))]
            [(eq? type 'constant) (make-constant s (strip e))]
            [(eq? type 'call) (expand-call (expand (car e) r w mod) r w s mod)]
            [(eq? type 'begin-form)
                (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_ any . each-any))))
                  (if tmp-1
                      (apply (lambda (e1 e2) (expand-sequence (cons e1 e2) r w s mod)) tmp-1)
                      (let ((tmp-1 ($sc-dispatch tmp '(_))))
                        (if tmp-1
                            (apply (lambda ()
                                     (syntax-violation #f "sequence of zero expressions" (source-wrap e w s mod)))
                                   tmp-1)
                            (syntax-violation #f "source expression failed to match any pattern" tmp)))))]
            [(or (eq? type 'define-form) (eq? type 'define-syntax-form) (eq? type 'define-syntax-parameter-form))
                (syntax-violation #f "definition in expression context, where definitions are not allowed" (source-wrap e w s mod))]
            [else (syntax-violation #f "unexpected syntax" (source-wrap e w s mod))]))
    (define (expand-call x e r w s mod)
        (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(any . each-any))))
          (if tmp
              (apply (lambda (e0 e1) (build-call s x (map (lambda (e) (expand e r w mod)) e1))) tmp)
              (syntax-violation #f "source expression failed to match any pattern" tmp-1))))

    (define (expand-body body outer-form r w mod)
        (let* ([r (cons '("placeholder" . (placeholder)) r)]
               [ribcage (make-empty-ribcage)]
               [w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))])
            (let parse ([body (map (lambda (x) (cons r (wrap x w mod))) body)]
                        [ids '()] [labels '()]
                        [var-ids '()] [vars '()] [vals '()] [bindings '()]
                        [expand-tail-expr #f])
                (cond
                 [(null? body)
                    (if (not expand-tail-expr)
                        (if (null? ids)
                            (syntax-violation #f "empty body" outer-form)
                            (syntax-violation #f "body should end with an expression" outer-form)))
                    (if (not (valid-bound-ids? ids))
                        (syntax-violation #f "invalid or duplicate identifier in definition" outer-form))
                    (set-cdr! r (extend-env labels bindings (cdr r)))
                    (let ([src (source-annotation outer-form)])
                        (let lp ([var-ids var-ids] [vars vars] [vals vals] [tail (expand-tail-expr)])
                            (cond
                             [(null? var-ids) tail]
                             [(not (car var-ids))
                                 (lp (cdr var-ids) (cdr vars) (cdr vals) (make-seq src ((car vals)) tail))]
                             [else
                                 (let ([var-ids (map (lambda (id)
                                                         (if id (syntax->datum id) '_)) (reverse var-ids))]
                                       [vars (map (lambda (var) (or var (gen-lexical '_))) (reverse vars))]
                                       [vals (map (lambda (expand-expr id)
                                                    (if id
                                                        (expand-expr)
                                                        (make-seq
                                                            src
                                                            (expand-expr)
                                                            (make-void #f))))
                                              (reverse vals))])
                                 (build-letrec src #t var-ids vars vals tail))])))]
                     [expand-tail-expr
                         (parse body ids labels (cons #f var-ids) (cons #f vars) (cons expand-tail-expr vals) bindings #f)]
                     [else
                        (let ([e (cdar body)] [er (caar body)] [body (cdr body)])
                            (call-with-values
                                (lambda () (synta-type e er empty-wrap (source-annotation e) ribcage mod #f))
                                (lambda (type value form e w s mod)
                                    (cond
                                    [(eq? type 'define-form)
                                         (let ([id (wrap value w mod)] [label (gen-label)])
                                             (let ([var (gen-var id)])
                                                 (extend-ribcage! ribcage id label)
                                                 (parse body
                                                        (cons id ids) (cons label labels)
                                                        (cons id var-ids)
                                                        (cons var vars)
                                                        (cons (let ([wrapped (source-wrap e w s mod)])
                                                            (lambda ()
                                                                (expand wrapped er empty-wrap mod)))
                                                            vals)
                                                        (cons (make-binding 'lexical var) bindings)
                                                       #f)))]
                                    [(eq? type 'begin-form)
                                        (let* ((tmp-1 e) (tmp ($sc-dispatch tmp-1 '(_ . each-any))))
                                          (if tmp
                                              (apply (lambda (e1)
                                                       (parse (let f ((forms e1))
                                                                (if (null? forms)
                                                                    body
                                                                    (cons (cons er (wrap (car forms) w mod))
                                                                          (f (cdr forms)))))
                                                              ids
                                                              labels
                                                              var-ids
                                                              vars
                                                              vals
                                                              bindings
                                                              #f))
                                                     tmp)
                                              (syntax-violation #f "source expression failed to match any pattern" tmp-1)))]
                                    [(eq? type 'define-syntax-form) (error 'expand-body "not yet implemented: define-syntax")]
                                    [(eq? type 'define-syntax-parameter-form) (error 'expand-body "not yet implemented: define-syntax-parameter")]
                                    [(eq? type 'local-syntax-form) (error 'expand-body "not yet implemented: local-syntax")]
                                    [else
                                        (let ([wrapped (source-wrap e w s mod)])
                                            (parse body ids labels var-ids vars vals bindings
                                                (lambda ()
                                                    (expand wrapped er empty-wrap mod))))])))]))))
    (define (gen-var id)
        (let ([id (if (syntax? id) (syntax-expression id) id)])
            (gen-lexical id)))
    (define (strip x)
      (cond
        [(syntax? x) (strip (syntax-expression x))]
        [(pair? x) (cons (strip (car x)) (strip (cdr x)))]
        [(vector? x) (list->vector (strip (vector->list x)))]
        [else x]))
    (set! syntax->datum (lambda (x) (strip x)))
    (set! $sc-dispatch (lambda (e p)
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
        [else (match* e p empty-wrap '() #f)])))



    (set! identifier? (lambda (x) (nonsymbol-id? x)))
    (set! datum->syntax (lambda (id datum source)
        (define (props->sourcev alist)
            (and (pair? alist)
                (vector (assq-ref alist 'filename)
                        (assq-ref alist 'line)
                        (assq-ref alist 'column))))
        (make-syntax
            datum
            (if id (syntax-wrap id) empty-wrap)
            (if id (syntax-module id) #f)
            (cond
                [(not source) (props->sourcev (source-properties datum))]
                [(and (alist? source)) (props->sourcev source)]
                [(and (vector? source) (= (vector-length source 3))) source]
                [else (syntax-sourcev source)]))))
    (set! free-identifier=? (lambda (x y)
        (if (not (nonsymbol-id? x))
            (assertion-violation 'free-identifier=? "Expected syntax identifier" x))
        (if (not (nonsymbol-id? y))
            (assertion-violation 'free-identifier=? "Expected syntax identifier" y))
        (free-id=? x y)))
    (set! bound-identifier=? (lambda (x y)
        (if (not (nonsymbol-id? x))
            (assertion-violation 'bound-identifier=? "Expected syntax identifier" x))
        (if (not (nonsymbol-id? y))
            (assertion-violation 'bound-identifier=? "Expected syntax identifier" y))
        (bound-id=? x y)))

    (set! macroexpand (lambda (x . rest)
        (define (unstrip x)
            (define (annotate result)
                (let ([props (source-properties x)])
                    (if (pair? props)
                        (datum->syntax #f result props)
                        result)))

            (cond
             [(pair? x) (annotate (cons (unstrip (car x)) (unstrip (cdr x))))]
             [(vector? x)
                 (annotate (list->vector (map unstrip (vector->list x))))]
             [(syntax? x) x]
             [else (annotate x)]))
        (let ([m (if (null? rest) 'e (car rest))]
              [essew (if (= (length rest) 2) (car (cdr rest)) '(eval))])
            (expand-top-sequence (list (unstrip x)) null-env top-wrap #f m essew
                 (cons 'hygiene (module-name (current-module)))))))



    (global-extend 'define 'define '())
    (global-extend 'begin 'begin '())
    (global-extend 'core 'if
        (lambda (e r w s mod)
          (let* ((tmp e) (tmp-1 ($sc-dispatch tmp '(_ any any))))
            (if tmp-1
                (apply (lambda (test then)
                         (build-conditional s (expand test r w mod) (expand then r w mod) (build-void no-source)))
                       tmp-1)
                (let ((tmp-1 ($sc-dispatch tmp '(_ any any any))))
                  (if tmp-1
                      (apply (lambda (test then else)
                               (build-conditional
                                s
                                (expand test r w mod)
                                (expand then r w mod)
                                (expand else r w mod)))
                             tmp-1)
                      (syntax-violation #f "source expression failed to match any pattern" tmp)))))))
    )

(define tree (macroexpand '(if 1 2 3)))
(define closure (interpret/preprocess tree '()))

(print (closure '()))
