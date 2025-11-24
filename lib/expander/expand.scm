(define app-sym (string->symbol "#%app"))
(define datum-sym (string->symbol "#%datum"))
(define top-sym (string->symbol "#%top"))

(define (expand s ctx)
    (cond 
        [(identifier? s)
            (expand-identifier s ctx)]
        [(and (pair? (syntax-e s))
              (identifier? (car (syntax-e s))))
            (expand-id-application-form s ctx)]
        [(or (pair? (syntax-e s))
             (null? (syntax-e s)))
            (expand-implicit app-sym s ctx)]
        [else 
            (expand-implicit datum-sym s ctx)]))

(define (expand-identifier s ctx)
    (define binding (resolve s (expand-context-phase ctx)))
    (cond 
        [(not binding)
            (expand-implicit top-sym s ctx)]
        [else 
            (dispatch (lookup binding ctx s) s ctx)]))

(define (expand-id-application-form s ctx)
    (define id (car (syntax-e s)))
    (define binding (resolve id (expand-context-phase ctx)))
    (cond 
        [(not binding)
            (expand-implicit app-sym s ctx)]
        [else 
            (let ([t (lookup binding ctx id)])
                (cond 
                    [(var? t)
                        (expand-implicit app-sym s ctx)]
                    [else 
                        (dispatch t s ctx)]))]))

(define (expand-implicit sym s ctx)
    (define id (datum->syntax s sym))
    (define b (resolve id (expand-context-phase ctx)))
    (define t (and b (lookup b ctx id)))

    (cond 
        [(core-form? t)
            (if (expand-context-only-immediate? ctx)
                s 
                (dispatch t (datum->syntax s (cons sym s) s) ctx))]
        [(transformer? t)
            (dispatch t (datum->syntax s (cons sym s) s) ctx)]
        [else 
            (syntax-violation #f (format "no transformer binding for ~a" sym) s)]))

(define (dispatch t s ctx)
    (cond 
        [(core-form? t)
            (if (expand-context-only-immediate? ctx)
                s 
                ((core-form-expander t) s ctx))]
        [(transformer? t)
            (expand (apply-transformer t s ctx) ctx)]
        [else (syntax-violation #f "illegal use of syntax" s)]))

(define (apply-transformer t s ctx)
    (define intro-scope (new-scope))
    (define intro-s (add-scope s intro-scope))
    (define use-s (maybe-add-use-site-scope intro-s ctx))

    (define transformed-s 
        (let ()
            (define p current-expand-context)
            (define y ctx)
            (define (swap)
                (let ([t (p)])
                    (p y)
                    (set! y t)))
            (dynamic-wind 
                swap 
                (lambda () (t use-s ctx))
                swap)))
    (unless (syntax? transformed-s)
        (syntax-violation #f "transformer did not return syntax" transformed-s))
    (let ([result-s (flip-scope transformed-s intro-scope)])
        (maybe-add-post-expansion-scope result-s ctx)))

(define (maybe-add-use-site-scope s ctx)
    (cond 
        [(expand-context-use-site-scopes ctx)
            (let ()
                (define sc (new-scope))
                (define b (expand-context-use-site-scopes ctx))
                (set-car! b (cons sc (car b)))
                (add-scope s sc))]
        [else s]))

(define (maybe-add-post-expansion-scope s ctx)
    (cond 
        [(expand-context-post-expansion-scopes ctx)
            (add-scope s (expand-context-post-expansion-scopes ctx))]
        [else s]))  

(define (lookup b ctx id)
  "Helper to lookup a binding in an expansion context"
  (binding-lookup b
                  (expand-context-env ctx)
                  (expand-context-namespace ctx)
                  (expand-context-phase ctx)
                  id))


(define (expand-body bodys sc s ctx)
    ;; The outside-edge scope identifies the original content of the
    ;; definition context
    (define outside-sc (new-scope))
    ;; The inside-edge scope identifiers any form that appears (perhaps
    ;; through macro expansion) in the definition context
    (define inside-sc (new-scope))
    (define init-bodys
        (map (lambda (body)
            (add-scope (add-scope (add-scope body sc) outside-sc) inside-sc)) bodys))
    
    (define phase (expand-context-phase ctx))
    ;; Create an expansion context for expanding only immediate macros;
    ;; this partial-expansion phase uncovers macro- and variable
    ;; definitions in the definition context
    (define body-ctx (%make-expand-context 
        (list* outside-sc  ; scopes
               inside-sc 
               (expand-context-scopes ctx))
        (cons '() #f) ; use-site scopes
        (expand-context-module-scopes ctx)
        (expand-context-context ctx)
        (expand-context-phase ctx)
        (expand-context-namespace ctx)
        (expand-context-env ctx)
        #t ; only-immediate?
        inside-sc ; post-expansion-scope
    ))

    (let loop ([body-ctx body-ctx]
               [bodys init-bodys]
               [done-bodys '()]
               [val-binds '()])
        (cond 
            [(null? bodys)
                (finish-expanding-body body-ctx done-bodys val-binds s)]
            [else 
                (let () 
                    (define exp-body (expand (car bodys) body-ctx))
                    (case (core-form-sym exp-body phase)
                        [(begin)
                            ;; splice a begin form
                            (let ([m (match-syntax exp-body '(begin e ...))])
                                (loop body-ctx 
                                      (append (m 'e) (cdr bodys))
                                      done-bodys
                                      val-binds
                                      dups))]
                        [(define)
                            ;; found a variable definition; add bindings, extend
                            ;; the environment, and continue
                            (let* ([m (match-syntax exp-body '(define id rhs))]
                                   [id (remove-use-site-scopes (m 'id) body-ctx)]
                                   [key (add-local-binding! id phase)]
                                   [extended-env (env-extend (expand-context-env body-ctx) key variable)])
                                (loop (%make-expand-context 
                                        (expand-context-scopes body-ctx)
                                        (expand-context-use-site-scopes body-ctx)
                                        (expand-context-module-scopes body-ctx)
                                        (expand-context-context body-ctx)
                                        (expand-context-phase body-ctx)
                                        (expand-context-namespace body-ctx)
                                        extended-env
                                        (expand-context-only-immediate? body-ctx)
                                        (expand-context-post-expansion-scope body-ctx))
                                      (cdr bodys)
                                      '()
                                      (cons (list id (m 'rhs))
                                        (append 
                                            (map 
                                                (lambda (done-body)
                                                    (no-binds done-body s phase))
                                                val-binds)))))]
                        [else 
                            (loop body-ctx 
                                  (cdr bodys)
                                  (cons exp-body done-bodys)
                                  val-binds)]))])))

(define (finish-expanding-body body-ctx done-bodys val-binds s)
    (when (null? done-bodys)
        (syntax-violation #f "body has no expressions" s))
        
    (let () 
        (define s-core-ctx (syntax-shift-phase-level core-stx (expand-context-phase body-ctx)))
        (define finish-ctx (%make-expand-context 
            (append (car (expand-context-use-site-scopes body-ctx))
                   (expand-context-scopes body-ctx))
            #f ; use-site scopes
            (expand-context-use-site-scopes body-ctx)
            (expand-context-module-scopes body-ctx)
            (expand-context-context body-ctx)
            (expand-context-phase body-ctx)
            (expand-context-namespace body-ctx)
            (expand-context-env body-ctx)
            #f ; only-immediate?
            #f))
        (define (finish-bodys)
            (cond 
                [(null? (cdr done-bodys)
                    (expand (car done-bodys) finish-ctx))]
                [else 
                    (datum->syntax 
                        #f 
                        (cons (datum->syntax s-core-ctx 'begin)
                              (map (lambda (body) (expand body finish-ctx)) (reverse done-bodys)))
                        s)]))
        (cond 
            [(null? val-binds)
                (finish-bodys)]
            [else 
                ;; add `letrec` wrapper, finish expanding the right hand sides,
                ;; and then finish the body expression:
                (datum->syntax 
                    #f 
                    (cons (datum->syntax s-core-ctx 'letrec)
                        (list 
                            (map (lambda (vb)
                                (list (datum->syntax #f (car vb) s)
                                      (expand (cadr vb) finish-ctx)))
                               (reverse val-binds))
                            (finish-bodys))))])))
    
