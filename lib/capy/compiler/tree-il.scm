(library (capy compiler tree-il)
  (export

    tree-il->scheme)
  (import (capy compiler tree-il terms)
    (capy compiler tree-il fold)
    (srfi 1)
    (srfi 26)

    (capy)
    (core match))

  (define (tree-il->scheme t . ops?)
    (define ops (if (null? ops?) '() (car ops?)))
    (define denoise-lexicals?
      (or (memq 'denoise-lexicals? ops)
          (memq 'strip-numeric-suffixes? ops)))
    (define use-case? (memq 'use-case? ops))

    (define (atom? x) (not (or (pair? x) (vector? x))))
    (define (const x) (lambda (_) x))

    (define (simplify-test e)
      (match e
        (('if ('eqv? (? atom? v) ('quote a)) #t ('eqv? v ('quote b)))
          `(memv ,v '(,a ,b)))
        (('if ('eqv? (? atom? v) ('quote a)) #t ('memv v ('quote (bs ...))))
          `(memv ,v '(,a ,@bs)))
        (('case (? atom? v)
            ((datum) #t)
            ...
            ('else ('eqv? v ('quote last-datum))))
          `(memv ,v '(,@datum ,last-datum)))
        (_ e)))

    (define (build-and xs)
      (match xs
        [() #t]
        [(x) x]
        [_ `(and ,@xs)]))

    (define (build-or xs)
      (match xs
        [() #f]
        [(x) x]
        [_ `(or ,@xs)]))

    (define (build-begin-body e)
      (match e
        [('begin es ...) es]
        [_ (list e)]))

    (define (case-test-var test)
      (match test
        (('memv (? atom? v) ('quote (datums ...)))
          v)
        (('eqv? (? atom? v) ('quote datum))
          v)
        (_ #f)))

    (define (test->datums v test)
      (match (cons v test)
        ((v 'memv v ('quote (xs ...)))
          xs)
        ((v 'eqv? v ('quote x))
          (list x))
        (_ #f)))

    (define (build-else-tail e)
      (match e
        [('if #f _) '()]
        [('and xs ... x) `((,(build-and xs) ,@(build-begin-body x))
                           (else #f))]
        [_ `((else ,@(build-begin-body e)))]))

    (define (build-cond-else-tail e)
      (match e
        [('cond clauses ...) clauses]
        [_ (build-else-tail e)]))

    (define (build-case-else-tail v e)
      (match (cons v e)
        ((v 'case v clauses ...)
          clauses)
        ((v 'if ('memv v ('quote (xs ...))) consequent . alternate*)
          `((,xs ,@(build-begin-body consequent))
            ,@(build-case-else-tail v (build-begin alternate*))))
        ((v 'if ('eqv? v ('quote x)) consequent . alternate*)
          `(((,x) ,@(build-begin-body consequent))
            ,@(build-case-else-tail v (build-begin alternate*))))
        (_ (build-else-tail e))))

    (define (clauses+tail clauses)
      (match clauses
        ((cs ... (and c ('else . _))) (values cs (list c)))
        (_ (values clauses '()))))

    (define (build-if test consequent alternate)
      (match alternate
        (('if #f _) `(if ,test ,consequent))
        (_ `(if ,test ,consequent ,alternate))))

    (define (build-cond tests consequents alternate)
      (case (length tests)
        ((0) alternate)
        ((1) (build-if (car tests) (car consequents) alternate))
        (else `(cond ,@(map (lambda (test consequent)
                             `(,test ,@(build-begin-body consequent)))
                        tests
                        consequents)
                ,@(build-cond-else-tail alternate)))))

    (define (build-cond-or-case tests consequents alternate)
      (if (not use-case?)
        (build-cond tests consequents alternate)
        (let* ((v (and (not (null? tests))
                   (case-test-var (car tests))))
               (datum-lists (take-while identity
                             (map (cut test->datums v <>)
                               tests)))
               (n (length datum-lists))
               (tail (build-case-else-tail v (build-cond
                                              (drop tests n)
                                              (drop consequents n)
                                              alternate))))
          (receive (clauses tail) (clauses+tail tail)
            (let ((n (+ n (length clauses)))
                  (datum-lists (append datum-lists
                                (map car clauses)))
                  (consequents (append consequents
                                (map build-begin
                                  (map cdr clauses)))))
              (if (< n 2)
                (build-cond tests consequents alternate)
                `(case ,v
                  ,@(map cons datum-lists (map build-begin-body
                                           (take consequents n)))
                  ,@tail)))))))

    (define (build-begin es)
      (match es
        (() (build-void))
        ((e) e)
        (_ `(begin ,@es))))

    (define (build-void)
      '(if #f #f))

    (define (string-all-digits? s start end)
      (let loop ((i start))
        (cond
          [(= i end) (< start i)]
          [(char-numeric? (string-ref s i)) (loop (+ i 1))]
          [else #f])))

    (define (generated-suffix-start s)
      (let loop ((end (string-length s)) (segments 0))
        (let scan ((i (- end 1)))
          (cond
            [(< i 0) #f]
            [(char=? (string-ref s i) #\-)
              (if (string-all-digits? s (+ i 1) end)
                (if (= segments 1)
                  i
                  (loop i (+ segments 1)))
                #f)]
            [else (scan (- i 1))]))))

    (define (clean-symbol name)
      (let* ((s (symbol->string name))
             (start (generated-suffix-start s))
             (base (if start (substring s 0 start) s)))
        (if (= (string-length base) 0)
          name
          (string->symbol base))))

    (define (readable-name->symbol identity readable-name)
      (let ((datum (if (syntax? readable-name)
                     (syntax-expression readable-name)
                     readable-name)))
        (clean-symbol
          (cond
            [(symbol? datum) datum]
            [(symbol? identity) identity]
            [else 'lexical]))))

    (define (alias-used? alias used)
      (memq alias used))

    (define (numbered-alias base n)
      (string->symbol
        (string-append
          (symbol->string base)
          "."
          (number->string n))))

    (define (fresh-alias base used)
      (if (not (alias-used? base used))
        base
        (let loop ((n 1))
          (let ((candidate (numbered-alias base n)))
            (if (alias-used? candidate used)
              (loop (+ n 1))
              candidate)))))

    (define (lookup-alias identity env fallback)
      (let ((entry (and denoise-lexicals? (assq identity env))))
        (if entry (cdr entry) fallback)))

    (define (allocate-alias identity readable-name env used)
      (if (not denoise-lexicals?)
        (values readable-name env used)
        (let ((entry (assq identity env)))
          (if entry
            (values (cdr entry) env used)
            (let* ((base (readable-name->symbol identity readable-name))
                   (alias (fresh-alias base used)))
              (values alias
                      (cons (cons identity alias) env)
                      (cons alias used)))))))

    (define (allocate-aliases identities readable-names env used)
      (let loop ((identities identities)
                 (readable-names readable-names)
                 (aliases '())
                 (env env)
                 (used used))
        (if (null? identities)
          (values (reverse aliases) env used)
          (receive (alias env used)
            (allocate-alias (car identities) (car readable-names) env used)
            (loop (cdr identities)
                  (cdr readable-names)
                  (cons alias aliases)
                  env
                  used)))))

    (define (formal-readable-name readable-names)
      (if (pair? readable-names)
        (car readable-names)
        readable-names))

    (define (allocate-formals identities readable-names env used)
      (cond
        [(null? identities)
          (values '() env used)]
        [(pair? identities)
          (receive (alias env used)
            (allocate-alias (car identities) (car readable-names) env used)
            (receive (tail env used)
              (allocate-formals (cdr identities) (cdr readable-names) env used)
              (values (cons alias tail) env used)))]
        [else
          (allocate-alias identities
                          (formal-readable-name readable-names)
                          env
                          used)]))

    (define (convert-let*-bindings identities readable-names expressions env used)
      (let recur ((identities identities)
                  (readable-names readable-names)
                  (expressions expressions)
                  (aliases '())
                  (converted '())
                  (env env)
                  (used used))
        (if (null? identities)
          (values (reverse aliases) (reverse converted) env used)
          (let ((expression (loop (car expressions) env used)))
            (receive (alias env used)
              (allocate-alias (car identities) (car readable-names) env used)
              (recur (cdr identities)
                     (cdr readable-names)
                     (cdr expressions)
                     (cons alias aliases)
                     (cons expression converted)
                     env
                     used))))))

    (unless (term? t)
      (error 'tree-il->scheme "not a term" t))
    (define (loop t env used)
      (cond
        [(constant? t)
          `(quote ,(constant-value t))]
        [(void? t)
          (build-void)]
        [(lref? t)
          (lookup-alias (lref-sym t) env (lref-sym t))]
        [(lset? t)
          `(set! ,(lookup-alias (lset-sym t) env (lset-name t))
            ,(loop (lset-value t) env used))]
        [(module-ref? t)
          (define m (if (module-ref-public? t)
                     '@
                     '@@))
          `(,m ,(module-ref-module t) ,(module-ref-name t))]
        [(module-set? t)
          (define m (if (module-set-public? t)
                     '@
                     '@@))
          `(set! (,m ,(module-set-module t) ,(module-set-name t))
            ,(loop (module-set-value t) env used))]
        [(toplevel-ref? t)
          (toplevel-ref-name t)]
        [(toplevel-set? t)
          `(set! ,(toplevel-set-name t)
            ,(loop (toplevel-set-value t) env used))]
        [(toplevel-define? t)
          `(define ,(toplevel-define-name t)
            ,(loop (toplevel-define-value t) env used))]
        [(if? t)
          (match `(if ,(simplify-test (loop (if-test t) env used))
                   ,(loop (if-then t) env used)
                   ,(loop (if-else t) env used))
            [('if test ('if ('and xs ...) consequent))
              (build-if (build-and (cons test xs))
                consequent
                (build-void))]
            [('if test1 ('if test2 consequent))
              (build-if (build-and (cons test1 test2))
                consequent
                (build-void))]
            [('if (? atom? x) x ('or ys ...))
              `(or ,x ,@ys)]
            [('if (? atom? x) x y)
              `(or ,x ,y)]
            [('if test consequent)
              `(if ,test ,consequent)]
            [('if test ('and xs ...) #f)
              `(and ,test ,@xs)]
            [('if test consequent #f)
              `(and ,test ,consequent)]
            [('if test1 consequent1
                ('if test2 consequent2 . alternate*))
              (build-cond-or-case
                (list test1 test2)
                (list consequent1 consequent2)
                (build-begin alternate*))]
            [('if test consequent ('cond clauses ...))
              `(cond (,test ,@(build-begin-body consequent))
                ,@clauses)]
            [('if ('memv (? atom? v) ('quote (xs ...))) consequent
                ('case v clauses ...))
              `(case ,v (,xs ,@(build-begin-body consequent))
                ,@clauses)]
            [('if ('eqv? (? atom? v) ('quote x)) consequent
                ('case v clauses ...))
              `(case ,v ((,x) ,@(build-begin-body consequent))
                ,@clauses)]
            [e e])]
        [(let? t)
          (define style (let-style t))
          (if (not denoise-lexicals?)
            (let ((ids (let-lhs t))
                  (rhs (map (lambda (x) (loop x env used)) (let-rhs t)))
                  (body (loop (let-body t) env used)))
              `(,style
                ,(map (lambda (id rhs) `(,id ,rhs)) ids rhs)
                ,body))
            (let ((readable-names (let-ids t))
                  (identities (let-lhs t)))
              (cond
                [(eq? style 'let)
                  (define rhs (map (lambda (x) (loop x env used)) (let-rhs t)))
                  (receive (aliases body-env body-used)
                    (allocate-aliases identities readable-names env used)
                    (define body (loop (let-body t) body-env body-used))
                    `(,style
                      ,(map (lambda (id rhs) `(,id ,rhs)) aliases rhs)
                      ,body))]
                [(eq? style 'let*)
                  (receive (aliases rhs body-env body-used)
                    (convert-let*-bindings identities readable-names (let-rhs t) env used)
                    (define body (loop (let-body t) body-env body-used))
                    `(,style
                      ,(map (lambda (id rhs) `(,id ,rhs)) aliases rhs)
                      ,body))]
                [(memq style '(letrec letrec*))
                  (receive (aliases body-env body-used)
                    (allocate-aliases identities readable-names env used)
                    (define rhs
                      (map (lambda (x) (loop x body-env body-used)) (let-rhs t)))
                    (define body (loop (let-body t) body-env body-used))
                    `(,style
                      ,(map (lambda (id rhs) `(,id ,rhs)) aliases rhs)
                      ,body))]
                [else
                  (define rhs (map (lambda (x) (loop x env used)) (let-rhs t)))
                  (receive (aliases body-env body-used)
                    (allocate-aliases identities readable-names env used)
                    (define body (loop (let-body t) body-env body-used))
                    `(,style
                      ,(map (lambda (id rhs) `(,id ,rhs)) aliases rhs)
                      ,body))])))]
        [(receive? t)
          (define producer (loop (receive-producer t) env used))
          (if (not denoise-lexicals?)
            (let ((vars (receive-vars t))
                  (consumer (loop (receive-consumer t) env used)))
            `(receive
                ,vars
                ,producer
                ,consumer))
            (let ((readable-names (receive-ids t))
                  (identities (receive-vars t)))
              (receive (aliases consumer-env consumer-used)
                (allocate-formals identities readable-names env used)
                (define consumer (loop (receive-consumer t) consumer-env consumer-used))
                `(receive
                  ,aliases
                  ,producer
                  ,consumer))))]
        [(fix? t)
          (if (not denoise-lexicals?)
            (let ((ids (fix-ids t))
                  (rhs (map (lambda (x) (loop x env used)) (fix-rhs t)))
                  (body (loop (fix-body t) env used)))
              `(fix ,ids
                ,rhs
                ,body))
            (let ((identities (fix-lhs t))
                  (readable-names (fix-ids t)))
              (receive (aliases fix-env fix-used)
                (allocate-aliases identities readable-names env used)
                (define rhs (map (lambda (x) (loop x fix-env fix-used)) (fix-rhs t)))
                (define body (loop (fix-body t) fix-env fix-used))
                `(fix ,aliases
                  ,rhs
                  ,body))))]
        [(application? t)
          (define operator (loop (application-operator t) env used))
          (define operands
            (map (lambda (x) (loop x env used)) (application-operands t)))
          `(,operator ,@operands)]
        [(primcall? t)
          (define prim (primcall-prim t))
          (define args (map (lambda (x) (loop x env used)) (primcall-args t)))
          `(,prim ,@args)]
        [(primref? t)
          (define prim (primref-prim t))
          prim]
        [(proc? t)
          (if (not denoise-lexicals?)
            (let ((args (proc-args t))
                  (body (loop (proc-body t) env used)))
              `(lambda ,args
                ,body))
            (let ((identities (proc-args t))
                  (readable-names (proc-ids t)))
              (receive (aliases body-env body-used)
                (allocate-formals identities readable-names env used)
                (define body (loop (proc-body t) body-env body-used))
                `(lambda ,aliases
                  ,body))))]
        [(values? t)
          (define vals (map (lambda (x) (loop x env used)) (values-values t)))
          `(values ,@vals)]
        [(sequence? t)
          (define head (loop (sequence-head t) env used))
          (define tail (loop (sequence-tail t) env used))
          `(begin ,head ,tail)]
        [(wcm? t)
          (define key (wcm-key t))
          (define mark (loop (wcm-mark t) env used))
          (define result (loop (wcm-result t) env used))
          `(with-continuation-mark ,key ,mark ,result)]))

    (loop t '() '())))
