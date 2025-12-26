(library (capy compiler tree-il)
    (export 
        &term term? term-src 
        &lref lref-name lref-sym make-lref 
        &lset lset-name lset-sym make-lset
        &module-ref module-ref-module module-ref-name module-ref-public? make-module-ref
        &module-set module-set-module module-set-name module-set-value module-set-public? make-module-set
        &toplevel-ref toplevel-ref-mod toplevel-ref-name make-toplevel-ref
        &toplevel-set toplevel-set-mod toplevel-set-name toplevel-set-value make-toplevel
        &if if-test if-then if-else make-if 
        &let let-style let-ids let-lhs let-rhs let-body make-let let?
        &fix fix-ids fix-lhs fix-rhs fix-body make-fix fix? 
        &receive receive-ids receive-vars receive-producer receive-consumer make-receive 
        &application
        application-operator application-operands make-application
        application?

        &primcall 
        primcall? primcall-prim primcall-args make-primcall

        &primref 
        primref? primref-prim make-primref

        &constant
        constant? 
        constant-value 
        make-constant
        
        &void 
        void?
        make-void

        &proc 
        proc-args 
        proc-body
        proc-meta
        proc-ids
        make-proc
        proc?

        &values
        values?
        make-values
        values-values

        &sequence
        sequence-head
        sequence-tail
        make-sequence 

        &wcm 
        wcm-key
        wcm-mark
        wcm-result
        make-wcm
        wcm?
        
        tree-il->scheme
        pre-order 
        post-order
        pre-post-order)
    (import (capy)
            (srfi 1)
            (srfi 26)
            (core match))
    
  (define (tree-il->scheme t . ops?)
    (define ops (if (null? ops?) '() (car ops?)))
    (define strip-numeric-suffixes? (memq 'strip-numeric-suffixes? ops))
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
          ((datum) #t) ...
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
                            tests consequents)
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

    (unless (term? t)
      (error 'tree-il->scheme "not a term" t))
    (define (loop t)
      (cond 
        [(constant? t)
          `(quote ,(constant-value t))]
        [(void? t)
          (build-void)]
        [(lref? t)
          (lref-sym t)]
        [(lset? t)
          `(set! ,(lset-name t) ,(loop (lset-value t)))]
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
                ,(loop (module-set-value t)))]
        [(toplevel-ref? t)
          (toplevel-ref-name t)]
        [(toplevel-set? t)
          `(set! ,(toplevel-set-name t) ,(loop (toplevel-set-value t)))]
        [(toplevel-define? t)
          `(define ,(toplevel-define-name t) 
            ,(loop (toplevel-define-value t)))]
        [(if? t)
          (match `(if ,(simplify-test (loop (if-test t)))
                      ,(loop (if-then t))
                      ,(loop (if-else t)))
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
          (define ids (let-lhs t))
          (define rhs (map loop (let-rhs t)))
          (define body (loop (let-body t)))
          `(,style
            ,(map (lambda (id rhs) `(,id ,rhs) ) ids rhs)
            ,body)]
        [(receive? t)
          (define ids (receive-ids t))
          (define vars (map loop (receive-vars t)))
          (define producer (loop (receive-producer t)))
          (define consumer (loop (receive-consumer t)))
          `(receive 
                    ,vars
                    ,producer
                    ,consumer)]
        [(fix? t)
          (define ids (fix-ids t))
          (define rhs (map loop (fix-rhs t)))
          (define body (loop (fix-body t)))
          `(fix ,ids
                ,rhs
                ,body)]
        [(application? t)
          (define operator (loop (application-operator t)))
          (define operands (map loop (application-operands t)))
          `(,operator ,@operands)]
        [(primcall? t)
          (define prim (primcall-prim t))
          (define args (map loop (primcall-args t)))
          `(,prim ,@args)]
        [(primref? t)
          (define prim (primref-prim t))
          prim]
        [(proc? t)
          (define args (proc-args t))
          (define body (loop (proc-body t)))
          `(lambda ,args
            ,body)]
        [(values? t)
          (define vals (map loop (values-values t)))
          `(values ,@vals)]
        [(sequence? t)
          (define head (loop (sequence-head t)))
          (define tail (loop (sequence-tail t)))
          `(begin ,head ,tail)]
        [(wcm? t)
          (define key (wcm-key t))
          (define mark (loop (wcm-mark t)))
          (define result (loop (wcm-result t)))
          `(with-continuation-mark ,key ,mark ,result)]))
  
    (loop t))

  (define (pre-post-order pre post x)
    (define (elts-eq? a b)
      (or (null? a)
          (and (eq? (car a) (car b))
               (elts-eq? (cdr a) (cdr b)))))
  
    (let loop ([x x])
      
      (post (let ([x (pre x)])
        (cond 
          [(or (void? x)
              (constant? x)
              (lref? x)
              (primref? x)
              (module-ref? x)
              (toplevel-ref? x)
            x)]
          [(lset? x)
            (define exp* (loop (lset-value x)))
            (if (eq? exp* (lset-value x))
              x 
              (make-lset (term-src x) (lset-name x) (lset-sym x) exp*))]
          [(module-set? x)
            (define exp* (loop (module-set-value x)))
            (if (eq? exp* (module-set-value x))
              x 
              (make-module-set (term-src x) (module-set-module x)
                              (module-set-name x)
                              (module-set-public? x)
                              exp*))]
                        
          [(toplevel-set? x)
            (define exp* (loop (toplevel-set-value x)))
            
            (if (eq? exp* (toplevel-set-value x))
              x 
              (make-toplevel-set (term-src x)
                                (toplevel-set-mod x)
                                (toplevel-set-name x)
                                exp*))]
          [(toplevel-define? x)
            (define exp* (loop (toplevel-define-value x)))
            (if (eq? exp* (toplevel-define-value x))
              x 
              (make-toplevel-define (term-src x)
                                    (toplevel-define-mod x)
                                    (toplevel-define-name x)
                                    exp*))]
          [(if? x)
            (define test* (loop (if-test x)))
            (define then* (loop (if-then x)))
            (define else* (loop (if-else x)))
            (if (and (eq? test* (if-test x))
                    (eq? then* (if-then x))
                    (eq? else* (if-else x)))
              x 
              (make-if (term-src x) test* then* else*))]
          [(let? x)
            (define rhs* (map loop (let-rhs x)))
            (define body* (loop (let-body x)))
            (if (and (elts-eq? rhs* (let-rhs x))
                    (eq? body* (let-body x)))
              x 
              (make-let (term-src x)
                        (let-style x)
                        (let-ids x)
                        (let-lhs x)
                        rhs* 
                        body*))]
          [(fix? x)
            (define rhs* (map loop (fix-rhs x)))
            (define body* (loop (fix-body x)))
            (if (and (elts-eq? rhs* (fix-rhs x))
                    (eq? body* (fix-body x)))
              x 
              (make-fix (term-src x)
                        (fix-ids x)
                        (fix-lhs x)
                        rhs* 
                        body*))]
          [(receive? x)
            (define producer* (loop (receive-producer x)))
            (define consumer* (loop (receive-consumer x)))
            (if (and (eq? producer* (receive-producer x))
                    (eq? consumer* (receive-consumer x)))
              x 
              (make-receive (term-src x)
                            (receive-ids x)
                            (receive-vars x)
                            producer*
                            consumer*))]
          [(application? x)
            (define operator* (loop (application-operator x)))
            (define operands* (map loop (application-operands x)))
            (if (and (eq? operator* (application-operator x))
                    (elts-eq? operands* (application-operands x)))
              x 
              (make-application (term-src x)
                                operator*
                                operands*))]
          [(primcall? x)
            (define args* (map loop (primcall-args x)))
            (if (elts-eq? args* (primcall-args x))
              x 
              (make-primcall (term-src x)
                            (primcall-prim x)
                            args*))]
          [(proc? x)
            (define body* (loop (proc-body x)))
            (if (eq? body* (proc-body x))
              x 
              (make-proc (term-src x)
                        (proc-args x)
                        body*
                        (proc-meta x)
                        (proc-ids x)))]

          [(values? x)
            (define vals* (map loop (values-values x)))
            (if (elts-eq? vals* (values-values x))
              x 
              (make-values (term-src x)
                          vals*))]
          [(sequence? x)
            (define head* (loop (sequence-head x)))
            (define tail* (loop (sequence-tail x)))
            (if (and (eq? head* (sequence-head x))
                    (eq? tail* (sequence-tail x)))
              x 
              (make-sequence (term-src x)
                            head*
                            tail*))]
          [(wcm? x)
            (define mark* (loop (wcm-mark x)))
            (define result* (loop (wcm-result x)))
            (if (and (eq? mark* (wcm-mark x))
                    (eq? result* (wcm-result x)))
              x 
              (make-wcm (term-src x)
                        (wcm-key x)
                        mark*
                        result*))])))))

  (define (post-order f x)
    (pre-post-order (lambda (x) x) f x))
  (define (pre-order f x)
    (pre-post-order f (lambda (x) x) x))
)