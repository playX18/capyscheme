;; Rendering TreeIL terms as Scheme source.
;;
;; `tree-il->scheme' is the inverse of macro expansion: it walks a TreeIL
;; term and produces the equivalent Scheme datum.  Besides the plain
;; one-to-one mapping of nodes to syntax, the printer also
;;
;;   * folds nested conditionals back into `cond' / `case' forms, and
;;     recognizes a few idempotent simplifications such as
;;     (if (eqv? v 'a) #t (eqv? v 'b))  =>  (memv v '(a b));
;;
;;   * can replace the generated identifiers of a term with readable
;;     names derived from the original source names, stripping the
;;     "-N-M" suffixes that macro expansion attaches and disambiguating
;;     collisions with a ".N" suffix.
;;
;; The printer operates on the unparsed output: sub-terms are first
;; converted to Scheme datums and the resulting structure is then matched
;; and reshaped.  Options are passed as a list of symbols; recognized
;; options are:
;;
;;   denoise-lexicals?       use readable names for lexical variables
;;   strip-numeric-suffixes? same as denoise-lexicals?
;;   use-case?               prefer `case' over `cond' when rebuilding
;;                           chains of equivalence tests

(library (capy compiler tree-il)
  (export

    tree-il->scheme)
  (import (capy compiler tree-il terms)
    (srfi 1)
    (srfi 26)

    (capy)
    (core match))

  (define (tree-il->scheme term . option-arg)
    (define options (if (null? option-arg) '() (car option-arg)))
    (define rename-lexicals?
      (or (memq 'denoise-lexicals? options)
        (memq 'strip-numeric-suffixes? options)))
    (define case-style? (memq 'use-case? options))

    (unless (term? term)
      (error 'tree-il->scheme "not a term" term))

    ;; ------------------------------------------------------------------
    ;; Datum shaping
    ;;
    ;; Helpers that assemble Scheme datums from already-printed pieces.
    ;; They deliberately match against the printed form, not against the
    ;; term structure, so the shaping stays in sync with what was emitted.
    ;; ------------------------------------------------------------------

    (define (atomic? x) (not (or (pair? x) (vector? x))))

    (define (void-form)
      '(if #f #f))

    (define (shape-begin es)
      (match es
        (() (void-form))
        ((e) e)
        (_ `(begin ,@es))))

    (define (shape-begin-body e)
      (match e
        [('begin es ...) es]
        [_ (list e)]))

    (define (shape-if test consequent alternate)
      (match alternate
        [('if #f _) `(if ,test ,consequent)]
        [_ `(if ,test ,consequent ,alternate)]))

    (define (shape-and xs)
      (match xs
        [() #t]
        [(x) x]
        [_ `(and ,@xs)]))

    (define (shape-or xs)
      (match xs
        [() #f]
        [(x) x]
        [_ `(or ,@xs)]))

    ;; Reassemble nested equivalence tests as a single memv lookup.
    (define (condense-test e)
      (match e
        [('if ('eqv? (? atomic? v) ('quote a)) #t ('eqv? v ('quote b)))
          `(memv ,v '(,a ,b))]
        [('if ('eqv? (? atomic? v) ('quote a)) #t ('memv v ('quote (bs ...))))
          `(memv ,v '(,a ,@bs))]
        [('case (? atomic? v)
            ((datum) #t)
            ...
            ('else ('eqv? v ('quote last-datum))))
          `(memv ,v '(,@datum ,last-datum))]
        [_ e]))

    ;; If all the tests of a cond-shaped chain compare the same variable,
    ;; return that variable; otherwise #f.
    (define (case-subject test)
      (match test
        [('memv (? atomic? v) ('quote (datums ...))) v]
        [('eqv? (? atomic? v) ('quote datum)) v]
        [_ #f]))

    ;; The datum list tested by one equivalence test on SUBJECT.
    (define (test-datums subject test)
      (match (cons subject test)
        ((subject 'memv subject ('quote (xs ...)))
          xs)
        ((subject 'eqv? subject ('quote x))
          (list x))
        (_ #f)))

    ;; The tail of a rebuilt cond / case: either the else clause, or a
    ;; chunk that can serve as further case clauses.
    (define (else-tail e)
      (match e
        [('if #f _) '()]
        [('and xs ... x)
          `((,(shape-and xs) ,@(shape-begin-body x))
            (else #f))]
        [_ `((else ,@(shape-begin-body e)))]))

    (define (cond-tail e)
      (match e
        [('cond clauses ...) clauses]
        [_ (else-tail e)]))

    (define (case-tail subject e)
      (match (cons subject e)
        ((subject 'case subject clauses ...)
          clauses)
        ((subject 'if ('memv subject ('quote (xs ...))) consequent . alternate*)
          `((,xs ,@(shape-begin-body consequent))
            ,@(case-tail subject (shape-begin alternate*))))
        ((subject 'if ('eqv? subject ('quote x)) consequent . alternate*)
          `(((,x) ,@(shape-begin-body consequent))
            ,@(case-tail subject (shape-begin alternate*))))
        (_ (else-tail e))))

    ;; Split a clause list into the clauses before an (else . _) clause
    ;; and that trailing else clause, if present.
    (define (split-else-clause clauses)
      (match clauses
        ((cs ... (and c ('else . _))) (values cs (list c)))
        (_ (values clauses '()))))

    (define (shape-cond tests consequents alternate)
      (case (length tests)
        ((0) alternate)
        ((1) (shape-if (car tests) (car consequents) alternate))
        (else
          `(cond
            ,@(map (lambda (test consequent)
                     `(,test ,@(shape-begin-body consequent)))
               tests
               consequents)
            ,@(cond-tail alternate)))))

    (define (shape-cond-or-case tests consequents alternate)
      (if (not case-style?)
        (shape-cond tests consequents alternate)
        (let* ((subject (and (not (null? tests))
                         (case-subject (car tests))))
               (datum-lists (take-while identity
                             (map (cut test-datums subject <>) tests)))
               (n (length datum-lists))
               (tail (case-tail subject
                       (shape-cond
                         (drop tests n)
                         (drop consequents n)
                         alternate))))
          (receive (clauses tail) (split-else-clause tail)
            (let ((n (+ n (length clauses)))
                  (datum-lists (append datum-lists
                                (map car clauses)))
                  (consequents (append consequents
                                (map shape-begin
                                  (map cdr clauses)))))
              (if (< n 2)
                (shape-cond tests consequents alternate)
                `(case ,subject
                  ,@(map cons datum-lists (map shape-begin-body
                                           (take consequents n)))
                  ,@tail)))))))

    ;; ------------------------------------------------------------------
    ;; Lexical renaming
    ;;
    ;; When `rename-lexicals?' is active, each lexical identity is mapped
    ;; to a stable output name: the readable name from the binding site,
    ;; minus the "-N-M" suffix that macro expansion appended, plus a ".N"
    ;; disambiguator when the base name is already taken.  The mapping is
    ;; threaded through the walk as an association list (env), together
    ;; with the set of names already handed out (used).
    ;; ------------------------------------------------------------------

    (define (digit-run? s start end)
      (let loop ([i start])
        (cond
          [(= i end) (< start i)]
          [(char-numeric? (string-ref s i)) (loop (+ i 1))]
          [else #f])))

    ;; Position of the generated "-N-M" suffix in a name, if any.  The
    ;; suffix must consist of two dash-separated digit runs so that plain
    ;; user names ending in "-1" are left alone.
    (define (generated-tail s)
      (let loop ([end (string-length s)] [segments 0])
        (let scan ([i (- end 1)])
          (cond
            [(< i 0) #f]
            [(char=? (string-ref s i) #\-)
              (if (digit-run? s (+ i 1) end)
                (if (= segments 1)
                  i
                  (loop i (+ segments 1)))
                #f)]
            [else (scan (- i 1))]))))

    (define (base-symbol name)
      (let* ([s (symbol->string name)]
             [start (generated-tail s)]
             [base (if start (substring s 0 start) s)])
        (if (= (string-length base) 0)
          name
          (string->symbol base))))

    ;; The name a lexical identity would like to be called.
    (define (name-base identity readable-name)
      (let ([datum (if (syntax? readable-name)
                    (syntax-expression readable-name)
                    readable-name)])
        (base-symbol
          (cond
            [(symbol? datum) datum]
            [(symbol? identity) identity]
            [else 'lexical]))))

    (define (name-in-use? name used)
      (memq name used))

    (define (suffixed-name base n)
      (string->symbol
        (string-append
          (symbol->string base)
          "."
          (number->string n))))

    (define (unused-name base used)
      (if (not (name-in-use? base used))
        base
        (let loop ([n 1])
          (let ([candidate (suffixed-name base n)])
            (if (name-in-use? candidate used)
              (loop (+ n 1))
              candidate)))))

    ;; The printed name for an identity in ENV, or FALLBACK when the
    ;; renaming is off or the identity was never bound here.
    (define (alias-for identity env fallback)
      (let ([entry (and rename-lexicals? (assq identity env))])
        (if entry (cdr entry) fallback)))

    ;; Allocate (or reuse) the output name for one identity.  Returns the
    ;; name, the extended environment and the extended used-set.
    (define (assign-alias identity readable-name env used)
      (if (not rename-lexicals?)
        (values readable-name env used)
        (let ([entry (assq identity env)])
          (if entry
            (values (cdr entry) env used)
            (let* ([base (name-base identity readable-name)]
                   [alias (unused-name base used)])
              (values alias
                (cons (cons identity alias) env)
                (cons alias used)))))))

    (define (assign-aliases identities readable-names env used)
      (let loop ([identities identities]
                 [readable-names readable-names]
                 [aliases '()]
                 [env env]
                 [used used])
        (if (null? identities)
          (values (reverse aliases) env used)
          (receive (alias env used)
            (assign-alias (car identities) (car readable-names) env used)
            (loop (cdr identities)
              (cdr readable-names)
              (cons alias aliases)
              env
              used)))))

    ;; A single (rest) formal gets the readable name directly.
    (define (single-formal-name readable-names)
      (if (pair? readable-names)
        (car readable-names)
        readable-names))

    ;; Allocate names for a formal parameter list, which may be empty, a
    ;; proper list, or a single rest parameter.
    (define (assign-formals identities readable-names env used)
      (cond
        [(null? identities)
          (values '() env used)]
        [(pair? identities)
          (receive (alias env used)
            (assign-alias (car identities) (car readable-names) env used)
            (receive (tail env used)
              (assign-formals (cdr identities) (cdr readable-names) env used)
              (values (cons alias tail) env used)))]
        [else
          (assign-alias identities
            (single-formal-name readable-names)
            env
            used)]))

    ;; let*: each value is printed in the environment extended by the
    ;; preceding bindings, so the output mirrors the sequential scoping.
    (define (assign-let*-bindings identities readable-names expressions env used)
      (let recur ([identities identities]
                  [readable-names readable-names]
                  [expressions expressions]
                  [aliases '()]
                  [converted '()]
                  [env env]
                  [used used])
        (if (null? identities)
          (values (reverse aliases) (reverse converted) env used)
          (let ([expression (emit (car expressions) env used)])
            (receive (alias env used)
              (assign-alias (car identities) (car readable-names) env used)
              (recur (cdr identities)
                (cdr readable-names)
                (cdr expressions)
                (cons alias aliases)
                (cons expression converted)
                env
                used))))))

    ;; ------------------------------------------------------------------
    ;; The walk
    ;; ------------------------------------------------------------------

    (define (binding-pair name init)
      `(,name ,init))

    (define (emit-let-form style ids lhs rhs body env used)
      (if (not rename-lexicals?)
        ;; without renaming, the identities themselves are the names
        (let ([names lhs]
              [rhs* (map (lambda (x) (emit x env used)) rhs)]
              [body* (emit body env used)])
          `(,style ,(map binding-pair names rhs*) ,body*))
        (cond
          [(eq? style 'let*)
            (receive (aliases rhs* body-env body-used)
              (assign-let*-bindings lhs ids rhs env used)
              (let ([body* (emit body body-env body-used)])
                `(let* ,(map binding-pair aliases rhs*) ,body*)))]
          [(memq style '(letrec letrec*))
            ;; recursive bindings: values see the extended environment
            (receive (aliases body-env body-used)
              (assign-aliases lhs ids env used)
              (let ([rhs* (map (lambda (x) (emit x body-env body-used)) rhs)]
                    [body* (emit body body-env body-used)])
                `(,style ,(map binding-pair aliases rhs*) ,body*)))]
          [else
            (let ([rhs* (map (lambda (x) (emit x env used)) rhs)])
              (receive (aliases body-env body-used)
                (assign-aliases lhs ids env used)
                (let ([body* (emit body body-env body-used)])
                  `(,style ,(map binding-pair aliases rhs*) ,body*))))])))

    (define (emit term env used)
      (cond
        [(constant? term)
          `(quote ,(constant-value term))]
        [(void? term)
          (void-form)]
        [(lref? term)
          (alias-for (lref-sym term) env (lref-sym term))]
        [(lset? term)
          `(set! ,(alias-for (lset-sym term) env (lset-name term))
            ,(emit (lset-value term) env used))]
        [(module-ref? term)
          `(,(if (module-ref-public? term) '@ '@@)
            ,(module-ref-module term)
            ,(module-ref-name term))]
        [(module-set? term)
          `(set! (,(if (module-set-public? term) '@ '@@)
                  ,(module-set-module term)
                  ,(module-set-name term))
            ,(emit (module-set-value term) env used))]
        [(toplevel-ref? term)
          (toplevel-ref-name term)]
        [(toplevel-set? term)
          `(set! ,(toplevel-set-name term)
            ,(emit (toplevel-set-value term) env used))]
        [(toplevel-define? term)
          `(define ,(toplevel-define-name term)
            ,(emit (toplevel-define-value term) env used))]
        [(if? term)
          (match `(if ,(condense-test (emit (if-test term) env used))
                   ,(emit (if-then term) env used)
                   ,(emit (if-else term) env used))
            [('if test ('if ('and xs ...) consequent))
              (shape-if (shape-and (cons test xs))
                consequent
                (void-form))]
            [('if test1 ('if test2 consequent))
              (shape-if (shape-and (cons test1 test2))
                consequent
                (void-form))]
            [('if (? atomic? x) x ('or ys ...))
              `(or ,x ,@ys)]
            [('if (? atomic? x) x y)
              `(or ,x ,y)]
            [('if test consequent)
              `(if ,test ,consequent)]
            [('if test ('and xs ...) #f)
              `(and ,test ,@xs)]
            [('if test consequent #f)
              `(and ,test ,consequent)]
            [('if test1 consequent1
                ('if test2 consequent2 . alternate*))
              (shape-cond-or-case
                (list test1 test2)
                (list consequent1 consequent2)
                (shape-begin alternate*))]
            [('if test consequent ('cond clauses ...))
              `(cond (,test ,@(shape-begin-body consequent))
                ,@clauses)]
            [('if ('memv (? atomic? v) ('quote (xs ...))) consequent
                ('case v clauses ...))
              `(case ,v (,xs ,@(shape-begin-body consequent))
                ,@clauses)]
            [('if ('eqv? (? atomic? v) ('quote x)) consequent
                ('case v clauses ...))
              `(case ,v ((,x) ,@(shape-begin-body consequent))
                ,@clauses)]
            [e e])]
        [(let? term)
          (emit-let-form (let-style term)
            (let-ids term)
            (let-lhs term)
            (let-rhs term)
            (let-body term)
            env
            used)]
        [(receive? term)
          (let ([producer (emit (receive-producer term) env used)])
            (if (not rename-lexicals?)
              (let ([vars (receive-vars term)]
                    [consumer (emit (receive-consumer term) env used)])
                `(receive
                  ,vars
                  ,producer
                  ,consumer))
              (let ([identities (receive-vars term)]
                    [readable-names (receive-ids term)])
                (receive (aliases consumer-env consumer-used)
                  (assign-formals identities readable-names env used)
                  (let ([consumer (emit (receive-consumer term) consumer-env consumer-used)])
                    `(receive
                      ,aliases
                      ,producer
                      ,consumer))))))]
        [(fix? term)
          (if (not rename-lexicals?)
            (let ([names (fix-ids term)]
                  [rhs* (map (lambda (x) (emit x env used)) (fix-rhs term))]
                  [body* (emit (fix-body term) env used)])
              `(fix ,names
                ,rhs*
                ,body*))
            (let ([identities (fix-lhs term)]
                  [readable-names (fix-ids term)])
              ;; like letrec, the values see the extended environment
              (receive (aliases fix-env fix-used)
                (assign-aliases identities readable-names env used)
                (let ([rhs* (map (lambda (x) (emit x fix-env fix-used)) (fix-rhs term))]
                      [body* (emit (fix-body term) fix-env fix-used)])
                  `(fix ,aliases
                    ,rhs*
                    ,body*)))))]
        [(application? term)
          (let ([operator (emit (application-operator term) env used)]
                [operands (map (lambda (x) (emit x env used)) (application-operands term))])
            `(,operator ,@operands))]
        [(primcall? term)
          (let ([prim (primcall-prim term)]
                [args (map (lambda (x) (emit x env used)) (primcall-args term))])
            `(,prim ,@args))]
        [(primref? term)
          (primref-prim term)]
        [(proc? term)
          (if (not rename-lexicals?)
            (let ([args (proc-args term)]
                  [body (emit (proc-body term) env used)])
              `(lambda ,args
                ,body))
            (let ([identities (proc-args term)]
                  [readable-names (proc-ids term)])
              (receive (aliases body-env body-used)
                (assign-formals identities readable-names env used)
                (let ([body (emit (proc-body term) body-env body-used)])
                  `(lambda ,aliases
                    ,body)))))]
        [(values? term)
          (let ([vals (map (lambda (x) (emit x env used)) (values-values term))])
            `(values ,@vals))]
        [(sequence? term)
          (let ([head (emit (sequence-head term) env used)]
                [tail (emit (sequence-tail term) env used)])
            `(begin ,head ,tail))]
        [(wcm? term)
          (let ([mark (emit (wcm-mark term) env used)]
                [result (emit (wcm-result term) env used)])
            `(with-continuation-mark ,(wcm-key term) ,mark ,result))]))

    (emit term '() '())))