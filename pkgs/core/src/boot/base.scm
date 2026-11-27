(|#%require| (boot prim) (boot control) (boot environments) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter) (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder) (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports) (boot console-ports) (boot port-api) (boot print) (boot format) (boot log) (boot match-syntax) (boot units) (boot psyntax-exp) (boot str2num) (boot num2str) (boot reader) (boot eval) (boot time))
(|#%provide| all-defined)

;; The optimizing JIT calls `apply` itself for an `apply` it cannot spread.
(install-runtime-hook! 'apply apply)

(define-syntax syntax-rules
  (lambda (xx)
    (define (expand-clause clause)
      ;; Convert a 'syntax-rules' clause into a 'syntax-case' clause.
      (syntax-case clause (syntax-error)
        ;; If the template is a 'syntax-error' form, use the extended
        ;; internal syntax, which adds the original form as the first
        ;; operand for improved error reporting.
        (((keyword . pattern) (syntax-error message arg ...))
          (string? (syntax->datum #'message))
          #'((dummy . pattern) #'(syntax-error (dummy . pattern) message arg ...)))
        ;; Normal case
        (((keyword . pattern) template)
          #'((dummy . pattern) #'template))
        ;; An empty pattern () is a legal (degenerate) syntax-rules pattern:
        ;; keep it as the empty syntax-case pattern.
        ((() template)
          #'(() #'template))))
    (define (expand-syntax-rules dots keys docstrings clauses)
      ;; CLAUSES is a list of raw (pattern template) syntax objects; each is
      ;; converted individually so that empty patterns are supported.
      (with-syntax
        (((k ...) keys)
          ((docstring ...) docstrings)
          ((clause ...) (map expand-clause clauses))
          ;; Keep the pattern as a syntax object (not a datum) for the
          ;; procedure metadata: raw symbols in macro output are rejected.
          ((pattern ...)
            (map (lambda (cl)
                   (syntax-case cl ()
                     ((p t) #'p)
                     (_ #f)))
              clauses)))
        (with-syntax
          ((form #'(lambda (x)
                    docstring ; optional docstring
                    ...
                    #((macro-type . syntax-rules)
                      (patterns pattern ...)) ; embed patterns as procedure metadata
                    (syntax-case x (k ...)
                     clause
                     ...))))
          (if dots
            (with-syntax ((dots dots))
              #'(with-ellipsis dots form))
            #'form))))
    (syntax-case xx ()
      ((_ (k ...) docstring clause ...)
        (string? (syntax->datum #'docstring))
        (expand-syntax-rules #f #'(k ...) #'(docstring) #'(clause ...)))
      ((_ (k ...) clause ...)
        (expand-syntax-rules #f #'(k ...) #'() #'(clause ...)))
      ((_ dots (k ...) docstring clause ...)
        ;; `if', not `and': `and' is defined below, and a reference to it here would be to
        ;; whatever `and' the expanding process already has.
        (if (identifier? #'dots) (string? (syntax->datum #'docstring)) #f)
        (expand-syntax-rules #'dots #'(k ...) #'(docstring) #'(clause ...)))
      ((_ dots (k ...) clause ...)
        (identifier? #'dots)
        (expand-syntax-rules #'dots #'(k ...) #'() #'(clause ...))))))


(define-syntax ...
  (lambda (x)
    (syntax-violation '... "bad use of '...' syntactic keyword" x x)))(define-syntax _
  (lambda (x)
    (syntax-violation '_ "bad use of '_' syntactic keyword" x x)))

(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [(_ e1 e2 e3 ...)
        #'(let ([tmp e1]) (if tmp tmp (or e2 e3 ...)))]
      [(_ e) #'e]
      [(_) #'#f])))

(define-syntax and
  (lambda (stx)
    (syntax-case stx ()
      [(_ e1 e2 e3 ...)
        #'(if e1 (and e2 e3 ...) #f)]
      [(_ e) #'e]
      [(_) #'#t])))

;;; Chez-style `time` (see boot/time.scm for `$as-time-goes-by`).
(define-syntax time
  (syntax-rules ()
    ((_ e) ($as-time-goes-by 'e (lambda () e)))))

(define-syntax define-syntax-rule
  (lambda (x)
    (syntax-case x ()
      ((_ (name . pattern) template)
        #'(define-syntax name
           (syntax-rules ()
            ((_ . pattern) template))))
      ((_ (name . pattern) docstring template)
        (string? (syntax->datum #'docstring))
        #'(define-syntax name
           (syntax-rules ()
            docstring
            ((_ . pattern) template)))))))

(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      ((_ c1 c2 ...)
        (let f ((c1 (syntax c1))
                (c2* (syntax (c2 ...))))
          (syntax-case c2* ()
            (()
              (syntax-case c1 (else =>)
                ((else e1 e2 ...) (syntax (let () e1 e2 ...)))
                ((e0) (syntax (let ((t e0)) (if t t))))
                ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t)))))
                ((e0 e1 e2 ...) (syntax (if e0 (let () e1 e2 ...))))
                (_ (syntax-violation
                    'cond
                    "Invalid expression"
                    x))))
            ((c2 c3 ...)
              (with-syntax ((rest (f (syntax c2)
                                   (syntax (c3 ...)))))
                (syntax-case c1 (else =>)
                  ((e0) (syntax (let ((t e0)) (if t t rest))))
                  ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                  ((e0 e1 e2 ...) (syntax (if e0 (let () e1 e2 ...) rest)))
                  (_ (syntax-violation
                      'cond
                      "Invalid expression"
                      x)))))))))))

;; Andre van Tonder's case macro has been replaced by the R7RS definition.
;; This is, strictly speaking, an incompatible change to the R6RS semantics,
;; which is of course forbidden, but it won't break anything and most
;; people would regard the R7RS semantics as an improvement.
;; Besides, using the same definition for R6RS as for R7RS is essential
;; for smooth interoperability.

;  (define-syntax case
;    (lambda (x)
;      (syntax-case x ()
;        ((_ e c1 c2 ...)
;         (with-syntax ((body
;                        (let f ((c1 (syntax c1))
;                                (cmore (syntax (c2 ...))))
;                          (if (null? cmore)
;                              (syntax-case c1 (else)
;                                ((else e1 e2 ...)
;                                 (syntax (begin e1 e2 ...)))
;                                (((k ...) e1 e2 ...)
;                                 (syntax (if (memv t '(k ...))
;                                             (begin e1 e2 ...)))))
;                              (with-syntax ((rest (f (car cmore) (cdr cmore))))
;                                (syntax-case c1 ()
;                                  (((k ...) e1 e2 ...)
;                                   (syntax (if (memv t '(k ...))
;                                               (begin e1 e2 ...)
;                                               rest)))))))))
;           (syntax (let ((t e)) body)))))))

;; From R7RS (small) erratum 6, which corrects the defn in R7RS 7.3

(define-syntax case
  (syntax-rules (else =>)

    ((case (key ...)
        clauses
        ...)
      (let ((atom-key (key ...)))
        (case atom-key clauses ...)))

    ((case key
        (else => result))
      (result key))

    ((case key
        (else result1 result2 ...))
      (let () result1 result2 ...))

    ((case key
        ((atoms ...) => result))
      (if (memv key '(atoms ...))
        (result key)))

    ((case key
        ((atoms ...) => result)
        clause
        clauses
        ...)
      (if (memv key '(atoms ...))
        (result key)
        (case key clause clauses ...)))

    ((case key
        ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
        (let () result1 result2 ...)))

    ((case key
        ((atoms ...) result1 result2 ...)
        clause
        clauses
        ...)
      (if (memv key '(atoms ...))
        (let () result1 result2 ...)
        (case key clause clauses ...)))))

(define-syntax =>
  (lambda (x)
    (syntax-violation '=> "Invalid expression" x)))

(define-syntax else
  (lambda (x)
    (syntax-violation 'else "Invalid expression" x)))

;; From Andre van Tonder's expander
(define-syntax quasisyntax
  (lambda (e)

    ;; Expand returns a list of the form
    ;;    [template[t/e, ...] (replacement ...)]
    ;; Here template[t/e ...] denotes the original template
    ;; with unquoted expressions e replaced by fresh
    ;; variables t, followed by the appropriate ellipses
    ;; if e is also spliced.
    ;; The second part of the return value is the list of
    ;; replacements, each of the form (t e) if e is just
    ;; unquoted, or ((t ...) e) if e is also spliced.
    ;; This will be the list of bindings of the resulting
    ;; with-syntax expression.

    (define (expand x level)
      (syntax-case x (quasisyntax unsyntax unsyntax-splicing)
        ((quasisyntax e)
          (with-syntax (((k _) x) ;; original identifier must be copied
                        ((e* reps) (expand (syntax e) (+ level 1))))
            (syntax ((k e*) reps))))
        ((unsyntax e)
          (= level 0)
          (with-syntax (((t) (generate-temporaries '(t))))
            (syntax (t ((t e))))))
        (((unsyntax e ...) . r)
          (= level 0)
          (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                        ((t ...) (generate-temporaries (syntax (e ...)))))
            (syntax ((t ... . r*)
                     ((t e) ... rep ...)))))
        (((unsyntax-splicing e ...) . r)
          (= level 0)
          (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                        ((t ...) (generate-temporaries (syntax (e ...)))))
            (with-syntax ((((t ...) ...) (syntax ((t (... ...)) ...))))
              (syntax ((t ... ... . r*)
                       (((t ...) e) ... rep ...))))))
        ((k . r)
          (and (> level 0)
            (identifier? (syntax k))
            (or (free-identifier=? (syntax k) (syntax unsyntax))
              (free-identifier=? (syntax k) (syntax unsyntax-splicing))))
          (with-syntax (((r* reps) (expand (syntax r) (- level 1))))
            (syntax ((k . r*) reps))))
        ((h . t)
          (with-syntax (((h* (rep1 ...)) (expand (syntax h) level))
                        ((t* (rep2 ...)) (expand (syntax t) level)))
            (syntax ((h* . t*)
                     (rep1 ... rep2 ...)))))
        (#(e ...)
          (with-syntax ((((e* ...) reps)
                          (expand (vector->list (syntax #(e ...))) level)))
            (syntax (#(e* ...) reps))))
        (other
          (syntax (other ())))))

    (syntax-case e ()
      ((_ template)
        (with-syntax (((template* replacements) (expand (syntax template) 0)))
          (syntax
            (with-syntax replacements (syntax template*))))))))

(define-syntax unsyntax
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

(define-syntax unsyntax-splicing
  (lambda (e)
    (syntax-violation 'unsyntax "Invalid expression" e)))

(define-syntax quasiquote
  (let ()
    (define (quasi p lev)
      (syntax-case p (unquote quasiquote)
        ((unquote p)
          (if (= lev 0)
            (syntax ("value" p))
            (quasicons (syntax ("quote" unquote))
              (quasi (syntax (p)) (- lev 1)))))
        ((quasiquote p) (quasicons (syntax ("quote" quasiquote))
                         (quasi (syntax (p)) (+ lev 1))))
        ((p . q)
          (syntax-case (syntax p) (unquote unquote-splicing)
            ((unquote p ...)
              (if (= lev 0)
                (quasilist* (syntax (("value" p) ...))
                  (quasi (syntax q) lev))
                (quasicons
                  (quasicons (syntax ("quote" unquote))
                    (quasi (syntax (p ...)) (- lev 1)))
                  (quasi (syntax q) lev))))
            ((unquote-splicing p ...)
              (if (= lev 0)
                (quasiappend (syntax (("value" p) ...))
                  (quasi (syntax q) lev))
                (quasicons
                  (quasicons (syntax ("quote" unquote-splicing))
                    (quasi (syntax (p ...)) (- lev 1)))
                  (quasi (syntax q) lev))))
            (_ (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))))
        (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
        (p (syntax ("quote" p)))))
    (define (vquasi p lev)
      (syntax-case p ()
        ((p . q)
          (syntax-case (syntax p) (unquote unquote-splicing)
            ((unquote p ...)
              (if (= lev 0)
                (quasilist* (syntax (("value" p) ...))
                  (vquasi (syntax q) lev))
                (quasicons
                  (quasicons (syntax ("quote" unquote))
                    (quasi (syntax (p ...)) (- lev 1)))
                  (vquasi (syntax q) lev))))
            ((unquote-splicing p ...)
              (if (= lev 0)
                (quasiappend (syntax (("value" p) ...))
                  (vquasi (syntax q) lev))
                (quasicons
                  (quasicons
                    (syntax ("quote" unquote-splicing))
                    (quasi (syntax (p ...)) (- lev 1)))
                  (vquasi (syntax q) lev))))
            (_ (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
        (() (syntax ("quote" ())))))
    (define (quasicons x y)
      (with-syntax ((x x) (y y))
        (syntax-case (syntax y) ()
          (("quote" dy)
            (syntax-case (syntax x) ()
              (("quote" dx) (syntax ("quote" (dx . dy))))
              (_ (if (null? (syntax dy))
                  (syntax ("list" x))
                  (syntax ("list*" x y))))))
          (("list" . stuff) (syntax ("list" x . stuff)))
          (("list*" . stuff) (syntax ("list*" x . stuff)))
          (_ (syntax ("list*" x y))))))
    (define (quasiappend x y)
      (syntax-case y ()
        (("quote" ())
          (cond
            ((null? x) (syntax ("quote" ())))
            ((null? (cdr x)) (car x))
            (else (with-syntax (((p ...) x)) (syntax ("append" p ...))))))
        (_
          (cond
            ((null? x) y)
            (else (with-syntax (((p ...) x) (y y))
                   (syntax ("append" p ... y))))))))
    (define (quasilist* x y)
      (let f ((x x))
        (if (null? x)
          y
          (quasicons (car x) (f (cdr x))))))
    (define (quasivector x)
      (syntax-case x ()
        (("quote" (x ...)) (syntax ("quote" #(x ...))))
        (_
          (let f ((y x) (k (lambda (ls)
                            (quasisyntax
                              ("vector" (unsyntax-splicing ls))))))
            (syntax-case y ()
              (("quote" (y ...)) (k (syntax (("quote" y) ...))))
              (("list" y ...) (k (syntax (y ...))))
              (("list*" y ... z)
                (f (syntax z) (lambda (ls) (k (append (syntax (y ...)) ls)))))
              (else (quasisyntax ("list->vector" (unsyntax x)))))))))
    (define (emit x)
      (syntax-case x ()
        (("quote" x) (syntax 'x))
        (("list" x ...)
          (quasisyntax
            (list (unsyntax-splicing (map emit (syntax (x ...)))))))
        ;; could emit list* for 3+ arguments if implementation supports list*
        (("list*" x ... y)
          (let f ((x* (syntax (x ...))))
            (if (null? x*)
              (emit (syntax y))
              (quasisyntax
                (cons (unsyntax (emit (car x*))) (unsyntax (f (cdr x*))))))))
        (("append" x ...)
          (quasisyntax
            (append (unsyntax-splicing (map emit (syntax (x ...)))))))
        (("vector" x ...)
          (quasisyntax
            (vector (unsyntax-splicing (map emit (syntax (x ...)))))))
        (("list->vector" x)
          (quasisyntax (list->vector (unsyntax (emit (syntax x))))))
        (("value" x) (syntax x))))
    (lambda (x)
      (syntax-case x ()
        ;; convert to intermediate language, combining introduced (but not
        ;; unquoted source) quote expressions where possible and choosing
        ;; optimal construction code otherwise, then emit Scheme code
        ;; corresponding to the intermediate language forms.
        ((_ e) (emit (quasi (syntax e) 0)))))))

(define-syntax unquote
  (lambda (e)
    (syntax-violation 'unquote "Invalid expression" e)))

(define-syntax unquote-splicing
  (lambda (e)
    (syntax-violation 'unquote-splicing "Invalid expression" e)))

(eval-when (expand load eval)
  (define (unwrap stx)
    (syntax-case stx ()
      ((a . b) (cons (unwrap #'a) (unwrap #'b)))
      (#(a ...) (list->vector (map unwrap (vector->list #'(a ...)))))
      (id (identifier? #'id) #'id)
      (_ (syntax->datum stx))))

  (define (rewrap ctx expr)
    (let rewrap* ((e expr))
      (cond
        ((pair? e) (cons (rewrap* (car e)) (rewrap* (cdr e))))
        ((vector? e) (vector-map rewrap* e))
        ((identifier? e) e)
        (else (datum->syntax ctx e #f)))))

  (define (make-compare ctx)
    (lambda (x y)
      (free-identifier=? (rewrap ctx x) (rewrap ctx y))))

  (define (make-rename ctx)
    (lambda (x)
      (datum->syntax ctx x (syntax-source ctx)))))

(define-syntax er-macro-transformer
  (lambda (stx)
    (syntax-case stx ()
      ((k proc-expr)
        #'(let ((proc proc-expr))
           (lambda (stx)
            (syntax-case stx ()
             ((m . _)
              (rewrap #'m
               (proc (unwrap stx)
                (make-rename #'k)
                (make-compare #'m)))))))))))

(define-syntax ir-macro-transformer
  (lambda (stx)
    (syntax-case stx ()
      ((k proc-expr)
        #'(let ((proc proc-expr))
           (lambda (stx)
            (syntax-case stx ()
             ((m . _)
              (rewrap #'k
               (proc (unwrap stx)
                (make-rename #'m)
                (make-compare #'m)))))))))))

(define-syntax define-values
  (lambda (orig-form)
    (define (expand-define-values formals vars expr)
      ;; XXX Work around the lack of hygienic toplevel identifiers
      (with-syntax (((dummy) (generate-temporaries '(dummy)))
                    (formals formals)
                    ((var ...) vars)
                    ((index ...)
                      (let loop ([i (length (syntax->datum vars))] [acc '()])
                        (if (= i 0) acc (loop (- i 1) (cons (- i 1) acc)))))
                    (expr expr))
        #'(begin
           (define dummy
            (call-with-values (lambda () expr)
             (lambda formals (list var ...))))
           (define var (list-ref dummy index))
           ...)))
    (syntax-case orig-form ()
      ((_ () expr)
        ;; XXX Work around the lack of hygienic top-level identifiers
        (with-syntax (((dummy) (generate-temporaries '(dummy))))
          #`(define dummy
             (call-with-values (lambda () expr)
              (lambda () #f)))))
      ((_ (var) expr)
        (identifier? #'var)
        #`(define var
           (call-with-values (lambda () expr)
            (lambda (v) v))))
      ((_ (var0 ... varn) expr)
        (and-map identifier? #'(var0 ... varn))
        (expand-define-values #'(var0 ... varn) #'(var0 ... varn) #'expr))
      ((_ var expr)
        (identifier? #'var)
        #'(define var
           (call-with-values (lambda () expr)
            list)))
      ((_ (var0 ... . varn) expr)
        (and-map identifier? #'(var0 ... varn))
        (expand-define-values #'(var0 ... . varn) #'(var0 ... varn) #'expr)))))

(define-syntax do
  (lambda (orig-x)
    (syntax-case orig-x ()
      ((_ ((var init . step) ...) (e0 e1 ...) c ...)
        (with-syntax (((step ...)
                        (map (lambda (v s)
                              (syntax-case s ()
                                (() v)
                                ((e) (syntax e))
                                (_ (syntax-violation
                                    'do
                                    "Invalid step"
                                    orig-x
                                    s))))
                          (syntax (var ...))
                          (syntax (step ...)))))
          (syntax-case (syntax (e1 ...)) ()
            (() (syntax (let do ((var init) ...)
                         (if (not e0)
                           (let () c ... (do step ...))))))
            ((e1 e2 ...) (syntax (let do ((var init) ...)
                                  (if e0
                                    (let () e1 e2 ...)
                                    (let () c ... (do step ...))))))))))))

(define-syntax do*
  (lambda (stx)
    "(do* ({(var [init] [step])}*) (test exit-form*) declaration-form*)
    Iteration construct. Each var is initialized sequentially like let* to the 
    value specified by init form. On subsequent iterations, the vars are
    sequentially assigned the value of the step form (if any). The test
    is evaluated before each evaluation of the body forms."
    (syntax-case stx ()
      [(_ ((var init . step) ...) (e0 e1 ...) c ...)
        (with-syntax (((step ...)
                        (map (lambda (v s)
                              (syntax-case s ()
                                [() v]
                                [(e) #'e]
                                [_ (syntax-violation 'do* "invalid step" stx s)]))
                          #'(var ...)
                          #'(step ...))))
          (syntax-case #'(e1 ...) ()
            [() #'(let* ([var init] ...)
                   (let do* ((var var) ...)
                    (if (not e0)
                     (let () c ...
                      (let* ([var step] ...)
                       (do* var ...))))))]
            [(e1 e2 ...)
              #'(let* ([var init] ...)
                 (let do* ([var var] ...)
                  (if e0
                   (let () e1 e2 ...)
                   (let () c ...
                    (let* ([var step] ...)
                     (do* var ...))))))]))])))

;; Implements `receive` syntax by translating to `call-with-values`.
;; Compiler will later on eliminate `call-with-values` to `receive` IR form.
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
      (call-with-values (lambda () expression)
        (lambda formals body ...)))))
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
      (if test (begin body ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test body ...)
      (if test #f (begin body ...)))))

(define-syntax include
  (lambda (stx)
    (syntax-case stx ()
      [(include filename)
        (call-with-include-port
          #'filename
          (lambda (port)
            (cons #'begin
              (let lp ()
                (let ([x (read-syntax port)])
                  (if (eof-object? x)
                    '()
                    (cons (datum->syntax #'filename x) (lp))))))))])))

;; TODO: Implement case-insensitive file lookup for include-ci
(define-syntax include-ci
  (lambda (stx)
    (syntax-case stx ()
      [(include-ci filename)
        (call-with-include-port
          #'filename
          (lambda (port)
            (cons #'begin
              (let lp ()
                (let ([x (read-syntax port)])
                  (if (eof-object? x)
                    '()
                    (cons (datum->syntax #'filename x) (lp))))))))])))

(define (generate-temporary-symbol)
  (env-gensym ".L"))

;;;;;;;;;;;;;;
;; let-values
;;
;; Current approach is to translate
;;
;;   (let-values (((x y . z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (<tmp-x> <tmp-y> . <tmp-z>)
;;       (call-with-values (lambda () (bar c))
;;         (lambda (<tmp-p> <tmp-q>)
;;           (let ((x <tmp-x>)
;;                 (y <tmp-y>)
;;                 (z <tmp-z>)
;;                 (p <tmp-p>)
;;                 (q <tmp-q>))
;;             (baz x y z p q))))))

;; We could really use quasisyntax here...
(define-syntax let-values
  (lambda (x)
    (syntax-case x ()
      ((_ ((binds exp)) b0 b1 ...)
        (syntax (call-with-values (lambda () exp)
                 (lambda binds b0 b1 ...))))
      ((_ (clause ...) b0 b1 ...)
        (let lp ((clauses (syntax (clause ...)))
                 (ids '())
                 (tmps '()))
          (if (null? clauses)
            (with-syntax (((id ...) ids)
                          ((tmp ...) tmps))
              (syntax (let ((id tmp) ...)
                       b0
                       b1
                       ...)))
            (syntax-case (car clauses) ()
              (((var ...) exp)
                (with-syntax (((new-tmp ...) (generate-temporaries
                                              (syntax (var ...))))
                              ((id ...) ids)
                              ((tmp ...) tmps))
                  (with-syntax ((inner (lp (cdr clauses)
                                        (syntax (var ... id ...))
                                        (syntax (new-tmp ... tmp ...)))))
                    (syntax (call-with-values (lambda () exp)
                             (lambda (new-tmp ...) inner))))))
              ((vars exp)
                (with-syntax ((((new-var . new-tmp) ...)
                                (let lp ((vars (syntax vars)))
                                  (syntax-case vars ()
                                    ((id . rest)
                                      (acons (syntax id)
                                        (car
                                          (generate-temporaries (syntax (id))))
                                        (lp (syntax rest))))
                                    (id (acons (syntax id)
                                         (car
                                           (generate-temporaries (syntax (id))))
                                         '())))))
                              ((id ...) ids)
                              ((tmp ...) tmps))
                  (with-syntax ((inner (lp (cdr clauses)
                                        (syntax (new-var ... id ...))
                                        (syntax (new-tmp ... tmp ...))))
                                (args (let lp ((tmps (syntax (new-tmp ...))))
                                       (syntax-case tmps ()
                                         ((id) (syntax id))
                                         ((id . rest) (cons (syntax id)
                                                       (lp (syntax rest))))))))
                    (syntax (call-with-values (lambda () exp)
                             (lambda args inner)))))))))))))

;;;;;;;;;;;;;;
;; let*-values
;;
;; Current approach is to translate
;;
;;   (let*-values (((x y z) (foo a b))
;;                ((p q) (bar c)))
;;     (baz x y z p q))
;;
;; into
;;
;;   (call-with-values (lambda () (foo a b))
;;     (lambda (x y z)
;;       (call-with-values (lambda (bar c))
;;         (lambda (p q)
;;           (baz x y z p q)))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body ...)
      (let () body ...))
    ((let*-values ((vars-1 binding-1) (vars-2 binding-2) ...) body ...)
      (call-with-values (lambda () binding-1)
        (lambda vars-1
          (let*-values ((vars-2 binding-2) ...)
            body
            ...))))))
(define-syntax with-implicit
  (lambda (x)
    (syntax-case x ()
      [(_ (k x ...) e1 ... e2)
        #'(with-syntax ([x (datum->syntax #'k 'x)] ...)
           e1
           ...
           e2)]
      [_ (syntax-violation 'with-implicit "invalid syntax" x)])))

(define-syntax false-if-exception
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr)
        #'(call/cc (lambda (k)
                    (with-exception-handler
                     (lambda ex (k #f))
                     (lambda () expr))))])))

;;; {`cond-expand' for SRFI-0 support.}
;;;
;;; This syntactic form expands into different commands or
;;; definitions, depending on the features provided by the Scheme
;;; implementation.
;;;
;;; Syntax:
;;;
;;; <cond-expand>
;;;   --> (cond-expand <cond-expand-clause>+)
;;;     | (cond-expand <cond-expand-clause>* (else <command-or-definition>))
;;; <cond-expand-clause>
;;;   --> (<feature-requirement> <command-or-definition>*)
;;; <feature-requirement>
;;;   --> <feature-identifier>
;;;     | (and <feature-requirement>*)
;;;     | (or <feature-requirement>*)
;;;     | (not <feature-requirement>)
;;; <feature-identifier>
;;;   --> <a symbol which is the name or alias of a SRFI>
;;;
;;; Additionally, this implementation provides the
;;; <feature-identifier>s `guile' and `r5rs', so that programs can
;;; determine the implementation type and the supported standard.
;;;
;;; Remember to update the features list when adding more SRFIs.
;;;

(define-syntax cond-expand
  (lambda (x)
    (define (condition-matches? condition)
      (syntax-case condition (and or not library)
        ((library libname)
          (false-if-exception (resolve-r6rs-interface (syntax->datum #'libname))))
        ((and c ...)
          (and-map condition-matches? #'(c ...)))
        ((or c ...)
          (or-map condition-matches? #'(c ...)))
        ((not c)
          (if (condition-matches? #'c) #f #t))
        (c
          (identifier? #'c)
          (let ((sym (syntax->datum #'c)))
            (if (memq sym %cond-expand-features)
              #t
              #f)))))

    (define (match clauses alternate)
      (syntax-case clauses ()
        (((condition form ...) . rest)
          (if (condition-matches? #'condition)
            #'(begin form ...)
            (match #'rest alternate)))
        (() (alternate))))

    (syntax-case x (else)
      ((_ clause ... (else form ...))
        (match #'(clause ...)
          (lambda ()
            #'(begin form ...))))
      ((_ clause ...)
        (match #'(clause ...)
          (lambda ()
            (syntax-violation 'cond-expand "unfulfilled cond-expand" x)))))))

(define-syntax require-extension
  (lambda (x)
    (syntax-case x (srfi)
      ((_ (srfi n ...))
        (and-map integer? (syntax->datum #'(n ...)))
        (with-syntax
          (((srfi-n ...)
              (map (lambda (n)
                    (datum->syntax x (symbol-append 'srfi- n)))
                (map string->symbol
                  (map number->string (syntax->datum #'(n ...)))))))
          #'(import (srfi srfi-n) ...)))
      ((_ (type arg ...))
        (identifier? #'type)
        (syntax-violation 'require-extension "Not a recognized extension type"
          x)))))

(define-syntax identifier-syntax
  (lambda (xx)
    (syntax-case xx (set!)
      ((_ e)
        #'(lambda (x)
           #((macro-type . identifier-syntax))
           (syntax-case x ()
            (id
             (identifier? #'id)
             #'e)
            ((_ x (... ...))
             #'(e x (... ...))))))
      ((_ (id exp1) ((set! var val) exp2))
        (and (identifier? #'id) (identifier? #'var))
        #'(make-variable-transformer
           (lambda (x)
            #((macro-type . variable-transformer))
            (syntax-case x (set!)
             ((set! var val) #'exp2)
             ((id x (... ...)) #'(exp1 x (... ...)))
             (id (identifier? #'id) #'exp1))))))))

(define-syntax error-handling-mode
  (lambda (x)
    (syntax-case x ()
      [(_ style)
        #`'style]
      [(_) (syntax-violation 'eol-style "missing eol style" x)])))

(define-syntax with-mutex
  (syntax-rules ()
    "Run code inside this macro while acquiring the given mutex."
    [(_ mutex body body* ...)
      (let ([mtx mutex])
        (dynamic-wind
          (lambda () (mutex-acquire mtx))
          (lambda () body body* ...)
          (lambda () (mutex-release mtx))))]))

(define-syntax with-continuation-marks
  (syntax-rules ()
    [(_ () body body* ...)
      (begin body body* ...)]
    [(_ ((key value) rest ...) body body* ...)
      (with-continuation-mark
        key
        value
        (with-continuation-marks
          (rest ...)
          body
          body*
          ...))]))

(define (expand expr)
  "Given an expression, expand all macros in it and return expanded TreeIL. 
   TreeIL can be converted back to Scheme using `tree-il->scheme`."
  (macroexpand expr 'c '(compile expand load)))

(define-syntax define-for-syntax
  (syntax-rules ()
    "Defines a variable available at macro expansion time. Unlike Racket's 
    `define-for-syntax`, this does not restrict variable usage to macro expansion time only.
    Implemented as expanding into `(eval-when (expand load eval) (define ...))`."
    [(_ (name . formals) body1 body ...)
      (eval-when (expand load eval) (define (name . formals)
                                     body1
                                     body
                                     ...))]
    [(_ name value)
      (eval-when (expand load eval)
        (define name value))]))

(define-syntax and-let*
  (syntax-rules ()
    ((and-let* ())
      #t)
    ((and-let* () . body)
      (let () . body))
    ((and-let* ((var expr)))
      expr)
    ((and-let* ((expr)))
      expr)
    ((and-let* (expr)) ; Extension: in SRFI-2 this can only be a var ref
      expr)
    ((and-let* ((var expr) . rest) . body)
      (let ((var expr))
        (and var (and-let* rest . body))))
    ((and-let* ((expr) . rest) . body)
      (and expr (and-let* rest . body)))
    ((and-let* (expr . rest) . body) ; Same extension as above
      (let ((tmp expr))
        (and tmp (and-let* rest . body))))))

(eval-when (expand load eval)
  (define (%let-keywords-rec stx %let)
    ;; arg is either
    ;; - identifier
    ;; - (identifier default-expr)
    ;; - (identifier keyword default-expr)
    (define (triplet var&default)
      (syntax-case var&default ()
        ((id)
          (identifier? #'id)
          (values #'id (symbol->keyword (syntax->datum #'id)) (unspecified)))
        ((id default-expr)
          (and (identifier? #'id)
            (not (keyword? #'default-expr)))
          (values #'id (symbol->keyword (syntax->datum #'id)) #'default-expr))
        ((id keyword default-expr)
          (and
            (identifier? #'id)
            (keyword? (syntax->datum #'keyword)))
          (values #'id #'keyword #'default-expr))
        (_ (syntax-violation #f "bad binding form in let-keywords" var&default))))
    (define (process specs)
      (let loop ((specs specs)
                 (vars '())
                 (keys '())
                 (defaults '())
                 (tmps '()))
        (define (finish restvar)
          (values (reverse vars)
            (reverse keys)
            (reverse defaults)
            (reverse tmps)
            restvar))
        (syntax-case specs ()
          [() (finish #f)]
          [(arg . rest)
            (receive (var key default) (triplet #'arg)
              (loop #'rest
                (cons var vars)
                (cons key keys)
                (cons default defaults)
                (cons (datum->syntax #f (generate-temporary-symbol)) tmps)))]
          [rest
            (identifier? #'rest)
            (finish #'rest)])))
    (syntax-case stx ()
      [(_ arg specs . body)
        (let ([argvar (datum->syntax #f 'args)]
              [loop (datum->syntax #f 'loop)])
          (receive (vars keys defaults tmps restvar) (process #'specs)
            #`(let #,loop ((#,argvar arg)
                           #,@(if (boolean? restvar) '() #`((#,restvar '())))
                           #,@(map (lambda (tmp) #`(#,tmp (unspecified))) tmps))
               (cond
                [(null? #,argvar)
                 (#,%let #,(map (lambda (var tmp default)
                                 #`(#,var (if (unspecified? #,tmp) #,default #,tmp)))
                            vars
                            tmps
                            defaults)
                  .
                  body)]
                [(null? (cdr #,argvar))
                 (error 'let-keywords "keyword list not even" #,argvar)]
                [else
                 (case (car #,argvar)
                  #,@(map (lambda (key)
                           #`((#,key)
                              (#,loop (cddr #,argvar)
                               #,@(if (boolean? restvar) '() #`(#,restvar))
                               #,@(map (lambda (k t)
                                        (if (eq? key k)
                                          #`(cadr #,argvar)
                                          t))
                                   keys
                                   tmps))))
                      keys)
                  (else
                   #,(cond
                      [(eq? restvar #t)
                        #`(#,loop (cddr #,argvar) #,@tmps)]
                      [(eq? restvar #f)
                        #`(begin
                           (fprintf (current-error-port) "unknown keyword ~s~%" (car #,argvar))
                           (#,loop (cddr #,argvar) #,@tmps))]
                      [else
                        #`(#,loop
                           (cddr #,argvar)
                           (list* (car #,argvar) (cadr #,argvar) #,restvar)
                           #,@tmps)])))]))))])))

(define-syntax let-keywords
  (lambda (stx)
    (%let-keywords-rec stx #'let)))

(define-syntax let-keywords*
  (lambda (stx)
    (%let-keywords-rec stx #'let*)))
(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
      (let ((rest arg))
        (%let-optionals* rest (opt-clause ...) body ...)))))

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
      (call-with-values (lambda () (xparser arg))
        (lambda (rest var ...)
          (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
      (call-with-values (lambda () (if (null? arg) (values default '())
                                    (values (car arg) (cdr arg))))
        (lambda (var rest)
          (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
      (call-with-values (lambda ()
                         (if (null? arg) (values default '())
                           (let ((var (car arg)))
                             (if test (values var (cdr arg))
                               (error "arg failed LET-OPT test" var)))))
        (lambda (var rest)
          (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
      (call-with-values (lambda ()
                         (if (null? arg) (values default #f '())
                           (let ((var (car arg)))
                             (if test (values var #t (cdr arg))
                               (error "arg failed LET-OPT test" var)))))
        (lambda (var supplied? rest)
          (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
      (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
      (if (null? arg) (begin body ...)
        (error "Too many arguments in let-opt" arg)))))

(define (set-record-type-printer! typ printer)
  (unless (record-type? typ)
    (assertion-violation 'set-record-type-printer! "Not an record type"))
  (set-rtd-printer! (record-type-rtd typ) printer))

;; `define-class` accessor options may name a generic an earlier class already
;; defined (`#:accessor name` on several classes). Reuse a bound generic so the
;; new class adds a method to it instead of replacing it.

(define %shared-generics (make-core-hash-eq))

(define (%remembered-generic env name)
  (let ([table (core-hash-ref %shared-generics env #f)])
    (and table (core-hash-ref table name #f))))

(define (%remember-generic! env name generic)
  (let ([table (or (core-hash-ref %shared-generics env #f)
                  (let ([fresh (make-core-hash-eq)])
                    (core-hash-put! %shared-generics env fresh)
                    fresh))])
    (core-hash-put! table name generic)
    generic))

(define (%reusable-generic env var name)
  (cond
    [(and var (variable-bound? var))
      (if (generic? (variable-ref var))
        (variable-ref var)
        (make-generic name))]
    [var (make-generic name)]
    [else
      (or (%remembered-generic env name)
        (%remember-generic! env name (make-generic name)))]))

(define (%ensure-setter-generic! getter name)
  (unless (generic? (setter getter))
    (set-setter! getter (make-generic name))))

(define-syntax %define-shared-generic
  (syntax-rules ()
    [(_ name)
      (define name
        (%reusable-generic (expansion-env) (env-local-variable (expansion-env) 'name) 'name))]))

(define-syntax %ensure-accessor-setter
  (syntax-rules ()
    [(_ name)
      (%ensure-setter-generic! name 'name)]))

(define (class-slot-definition class slot-name)
  (let loop ([slots (class-slots class)])
    (cond
      [(null? slots) #f]
      [(eq? (slot-definition-name (car slots)) slot-name) (car slots)]
      [else (loop (cdr slots))])))

(define (class-slot-accessor class slot-name)
  (let loop ([accessors (class-accessors class)])
    (cond
      [(null? accessors) #f]
      [(eq? (slot-accessor-name (car accessors)) slot-name) (car accessors)]
      [else (loop (cdr accessors))])))

(define (%slot-name slot)
  (if (slot-definition? slot)
    (slot-definition-name slot)
    slot))

(define (%slot-ref-using-class class obj slot . fallback)
  (define slot-name (%slot-name slot))
  (if (not (subclass? (class-of obj) class))
    (assertion-violation 'slot-ref-using-class
      "object is not an instance of class"
      obj
      class)
    (let ([accessor (class-slot-accessor class slot-name)])
      (cond
        [accessor
          (if (slot-bound-using-accessor? obj accessor)
            (slot-ref-using-accessor obj accessor)
            (slot-unbound class obj slot-name))]
        [(null? fallback)
          (slot-missing class obj slot-name)]
        [else (car fallback)]))))

(define (%slot-set-using-class! class obj slot value)
  (define slot-name (%slot-name slot))
  (if (not (subclass? (class-of obj) class))
    (assertion-violation 'slot-set-using-class!
      "object is not an instance of class"
      obj
      class)
    (let ([accessor (class-slot-accessor class slot-name)])
      (if accessor
        (slot-set-using-accessor! obj accessor value)
        (slot-missing class obj slot-name value)))))

(define (%slot-bound-using-class? class obj slot)
  (define slot-name (%slot-name slot))
  (if (not (subclass? (class-of obj) class))
    (assertion-violation 'slot-bound-using-class?
      "object is not an instance of class"
      obj
      class)
    (let ([accessor (class-slot-accessor class slot-name)])
      (if accessor
        (slot-bound-using-accessor? obj accessor)
        (assertion-violation 'slot-bound-using-class?
          "class has no slot"
          class
          slot-name)))))

(define (%slot-unbind-using-class! class obj slot)
  (define slot-name (%slot-name slot))
  (if (not (subclass? (class-of obj) class))
    (assertion-violation 'slot-unbind-using-class!
      "object is not an instance of class"
      obj
      class)
    (let ([accessor (class-slot-accessor class slot-name)])
      (if accessor
        (slot-unbind-using-accessor! obj accessor)
        (slot-missing class obj slot-name)))))

(define (slot-definition-allocation slot)
  (%slot-definition-allocation slot))

(define (class-slot-allocation-error who class slot-name slot)
  (assertion-violation who
    "slot is not class allocated"
    class
    slot-name
    (slot-definition-allocation slot)))

(define (class-slot-missing-error who class slot-name)
  (assertion-violation who
    "class has no slot"
    class
    slot-name))

(define (class-slot-ref class slot-name)
  (let ([slot (class-slot-definition class slot-name)])
    (if slot
      (if (eq? (slot-definition-allocation slot) #:class)
        (%class-slot-ref class slot)
        (class-slot-allocation-error 'class-slot-ref class slot-name slot))
      (class-slot-missing-error 'class-slot-ref class slot-name))))

(define (class-slot-set! class slot-name value)
  (let ([slot (class-slot-definition class slot-name)])
    (if slot
      (if (eq? (slot-definition-allocation slot) #:class)
        (%class-slot-set! class slot value)
        (class-slot-allocation-error 'class-slot-set! class slot-name slot))
      (class-slot-missing-error 'class-slot-set! class slot-name))))

(define (class-slot-bound? class slot-name)
  (let ([slot (class-slot-definition class slot-name)])
    (if slot
      (if (eq? (slot-definition-allocation slot) #:class)
        (%class-slot-bound? class slot)
        (class-slot-allocation-error 'class-slot-bound? class slot-name slot))
      (class-slot-missing-error 'class-slot-bound? class slot-name))))

(set-setter! class-slot-ref class-slot-set!)

(define (slot-definition-options slot)
  (list #:allocation (slot-definition-allocation slot)
    #:init-keyword
    (slot-definition-init-keyword slot)
    #:init-value
    (slot-definition-init-value slot)
    #:init-thunk
    (slot-definition-init-thunk slot)
    #:slot-ref
    (slot-definition-slot-ref slot)
    #:slot-set!
    (slot-definition-slot-set! slot)
    #:slot-bound?
    (slot-definition-slot-bound? slot)
    #:getter
    (slot-definition-getter slot)
    #:setter
    (slot-definition-setter slot)
    #:accessor
    (slot-definition-accessor slot)
    #:initializable
    (slot-definition-initializable? slot)
    #:settable
    (slot-definition-settable? slot)
    #:immutable
    (slot-definition-immutable? slot)))

(define (slot-accessor-options accessor)
  (list #:init-keyword (slot-accessor-init-keyword accessor)
    #:init-value
    (slot-accessor-init-value accessor)
    #:init-thunk
    (slot-accessor-init-thunk accessor)
    #:slot-ref
    (slot-accessor-slot-ref accessor)
    #:slot-set!
    (slot-accessor-slot-set! accessor)
    #:slot-bound?
    (slot-accessor-slot-bound? accessor)
    #:getter
    (slot-accessor-getter accessor)
    #:setter
    (slot-accessor-setter accessor)
    #:accessor
    (slot-accessor-accessor accessor)
    #:initializable
    (slot-accessor-initializable? accessor)
    #:settable
    (slot-accessor-settable? accessor)
    #:immutable
    (slot-accessor-immutable? accessor)))

(define (%mop-option-name key)
  (cond
    [(keyword? key) (keyword->symbol key)]
    [(symbol? key)
      (let ([name (symbol->string key)])
        (if (and (> (string-length name) 0)
             (char=? (string-ref name 0) #\:))
          (string->symbol (substring name 1 (string-length name)))
          key))]
    [else key]))

(define (slot-definition-option slot key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (slot-definition-options slot)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (slot-accessor-option accessor key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (slot-accessor-options accessor)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (generic-options generic)
  (list #:fallback (generic-fallback generic)
    #:required-dispatch-arg-count
    (generic-required-dispatch-arg-count generic)
    #:sealed
    (generic-sealed? generic)))

(define (generic-option generic key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (generic-options generic)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (method-options method)
  (list #:generic (method-generic method)
    #:specializers
    (method-specializers method)
    #:required-arg-count
    (method-required-arg-count method)
    #:body
    (method-body method)
    #:locked
    (method-locked? method)))

(define (method-option method key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (method-options method)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (next-method-options next-method)
  (list #:generic (next-method-generic next-method)
    #:methods
    (next-method-methods next-method)
    #:args
    (next-method-args next-method)
    #:index
    (next-method-index next-method)
    #:body
    (next-method-body next-method)
    #:has-next?
    (next-method-has-next? next-method)
    #:next
    (next-method-next next-method)))

(define (next-method-option next-method key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (next-method-options next-method)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (class-options class)
  (list #:name (class-name class)
    #:direct-supers
    (class-direct-supers class)
    #:cpl
    (class-precedence-list class)
    #:direct-slots
    (class-direct-slots class)
    #:slots
    (class-slots class)
    #:accessors
    (class-accessors class)
    #:initargs
    (class-initargs class)
    #:direct-methods
    (class-direct-methods class)
    #:direct-subclasses
    (class-direct-subclasses class)
    #:applicable
    (class-applicable? class)
    #:malleable
    (class-malleable? class)
    #:sealed
    (class-sealed? class)))

(define (class-option class key . default)
  (let ([name (%mop-option-name key)])
    (let loop ([options (class-options class)])
      (cond
        [(null? options)
          (if (null? default) #f (car default))]
        [(eq? (%mop-option-name (car options)) name)
          (cadr options)]
        [else (loop (cddr options))]))))

(define (subclass? class super)
  (let loop ([cpl (class-precedence-list class)])
    (cond
      [(null? cpl) #f]
      [(eq? (car cpl) super) #t]
      [else (loop (cdr cpl))])))

(define (is-a? obj class)
  (subclass? (class-of obj) class))

(define (method-applicable-for-classes? method . classes)
  (and (>= (length classes) (method-required-arg-count method))
    (let loop ([specializers (method-specializers method)]
               [classes classes])
      (cond
        [(null? specializers) #t]
        [(null? classes) #f]
        [(subclass? (car classes) (car specializers))
          (loop (cdr specializers) (cdr classes))]
        [else #f]))))

(define (method-specificity-score method classes)
  (define unspecialized-distance 1000000000)
  (define (class-distance class specializer)
    (let loop ([cpl (class-precedence-list class)]
               [distance 0])
      (cond
        [(null? cpl) #f]
        [(eq? (car cpl) specializer) distance]
        [else (loop (cdr cpl) (+ distance 1))])))
  (and (>= (length classes) (method-required-arg-count method))
    (let loop ([classes classes]
               [specializers (method-specializers method)]
               [score '()])
      (cond
        [(null? classes)
          (reverse
            (cons (- unspecialized-distance
                   (method-required-arg-count method))
              score))]
        [(null? specializers)
          (loop (cdr classes)
            specializers
            (cons unspecialized-distance score))]
        [else
          (let ([distance (class-distance (car classes) (car specializers))])
            (and distance
              (loop (cdr classes)
                (cdr specializers)
                (cons distance score))))]))))

(define (score<? left right)
  (cond
    [(null? left) #f]
    [(null? right) #f]
    [(< (car left) (car right)) #t]
    [(> (car left) (car right)) #f]
    [else (score<? (cdr left) (cdr right))]))

(define (method-more-specific? left right classes)
  (let ([left-score (method-specificity-score left classes)]
        [right-score (method-specificity-score right classes)])
    (and left-score
      right-score
      (score<? left-score right-score))))

(define (sort-applicable-methods generic methods args)
  (let ([classes (map class-of args)])
    (list-sort
      (lambda (left right)
        (method-more-specific? left right classes))
      methods)))

(define (apply-method generic methods build-next args)
  (if (null? methods)
    (apply generic-invoke generic args)
    (apply (method-body (car methods))
      (cons (build-next generic methods args) args))))

(define (apply-methods generic methods args)
  (apply-method generic methods %make-next-method args))

;; Calls GENERIC on ARGS the way a direct call does, so a method of the dispatch protocol
;; generics (`compute-applicable-methods` and the others) means the same here.
(define (apply-generic generic args)
  (apply generic args))

(define (slot-exists? obj slot-name)
  (slot-exists-using-class? (class-of obj) obj slot-name))

(define (slot-exists-using-class? class obj slot-name)
  (not (not (class-slot-definition class slot-name))))

(define (slot-push! obj slot-name value)
  (slot-set! obj slot-name (cons value (slot-ref obj slot-name))))

(define (slot-pop! obj slot-name . default)
  (if (or (not (slot-exists? obj slot-name))
       (not (slot-bound? obj slot-name))
       (not (pair? (slot-ref obj slot-name))))
    (if (null? default)
      (assertion-violation 'slot-pop! "slot value is not a pair" obj slot-name)
      (car default))
    (let ([value (slot-ref obj slot-name)])
      (slot-set! obj slot-name (cdr value))
      (car value))))

(define-for-syntax (%define-class-syntax-list->list form stx)
  (syntax-case stx ()
    [() '()]
    [(head . tail)
      (cons #'head (%define-class-syntax-list->list form #'tail))]
    [_ (syntax-violation 'define-class
        "slot list must be a proper list"
        form
        stx)]))

(define-for-syntax (%define-class-colon-symbol? datum)
  (and (symbol? datum)
    (let ([name (symbol->string datum)])
      (and (> (string-length name) 0)
        (char=? (string-ref name 0) #\:)))))

(define-for-syntax (%define-class-option-name option)
  (let ([datum (syntax->datum option)])
    (cond
      [(keyword? datum) (keyword->symbol datum)]
      [(%define-class-colon-symbol? datum)
        (let ([name (symbol->string datum)])
          (string->symbol (substring name 1 (string-length name))))]
      [else datum])))


(define-for-syntax (%define-class-quote-keyword stx)
  (if (keyword? (syntax->datum stx))
    #`'#,stx
    stx))

(define-for-syntax (%define-class-runtime-slot-option option)
  (%define-class-quote-keyword
    (case (%define-class-option-name option)
      [(init-value) #'#:init-value]
      [(init-keyword) #'#:init-keyword]
      [(init-thunk) #'#:init-thunk]
      [(slot-ref) #'#:slot-ref]
      [(slot-set!) #'#:slot-set!]
      [(slot-bound?) #'#:slot-bound?]
      [(allocation) #'#:allocation]
      [(getter) #'#:getter]
      [(setter) #'#:setter]
      [(accessor) #'#:accessor]
      [(immutable) #'#:immutable]
      [(initializable) #'#:initializable]
      [(settable) #'#:settable]
      [else option])))

(define-for-syntax (%define-class-slot-option-pairs form options slot)
  (syntax-case options ()
    [() '()]
    [(option value . rest)
      (cons (cons #'option #'value)
        (%define-class-slot-option-pairs form #'rest slot))]
    [_ (syntax-violation 'define-class
        "slot options must be keyword/value pairs"
        form
        slot)]))

(define-for-syntax (%define-class-option-pairs form options)
  (syntax-case options ()
    [() '()]
    [(option value . rest)
      (cons (cons #'option #'value)
        (%define-class-option-pairs form #'rest))]
    [_ (syntax-violation 'define-class
        "class options must be keyword/value pairs"
        form
        options)]))

(define-for-syntax (%define-class-applicable-option form options)
  (let loop ([pairs (%define-class-option-pairs form options)]
             [applicable? #f]
             [seen? #f])
    (cond
      [(null? pairs) applicable?]
      [else
        (let ([value (cdar pairs)])
          (cond
            [(eq? (%define-class-option-name (caar pairs)) 'applicable)
              (when seen?
                (syntax-violation 'define-class
                  "duplicate #:applicable class option"
                  form
                  (caar pairs)))
              (let ([datum (syntax->datum value)])
                (unless (boolean? datum)
                  (syntax-violation 'define-class
                    "#:applicable class option must be a boolean literal"
                    form
                    value))
                (loop (cdr pairs) datum #t))]
            [(memq (%define-class-option-name (caar pairs))
                   '(metaclass default-initargs sealed final allow-subclasses))
              (loop (cdr pairs) applicable? seen?)]
            [else
              (syntax-violation 'define-class
                "unsupported class option"
                form
                (caar pairs))]))])))

;; Boolean #:sealed / #:final / #:allow-subclasses options.
(define-for-syntax (%define-class-boolean-option form options key)
  (let loop ([pairs (%define-class-option-pairs form options)] [value #f])
    (cond
      [(null? pairs) value]
      [(eq? (%define-class-option-name (caar pairs)) key)
        (when value
          (syntax-violation 'define-class
            "duplicate class option"
            form
            (caar pairs)))
        (let ([datum (syntax->datum (cdar pairs))])
          (unless (boolean? datum)
            (syntax-violation 'define-class
              "class option must be a boolean literal"
              form
              (cdar pairs)))
          (loop (cdr pairs) (if datum 'on 'off)))]
      [else (loop (cdr pairs) value)])))

(define-for-syntax (%define-class-seal-forms form name options)
  (let* ([sealed (eq? (%define-class-boolean-option form options 'sealed) 'on)]
         [final (eq? (%define-class-boolean-option form options 'final) 'on)]
         [allow (eq? (%define-class-boolean-option form options 'allow-subclasses) 'on)])
    (when (and final allow)
      (syntax-violation 'define-class
        "#:final class cannot allow subclasses"
        form))
    (when (and allow (not sealed))
      (syntax-violation 'define-class
        "#:allow-subclasses requires #:sealed"
        form))
    (cond
      [(and sealed allow) (with-syntax ([name name])
                            #'((%class-seal-allowing-subclasses! name)))]
      [(or sealed final) (with-syntax ([name name]) #'((class-seal! name)))]
      [else '()])))

;; #:default-initargs as thunks.
(define-for-syntax (%define-class-default-initarg-forms form value)
  (let loop ([value value])
    (syntax-case value ()
      [() '()]
      [((key expr) . rest)
        (begin
          (unless (keyword? (syntax->datum #'key))
            (syntax-violation 'define-class
              "#:default-initargs keys must be keywords"
              form
              #'key))
          (cons #'(list 'key (lambda () expr))
            (loop #'rest)))]
      [_ (syntax-violation 'define-class
          "#:default-initargs must be a list of (#:key expr) pairs"
          form
          value)])))

(define-for-syntax (%define-class-default-initargs-forms form options)
  (let loop ([pairs (%define-class-option-pairs form options)]
             [forms #f])
    (cond
      [(null? pairs) (or forms '())]
      [(eq? (%define-class-option-name (caar pairs)) 'default-initargs)
        (when forms
          (syntax-violation 'define-class
            "duplicate #:default-initargs class option"
            form
            (caar pairs)))
        (loop (cdr pairs) (%define-class-default-initarg-forms form (cdar pairs)))]
      [else (loop (cdr pairs) forms)])))

(define-for-syntax (%define-class-metaclass-option form options)
  (let loop ([pairs (%define-class-option-pairs form options)]
             [metaclass #f])
    (cond
      [(null? pairs) metaclass]
      [(eq? (%define-class-option-name (caar pairs)) 'metaclass)
        (when metaclass
          (syntax-violation 'define-class
            "duplicate #:metaclass class option"
            form
            (caar pairs)))
        (loop (cdr pairs) (cdar pairs))]
      [else (loop (cdr pairs) metaclass)])))

(define-for-syntax (%define-class-runtime-option-values form options slot)
  (let loop ([pairs (%define-class-slot-option-pairs form options slot)]
             [out '()])
    (cond
      [(null? pairs) (reverse out)]
      [else
        (let ([value (cdar pairs)]
              [option-name (%define-class-option-name (caar pairs))])
          (cond
            [(memq option-name '(getter setter accessor))
              (loop (cdr pairs)
                (cons #`'#,value
                  (cons (%define-class-runtime-slot-option (caar pairs)) out)))]
            [(memq option-name '(initform init-form))
              (loop (cdr pairs)
                (cons #`(lambda () #,value)
                  (cons (%define-class-quote-keyword
                          (datum->syntax (caar pairs) '#:init-thunk))
                    out)))]
            [(and (eq? option-name 'allocation)
                (%define-class-colon-symbol? (syntax->datum value)))
              (loop (cdr pairs)
                (cons #`'#,value
                  (cons (%define-class-runtime-slot-option (caar pairs)) out)))]
            [else
              (loop (cdr pairs)
                (cons (%define-class-quote-keyword value)
                  (cons (%define-class-runtime-slot-option (caar pairs)) out)))]))])))

(define-for-syntax (%process-slot-definition form slot)
  (syntax-case slot ()
    [name
      (identifier? #'name)
      #''name]
    [(name option value ...)
      (identifier? #'name)
      (with-syntax ([(runtime-option-value ...)
                      (%define-class-runtime-option-values
                        form
                        #'(option value ...)
                        slot)])
        #'(list 'name runtime-option-value ...))]
    [_ (syntax-violation 'define-class
        "expected a slot name or (slot-name option value ...)"
        form
        slot)]))

(define-for-syntax (%define-class-getter-forms class-name slot-name getter-name)
  (with-syntax ([class-name class-name]
                [slot-name slot-name]
                [getter-name getter-name])
    #'((%define-shared-generic getter-name)
       (define-method (getter-name (obj class-name))
        (slot-ref obj 'slot-name)))))

(define-for-syntax (%define-class-setter-forms class-name slot-name setter-name)
  (with-syntax ([class-name class-name]
                [slot-name slot-name]
                [setter-name setter-name])
    #'((%define-shared-generic setter-name)
       (define-method (setter-name (obj class-name) value)
        (slot-set! obj 'slot-name value)
        value))))

(define-for-syntax (%define-class-accessor-forms class-name slot-name accessor-name)
  (with-syntax ([class-name class-name]
                [slot-name slot-name]
                [accessor-name accessor-name])
    #'((%define-shared-generic accessor-name)
       (%ensure-accessor-setter accessor-name)
       (define-method (accessor-name (obj class-name))
        (slot-ref obj 'slot-name))
       (define-method ((setter accessor-name) (obj class-name) value)
        (slot-set! obj 'slot-name value)
        value))))

(define-for-syntax (%define-class-slot-accessor-forms form class-name slot)
  (syntax-case slot ()
    [name
      (identifier? #'name)
      '()]
    [(name option value ...)
      (identifier? #'name)
      (let loop ([pairs (%define-class-slot-option-pairs form #'(option value ...) slot)]
                 [forms '()])
        (if (null? pairs)
          (apply append (reverse forms))
          (let ([target (cdar pairs)])
            (cond
              [(eq? (%define-class-option-name (caar pairs)) 'getter)
                (loop (cdr pairs)
                  (cons (%define-class-getter-forms class-name #'name target) forms))]
              [(eq? (%define-class-option-name (caar pairs)) 'setter)
                (loop (cdr pairs)
                  (cons (%define-class-setter-forms class-name #'name target) forms))]
              [(eq? (%define-class-option-name (caar pairs)) 'accessor)
                (loop (cdr pairs)
                  (cons (%define-class-accessor-forms class-name #'name target) forms))]
              [else
                (loop (cdr pairs) forms)]))))]
    [_ '()]))

;; Forward-referenced superclasses.
(define-for-syntax (%define-class-forward-supers form supers)
  (filter
    (lambda (super)
      (and (identifier? super)
        ;; Lexical or existing names are not forward refs.
        (not (eq? (identifier-binding super) 'lexical))
        (not (env-variable (expansion-env) (syntax->datum super)))))
    (%define-class-syntax-list->list form supers)))

;; Redefined variable looked up at expansion time.
(define-for-syntax (%define-class-redefined-variable name)
  (and (identifier? name)
    (not (eq? (identifier-binding name) 'lexical))
    (env-variable (expansion-env) (syntax->datum name))
    name))

(define-for-syntax (%expand-define-class form name supers slots options)
  (let* ([slot-list (%define-class-syntax-list->list form slots)]
         [applicable? (%define-class-applicable-option form options)]
         [metaclass (%define-class-metaclass-option form options)]
         [default-initarg-forms (%define-class-default-initargs-forms form options)]
         [seal-forms (%define-class-seal-forms form name options)]
         [forward-supers (%define-class-forward-supers form supers)]
         [redefined (%define-class-redefined-variable name)]
         [redefined-value
           (if redefined
             #`(lambda () #,redefined)
             #'#f)])
    (when (and applicable? metaclass)
      (syntax-violation 'define-class
        "#:metaclass is not supported with #:applicable"
        form
        metaclass))
    (with-syntax ([name name]
                  [(super ...) supers]
                  [(forward-define ...)
                    (with-syntax ([(forward ...) forward-supers])
                      #'((define forward (%make-forward-referenced-class (quote forward))) ...))]
                  [(default-initarg ...) default-initarg-forms]
                  [(slot-spec ...)
                    (map (lambda (slot) (%process-slot-definition form slot))
                      slot-list)]
                  [(accessor-form ...)
                    (apply append
                      (map (lambda (slot)
                            (%define-class-slot-accessor-forms form name slot))
                        slot-list))]
                  ;; Default metaclass for applicable class.
                  [metaclass-value (or metaclass #'#f)]
                  [redefined-value redefined-value]
                  [(seal-form ...) seal-forms]
                  [applicable-value (datum->syntax form applicable?)])
      #'(begin
         forward-define ...
         (define name
          (ensure-class 'name
            #:redefined redefined-value
            #:direct-superclasses (list super ...)
            #:direct-slots (list slot-spec ...)
            #:default-initargs (list default-initarg ...)
            #:metaclass metaclass-value
            #:applicable applicable-value))
         (class-post-initialize
          name
          (list #:name 'name
           #:supers
           (list super ...)
           #:slots
           (list slot-spec ...)
           #:applicable
           applicable-value))
         accessor-form
         ...
         seal-form ...))))

(define-syntax define-class
  (lambda (form)
    (syntax-case form ()
      [(_ name supers slots option ...)
        (identifier? #'name)
        (%expand-define-class form #'name #'supers #'slots #'(option ...))]
      [_ (syntax-violation 'define-class
          "expected (define-class name supers slots . options)"
          form)])))

(define-syntax define-generic
  (lambda (x)
    (define (colon-symbol? datum)
      (and (symbol? datum)
        (let ([name (symbol->string datum)])
          (and (> (string-length name) 0)
            (char=? (string-ref name 0) #\:)))))
    (define (option-name option)
      (let ([datum (syntax->datum option)])
        (cond
          [(keyword? datum) (keyword->symbol datum)]
          [(colon-symbol? datum)
            (let ([name (symbol->string datum)])
              (string->symbol (substring name 1 (string-length name))))]
          [else datum])))
    (define (generic-option-pairs options)
      (syntax-case options ()
        [() '()]
        [(option value . rest)
          (cons (cons #'option #'value)
            (generic-option-pairs #'rest))]
        [_ (syntax-violation 'define-generic
            "generic options must be keyword/value pairs"
            x
            options)]))
    ;; Sealing/precompute after other options.
    (define (select pred items)
      (cond
        [(null? items) '()]
        [(pred (car items)) (cons (car items) (select pred (cdr items)))]
        [else (select pred (cdr items))]))
    (define (generic-option-forms generic options)
      (let loop ([pairs
                   (let ([pairs (generic-option-pairs options)])
                     (append
                       (select (lambda (pair)
                                 (not (memq (option-name (car pair)) '(sealed precompute))))
                         pairs)
                       (select (lambda (pair) (eq? (option-name (car pair)) 'sealed)) pairs)
                       (select (lambda (pair) (eq? (option-name (car pair)) 'precompute)) pairs)))]
                 [forms '()])
        (if (null? pairs)
          (apply append (reverse forms))
          (let ([option (option-name (caar pairs))]
                [value (cdar pairs)])
            (case option
              [(fallback)
                (loop (cdr pairs)
                  (cons
                    (with-syntax ([generic generic]
                                  [value value])
                      #'((set-generic-fallback! generic value)))
                    forms))]
              [(sealed)
                (loop (cdr pairs)
                  (cons
                    (with-syntax ([generic generic]
                                  [value value])
                      #'((when value (generic-seal! generic))))
                    forms))]
              [(precompute)
                (loop (cdr pairs)
                  (cons
                    (with-syntax ([generic generic]
                                  [value value])
                      #'((when value (generic-precompute! generic))))
                    forms))]
              ;; Quote combination/precedence.
              [(method-combination)
                (loop (cdr pairs)
                  (cons
                    (with-syntax ([generic generic]
                                  [value (datum->syntax value
                                           (list 'quote (syntax->datum value)))])
                      #'((set-generic-method-combination! generic value)))
                    forms))]
              [(argument-precedence-order)
                (loop (cdr pairs)
                  (cons
                    (with-syntax ([generic generic]
                                  [value (datum->syntax value
                                           (list 'quote (syntax->datum value)))])
                      #'((%set-generic-precedence! generic value)))
                    forms))]
              ;; Generic class option.
              [(generic-function-class) (loop (cdr pairs) forms)]
              [else
                (syntax-violation 'define-generic
                  "unsupported generic option"
                  x
                  (caar pairs))])))))
    (define (generic-class-option options)
      (let loop ([pairs (generic-option-pairs options)])
        (cond
          [(null? pairs) #f]
          [(eq? (option-name (caar pairs)) 'generic-function-class) (cdar pairs)]
          [else (loop (cdr pairs))])))
    (define (setter-binding-name name)
      (datum->syntax name
        (string->symbol
          (string-append "setter of " (symbol->string (syntax->datum name))))))
    (define (expand-setter-generic name required-dispatch-args options)
      (unless (identifier? name)
        (syntax-violation 'define-generic "expected a generic identifier" x name))
      (when (generic-class-option options)
        (syntax-violation 'define-generic
          "generic-function-class is not supported for setter generics"
          x))
      (with-syntax ([getter-id name]
                    [setter-id (setter-binding-name name)]
                    [required-dispatch-args required-dispatch-args]
                    [(option-form ...)
                      (generic-option-forms (setter-binding-name name) options)])
        #'(begin
           (define setter-id (make-generic 'setter-id required-dispatch-args))
           option-form
           ...
           (set-setter! getter-id setter-id))))
    (define (expand-generic name required-dispatch-args options)
      (let ([class (generic-class-option options)])
        (with-syntax ([name name]
                      [required-dispatch-args required-dispatch-args]
                      [(option-form ...) (generic-option-forms name options)])
          (with-syntax ([maker
                          (if class
                            (with-syntax ([class class])
                              #'(%make-generic-of-class 'name required-dispatch-args class))
                            #'(make-generic 'name required-dispatch-args))])
            #'(begin
               (define name (%remember-generic! (expansion-env) 'name maker))
               option-form
               ...)))))
    (syntax-case x (setter)
      [(_ name)
        (identifier? #'name)
        #'(define name (%remember-generic! (expansion-env) 'name (make-generic 'name)))]
      [(_ name option value ...)
        (and (identifier? #'name)
          (or (keyword? (syntax->datum #'option))
            (colon-symbol? (syntax->datum #'option))))
        (expand-generic #'name #'1 #'(option value ...))]
      [(_ (setter name))
        (expand-setter-generic #'name #'1 #'())]
      [(_ (setter name) option value ...)
        (or (keyword? (syntax->datum #'option))
          (colon-symbol? (syntax->datum #'option)))
        (expand-setter-generic #'name #'1 #'(option value ...))]
      [(_ name required-dispatch-args)
        (identifier? #'name)
        #'(define name (%remember-generic! (expansion-env) 'name (make-generic 'name required-dispatch-args)))]
      [(_ name required-dispatch-args option value ...)
        (identifier? #'name)
        (expand-generic #'name #'required-dispatch-args #'(option value ...))]
      [(_ (setter name) required-dispatch-args)
        (expand-setter-generic #'name #'required-dispatch-args #'())]
      [(_ (setter name) required-dispatch-args option value ...)
        (expand-setter-generic #'name #'required-dispatch-args #'(option value ...))]
      [_ (syntax-violation 'define-generic "invalid generic definition" x)])))

(define-syntax define-method
  (lambda (x)
    (define (parse-specialized-args args)
      (let loop ([rest args]
                 [vars '()]
                 [specializers '()]
                 [required 0]
                 [specialized-prefix? #t])
        (syntax-case rest ()
          [()
            (values (reverse vars) (reverse specializers) required #f #'())]
          [(keyword . _)
            (keyword? (syntax->datum #'keyword))
            (values (reverse vars) (reverse specializers) required #f rest)]
          [rest-var
            (identifier? #'rest-var)
            (values (reverse vars) (reverse specializers) required #'rest-var #'())]
          [((var class) . more)
            (identifier? #'var)
            (if specialized-prefix?
              (loop #'more
                (cons #'var vars)
                (cons (specializer-expression #'class) specializers)
                (+ required 1)
                #t)
              (syntax-violation 'define-method
                "specialized arguments must precede unspecialized arguments"
                args
                #'(var class)))]
          [(var . more)
            (identifier? #'var)
            (loop #'more
              (cons #'var vars)
              specializers
              (+ required 1)
              #f)]
          [_ (syntax-violation 'define-method
              "expected arguments as variables, (variable class), or a rest variable"
              args)])))
    ;; (eql expr) specializer.
    (define (specializer-expression class)
      (let ([datum (syntax->datum class)])
        (if (and (pair? datum)
              (eq? (car datum) 'eql)
              (pair? (cdr datum))
              (null? (cddr datum)))
          (syntax-case class ()
            [(_ object) #'(intern-eql-specializer object)])
          class)))
    (define (generic-expression form generic)
      (syntax-case generic (setter)
        [id
          (identifier? #'id)
          #'id]
        [(setter id)
          (identifier? #'id)
          #'(setter id)]
        [_ (syntax-violation 'define-method
            "expected a generic identifier or (setter identifier)"
            form
            generic)]))
    (define (locked-qualifier? qualifier)
      (let ([datum (syntax->datum qualifier)])
        (or (eq? datum '#:locked)
          (eq? datum ':locked))))
    (define (colon-symbol? datum)
      (and (symbol? datum)
        (let ([name (symbol->string datum)])
          (and (> (string-length name) 0)
            (char=? (string-ref name 0) #\:)))))
    (define (keyword-qualifier? qualifier)
      (let ([datum (syntax->datum qualifier)])
        (or (keyword? datum)
          (colon-symbol? datum))))
    ;; Leading qualifiers.
    (define (leading-qualifiers forms)
      (let loop ([forms forms] [locked? #f] [qualifiers '()])
        (syntax-case forms ()
          [(q next . more)
            (keyword-qualifier? #'q)
            (cond
              [(locked-qualifier? #'q)
                (loop #'(next . more) #t qualifiers)]
              [(keyword? (syntax->datum #'q))
                (loop #'(next . more) locked?
                  (cons (syntax->datum #'q) qualifiers))]
              [else
                (syntax-violation 'define-method
                  "method qualifier must be a keyword"
                  x
                  #'q)])]
          [_ (values locked? (reverse qualifiers) forms)])))
    ;; Conservative: any mention of the next-method names anywhere in the
    ;; body (even quoted) keeps the full next-method machinery. A body that
    ;; never mentions them gets #f in the next slot and skips the
    ;; NextMethodDescriptor allocation on every call
    (define (mentions-next? datum)
      (cond
        [(symbol? datum)
          (or (eq? datum 'next)
            (eq? datum 'next-method)
            (eq? datum 'next-method-invoke))]
        [(pair? datum)
          (or (mentions-next? (car datum)) (mentions-next? (cdr datum)))]
        [(vector? datum)
          (let loop ([i 0])
            (and (< i (vector-length datum))
              (or (mentions-next? (vector-ref datum i)) (loop (+ i 1)))))]
        [else #f]))
    ;; A body that is exactly `(slot-ref v 'name)` / `(slot-set! v 'name w) w`
    ;; over the method's own arguments is a plain slot accessor: dispatch can
    ;; serve it without entering the body. Returns (values kind name), kind
    ;; being 'reader, 'writer or #f.
    (define (slot-access-of vars body rest-var tail-formals)
      (if (or rest-var (not (null? (syntax->datum tail-formals))))
        (values #f #f)
        (syntax-case body (slot-ref slot-set! quote)
          [((slot-ref v (quote name)))
            (and (identifier? #'v)
              (identifier? #'name)
              (= (length vars) 1)
              (bound-identifier=? #'v (car vars)))
            (values 'reader (syntax->datum #'name))]
          [((slot-set! v (quote name) w) w2)
            (and (identifier? #'v)
              (identifier? #'name)
              (identifier? #'w)
              (identifier? #'w2)
              (= (length vars) 2)
              (bound-identifier=? #'v (car vars))
              (bound-identifier=? #'w (cadr vars))
              (bound-identifier=? #'w2 (cadr vars)))
            (values 'writer (syntax->datum #'name))]
          [_ (values #f #f)])))
    ;; Builds a method procedure `(lambda (next var ... . tail) body ...)`.
    ;; `tail` holds Racket-style keyword formals (`#:key id`, `#:key [id
    ;; init]`, optionally ending in a rest identifier), which are parsed from
    ;; the trailing `kw value ...` arguments generics pass through.
    (define (method-lambda form leading tail body)
      (define (parse-tail xs specs)
        (syntax-case xs ()
          [() (values (reverse specs) #f)]
          [rst (identifier? #'rst) (values (reverse specs) #'rst)]
          [(kw [id init] . more)
            (keyword? (syntax->datum #'kw))
            (parse-tail #'more (cons #'(id kw init) specs))]
          [(kw id . more)
            (and (keyword? (syntax->datum #'kw)) (identifier? #'id))
            (parse-tail #'more
              (cons #'(id kw (error 'define-method "required keyword argument not supplied" 'kw))
                specs))]
          [_ (syntax-violation 'define-method
              "expected keyword formals as #:key id or #:key [id init]" form xs)]))
      (if (null? (syntax->datum tail))
        #`(lambda (#,@leading) #,@body)
        (let-values ([(specs rst) (parse-tail tail '())])
          (with-syntax ([(g) (generate-temporaries '(kws))])
            #`(lambda (#,@leading . g)
                (let-keywords* g #,(if rst (append specs rst) specs)
                  #,@body))))))
    ;; Next-method binding.
    ;; Cursor versus full next.
    (define (mentions-lead? datum)
      (cond
        [(symbol? datum)
          (or (eq? datum 'next) (eq? datum 'next-method-invoke))]
        [(pair? datum)
          (or (mentions-lead? (car datum)) (mentions-lead? (cdr datum)))]
        [(vector? datum)
          (let loop ([i 0])
            (and (< i (vector-length datum))
              (or (mentions-lead? (vector-ref datum i)) (loop (+ i 1)))))]
        [else #f]))
    (define (method-body-forms next-id next-method-id body mentions? cursor? vars)
      (cond
        [cursor?
          (with-syntax ([next-id next-id]
                        [next-method-id next-method-id]
                        [(body ...) body]
                        [(var ...) vars]
                        [(entry ...) (generate-temporaries vars)])
            #'((let ([entry var] ...)
                 (let ([next-method-id
                        (lambda new-args
                          (if (null? new-args)
                            (next-method-invoke-with next-id entry ...)
                            (apply next-method-invoke-with next-id new-args)))])
                   body
                   ...))))]
        [mentions?
          (with-syntax ([next-id next-id]
                        [next-method-id next-method-id]
                        [(body ...) body])
            #'((let ([next-method-id
                      (lambda new-args
                        (if (null? new-args)
                          (next-method-invoke next-id)
                          (apply next-method-invoke-with next-id new-args)))])
                body
                ...)))]
        [else
          (with-syntax ([(body ...) body])
            #'((let () body ...)))]))
    ;; declare-slots-inline forms.
    (define (declare-slots-inline-name? id)
      (and (identifier? id) (eq? (syntax->datum id) 'declare-slots-inline)))
    (define (split-slots-inline form body)
      (let loop ([body body] [declared '()])
        (syntax-case body ()
          [((f (var class)) . more)
            (and (declare-slots-inline-name? #'f) (identifier? #'var) (identifier? #'class))
            (loop #'more (cons (cons #'var #'class) declared))]
          [((f . bad) . more)
            (declare-slots-inline-name? #'f)
            (syntax-violation 'define-method
              "expected (declare-slots-inline (variable class))"
              form
              #'(f . bad))]
          [_ (values (reverse declared) body)])))
    ;; Known slot-ref rewrite.
    (define (rewrite-known-slot-refs forms pairs)
      (define (known-class v)
        (let loop ([pairs pairs])
          (cond
            [(null? pairs) #f]
            [(bound-identifier=? (caar pairs) v) (cdar pairs)]
            [else (loop (cdr pairs))])))
      (define (walk stx)
        (syntax-case stx (quote slot-ref)
          [(slot-ref v (quote name))
            (and (identifier? #'v) (identifier? #'name) (known-class #'v))
            (with-syntax ([class (known-class #'v)])
              #'(%slot-ref/known v class (quote name)))]
          [(quote _) stx]
          [(a . d) (with-syntax ([wa (walk #'a)] [wd (walk #'d)]) #'(wa . wd))]
          [_ stx]))
      (define (walk-forms forms)
        (syntax-case forms ()
          [() #'()]
          [(f . more)
            (with-syntax ([wf (walk #'f)] [(wm ...) (walk-forms #'more)])
              #'(wf wm ...))]))
      (if (null? pairs) forms (walk-forms forms)))
    (define (expand-method form generic args body locked? qualifiers)
      (receive (vars specializers required rest-var tail-formals)
        (parse-specialized-args args)
        (receive (declared body) (split-slots-inline form body)
        (receive (slot-kind slot-name)
          (slot-access-of vars body rest-var tail-formals)
        (let* ([known-pairs
                 (append
                   declared
                   (let loop ([vars vars] [specializers specializers])
                     (cond
                       [(or (null? vars) (null? specializers)) '()]
                       [(identifier? (car specializers))
                         (cons (cons (car vars) (car specializers))
                           (loop (cdr vars) (cdr specializers)))]
                       [else (loop (cdr vars) (cdr specializers))])))]
               [body (if slot-kind body (rewrite-known-slot-refs body known-pairs))]
               [checks (map (lambda (pair)
                              (with-syntax ([class (cdr pair)] [var (car pair)])
                                #'(%check-slots-inline! class (quote var))))
                         declared)]
               [next-id (datum->syntax x 'next)]
               ;; The binding takes the generic's context, so a body written
               ;; in a macro that wraps define-method still sees its next-method.
               [next-method-id (datum->syntax (if (identifier? generic) generic x) 'next-method)]
               [mentions? (mentions-next? (syntax->datum body))]
               [cursor? (and mentions?
                          (not rest-var)
                          (null? (syntax->datum tail-formals))
                          (not (mentions-lead? (syntax->datum body))))]
               [forms (method-body-forms next-id next-method-id body mentions? cursor? vars)])
        (with-syntax ([(check ...) checks]
                      [generic-id (generic-expression form generic)]
                      [slot-kind-value (datum->syntax x slot-kind)]
                      [slot-name-value (datum->syntax x slot-name)]
                      [qualifiers-value (datum->syntax x qualifiers)]
                      [no-next-value (not mentions?)]
                      [cursor-value cursor?]
                      [(var ...) vars]
                      [(specializer ...) specializers]
                      [required-count (datum->syntax form required)]
                      [locked-value locked?]
                      [next-id next-id]
                      [(form-body ...) forms])
         (with-syntax ([method-proc
                         (method-lambda form #'(next-id var ...) tail-formals
                           #'(form-body ...))])
          (if rest-var
            (with-syntax ([rest-id rest-var])
              #'(begin
                 check ...
                 (add-method! generic-id
                   (list specializer ...)
                   required-count
                   (lambda (next-id var ... . rest-id)
                     form-body ...)
                   locked-value
                   no-next-value
                   'slot-kind-value
                   'slot-name-value
                   'qualifiers-value
                   cursor-value)))
            #'(begin
               check ...
               (add-method! generic-id
                 (list specializer ...)
                 required-count
                 method-proc
                 locked-value
                 no-next-value
                 'slot-kind-value
                 'slot-name-value
                 'qualifiers-value
                 cursor-value))))))))))
    ;; `(define-method (name args ...) [qualifier ...] body ...)`, or the
    ;; same with the name bare and the qualifiers before the arguments.
    (syntax-case x ()
      [(_ (generic . args) body ...)
        (let-values ([(locked? qualifiers rest) (leading-qualifiers #'(body ...))])
          (expand-method x #'generic #'args rest locked? qualifiers))]
      [(_ generic form ...)
        (let-values ([(locked? qualifiers rest) (leading-qualifiers #'(form ...))])
          (syntax-case rest ()
            [(args body ...)
              (expand-method x #'generic #'args #'(body ...) locked? qualifiers)]
            [_ (syntax-violation 'define-method "invalid method definition" x)]))]
      [_ (syntax-violation 'define-method "invalid method definition" x)])))

(define <top> (%builtin-class 'top))
(define <bottom> (%builtin-class 'bottom))
(define <type> (%builtin-class 'type))
(define <object> (%builtin-class 'object))
(define <class> (%builtin-class 'class))
(define <pair> (%builtin-class 'pair))
(define <bool> (%builtin-class 'bool))
(define <null> (%builtin-class 'null))
(define <eof> (%builtin-class 'eof))
(define <void> (%builtin-class 'void))
(define <unspecified> (%builtin-class 'unspecified))
(define <undefined> (%builtin-class 'undefined))
(define <number> (%builtin-class 'number))
(define <fixnum> (%builtin-class 'fixnum))
(define <flonum> (%builtin-class 'flonum))
(define <bigint> (%builtin-class 'bigint))
(define <rational> (%builtin-class 'rational))
(define <complex> (%builtin-class 'complex))
(define <symbol> (%builtin-class 'symbol))
(define <keyword> (%builtin-class 'keyword))
(define <char> (%builtin-class 'char))
(define <string> (%builtin-class 'string))
(define <stringbuf-wide> (%builtin-class 'stringbuf-wide))
(define <stringbuf-narrow> (%builtin-class 'stringbuf-narrow))
(define <bytevector> (%builtin-class 'bytevector))
(define <immutable-bytevector> (%builtin-class 'immutable-bytevector))
(define <mapped-bytevector> (%builtin-class 'mapped-bytevector))
(define <vector> (%builtin-class 'vector))
(define <tuple> (%builtin-class 'tuple))
(define <hash-table> (%builtin-class 'hash-table))
(define <immutable-hash-table> (%builtin-class 'immutable-hash-table))
(define <persistent-map> (%builtin-class 'persistent-map))
(define <persistent-set> (%builtin-class 'persistent-set))
(define <weak-set> (%builtin-class 'weak-set))
(define <weak-table> (%builtin-class 'weak-table))
(define <weak-mapping> (%builtin-class 'weak-mapping))
(define <ephemeron> (%builtin-class 'ephemeron))
(define <box> (%builtin-class 'box))
(define <variable> (%builtin-class 'variable))
(define <fluid> (%builtin-class 'fluid))
(define <dynamic-state> (%builtin-class 'dynamic-state))
(define <closure> (%builtin-class 'closure))
(define <native-procedure> (%builtin-class 'native-procedure))
(define <native-continuation> (%builtin-class 'native-continuation))
(define <code-block> (%builtin-class 'code-block))
(define <code-buffer> (%builtin-class 'code-buffer))
(define <continuation> (%builtin-class 'continuation))
(define <environment> (%builtin-class 'environment))
(define <syntax> (%builtin-class 'syntax))
(define <syntax-transformer> (%builtin-class 'syntax-transformer))
(define <port> (%builtin-class 'port))
(define <socket> (%builtin-class 'socket))
(define <poller> (%builtin-class 'poller))
(define <ffi-pointer> (%builtin-class 'ffi-pointer))
(define <cif> (%builtin-class 'cif))
(define <thread> (%builtin-class 'thread))
(define <mutex> (%builtin-class 'mutex))
(define <condition> (%builtin-class 'condition))
(define <annotation> (%builtin-class 'annotation))
(define <continuation-marks> (%builtin-class 'continuation-marks))
(define <generic> (%builtin-class 'generic))
(define <method> (%builtin-class 'method))
(define <next-method> (%builtin-class 'next-method))
(define <slot-definition> (%builtin-class 'slot-definition))
(define <slot-accessor> (%builtin-class 'slot-accessor))
(define <metaobject> (%builtin-class 'metaobject))
(define <specializer> (%builtin-class 'specializer))
(define <eql-specializer> (%builtin-class 'eql-specializer))
(define <method-combination> (%builtin-class 'method-combination))
(define <funcallable-standard-object> (%builtin-class 'funcallable-standard-object))
(define <funcallable-standard-class> (%builtin-class 'funcallable-standard-class))
(define <standard-reader-method> (%builtin-class 'standard-reader-method))
(define <standard-writer-method> (%builtin-class 'standard-writer-method))
(define <standard-direct-slot-definition> (%builtin-class 'standard-direct-slot-definition))
(define <standard-effective-slot-definition> (%builtin-class 'standard-effective-slot-definition))
(define <forward-referenced-class> (%builtin-class 'forward-referenced-class))

;; Method mutators and dependents.

(define (%notify-dependents metaobject reason . args)
  (let ([dependents (%dependents metaobject)])
    (if (null? dependents)
      #f
      (for-each
        (lambda (dependent)
          (apply update-dependent metaobject dependent reason args))
        dependents))))

(define (add-method! generic . args)
  (let ([method (apply %add-method! generic args)])
    (%notify-dependents generic 'add-method method)
    method))

(define (remove-method! generic method)
  (let ([removed (%remove-method! generic method)])
    (if removed (%notify-dependents generic 'remove-method method))
    removed))

;; Generic precomputation.
(define *max-emf-precompute* (make-parameter 256))

(define (%subclasses-or-self class)
  (let loop ([todo (list class)] [seen '()])
    (cond
      [(null? todo) (reverse seen)]
      [(memq (car todo) seen) (loop (cdr todo) seen)]
      [else
        (loop (append (cdr todo) (class-direct-subclasses (car todo)))
          (cons (car todo) seen))])))

(define (%union-classes lists)
  (let loop ([lists lists] [out '()])
    (if (null? lists)
      (reverse out)
      (loop (cdr lists)
        (let add ([classes (car lists)] [out out])
          (cond
            [(null? classes) out]
            [(memq (car classes) out) (add (cdr classes) out)]
            [else (add (cdr classes) (cons (car classes) out))]))))))

;; Method dispatch width.
(define (%method-dispatch-width method)
  (let loop ([specializers (method-specializers method)] [index 0] [width 0])
    (cond
      [(null? specializers) width]
      [(eq? (car specializers) <top>) (loop (cdr specializers) (+ index 1) width)]
      [else (loop (cdr specializers) (+ index 1) (+ index 1))])))

(define (%position-candidates methods index)
  (let ([classes
          (let collect ([methods methods] [found '()])
            (if (null? methods)
              found
              (let ([specializers (method-specializers (car methods))])
                (collect (cdr methods)
                  (if (> (length specializers) index)
                    (let ([specializer (list-ref specializers index)])
                      (if (and (class? specializer)
                            (not (eq? specializer <top>))
                            (not (memq specializer found)))
                        (cons specializer found)
                        found))
                    found)))))])
    (if (null? classes)
      (list <top>)
      (%union-classes (map %subclasses-or-self classes)))))

(define (%default-precompute-lists gf)
  (let* ([methods (generic-methods gf)]
         [width (let loop ([methods methods] [width 0])
                  (if (null? methods)
                    width
                    (loop (cdr methods)
                      (max width (%method-dispatch-width (car methods))))))])
    (let loop ([index 0])
      (if (= index width)
        '()
        (cons (%position-candidates methods index) (loop (+ index 1)))))))

(define (generic-precompute! gf . class-lists)
  (let ([lists (if (null? class-lists)
                 (%default-precompute-lists gf)
                 (car class-lists))]
        [limit (*max-emf-precompute*)]
        [filled 0])
    (let fill ([lists lists] [prefix '()])
      (cond
        [(>= filled limit) #t]
        [(null? lists)
          (if (%generic-precompute-key! gf (reverse prefix))
            (set! filled (+ filled 1)))
          #t]
        [else
          (for-each
            (lambda (class) (fill (cdr lists) (cons class prefix)))
            (car lists))]))
    filled))

;; Known slot read.
(define (%slot-ref/known obj class name)
  (slot-ref obj name))

;; declare-slots-inline check.
(define (%check-slots-inline! class var)
  (unless (and (class? class) (class-sealed? class) (class-final? class))
    (error 'declare-slots-inline
      "class of the declared slot variable must be sealed and final"
      var
      class)))

(define (redefine-class! class . args)
  (let ([new (apply %redefine-class! class args)])
    (let loop ([todo (list new)] [seen '()])
      (if (null? todo)
        new
        (let ([c (car todo)])
          (if (memq c seen)
            (loop (cdr todo) seen)
            (begin
              (%notify-dependents c 'redefine-class new)
              (loop (append (cdr todo) (class-direct-subclasses c))
                (cons c seen)))))))))

(define (%initarg-ref initargs key default)
  (let ([name (keyword->symbol key)])
    (let scan ([rest initargs])
      (cond
        [(null? rest) default]
        [(eq? (keyword->symbol (car rest)) name) (cadr rest)]
        [else (scan (cddr rest))]))))

;; The class ensure-class-using-class may redefine: the value of NAME in
;; ENV when it is bound to a class, else #f.
(define (%environment-class env name)
  (let ([var (env-local-variable env name)])
    (and var
      (variable-bound? var)
      (let ([value (variable-ref var)])
        (and (class? value) value)))))

;; ensure-class-using-class.
(define-generic ensure-class-using-class 1)

(define (%class-declaration initargs)
  (values
    (%initarg-ref initargs #:direct-superclasses '())
    (%initarg-ref initargs #:direct-slots '())
    (%initarg-ref initargs #:default-initargs '())
    (%initarg-ref initargs #:metaclass #f)
    (%initarg-ref initargs #:applicable #f)))

(define-method (ensure-class-using-class (class <top>) name . initargs)
  (let-values ([(supers slots defaults metaclass applicable)
                (%class-declaration initargs)])
    (%with-class-protocol
      (if applicable
        (make-invocable-class name slots supers defaults)
        (make-class name slots supers metaclass defaults))
      supers)))

(define-method (ensure-class-using-class (class <class>) name . initargs)
  (let-values ([(supers slots defaults metaclass applicable)
                (%class-declaration initargs)])
    (%with-class-protocol
      (if applicable
        (redefine-invocable-class! class name slots supers defaults)
        (redefine-class! class name slots supers metaclass defaults))
      supers)))

;; Old class for ensure-class.
(define (%ensure-class-old-class name initargs)
  (let ([redefined (%initarg-ref initargs #:redefined 'environment)])
    (cond
      [(eq? redefined 'environment)
        (%environment-class (%initarg-ref initargs #:environment (expansion-env)) name)]
      [(procedure? redefined)
        ;; Unbound on first definition.
        (let ([value (false-if-exception (redefined))])
          (and (class? value) value))]
      [else #f])))

;; ensure-class entry point.
(define (ensure-class name . initargs)
  (let ([old (%ensure-class-old-class name initargs)])
    (if (and old (eq? (class-of old) <forward-referenced-class>))
      (%resolve-forward-reference! old (apply ensure-class-using-class #f name initargs))
      (apply ensure-class-using-class old name initargs))))

;; The class-finalization protocol.
;;
;; A class is built natively, with the computed precedence list and effective slots.
;; When a metaclass has a method of its own (not the boot library's stock one) on one of
;; `compute-class-precedence-list`, `compute-slots`, `finalize-inheritance` or
;; `validate-superclass`, the protocol runs after that: every direct superclass is checked
;; with `validate-superclass` and `finalize-inheritance` installs the lists the other two
;; return. Without such a method nothing is called and class creation costs what it did.
;; The check reads the methods at the time, so adding or removing one takes effect at once.
;;
;; `compute-class-precedence-list` returns a reordering of the class's computed list.
;; `compute-slots` returns the slot definitions of the class, in any order, each once, and
;; may add a slot with a spec (a symbol, or a list of a name and slot options); it may not
;; drop one. A class that is rejected by `validate-superclass` stays registered but is
;; not bound to its name.
(define-generic compute-class-precedence-list 1)
(define-method (compute-class-precedence-list (class <class>))
  (class-precedence-list class))

(define-generic compute-slots 1)
(define-method (compute-slots (class <class>))
  (if (%stock-methods-only? compute-effective-slot-definition class)
    (class-slots class)
    (map
      (lambda (slot)
        (let ([name (slot-definition-name slot)])
          (compute-effective-slot-definition class name (%direct-slots-named class name))))
      (class-slots class))))

(define-generic validate-superclass 1)
(define-method (validate-superclass (class <class>) super)
  (class? super))

(define (%slot-spec-name spec)
  (cond
    [(slot-definition? spec) (slot-definition-name spec)]
    [(pair? spec) (car spec)]
    [else spec]))

(define (%class-with-extra-slots class extras)
  (redefine-class! class (string->symbol (class-name class))
    (append
      (map (lambda (slot) (cons (slot-definition-name slot) (slot-definition-options slot)))
        (filter
          (lambda (slot) (not (memq (slot-definition-name slot) (map %slot-spec-name extras))))
          (class-direct-slots class)))
      (map (lambda (extra) (if (pair? extra) extra (list extra))) extras))
    (class-direct-supers class)
    (class-of class)
    (class-direct-default-initargs class)))

(define (%check-computed-slots! class slots)
  (let ([names (map %slot-spec-name slots)])
    (let scan ([rest names])
      (unless (null? rest)
        (when (memq (car rest) (cdr rest))
          (assertion-violation 'compute-slots "a slot is named twice" class (car rest)))
        (scan (cdr rest))))
    (for-each
      (lambda (slot)
        (unless (memq (slot-definition-name slot) names)
          (assertion-violation 'compute-slots "a slot of the class is missing" class
            (slot-definition-name slot))))
      (class-slots class))))

;; Slot definition protocol. The runtime's slot definitions are one native kind, so the
;; `...-class` generics name the class of the metaobject the runtime makes: a method must
;; return the standard class (a subclass of it is accepted and the native kind is used).
;; `compute-effective-slot-definition` may return the slot definition it is given or a spec
;; (a symbol, or a list of a name and slot options); a spec replaces the slot of that name
;; with a direct slot of the class being finalized.
(define-generic direct-slot-definition-class 1)
(define-method (direct-slot-definition-class (class <class>) . initargs)
  <standard-direct-slot-definition>)

(define-generic effective-slot-definition-class 1)
(define-method (effective-slot-definition-class (class <class>) . initargs)
  <standard-effective-slot-definition>)

(define-generic reader-method-class 1)
(define-method (reader-method-class (class <class>) direct-slot . initargs)
  <method>)

(define-generic writer-method-class 1)
(define-method (writer-method-class (class <class>) direct-slot . initargs)
  <method>)

(define-generic compute-effective-slot-definition 1)
(define-method (compute-effective-slot-definition (class <class>) name direct-slots)
  (let scan ([slots (class-slots class)])
    (cond
      [(null? slots) (car direct-slots)]
      [(eq? (slot-definition-name (car slots)) name) (car slots)]
      [else (scan (cdr slots))])))

(define (%direct-slots-named class name)
  (let loop ([classes (class-precedence-list class)] [found '()])
    (if (null? classes)
      (reverse found)
      (loop (cdr classes)
        (append (reverse (filter (lambda (slot) (eq? (slot-definition-name slot) name))
                           (class-direct-slots (car classes))))
          found)))))

(define (%check-slot-class who result base class)
  (unless (and (class? result) (subclass? result base))
    (assertion-violation who "the method returned something that is not the expected class"
      class result)))

(define (%slot-option options key)
  (let scan ([rest options])
    (cond
      [(or (null? rest) (null? (cdr rest))) #f]
      [(eq? (car rest) key) (cadr rest)]
      [else (scan (cddr rest))])))

;; Consult the `...-class` generics for every slot of CLASS.
(define (%consult-slot-classes class)
  (for-each
    (lambda (slot)
      (let ([options (slot-definition-options slot)])
        (%check-slot-class 'direct-slot-definition-class
          (apply direct-slot-definition-class class #:name (slot-definition-name slot) options)
          <slot-definition> class)
        (when (or (%slot-option options #:getter) (%slot-option options #:accessor))
          (%check-slot-class 'reader-method-class
            (apply reader-method-class class slot options) <method> class))
        (when (or (%slot-option options #:setter) (%slot-option options #:accessor))
          (%check-slot-class 'writer-method-class
            (apply writer-method-class class slot options) <method> class))))
    (class-direct-slots class))
  (for-each
    (lambda (slot)
      (%check-slot-class 'effective-slot-definition-class
        (effective-slot-definition-class class #:name (slot-definition-name slot))
        <slot-definition> class))
    (class-slots class)))

(define-generic finalize-inheritance 1)
(define-method (finalize-inheritance (class <class>))
  (%consult-slot-classes class)
  (let* ([cpl (compute-class-precedence-list class)]
         [slots (compute-slots class)])
    (%check-computed-slots! class slots)
    (let* ([extras (filter (lambda (slot) (not (slot-definition? slot))) slots)]
           [class (if (null? extras) class (%class-with-extra-slots class extras))])
      (if (equal? cpl (class-precedence-list class))
        class
        (%install-class-precedence! class cpl)))))

(%mark-stock-methods! compute-class-precedence-list)
(%mark-stock-methods! compute-slots)
(%mark-stock-methods! finalize-inheritance)
(%mark-stock-methods! validate-superclass)
(%mark-stock-methods! direct-slot-definition-class)
(%mark-stock-methods! effective-slot-definition-class)
(%mark-stock-methods! reader-method-class)
(%mark-stock-methods! writer-method-class)
(%mark-stock-methods! compute-effective-slot-definition)

(define (%class-protocol-custom? class)
  (not
    (and (%stock-methods-only? compute-class-precedence-list class)
      (%stock-methods-only? compute-slots class)
      (%stock-methods-only? finalize-inheritance class)
      (%stock-methods-only? validate-superclass class)
      (%stock-methods-only? direct-slot-definition-class class)
      (%stock-methods-only? effective-slot-definition-class class)
      (%stock-methods-only? reader-method-class class)
      (%stock-methods-only? writer-method-class class)
      (%stock-methods-only? compute-effective-slot-definition class))))

;; Run the protocol on CLASS, just built or redefined, when a method of its own applies.
(define (%with-class-protocol class supers)
  (if (%class-protocol-custom? class)
    (begin
      (for-each
        (lambda (super)
          (unless (validate-superclass class super)
            (assertion-violation 'ensure-class "validate-superclass rejected the superclass"
              class super)))
        supers)
      (finalize-inheritance class))
    class))

(define-generic add-dependent 1)
(define-generic remove-dependent 1)
(define-generic map-dependents 1)
(define-generic update-dependent 1)

(define-method (add-dependent (metaobject <top>) dependent)
  (%add-dependent! metaobject dependent))

(define-method (remove-dependent (metaobject <top>) dependent)
  (%remove-dependent! metaobject dependent))

(define-method (map-dependents (metaobject <top>) proc)
  (for-each proc (%dependents metaobject)))

;; Default update-dependent.
(define-method (update-dependent (metaobject <top>) dependent . initargs)
  #f)

(define-generic slot-ref-using-class 3)
(define-generic slot-set-using-class! 4)
(define-generic slot-bound-using-class? 3)
(define-generic slot-unbind-using-class! 3)
(define-generic slot-unbound)
(define-generic slot-missing)

(define-method (slot-ref-using-class (class <class>) obj slot . fallback)
  (apply %slot-ref-using-class class obj slot fallback))

(define-method (slot-set-using-class! (class <class>) obj slot value)
  (%slot-set-using-class! class obj slot value))

(define-method (slot-bound-using-class? (class <class>) obj slot)
  (%slot-bound-using-class? class obj slot))

(define-method (slot-unbind-using-class! (class <class>) obj slot)
  (%slot-unbind-using-class! class obj slot))

;; Stock slot protocol marks.
(%mark-boot-slot-protocol! slot-ref-using-class)
(%mark-boot-slot-protocol! slot-set-using-class!)
(%mark-boot-slot-protocol! slot-bound-using-class?)
(%mark-boot-slot-protocol! slot-unbind-using-class!)

(define-method (slot-unbound (class <class>) obj slot)
  (assertion-violation 'slot-unbound
    "slot is unbound"
    class
    slot))

(define-method (slot-missing (class <class>) obj slot . value)
  (assertion-violation 'slot-missing
    "class has no slot"
    class
    slot))

(define-generic class-post-initialize)

(define-method (class-post-initialize (class <class>) initargs)
  #f)

(define-generic initialize)
(%mark-boot-initialize! initialize)

(define-method (initialize obj initargs)
  (apply initialize-instance obj initargs))

(define-generic make 1)

;; Initarg supplied test.
(define (%initarg-supplied? key initargs)
  (let ([name (keyword->symbol key)])
    (let scan ([rest initargs])
      (cond
        [(null? rest) #f]
        [(eq? (keyword->symbol (car rest)) name) #t]
        [else (scan (cddr rest))]))))

;; Initargs with defaults.
(define (%initargs-with-defaults class initargs)
  (let loop ([defaults (class-default-initargs class)] [extra '()])
    (cond
      [(null? defaults)
        (if (null? extra) initargs (append initargs (reverse extra)))]
      [(%initarg-supplied? (caar defaults) initargs)
        (loop (cdr defaults) extra)]
      [else
        (let ([value ((cadar defaults))])
          (loop (cdr defaults) (cons value (cons (caar defaults) extra))))])))

(define %make-slow
  (lambda (next class . initargs)
    (let* ([initargs (%initargs-with-defaults class initargs)]
           [obj (if (%stock-methods-only? allocate-instance class)
                  (%make-instance-list class initargs)
                  (apply allocate-instance class initargs))])
      (initialize
        (or (and (procedure? obj) (procedure-property obj 'instance)) obj)
        initargs)
      obj)))

;; The stock method is a native body: while `initialize` keeps only its
;; default method it allocates straight from the frame (no rest list, no
;; `apply`, no `initialize` dispatch), else it runs `%make-slow`.
(add-method! make
  (list <class>)
  1
  (%make-method-body initialize
    (method-body (car (generic-methods initialize)))
    %make-slow)
  #f
  #t)

;; Instance lifecycle (AMOP).
;;
;; `make` allocates with `allocate-instance` and then calls `initialize`, whose stock method calls
;; `initialize-instance`, whose stock method calls `shared-initialize` with slot names `#t`.
;; Allocation already stores the initargs and initforms natively, so the stock methods do no work
;; of their own and `make` keeps its native fast path. The first method a program adds to
;; `allocate-instance`, `initialize-instance` or `shared-initialize` turns that fast path off for
;; the process and `make` runs the whole chain. `initialize` is kept for compatibility.
(define-generic allocate-instance 1)
(define-method (allocate-instance (class <class>) . initargs)
  (%make-instance-list class initargs))

(define-generic shared-initialize 1)
(define (%initarg-find initargs name)
  (let scan ([rest initargs])
    (cond
      [(or (null? rest) (null? (cdr rest))) #f]
      [(eq? (keyword->symbol (car rest)) name) rest]
      [else (scan (cddr rest))])))

;; Slots named in an initarg take its value. A slot in SLOT-NAMES (or any, for #t) that is
;; unbound takes its initform.
(define-method (shared-initialize (instance <top>) slot-names . initargs)
  (for-each
    (lambda (slot)
      (let* ([name (slot-definition-name slot)]
             [key (slot-definition-init-keyword slot)]
             [found (and key (%initarg-find initargs key))])
        (cond
          [found (slot-set! instance name (cadr found))]
          [(and (or (eq? slot-names #t) (memq name slot-names))
                (not (slot-bound? instance name)))
            (let ([thunk (slot-definition-init-thunk slot)])
              (cond
                [(procedure? thunk) (slot-set! instance name (thunk))]
                [(%slot-definition-has-init-value? slot)
                  (slot-set! instance name (slot-definition-init-value slot))]))])))
    (class-slots (class-of instance)))
  instance)

(define-generic initialize-instance 1)
(define-method (initialize-instance (instance <top>) . initargs)
  (when (not (%stock-methods-only? shared-initialize instance))
    (apply shared-initialize instance #t initargs))
  instance)

(define-generic reinitialize-instance 1)
(define-method (reinitialize-instance (instance <top>) . initargs)
  (apply shared-initialize instance '() initargs)
  instance)

(define-generic update-instance-for-different-class 1)
(define-method (update-instance-for-different-class (previous <top>) current . initargs)
  (let ([before (map slot-definition-name (class-slots (class-of previous)))])
    (apply shared-initialize current
      (filter (lambda (name) (not (memq name before)))
        (map slot-definition-name (class-slots (class-of current))))
      initargs)
    current))

;; A copy of INSTANCE as it is now, in its own class.
(define (%instance-copy instance)
  (let* ([class (class-of instance)]
         [copy (%make-instance-list class '())])
    (for-each
      (lambda (slot)
        (let ([name (slot-definition-name slot)])
          (when (slot-bound? instance name)
            (slot-set! copy name (slot-ref instance name)))))
      (class-slots class))
    copy))

(define-generic change-class 2)
(define-method (change-class (instance <top>) (new-class <class>) . initargs)
  (let ([previous (%instance-copy instance)])
    (%change-class instance new-class)
    (apply update-instance-for-different-class previous instance initargs)
    instance))

(%mark-stock-methods! allocate-instance)
(%mark-stock-methods! shared-initialize)
(%mark-stock-methods! initialize-instance)
(%mark-stock-methods! reinitialize-instance)
(%mark-stock-methods! update-instance-for-different-class)
(%mark-stock-methods! change-class)
(%mark-lifecycle-generic! allocate-instance)
(%mark-lifecycle-generic! shared-initialize)
(%mark-lifecycle-generic! initialize-instance)

(define describe-details
  (let ([flag #f])
    (lambda args
      (cond
        [(null? args) flag]
        [(null? (cdr args))
          (let ([value (car args)]
                [old flag])
            (unless (boolean? value)
              (assertion-violation
                'describe-details
                "expected boolean"
                value))
            (set! flag value)
            old)]
        [else
          (assertion-violation
            'describe-details
            "expected zero or one argument"
            args)]))))

(define (%describe-hidden-slot? slot-name)
  (let ([name (symbol->string slot-name)])
    (and (> (string-length name) 0)
      (char=? (string-ref name 0) #\%))))

(define (describe-common obj)
  (format #t "~s is an instance of class ~a~%" obj (class-name (class-of obj)))
  (values))

(define-generic describe-slots)

(define-method (describe-slots obj)
  (let ([slots (class-slots (class-of obj))])
    (unless (null? slots)
      (format #t "slots:~%")
      (for-each
        (lambda (slot)
          (let ([name (slot-definition-name slot)])
            (when (or (describe-details)
                   (not (%describe-hidden-slot? name)))
              (format #t "  ~s: " name)
              (if (slot-bound? obj name)
                (write (slot-ref obj name))
                (display "#<unbound>"))
              (newline))))
        slots)))
  (values))

(define-generic describe)

(define-method (describe obj)
  (describe-common obj)
  (describe-slots obj)
  (values))

(define-generic x->string)

(define-method (x->string (obj <string>)) obj)
(define-method (x->string (obj <number>)) (number->string obj))
(define-method (x->string (obj <symbol>)) (symbol->string obj))
(define-method (x->string (obj <char>)) (string obj))

(define-generic x->number)

(define-method (x->number (obj <number>)) obj)
(define-method (x->number (obj <string>)) (or (string->number obj) 0))
(define-method (x->number (obj <char>)) (char->integer obj))
(define-method (x->number obj) 0)

(define-generic x->integer)

(define-method (x->integer (obj <fixnum>)) obj)
(define-method (x->integer (obj <bigint>)) obj)
(define-method (x->integer (obj <number>))
  (inexact->exact (round (real-part obj))))
(define-method (x->integer (obj <char>)) (char->integer obj))
(define-method (x->integer obj) (x->integer (x->number obj)))

(define-generic ref)
(define-generic (setter ref))

(define-method (ref (obj <top>) (slot <symbol>))
  (slot-ref obj slot))
(define-method (ref (obj <top>) (slot <symbol>) fallback)
  (if (and (slot-exists? obj slot) (slot-bound? obj slot))
    (slot-ref obj slot)
    fallback))
(define-method ((setter ref) (obj <top>) (slot <symbol>) value)
  (slot-set! obj slot value))

(define-method (ref (obj <pair>) (index <fixnum>))
  (list-ref obj index))
(define-method ((setter ref) (obj <pair>) (index <fixnum>) value)
  (set-car! (list-tail obj index) value))

(define-method (ref (obj <vector>) (index <fixnum>))
  (vector-ref obj index))
(define-method ((setter ref) (obj <vector>) (index <fixnum>) value)
  (vector-set! obj index value))

(define-method (ref (obj <string>) (index <fixnum>))
  (string-ref obj index))
(define-method ((setter ref) (obj <string>) (index <fixnum>) value)
  (string-set! obj index value))

(define-method (ref (obj <bytevector>) (index <fixnum>))
  (bytevector-u8-ref obj index))
(define-method (ref (obj <immutable-bytevector>) (index <fixnum>))
  (bytevector-u8-ref obj index))
(define-method (ref (obj <mapped-bytevector>) (index <fixnum>))
  (bytevector-u8-ref obj index))
(define-method ((setter ref) (obj <bytevector>) (index <fixnum>) value)
  (bytevector-u8-set! obj index value))

(define-method (ref (obj <hash-table>) key)
  (core-hash-ref obj key #f))
(define-method (ref (obj <hash-table>) key fallback)
  (core-hash-ref obj key fallback))
(define-method (ref (obj <immutable-hash-table>) key)
  (core-hash-ref obj key #f))
(define-method (ref (obj <immutable-hash-table>) key fallback)
  (core-hash-ref obj key fallback))
(define-method (ref (obj <persistent-map>) key)
  (core-persistent-map-ref obj key #f))
(define-method (ref (obj <persistent-map>) key fallback)
  (core-persistent-map-ref obj key fallback))
(define-method (ref (obj <persistent-set>) key)
  (core-persistent-set-contains? obj key))
(define-method ((setter ref) (obj <hash-table>) key value)
  (core-hash-set! obj key value)
  value)

(define (%box-ref-field obj field)
  (case field
    [(* 0) (unbox obj)]
    [else
      (assertion-violation
        'ref
        "expected box field '* or 0"
        field)]))

(define-method (ref (obj <box>) field)
  (%box-ref-field obj field))

(define-method ((setter ref) (obj <box>) field value)
  (case field
    [(* 0)
      (set-box! obj value)
      value]
    [else
      (assertion-violation
        'setter-of-ref
        "expected box field '* or 0"
        field)]))

(define (~ obj selector . more)
  (if (null? more)
    (ref obj selector)
    (apply ~ (ref obj selector) more)))

(define (%set-~ obj selector . rest)
  (cond
    [(null? rest)
      (assertion-violation 'setter-of-~ "missing value" obj selector)]
    [(null? (cdr rest))
      ((setter ref) obj selector (car rest))]
    [else
      (apply (setter ~) (ref obj selector) rest)]))

(set-setter! ~ %set-~)

(define ref* ~)

;; Runtime hooks for Rust, see `install-runtime-hook!`.
(install-runtime-hook! 'slot-ref-using-class slot-ref-using-class)
(install-runtime-hook! 'slot-set-using-class! slot-set-using-class!)
(install-runtime-hook! 'slot-bound-using-class? slot-bound-using-class?)
(install-runtime-hook! 'slot-unbind-using-class! slot-unbind-using-class!)
(install-runtime-hook! 'slot-unbound slot-unbound)
(install-runtime-hook! 'slot-missing slot-missing)
(install-runtime-hook! 'slot-ref slot-ref)
(install-runtime-hook! 'slot-set! slot-set!)

;; Standard combination.

(define-generic no-applicable-method)
(define-method (no-applicable-method gf . args)
  (assertion-violation 'generic-invoke "no applicable method" gf))

;; Called by the runtime when a generic has no method for a call and no fallback.
(install-runtime-hook! 'no-applicable-method no-applicable-method)

(define-generic no-primary-method)
(define-method (no-primary-method gf . args)
  (assertion-violation 'generic-invoke "no primary method" gf))

(define-generic no-next-method)
(define-method (no-next-method gf method . args)
  (assertion-violation 'next-method-invoke "no next method" method))

;; Next-method chain.
(define (%call-chain gf methods args tail)
  (let ([m (car methods)])
    (apply (method-body m)
      (if (%method-no-next? m) #f (%make-next-method gf methods args tail))
      args)))

;; Run before/after methods.
(define (%run-methods methods args)
  (let loop ([ms methods])
    (when (pair? ms)
      (apply (method-body (car ms)) #f args)
      (loop (cdr ms)))))

;; Combined body without arounds.
(define (%combined-main gf befores prims afters args)
  (%run-methods befores args)
  (if (null? afters)
    (%call-chain gf prims args #f)
    (call-with-values
      (lambda () (%call-chain gf prims args #f))
      (lambda results
        (%run-methods afters args)
        (if (and (pair? results) (null? (cdr results)))
          (car results)
          (apply values results))))))

;; Fixed-arity EMF.
(define-syntax %emf-run
  (syntax-rules ()
    [(_ methods arg ...)
     (let loop ([ms methods])
       (when (pair? ms)
         ((method-body (car ms)) #f arg ...)
         (loop (cdr ms))))]))

(define-syntax %emf-chain
  (syntax-rules ()
    [(_ gf methods arg ...)
     (let ([m (car methods)])
       ((method-body m)
         (if (%method-no-next? m) #f (%make-next-method gf methods (list arg ...) #f))
         arg ...))]))

(define-syntax %define-emf-main
  (syntax-rules ()
    [(_ name (arg ...))
     (define (name gf befores prims afters arg ...)
       (%emf-run befores arg ...)
       (if (null? afters)
         (%emf-chain gf prims arg ...)
         (call-with-values
           (lambda () (%emf-chain gf prims arg ...))
           (lambda results
             (%emf-run afters arg ...)
             (if (and (pair? results) (null? (cdr results)))
               (car results)
               (apply values results))))))]))

(%define-emf-main %emf-main-1 (a))
(%define-emf-main %emf-main-2 (a b))
(%define-emf-main %emf-main-3 (a b c))

;; Standard EMF.
(define-syntax %emf-run-bodies
  (syntax-rules ()
    [(_ bodies arg ...)
     (let loop ([bs bodies])
       (when (pair? bs)
         ((car bs) #f arg ...)
         (loop (cdr bs))))]))

(define-syntax %emf-fast
  (syntax-rules ()
    [(_ befores prim afters arg ...)
     (begin
       (%emf-run-bodies befores arg ...)
       (if (null? afters)
         (prim #f arg ...)
         (call-with-values
           (lambda () (prim #f arg ...))
           (lambda results
             (%emf-run-bodies afters arg ...)
             (if (and (pair? results) (null? (cdr results)))
               (car results)
               (apply values results))))))]))

(define (%standard-emf-fast befores prim afters)
  (lambda (_next . args)
    (cond
      [(and (pair? args) (null? (cdr args)))
       (let ([a (car args)])
         (%emf-fast befores prim afters a))]
      [(and (pair? args) (pair? (cdr args)) (null? (cddr args)))
       (let ([a (car args)] [b (cadr args)])
         (%emf-fast befores prim afters a b))]
      [(and (pair? args) (pair? (cdr args)) (pair? (cddr args))
          (null? (cdddr args)))
       (let ([a (car args)] [b (cadr args)] [c (caddr args)])
         (%emf-fast befores prim afters a b c))]
      [else
        (%run-bodies-list befores args)
        (if (null? afters)
          (apply prim #f args)
          (call-with-values
            (lambda () (apply prim #f args))
            (lambda results
              (%run-bodies-list afters args)
              (apply values results))))])))

;; One-before/after EMF.
(define-syntax %emf1-fast
  (syntax-rules ()
    [(_ before prim after arg ...)
     (begin
       (before #f arg ...)
       (call-with-values
         (lambda () (prim #f arg ...))
         (lambda results
           (after #f arg ...)
           (if (and (pair? results) (null? (cdr results)))
             (car results)
             (apply values results)))))]))

(define (%standard-emf-fast1 before prim after)
  (lambda (_next . args)
    (cond
      [(and (pair? args) (null? (cdr args)))
       (let ([a (car args)])
         (%emf1-fast before prim after a))]
      [(and (pair? args) (pair? (cdr args)) (null? (cddr args)))
       (let ([a (car args)] [b (cadr args)])
         (%emf1-fast before prim after a b))]
      [(and (pair? args) (pair? (cdr args)) (pair? (cddr args))
          (null? (cdddr args)))
       (let ([a (car args)] [b (cadr args)] [c (caddr args)])
         (%emf1-fast before prim after a b c))]
      [else
        (apply before #f args)
        (call-with-values
          (lambda () (apply prim #f args))
          (lambda results
            (apply after #f args)
            (apply values results)))])))

(define (%run-bodies-list bodies args)
  (let loop ([bs bodies])
    (when (pair? bs)
      (apply (car bs) #f args)
      (loop (cdr bs)))))

(define (%standard-emf gf methods)
  (let loop ([ms methods] [arounds '()] [befores '()] [prims '()] [afters '()])
    (if (pair? ms)
      (let ([m (car ms)])
        (case (%method-role m)
          [(around) (loop (cdr ms) (cons m arounds) befores prims afters)]
          [(before) (loop (cdr ms) arounds (cons m befores) prims afters)]
          [(after) (loop (cdr ms) arounds befores prims (cons m afters))]
          [(primary) (loop (cdr ms) arounds befores (cons m prims) afters)]
          [else
            (assertion-violation '%standard-emf
              "qualifier not valid for the standard method combination"
              m)]))
      ;; Afters least specific first.
      (let ([arounds (reverse arounds)]
            [befores (reverse befores)]
            [prims (reverse prims)])
        (cond
          [(null? prims)
           (lambda (_next . args) (apply no-primary-method gf args))]
          [(and (null? arounds) (%method-no-next? (car prims))
                (pair? befores) (null? (cdr befores))
                (pair? afters) (null? (cdr afters)))
           (%standard-emf-fast1 (method-body (car befores))
             (method-body (car prims))
             (method-body (car afters)))]
          [(and (null? arounds) (%method-no-next? (car prims)))
           (%standard-emf-fast (map method-body befores)
             (method-body (car prims))
             (map method-body afters))]
          [(null? arounds)
           ;; 1-3 arg fast path.
           (lambda (_next . args)
             (cond
               [(and (pair? args) (null? (cdr args)))
                (%emf-main-1 gf befores prims afters (car args))]
               [(and (pair? args) (pair? (cdr args)) (null? (cddr args)))
                (%emf-main-2 gf befores prims afters (car args) (cadr args))]
               [(and (pair? args) (pair? (cdr args)) (pair? (cddr args))
                  (null? (cdddr args)))
                (%emf-main-3 gf befores prims afters
                  (car args) (cadr args) (caddr args))]
               [else (%combined-main gf befores prims afters args)]))]
          [else
           (lambda (_next . args)
             (%call-chain gf arounds args
               (lambda args (%combined-main gf befores prims afters args))))])))))

;; Effective method.
(define (%effective-method-of combination gf methods)
  (let ([builder (%method-combination-builder combination)])
    (if builder
      (builder gf methods (method-combination-options combination))
      (%standard-emf gf methods))))

;; Combined dispatch.
;;
;; Reached on a cache miss with the methods the runtime ranked. When the generic's class has a
;; method of its own (not the boot library's stock one) on `compute-applicable-methods`,
;; `compute-applicable-methods-using-classes` or `compute-effective-method`, the protocol
;; decides instead: see `%dispatch-with-protocol`. The check reads the methods at the time of the
;; miss. A generic that already holds a cache entry consults the protocol again at its next
;; refill: when a method of its own is added or removed, or on `generic-flush-cache!`.
(define (%generic-dispatch-emf gf epoch methods . args)
  (let ([custom-applicable?
          (not
            (and (%stock-methods-only? compute-applicable-methods gf)
              (%stock-methods-only? compute-applicable-methods-using-classes gf)))]
        [custom-emf? (not (%stock-methods-only? compute-effective-method gf))])
    (if (or custom-applicable? custom-emf?)
      (%dispatch-with-protocol gf epoch methods args custom-applicable?)
      (let ([emf (%effective-method-of (generic-method-combination gf) gf methods)])
        (%generic-cache-emf! gf epoch emf args)
        (apply emf #f args)))))

;; The methods for ARGS under a user `compute-applicable-methods[-using-classes]`, and whether
;; the answer may be cached (it depends on the classes of the arguments alone).
(define (%protocol-applicable-methods gf args)
  (define (by-arguments)
    (values
      (sort-applicable-methods gf (compute-applicable-methods gf args) args)
      #f))
  (if (%stock-methods-only? compute-applicable-methods-using-classes gf)
    (by-arguments)
    (let-values ([(chosen definitive?)
                  (compute-applicable-methods-using-classes gf (map class-of args))])
      (if definitive?
        (values chosen #t)
        (by-arguments)))))

(define (%dispatch-with-protocol gf epoch methods args custom-applicable?)
  (let-values ([(chosen cacheable?)
                (if custom-applicable?
                  (%protocol-applicable-methods gf args)
                  (values methods #t))])
    (if (null? chosen)
      (apply no-applicable-method gf args)
      (let ([emf (compute-effective-method gf (generic-method-combination gf) chosen)])
        (when cacheable? (%generic-cache-emf! gf epoch emf args))
        (apply emf #f args)))))

(install-runtime-hook! '%generic-dispatch-emf %generic-dispatch-emf)

;; Values spread.
(define (%values-spread lst) (apply values lst))

(install-runtime-hook! '%values-spread %values-spread)

;; Invocation generics.

;; Applicable methods.
(define-generic compute-applicable-methods)
(define-method (compute-applicable-methods gf args)
  (let ([applicable (generic-applicable-methods gf args)])
    (filter (lambda (m) (memq m applicable)) (generic-methods gf))))

;; Method applicability.
(define (%method-applicability method classes)
  (and (>= (length classes) (method-required-arg-count method))
    (let loop ([specializers (method-specializers method)]
               [classes classes]
               [kind 'class])
      (cond
        [(null? specializers) kind]
        [(null? classes) #f]
        [(eql-specializer? (car specializers))
          (and (eq? (car classes)
                 (class-of (eql-specializer-object (car specializers))))
            (loop (cdr specializers) (cdr classes) 'eql))]
        [(subclass? (car classes) (car specializers))
          (loop (cdr specializers) (cdr classes) kind)]
        [else #f]))))

;; Methods by class.
(define-generic compute-applicable-methods-using-classes)
(define-method (compute-applicable-methods-using-classes gf classes)
  (let loop ([ms (generic-methods gf)] [eqls '()] [by-class '()])
    (if (null? ms)
      (values
        (append (reverse eqls)
          (list-sort
            (lambda (left right) (method-more-specific? left right classes))
            (reverse by-class)))
        (null? eqls))
      (let ([kind (%method-applicability (car ms) classes)])
        (cond
          [(not kind) (loop (cdr ms) eqls by-class)]
          [(eq? kind 'eql) (loop (cdr ms) (cons (car ms) eqls) by-class)]
          [else (loop (cdr ms) eqls (cons (car ms) by-class))])))))

;; Effective method entry.
(define-generic compute-effective-method)
(define-method (compute-effective-method gf combination methods)
  (%effective-method-of combination gf methods))

(define-generic compute-effective-method-function)
(define-method (compute-effective-method-function gf methods)
  (%effective-method-of (generic-method-combination gf) gf methods))

(%mark-stock-methods! compute-applicable-methods)
(%mark-stock-methods! compute-applicable-methods-using-classes)
(%mark-stock-methods! compute-effective-method)

;; Method lambda.
(define-generic make-method-lambda)
(define-method (make-method-lambda gf method lambda-expr env)
  (values lambda-expr '()))

;; Lambda list.
(define (extract-lambda-list specialized-lambda-list)
  (let loop ([params specialized-lambda-list])
    (if (pair? params)
      (cons (if (pair? (car params)) (caar params) (car params))
        (loop (cdr params)))
      params)))

;; Specializer names.
(define (extract-specializer-names specialized-lambda-list)
  (let loop ([params specialized-lambda-list])
    (if (pair? params)
      (cons (if (and (pair? (car params)) (pair? (cdr (car params))))
              (cadr (car params))
              't)
        (loop (cdr params)))
      '())))

;; Method combinations.

(define %method-combinations (make-core-hash-eq))

(define (%register-combination! name combination)
  (core-hash-put! %method-combinations name combination)
  combination)

(define (find-method-combination name)
  (or (core-hash-ref %method-combinations name #f)
    (assertion-violation 'find-method-combination
      "unknown method combination"
      name)))

(define (set-generic-method-combination! gf spec)
  (let ([name (if (pair? spec) (car spec) spec)]
        [options (if (pair? spec) (cdr spec) '())])
    (let ([combination (find-method-combination name)])
      (%set-generic-combination! gf combination options)
      (%notify-dependents gf 'set-combination combination))))

(define (%call-one gf m args)
  (%call-chain gf (list m) args #f))

(define (%member-option? key options)
  (and (member key options) #t))

(define (%combination-emf gf arounds prims run identity-one?)
  (if (null? prims)
    (lambda (_next . args) (apply no-primary-method gf args))
    (let ([main (if (and identity-one? (null? (cdr prims)))
                  (lambda args (%call-one gf (car prims) args))
                  (lambda args (run gf prims args)))])
      (if (null? arounds)
        (lambda (_next . args) (apply main args))
        (lambda (_next . args) (%call-chain gf arounds args main))))))

(define (%short-combination-builder name keyword run identity-one?)
  (lambda (gf methods options)
    (for-each
      (lambda (m)
        (let ([qualifiers (method-qualifiers m)])
          (unless (or (equal? qualifiers (list keyword))
                  (equal? qualifiers '(#:around)))
            (assertion-violation name
              "invalid-method-error: qualifiers not valid for this method combination"
              m))))
      methods)
    (let* ([arounds (filter (lambda (m) (equal? (method-qualifiers m) '(#:around)))
                      methods)]
           [prims (filter (lambda (m) (equal? (method-qualifiers m) (list keyword)))
                    methods)]
           [prims (if (%member-option? '#:most-specific-last options)
                    (reverse prims)
                    prims)])
      (%combination-emf gf arounds prims run identity-one?))))

;; Primary values.
(define (%call-each gf prims args)
  (if (null? prims)
    '()
    (let ([v (%call-one gf (car prims) args)])
      (cons v (%call-each gf (cdr prims) args)))))

(define (%operator-run op)
  (lambda (gf prims args)
    (apply op (%call-each gf prims args))))

(define (%and-run gf prims args)
  (let loop ([ps prims])
    (let ([v (%call-one gf (car ps) args)])
      (if (or (null? (cdr ps)) (not v)) v (loop (cdr ps))))))

(define (%or-run gf prims args)
  (let loop ([ps prims])
    (let ([v (%call-one gf (car ps) args)])
      (if (or (null? (cdr ps)) v) v (loop (cdr ps))))))

(define (%progn-run gf prims args)
  (let loop ([ps prims])
    (let ([v (%call-one gf (car ps) args)])
      (if (null? (cdr ps)) v (loop (cdr ps))))))

(define (%register-run-combination! name run identity-one?)
  (%register-combination! name
    (%make-method-combination name
      (%short-combination-builder name (symbol->keyword name) run identity-one?))))

;; Group spec.

(define (%pattern-matches? patterns qualifiers)
  (let loop ([ps patterns])
    (cond
      [(null? ps) #f]
      [(or (eq? (car ps) '*) (equal? (car ps) qualifiers)) #t]
      [else (loop (cdr ps))])))

(define (%group-index specs qualifiers)
  (let loop ([specs specs] [index 0])
    (cond
      [(null? specs) #f]
      [(%pattern-matches? (cadr (car specs)) qualifiers) index]
      [else (loop (cdr specs) (+ index 1))])))

;; Grouped methods.
(define (%partition-methods name methods specs)
  (let ([buckets (make-vector (length specs) '())])
    (for-each
      (lambda (m)
        (let ([index (%group-index specs (method-qualifiers m))])
          (unless index
            (assertion-violation name "method matches no method group" m))
          (vector-set! buckets index (cons m (vector-ref buckets index)))))
      methods)
    (let loop ([specs specs] [index 0])
      (if (null? specs)
        '()
        (let* ([spec (car specs)]
               [bucket (vector-ref buckets index)]
               [members (if (equal? (list-ref spec 2) '#:most-specific-last)
                          bucket
                          (reverse bucket))])
          (when (and (list-ref spec 3) (null? members))
            (assertion-violation name "required method group is empty" (car spec)))
          (cons members (loop (cdr specs) (+ index 1))))))))

(define (%long-combination-builder name specs body)
  (lambda (gf methods options)
    (%emf-from-description gf
      (apply body (%partition-methods name methods specs)))))

;; Method descriptions.
(define (call-method m next-methods)
  (unless (method? m)
    (assertion-violation 'call-method "not a method" m))
  (vector '%call-method m next-methods))

(define (make-method proc)
  (unless (procedure? proc)
    (assertion-violation 'make-method "not a procedure" proc))
  (vector '%make-method proc))

;; Description items.
(define (%call-desc? d)
  (and (vector? d) (= (vector-length d) 3) (eq? (vector-ref d 0) '%call-method)))

(define (%make-desc? d)
  (and (vector? d) (= (vector-length d) 2) (eq? (vector-ref d 0) '%make-method)))

(define (%description-items d)
  (let loop ([items (list d)])
    (cond
      [(null? items) '()]
      [(method? (car items)) (cons (car items) (loop (cdr items)))]
      [(%make-desc? (car items)) (cons (car items) (loop (cdr items)))]
      [(%call-desc? (car items))
        (loop (cons (vector-ref (car items) 1)
                (append (vector-ref (car items) 2) (cdr items))))]
      [else
        (assertion-violation 'define-method-combination
          "not a method combination description"
          (car items))])))

(define (%run-items gf items args)
  (if (method? (car items))
    (let split ([rest items] [methods '()])
      (if (and (pair? rest) (method? (car rest)))
        (split (cdr rest) (cons (car rest) methods))
        (%call-chain gf (reverse methods) args
          (if (null? rest)
            #f
            (lambda more (%run-items gf rest more))))))
    (apply (vector-ref (car items) 1) args)))

(define (%emf-from-description gf description)
  (let ([items (%description-items description)])
    (lambda (_next . args)
      (%run-items gf items args))))

(define-syntax define-method-combination
  (lambda (x)
    (define (keyword-named? datum name)
      (and (keyword? datum) (eq? (keyword->symbol datum) name)))
    (define (parse-group datum)
      ;; Group datum.
      (let ()
        (define (bad)
          (syntax-violation 'define-method-combination
            "expected (group pattern ... option ...)"
            x
            datum))
        (unless (and (pair? datum) (symbol? (car datum))) (bad))
        (let loop ([items (cdr datum)] [patterns '()]
                   [order '#:most-specific-first] [required #f])
          (cond
            [(null? items)
              (list (car datum) (reverse patterns) order required)]
            [(keyword? (car items))
              (when (or (null? (cdr items)) (not (keyword? (car items)))) (bad))
              (cond
                [(keyword-named? (car items) 'order)
                  (unless (memq (cadr items)
                            '(#:most-specific-first #:most-specific-last))
                    (bad))
                  (loop (cddr items) patterns (cadr items) required)]
                [(keyword-named? (car items) 'required)
                  (loop (cddr items) patterns order (cadr items))]
                [else (bad)])]
            [(eq? (car items) '*)
              (loop (cdr items) (cons '* patterns) order required)]
            [(list? (car items))
              (loop (cdr items) (cons (car items) patterns) order required)]
            [else (bad)]))))
    (syntax-case x ()
      [(_ name kw op kw2 flag)
        (and (keyword-named? (syntax->datum #'kw) 'operator)
          (keyword-named? (syntax->datum #'kw2) 'identity-with-one-argument))
        #'(%register-run-combination! 'name (%operator-run op) flag)]
      [(_ name (option ...) (group ...) body1 body ...)
        ;; Group names.
        (let* ([group-datums (syntax->datum #'(group ...))]
               [specs (map parse-group group-datums)]
               [group-ids (map (lambda (g) (datum->syntax #'name (car g))) group-datums)])
          (with-syntax ([(group-id ...) group-ids]
                        [specs (datum->syntax #'name (list 'quote specs))])
            #'(%register-combination! 'name
                (%make-method-combination 'name
                  (%long-combination-builder 'name specs
                    (lambda (group-id ...)
                      (let () body1 body ...)))))))]
      [_ (syntax-violation 'define-method-combination
           "expected (define-method-combination name #:operator op #:identity-with-one-argument bool) or (define-method-combination name (option ...) ((group pattern ...) ...) body ...)"
           x)])))

;; Builtin combinations.
(%register-combination! 'standard (%standard-method-combination))
(%register-run-combination! '+ (%operator-run +) #t)
(%register-run-combination! 'list (%operator-run list) #f)
(%register-run-combination! 'append (%operator-run append) #t)
(%register-run-combination! 'nconc (%operator-run append) #t)
(%register-run-combination! 'max (%operator-run max) #t)
(%register-run-combination! 'min (%operator-run min) #t)
(%register-run-combination! 'and %and-run #t)
(%register-run-combination! 'or %or-run #t)
(%register-run-combination! 'progn %progn-run #t)

;; Custom discriminating functions.
(define-generic compute-discriminating-function 1)

(define (standard-discriminating-function gf)
  (lambda args (apply generic-invoke gf args)))

(define-method compute-discriminating-function ((gf <generic>))
  (standard-discriminating-function gf))

(define (%make-generic-of-class name required-dispatch-args class)
  (let ([gf (%make-generic-with-class name required-dispatch-args class)])
    (when (and (%mop-protocol-enabled?) (not (eq? class <generic>)))
      (set-funcallable-instance-function! gf (compute-discriminating-function gf)))
    gf))

;; ensure-generic-function.
(define (ensure-generic-function name class . required-dispatch-args)
  (%make-generic-of-class name
    (if (pair? required-dispatch-args) (car required-dispatch-args) 1)
    class))

;; Boot fixup.
(define (%mop-fixup!)
  (%enable-mop-protocol!))
(%mop-fixup!)
