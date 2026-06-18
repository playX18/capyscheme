(current-module (resolve-module '(capy) #f #f))

(define-syntax ...
  (lambda (x)
    (syntax-violation '... "bad use of '...' syntactic keyword" x x)))

(define-syntax _
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
          #'((dummy . pattern) #'template))))
    (define (expand-syntax-rules dots keys docstrings clauses)
      (with-syntax
        (((k ...) keys)
          ((docstring ...) docstrings)
          ((((keyword . pattern) template) ...) clauses)
          ((clause ...) (map expand-clause clauses)))
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
      ((_ (k ...) ((keyword . pattern) template) ...)
        (expand-syntax-rules #f #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ (k ...) docstring ((keyword . pattern) template) ...)
        (string? (syntax->datum #'docstring))
        (expand-syntax-rules #f #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) ((keyword . pattern) template) ...)
        (identifier? #'dots)
        (expand-syntax-rules #'dots #'(k ...) #'() #'(((keyword . pattern) template) ...)))
      ((_ dots (k ...) docstring ((keyword . pattern) template) ...)
        (and (identifier? #'dots) (string? (syntax->datum #'docstring)))
        (expand-syntax-rules #'dots #'(k ...) #'(docstring) #'(((keyword . pattern) template) ...))))))

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
        ;; XXX Work around the lack of hygienic toplevel identifiers
        (with-syntax (((dummy) (generate-temporaries '(dummy))))
          #`(begin
             ;; Avoid mutating the user-visible variables
             (define dummy
              (call-with-values (lambda () expr)
               (lambda (var0 ... varn)
                (list var0 ... varn))))
             (define var0
              (let ((v (car dummy)))
               (set! dummy (cdr dummy))
               v))
             ...
             (define varn
              (let ((v (car dummy)))
               (set! dummy #f) ; blackhole dummy
               v)))))
      ((_ var expr)
        (identifier? #'var)
        #'(define var
           (call-with-values (lambda () expr)
            list)))
      ((_ (var0 ... . varn) expr)
        (and-map identifier? #'(var0 ... varn))
        ;; XXX Work around the lack of hygienic toplevel identifiers
        (with-syntax (((dummy) (generate-temporaries '(dummy))))
          #`(begin
             ;; Avoid mutating the user-visible variables
             (define dummy
              (call-with-values (lambda () expr)
               (lambda (var0 ... . varn)
                (list var0 ... varn))))
             (define var0
              (let ((v (car dummy)))
               (set! dummy (cdr dummy))
               v))
             ...
             (define varn
              (let ((v (car dummy)))
               (set! dummy #f) ; blackhole dummy
               v))))))))

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

(eval-when (expand load eval)

  ;; TODO: Make this a proper implementation: right now basically expects `stx` object
  ;; to have valid source property which is not always the case. Maybe add `include` directly
  ;; to psyntax?
  (define call-with-include-port
    (let ([syntax-dirname
            (lambda (stx)
              (define src (syntax-source stx))
              (define filename (if (vector? src) (vector-ref src 0) (and src (assq-ref src 'filename))))
              (if filename
                (dirname filename)
                #f))])
      (lambda (filename proc)
        (define dir (syntax-dirname filename))
        (define file (syntax->datum filename))
        (define path
          (cond
            [(absolute-path-string? file) file]
            [(string? dir) (string-append dir "/" file)]
            [else file]))
        (call-with-input-file
          path
          proc)))))

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
  (module-gensym ".L"))

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

(define %cond-expand-features
  ;; This should contain only features that are present in core Guile,
  ;; before loading any modules.  Modular features are handled by
  ;; placing 'cond-expand-provide' in the relevant module.
  '(capy
    r5rs
    r6rs
    r7rs
    exact-closed ;; R7RS features.
    ieee-float
    full-unicode
    ratios
    srfi-0 ;; cond-expand itself
    srfi-6 ;; string ports
    srfi-8 ;; `receive` syntax
    srfi-9 ;; define-record-type
    srfi-11 ;; Syntax for receiving multiple values
    srfi-13 ;; string library
    srfi-14 ;; character sets
    srfi-16 ;; case-lambda
    srfi-23 ;; `error` procedure
    srif-26 ;; Notation for Specializing Parameters without Currying
    srfi-24 ;; define-syntax in local lexical scope
    srfi-27 ;; random sources
    srfi-28 ;; basic format strings
    srfi-30 ;; nested multi-line comments
    srfi-34 ;; exception handling for programs
    srfi-36 ;; I/O conditions
    srfi-39 ;; parameterize
    srfi-46 ;; basic syntax-rules extensions
    srfi-48 ;; Intermediate format strings
    srfi-55 ;; require-extension
    srfi-61 ;; general cond clause
    srfi-62 ;; s-expression comments
    srfi-64 ;; A Scheme API for test suites
    srfi-87 ;; => in case clauses
    srfi-98 ;; environment variables
    srfi-105 ;; curly infix expressions
    srfi-124 ;; ephemerons
    srfi-126 ;; R6RS hashtables
    srfi-125 ;; intermediate hashtables
    srfi-128 ;; comparators (reduced)
    srfi-130 ;; Cursor-based string library
    srfi-132 ;; sorting
    srfi-145 ;; assumptions
    srfi-157 ;; Continuation marks
    srfi-180 ;; JSON
    srfi-213 ;; Identifier properties
    srfi-214 ;; Flexvectors
    ; srfi-226 ;; Control features (TODO: BROKEN!)
    srfi-257 ;; Simple extendable pattern matcher with backtracking
    srfi-259)) ;; Tagged procedures with type safety

(set! %cond-expand-features
  (cons (string->symbol (host-os)) %cond-expand-features))
(set! %cond-expand-features
  (cons (string->symbol (host-family)) %cond-expand-features))
(set! %cond-expand-features
  (cons (string->symbol (host-arch)) %cond-expand-features))

;; This table maps module public interfaces to the list of features.
;;
(define %cond-expand-table (make-core-hash-eq))

;; Add one or more features to the `cond-expand' feature list of the
;; module `module'.
;;
(define (cond-expand-provide module features)
  (let ((mod (module-public-interface module)))
    (and mod
      (core-hash-put! %cond-expand-table mod
        (append (core-hash-ref %cond-expand-table mod '())
          features)))))

(define-syntax cond-expand
  (lambda (x)
    (define (module-has-feature? mod sym)
      (or-map (lambda (mod)
               (memq sym (core-hash-ref %cond-expand-table mod '())))
        (module-uses mod)))

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
              (module-has-feature? (current-module) sym))))))

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

(eval-when (expand load eval)
  (define (dd x)
    ;(pretty-print (syntax->datum x))
    x)
  (define (parse-lambda-args formals)
    (let loop ([rest formals] [as '()] [n 0])
      (syntax-case rest ()
        [() (values (reverse as) formals #f n 0 '())]
        [(x . _) (keyword? (syntax->datum #'x))
          (values (reverse as) formals #f n 0 rest)]
        [(x . y) (loop #'y (cons #'x as) (+ n 1))]
        [x (values (reverse as) formals #t n 1 '())])))

  (define (extended-lambda-body form garg kargs body)
    (define (collect-args xs r)
      (syntax-case xs ()
        [() (values (reverse r) '())]
        [(x . _) (keyword? (syntax->datum #'x))
          (values (reverse r) xs)]
        [(x . y) (collect-args #'y (cons #'x r))]))

    (define (parse-kargs c xs os ks r a)
      (syntax-case xs ()
        [() (expand-opt os ks r a)]
        [(#:optional . xs)
          (and
            (or (null? os) (syntax-violation 'lambda* "multiple #:optional sections" #'xs))
            (receive (os xs) (collect-args #'xs '())
              (parse-kargs c xs os ks r a)))]
        [(#:key . xs)
          (and
            (or (null? ks)
              (syntax-violation 'lambda* "multiple #:key sections" #'xs))
            (receive (ks xs) (collect-args #'xs '())
              (parse-kargs c xs os ks r a)))]
        [(#:rest . xs)
          (and
            (or (not r) (syntax-violation 'lambda* "multiple #:rest sections" #'xs))
            (receive (rs xs) (collect-args #'xs '())
              (syntax-case rs ()
                [(r)
                  (identifier? #'r)
                  (parse-kargs c xs os ks #'r a)]
                [_ (syntax-violation 'lambda* "#:rest keyword in lambda* must be followed by a single identifier" rs)])))]))

    (define (expand-opt os ks r a)

      (cond
        [(null? os)
          (if r
            #`((let ([#,r #,garg]) #,@(expand-key ks garg a)))
            (expand-key ks garg a))]
        [else

          (define binds
            (map (lambda (x)
                  (syntax-case x ()
                    [(o) (identifier? #'o) #'o]
                    [(o init) (identifier? #'o) #'(o init)]
                    [_ (syntax-violation 'lambda* "invalid optional argument" os)]))
              os))
          (define rest (list (or r (car (generate-temporaries '(rest))))))
          (dd #`((let-optionals* #,garg #,(append binds rest)
                  #,@(if (and (not r) (null? ks) (not a))
                      #`((unless (null? #,(car rest))
                          (error #f "too many arguments for" #,form))
                         (let () #,@(expand-key ks rest a)))
                      (expand-key ks (car rest) a)))))]))
    (define (expand-key ks garg a)
      (cond
        [(null? ks)
          (if a
            #`((let-keywords* #,garg #,(if (boolean? a) (car (generate-temporaries '(a))) a) #,@body))
            body)]
        [else
          (define args (map (lambda (o)
                             (syntax-case o ()
                               [(o) (identifier? #'o) #'o]
                               [((key o) init)
                                 (keyword? (syntax->datum #'key))
                                 #'(o key init)]
                               [(o init)
                                 #'(o init)]
                               [_ (syntax-violation 'lambda* "invalid keyword argument" ks)]))
                        ks))
          (dd #`((let-keywords* #,garg
                  #,(if a (append args a) args)
                  #,@body)))]))
    (parse-kargs #f kargs '() '() #f #f)))

(define-syntax lambda*
  (lambda (x)
    (syntax-case x ()
      [(_ formals body ...)
        (receive (fargs fformals ftypes nreqs nopts kargs)
          (parse-lambda-args #'formals)
          (if (null? kargs)
            #'(lambda formals body ...)
            (with-syntax ([(restarg) (generate-temporaries '(restarg))]
                          [(args ...) (datum->syntax #'x fargs)])
              #`(lambda (#,@fargs . restarg)
                 #,@(extended-lambda-body #`('#,x) #'restarg kargs #'(body ...))))))])))

(define-syntax define*
  (lambda (x)
    (syntax-case x ()
      [(_ (name . formals) body ...)
        #'(define name
           (lambda* formals
            body
            ...))])))
(define (set-record-type-printer! typ printer)
  (unless (record-type? typ)
    (assertion-violation 'set-record-type-printer! "Not an record type"))
  (set-rtd-printer! (record-type-rtd typ) printer))

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
        #:init-keyword (slot-definition-init-keyword slot)
        #:init-value (slot-definition-init-value slot)
        #:init-thunk (slot-definition-init-thunk slot)
        #:slot-ref (slot-definition-slot-ref slot)
        #:slot-set! (slot-definition-slot-set! slot)
        #:slot-bound? (slot-definition-slot-bound? slot)
        #:getter (slot-definition-getter slot)
        #:setter (slot-definition-setter slot)
        #:accessor (slot-definition-accessor slot)
        #:initializable (slot-definition-initializable? slot)
        #:settable (slot-definition-settable? slot)
        #:immutable (slot-definition-immutable? slot)))

(define (slot-accessor-options accessor)
  (list #:init-keyword (slot-accessor-init-keyword accessor)
        #:init-value (slot-accessor-init-value accessor)
        #:init-thunk (slot-accessor-init-thunk accessor)
        #:slot-ref (slot-accessor-slot-ref accessor)
        #:slot-set! (slot-accessor-slot-set! accessor)
        #:slot-bound? (slot-accessor-slot-bound? accessor)
        #:getter (slot-accessor-getter accessor)
        #:setter (slot-accessor-setter accessor)
        #:accessor (slot-accessor-accessor accessor)
        #:initializable (slot-accessor-initializable? accessor)
        #:settable (slot-accessor-settable? accessor)
        #:immutable (slot-accessor-immutable? accessor)))

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
        #:sealed (generic-sealed? generic)))

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
        #:specializers (method-specializers method)
        #:required-arg-count (method-required-arg-count method)
        #:body (method-body method)
        #:locked (method-locked? method)))

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
        #:methods (next-method-methods next-method)
        #:args (next-method-args next-method)
        #:index (next-method-index next-method)
        #:body (next-method-body next-method)
        #:has-next? (next-method-has-next? next-method)
        #:next (next-method-next next-method)))

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
        #:direct-supers (class-direct-supers class)
        #:cpl (class-precedence-list class)
        #:direct-slots (class-direct-slots class)
        #:slots (class-slots class)
        #:accessors (class-accessors class)
        #:initargs (class-initargs class)
        #:direct-methods (class-direct-methods class)
        #:direct-subclasses (class-direct-subclasses class)
        #:applicable (class-applicable? class)
        #:malleable (class-malleable? class)
        #:sealed (class-sealed? class)))

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

(define (compute-applicable-methods generic args)
  (let ([classes (map class-of args)])
    (filter
      (lambda (method)
        (apply method-applicable-for-classes? method classes))
      (generic-methods generic))))

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

(define (apply-generic generic args)
  (let ([methods (compute-applicable-methods generic args)])
    (apply-methods generic
                   (sort-applicable-methods generic methods args)
                   args)))

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

(define-for-syntax (%define-class-runtime-slot-option option)
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
    [else option]))

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
            [else
              (syntax-violation 'define-class
                "unsupported class option"
                form
                (caar pairs))]))])))

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
                          (cons (datum->syntax (caar pairs) '#:init-thunk)
                                out)))]
            [(and (eq? option-name 'allocation)
                  (%define-class-colon-symbol? (syntax->datum value)))
              (loop (cdr pairs)
                    (cons #`'#,value
                          (cons (%define-class-runtime-slot-option (caar pairs)) out)))]
            [else
              (loop (cdr pairs)
                    (cons value
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
    #'((define-generic getter-name)
       (define-method (getter-name (obj class-name))
         (slot-ref obj 'slot-name)))))

(define-for-syntax (%define-class-setter-forms class-name slot-name setter-name)
  (with-syntax ([class-name class-name]
                [slot-name slot-name]
                [setter-name setter-name])
    #'((define-generic setter-name)
       (define-method (setter-name (obj class-name) value)
         (slot-set! obj 'slot-name value)
         value))))

(define-for-syntax (%define-class-accessor-forms class-name slot-name accessor-name)
  (with-syntax ([class-name class-name]
                [slot-name slot-name]
                [accessor-name accessor-name])
    #'((define-generic accessor-name)
       (define-generic (setter accessor-name))
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

(define-for-syntax (%expand-define-class form name supers slots options)
  (let* ([slot-list (%define-class-syntax-list->list form slots)]
         [applicable? (%define-class-applicable-option form options)])
    (with-syntax ([name name]
                  [(super ...) supers]
                  [(slot-spec ...)
                    (map (lambda (slot) (%process-slot-definition form slot))
                         slot-list)]
                  [(accessor-form ...)
                    (apply append
                      (map (lambda (slot)
                             (%define-class-slot-accessor-forms form name slot))
                           slot-list))]
                  [(class-var old-class) (generate-temporaries '(class-var old-class))]
                  [make-class-id
                    (if applicable? #'make-invocable-class #'make-class)]
                  [redefine-class-id
                    (if applicable? #'redefine-invocable-class! #'redefine-class!)]
                  [applicable-value (datum->syntax form applicable?)])
      #'(begin
          (define name
            (let ([class-var (module-local-variable (current-module) 'name)])
              (if (and class-var (variable-bound? class-var))
                (let ([old-class (variable-ref class-var)])
                  (if (class? old-class)
                    (redefine-class-id
                      old-class
                      'name
                      (list slot-spec ...)
                      (list super ...))
                    (make-class-id 'name (list slot-spec ...) (list super ...))))
                (make-class-id 'name (list slot-spec ...) (list super ...)))))
          (class-post-initialize
            name
            (list #:name 'name
                  #:supers (list super ...)
                  #:slots (list slot-spec ...)
                  #:applicable applicable-value))
          accessor-form ...))))

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
    (define (generic-option-forms generic options)
      (let loop ([pairs (generic-option-pairs options)]
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
              [else
                (syntax-violation 'define-generic
                  "unsupported generic option"
                  x
                  (caar pairs))])))))
    (define (setter-binding-name name)
      (datum->syntax name
        (string->symbol
          (string-append "setter of " (symbol->string (syntax->datum name))))))
    (define (expand-setter-generic name required-dispatch-args options)
      (unless (identifier? name)
        (syntax-violation 'define-generic "expected a generic identifier" x name))
      (with-syntax ([getter-id name]
                    [setter-id (setter-binding-name name)]
                    [required-dispatch-args required-dispatch-args]
                    [(option-form ...)
                      (generic-option-forms (setter-binding-name name) options)])
        #'(begin
            (define setter-id (make-generic 'setter-id required-dispatch-args))
            option-form ...
            (set-setter! getter-id setter-id))))
    (define (expand-generic name required-dispatch-args options)
      (with-syntax ([name name]
                    [required-dispatch-args required-dispatch-args]
                    [(option-form ...) (generic-option-forms name options)])
        #'(begin
            (define name (make-generic 'name required-dispatch-args))
            option-form ...)))
    (syntax-case x (setter)
      [(_ name)
        (identifier? #'name)
        #'(define name (make-generic 'name))]
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
        #'(define name (make-generic 'name required-dispatch-args))]
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
                    (cons #'class specializers)
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
    (define (unsupported-qualifier form qualifier)
      (syntax-violation 'define-method
        "unsupported method qualifier"
        form
        qualifier))
    (define (expand-method form generic args body locked?)
      (receive (vars specializers required rest-var tail-formals)
        (parse-specialized-args args)
        (with-syntax ([generic-id (generic-expression form generic)]
                      [(var ...) vars]
                      [(specializer ...) specializers]
                      [required-count (datum->syntax form required)]
                      [locked-value locked?]
                      [next-id (datum->syntax x 'next)]
                      [next-method-id (datum->syntax x 'next-method)]
                      [(tail ...) tail-formals]
                      [(body ...) body])
          (if rest-var
            (with-syntax ([rest-id rest-var])
              #'(add-method! generic-id
                  (list specializer ...)
                  required-count
                  (lambda* (next-id var ... . rest-id)
                    (let ([next-method-id (lambda () (next-method-invoke next-id))])
                      body ...))
                  locked-value))
            #'(add-method! generic-id
                (list specializer ...)
                required-count
                (lambda* (next-id var ... tail ...)
                  (let ([next-method-id (lambda () (next-method-invoke next-id))])
                    body ...))
                locked-value)))))
    (syntax-case x ()
      [(_ (generic . args) qualifier body ...)
        (keyword-qualifier? #'qualifier)
        (if (locked-qualifier? #'qualifier)
            (expand-method x #'generic #'args #'(body ...) #'#t)
            (unsupported-qualifier x #'qualifier))]
      [(_ (generic . args) body ...)
        (expand-method x #'generic #'args #'(body ...) #'#f)]
      [(_ generic qualifier args body ...)
        (keyword-qualifier? #'qualifier)
        (if (locked-qualifier? #'qualifier)
            (expand-method x #'generic #'args #'(body ...) #'#t)
            (unsupported-qualifier x #'qualifier))]
      [(_ generic args body ...)
        (expand-method x #'generic #'args #'(body ...) #'#f)]
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
(define <relocatable-code-block> (%builtin-class 'relocatable-code-block))
(define <module> (%builtin-class 'module))
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

(define-generic slot-ref-using-class 3)
(define-generic slot-set-using-class! 4)
(define-generic slot-bound-using-class? 3)
(define-generic slot-unbound)
(define-generic slot-missing)

(define-method (slot-ref-using-class (class <class>) obj slot . fallback)
  (apply %slot-ref-using-class class obj slot fallback))

(define-method (slot-set-using-class! (class <class>) obj slot value)
  (%slot-set-using-class! class obj slot value))

(define-method (slot-bound-using-class? (class <class>) obj slot)
  (%slot-bound-using-class? class obj slot))

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

(define-method (initialize obj initargs)
  obj)

(define-generic make 1)

(define-method (make (class <class>) . initargs)
  (let ([obj (apply make-instance class initargs)])
    (initialize
      (or (and (procedure? obj) (procedure-property obj 'instance)) obj)
      initargs)
    obj))

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
