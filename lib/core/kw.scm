;; kw.scm - zero-cost keyword and optional arguments.
;;
;;
;;   (import (core))
;;   (define (greet name #:greeting [g "hello"] #:punct [p "!"])
;;     (string-append g " " name p))
;;   (greet "ada")                             ; => "hello ada!"
;;   (greet "ada" #:punct "?")                 ; => "hello ada?"
;;   (greet "ada" #:punct "?" #:greeting "hi") ; => "hi ada?"
;;
;; Design:
;;
;; - A `lambda` with optional or keyword arguments expands to a core
;;   procedure taking every argument (missing ones filled with `kw-undefined`
;;   or an immediate default at the call site), plus `case-lambda` entries
;;   dispatching on positional arity. With keywords there is also an `unpack`
;;   procedure parsing sorted keyword/value lists.
;; - `define` with keywords binds the name to a macro, so calls with
;;   literal keywords expand to direct core calls: keywords are sorted and
;;   defaults filled at expansion time.
;; - Calls through unknown operators, `keyword-apply`, and first-class uses
;;   take the slow path: a runtime check against metadata attached to the
;;   procedure via `procedure-properties`, then the `unpack` entry.

(library (core kw)
  (export new-lambda
          new-define
          new-app
          new-apply
          new-apply-proc
          make-keyword-procedure
          keyword-apply
          procedure-keywords
          procedure-reduce-keyword-arity
          keyword<?
          string->keyword)
  (import (capy)
          (core primitives)
          (rename (core primitives) (|#%app| |#%plain-app|))
          (core parameters)
          (core io)
          (core files)
          (core exceptions)
          (core arithmetic)
          (core sorting)
          (core bytevectors)
          (core syntax-case)
          (core r5rs)
          (core control)
          (core optargs)
          (core lists)
          (core records)
          (core conditions)
          (core hashtables))


  ;; Needed at expansion time (keyword sorting in transformers).
  (eval-when (expand load eval)
  (define (keyword<? a b)
    (unless (keyword? a)
      (assertion-violation 'keyword<? "expected a keyword" a))
    (unless (keyword? b)
      (assertion-violation 'keyword<? "expected a keyword" b))
    (string<? (keyword->string a) (keyword->string b)))
  )

  (define (string->keyword s)
    (unless (string? s)
      (assertion-violation 'string->keyword "expected a string" s))
    (symbol->keyword (string->symbol s)))

  (define kw-undefined (gensym "kw-undefined"))

  (define-record-type kwinfo
    (fields checker   ; (kws n) -> truthy: may this keyword call proceed?
            with-kws  ; entry procedure: (kws kw-args . positionals)
            required  ; sorted required keywords, or '()
            allowed   ; sorted allowed keywords, or #f for "any"
            name))    ; defining identifier, procedure name, or #f

  (define kwinfo-key (gensym "kwinfo"))

  (define (kwinfo-of proc)
    (and (is-a? proc <closure>)
         (procedure-property proc kwinfo-key)))

  (define (proc-name proc)
    (and (is-a? proc <closure>)
         (procedure-property proc 'name)))

  (define (attach-kwinfo! proc info name)
    (set-procedure-properties!
     proc
     (if name
         (list (cons kwinfo-key info) (cons 'name name))
         (list (cons kwinfo-key info))))
    proc)

  (define (subset? l1 l2)
    (cond [(null? l1) #t]
          [(null? l2) #f]
          [(eq? (car l1) (car l2)) (subset? (cdr l1) (cdr l2))]
          [else (subset? l1 (cdr l2))]))

  (define (subsets? l1 l2 l3)
    (cond [(null? l1) (subset? l2 l3)]
          [(null? l2) #f]
          [(null? l3) #f]
          [else (let ([v2 (car l2)])
                  (cond [(eq? (car l1) v2)
                         (subsets? (cdr l1) (cdr l2) (cdr l3))]
                        [(eq? v2 (car l3))
                         (subsets? l1 (cdr l2) (cdr l3))]
                        [else (subsets? l1 l2 (cdr l3))]))]))

  (define (equal-kws? l1 l2)
    (cond [(null? l1) (null? l2)]
          [(null? l2) #f]
          [(eq? (car l1) (car l2)) (equal-kws? (cdr l1) (cdr l2))]
          [else #f]))

  (define (arity-includes? arity n)
    (cond [(eq? arity #t) #t]
          [(and (pair? arity)
                (let ([mx (cdr arity)])
                  (or (not mx) (exact-integer? mx))))
           (and (>= n (car arity))
                (let ([mx (cdr arity)])
                  (or (not mx) (<= n mx))))]
          [else (and (memv n arity) #t)]))

  (define (make-keyword-checker req-kws allowed-kws arity)
    (lambda (kws n)
      (and (arity-includes? arity n)
           (cond [(not allowed-kws) (subset? req-kws kws)]
                 [(null? allowed-kws) (null? kws)]
                 [(null? req-kws) (subset? kws allowed-kws)]
                 [(= (length req-kws) (length allowed-kws))
                  (equal-kws? req-kws kws)]
                 [else (subsets? req-kws kws allowed-kws)]))))

  (define (check-kw-args info kws)
    (let loop ([kws kws]
               [required (kwinfo-required info)]
               [allowed (kwinfo-allowed info)])
      (cond [(null? kws)
             (if (null? required)
                 (values #f #f)
                 (values (car required) #f))]
            [(and (pair? required) (eq? (car required) (car kws)))
             (loop (cdr kws) (cdr required) (and allowed (cdr allowed)))]
            [(not allowed)
             (loop (cdr kws) required #f)]
            [(pair? allowed)
             (if (eq? (car allowed) (car kws))
                 (loop (cdr kws) required (cdr allowed))
                 (loop kws required (cdr allowed)))]
            [else (values #f (car kws))])))

  (define (raise-kw-error name kw-proc? proc? n kws kw-args missing-kw extra-kw args)
    (let ([shown-args
           (if (and (null? args) (null? kws))
               ""
               (string-append
                "\n  arguments...:"
                (apply string-append
                       (map (lambda (v) (format #f "\n   ~s" v)) args))
                (apply string-append
                       (map (lambda (kw arg) (format #f "\n   ~a ~s" kw arg))
                            kws kw-args))))]
          [label (format #f "~a" (or name "<procedure>"))])
      (cond [extra-kw
             (if kw-proc?
                 (error 'application
                        (format #f "procedure does not expect an argument with given keyword\n  procedure: ~a\n  given keyword: ~a~a"
                                label extra-kw shown-args))
                 (if proc?
                     (error 'application
                            (format #f "procedure does not accept keyword arguments\n  procedure: ~a~a"
                                    label shown-args))
                     (error 'application
                            (format #f "not a procedure;\n expected a procedure that can be applied to arguments\n  given: ~s~a"
                                    name shown-args))))]
            [missing-kw
             (error 'application
                    (format #f "required keyword argument not supplied\n  procedure: ~a\n  required keyword: ~a~a"
                            label missing-kw shown-args))]
            [else
             (assertion-violation
              'application
              (format #f "no case matching ~a non-keyword argument~a\n  procedure: ~a~a"
                      (- n 2) (if (= 1 (- n 2)) "" "s") label shown-args))])))

  ;; Extracts the keyword-entry procedure for a slow-path call. A procedure
  ;; with keyword metadata is checked against it. One without (a plain
  ;; `lambda`, a native, a generic) receives the keywords as ordinary
  ;; trailing arguments, `kw value ...`.
  (define (kw-extract kws n proc)
    (let ([info (kwinfo-of proc)])
      (cond
       [(and info ((kwinfo-checker info) kws n))
        (kwinfo-with-kws info)]
       [(and (not info) (procedure? proc))
        (lambda (call-kws call-kw-args . call-args)
          (apply proc
                 (append call-args
                         (let loop ([ks call-kws] [vs call-kw-args])
                           (if (null? ks)
                               '()
                               (cons (car ks)
                                     (cons (car vs) (loop (cdr ks) (cdr vs)))))))))]
       [else
        (let ([name (cond [info (or (kwinfo-name info) (proc-name proc) proc)]
                          [else proc])])
          (lambda (call-kws call-kw-args . call-args)
            (let-values ([(missing-kw extra-kw)
                          (if info
                              (check-kw-args info call-kws)
                              (values #f (and (pair? call-kws) (car call-kws))))])
              (raise-kw-error name (and info #t) (procedure? proc)
                              n call-kws call-kw-args
                              missing-kw extra-kw call-args))))])))

  (define (raise-missing-kw name req-kws args)
    (raise-kw-error (or name "<procedure>") #t #t 0 '() '() (car req-kws) #f args))

  (define (procedure-keywords proc)
    (cond [(kwinfo-of proc)
           => (lambda (info)
                (values (kwinfo-required info) (kwinfo-allowed info)))]
          [(procedure? proc) (values '() '())]
          [else (assertion-violation 'procedure-keywords "not a procedure" proc)]))

  (define (keyword-apply proc kws kw-vals normal-args . normal-argss)
    (unless (procedure? proc)
      (assertion-violation 'keyword-apply "expected a procedure" proc))
    (let loop ([ks kws])
      (cond [(null? ks) (if #f #f)]
            [(or (not (pair? ks)) (not (keyword? (car ks))))
             (assertion-violation 'keyword-apply "expected a list of keywords" kws)]
            [(null? (cdr ks)) (if #f #f)]
            [(or (not (pair? (cdr ks))) (not (keyword? (cadr ks))))
             (loop (cdr ks))]
            [(keyword<? (car ks) (cadr ks)) (loop (cdr ks))]
            [else (assertion-violation 'keyword-apply "expected sorted distinct keywords" kws)]))
    (unless (list? kw-vals)
      (assertion-violation 'keyword-apply "expected a list of keyword values" kw-vals))
    (unless (= (length kws) (length kw-vals))
      (error 'keyword-apply "keyword list length does not match value list length"
             kws kw-vals))
    (let ([all-args
           (let loop ([lists (cons normal-args normal-argss)] [pos 3])
             (if (null? (cdr lists))
                 (let ([l (car lists)])
                   (if (list? l)
                       l
                       (assertion-violation 'keyword-apply
                                            (format #f "expected a list for argument ~a" pos)
                                            l)))
                 (cons (car lists)
                       (loop (cdr lists) (+ pos 1)))))])
      (if (null? kws)
          (apply proc all-args)
          (apply (kw-extract kws (+ 2 (length all-args)) proc)
                 kws kw-vals all-args))))

  (define make-keyword-procedure
    (case-lambda
      [(proc)
       (unless (procedure? proc)
         (assertion-violation 'make-keyword-procedure "expected a procedure" proc))
       (make-keyword-procedure proc (lambda args (apply proc '() '() args)))]
      [(proc plain-proc)
       (unless (procedure? proc)
         (assertion-violation 'make-keyword-procedure "expected a procedure" proc))
       (unless (procedure? plain-proc)
         (assertion-violation 'make-keyword-procedure "expected a procedure" plain-proc))
       (let ([name (or (proc-name proc) (proc-name plain-proc))])
         (attach-kwinfo! (lambda args (apply plain-proc args))
                         (make-kwinfo (make-keyword-checker '() #f #t)
                                      proc '() #f name)
                         name))]))

  (define procedure-reduce-keyword-arity
    (case-lambda
      [(proc arity req-kws allowed-kws)
       (do-reduce-keyword-arity proc arity req-kws allowed-kws #f)]
      [(proc arity req-kws allowed-kws name)
       (do-reduce-keyword-arity proc arity req-kws allowed-kws name)]))

  ;; Builds a procedure like `proc` but accepting only `arity` positional
  ;; arguments (an exact nonnegative integer or a nonempty list of them) and
  ;; the given keyword sets. 
  (define (do-reduce-keyword-arity proc arity req-kws allowed-kws name)
    (define (sorted-kws? kws)
      (let loop ([kws kws])
        (cond [(null? kws) #t]
              [(not (pair? kws)) #f]
              [(not (keyword? (car kws))) #f]
              [(null? (cdr kws)) #t]
              [(keyword<? (car kws) (cadr kws)) (loop (cdr kws))]
              [else #f])))
    (define (bad-arity!)
      (assertion-violation 'procedure-reduce-keyword-arity
                           "expected an exact nonnegative integer or nonempty list of exact nonnegative integers"
                           arity))
    (define arity-list
      (let ([ok? (lambda (a) (and (exact-integer? a) (>= a 0)))])
        (cond [(ok? arity) (list arity)]
              [(and (list? arity) (pair? arity) (for-all ok? arity)) arity]
              [else (bad-arity!)])))
    (define arity-totals (map (lambda (a) (+ a 2)) arity-list))
    (unless (procedure? proc)
      (assertion-violation 'procedure-reduce-keyword-arity "expected a procedure" proc))
    (unless (sorted-kws? req-kws)
      (assertion-violation 'procedure-reduce-keyword-arity
                           "expected sorted distinct required keywords" req-kws))
    (when allowed-kws
      (unless (sorted-kws? allowed-kws)
        (assertion-violation 'procedure-reduce-keyword-arity
                             "expected sorted distinct allowed keywords or #f" allowed-kws))
      (unless (subset? req-kws allowed-kws)
        (error 'procedure-reduce-keyword-arity
               "allowed-keyword list does not include all required keywords"
               allowed-kws req-kws)))
    (let-values ([(old-req old-allowed) (procedure-keywords proc)])
      (unless (subset? old-req req-kws)
        (error 'procedure-reduce-keyword-arity
               "cannot reduce required keyword set" old-req req-kws))
      (when old-allowed
        (unless (subset? req-kws old-allowed)
          (error 'procedure-reduce-keyword-arity
                 "cannot require keywords not in original allowed set"
                 old-allowed req-kws))
        (when (and allowed-kws (not (subset? allowed-kws old-allowed)))
          (error 'procedure-reduce-keyword-arity
                 "cannot allow keywords not in original allowed set"
                 old-allowed allowed-kws))))
    (let ([pname (or name (proc-name proc))])
      (define (plain-entry . args)
        (cond [(not (memv (length args) arity-list))
               (assertion-violation
                (if (symbol? pname) pname 'application)
                (format #f "no case matching ~a arguments" (length args))
                args)]
              [(pair? req-kws)
               (raise-missing-kw pname req-kws args)]
              [else (apply proc args)]))
      (if (null? allowed-kws)
          plain-entry
          (attach-kwinfo!
           plain-entry
           (make-kwinfo (make-keyword-checker req-kws allowed-kws arity-totals)
                        (lambda (kws kw-args . args)
                          (keyword-apply proc kws kw-args args))
                        req-kws
                        allowed-kws
                        pname)
           pname))))


  (eval-when (expand load eval)

  (define (syntax->list stx)
    (let loop ([stx stx] [acc '()])
      (syntax-case stx ()
        [() (reverse acc)]
        [(h . t) (loop #'t (cons #'h acc))]
        [_ #f])))

  ;; Operators whose keywords are plain `kw value ...` arguments parsed by
  ;; the callee (initargs). Calls with literal keywords stay ordinary
  ;; applications: no keyword sorting, list building, or metadata lookup.
  (define (interleaved-keyword-operator? id)
    (and (identifier? id)
         (free-identifier=? id #'make-instance)))

  (define (keyword-literal? stx)
    (keyword? (syntax->datum stx)))

  (define (immediate-literal? v)
    (or (boolean? v)
        (number? v)
        (char? v)
        (null? v)
        (and (string? v) (< (string-length v) 8))
        (and (bytevector? v) (< (bytevector-length v) 8))))

  (define (interned-symbol? v)
    (and (symbol? v)
         (eq? v (string->symbol (symbol->string v)))))

  ;; A default-argument expression counts as "immediate" when it is safe and
  ;; cheap to evaluate at every call site:
  ;; a small literal, a quoted interned symbol or small datum, or eof-object.
  (define (immediate-default? expr)
    (or (immediate-literal? (syntax->datum expr))
        (syntax-case expr (quote eof-object)
          [(quote datum)
           (let ([v (syntax->datum #'datum)])
             (or (interned-symbol? v) (immediate-literal? v)))]
          [(eof-object) #t]
          [eof-object #t]
          [_ #f])))

  ;; First duplicate identifier (by `bound-identifier=?`), or #f.
  (define (find-duplicate-identifier ids)
    (let loop ([ids ids])
      (cond [(or (null? ids) (null? (cdr ids))) #f]
            [(exists (lambda (other)
                       (bound-identifier=? (car ids) other))
                     (cdr ids))
             (car ids)]
            [else (loop (cdr ids))])))

  (define (sort-by-keyword xs key)
    (let ([sorted (list-sort (lambda (a b)
                               (keyword<? (syntax->datum (key a))
                                          (syntax->datum (key b))))
                             xs)])
      (cons sorted
            (let loop ([xs sorted])
              (cond [(or (null? xs) (null? (cdr xs))) #f]
                    [(eq? (syntax->datum (key (car xs)))
                          (syntax->datum (key (cadr xs))))
                     (cadr xs)]
                    [else (loop (cdr xs))])))))

  (define (parse-formals stx args)
    (define (loop args needs-default? plain opts specs kws req-kws)
      (syntax-case args ()
        [id
         (identifier? #'id)
         (values (reverse plain) (reverse opts) (reverse specs)
                 (reverse kws) (reverse req-kws) #'id)]
        [()
         (values (reverse plain) (reverse opts) (reverse specs)
                 (reverse kws) (reverse req-kws) #f)]
        [(id . rest)
         (identifier? #'id)
         (begin
           (when needs-default?
             (syntax-violation #f "default-value expression missing" stx #'id))
           (loop #'rest #f
                 (cons #'id plain)
                 opts
                 (cons (list #'id #f '#:plain) specs)
                 kws
                 req-kws))]
        [([id default] . rest)
         (identifier? #'id)
         (loop #'rest #t
               plain
               (cons (list #'id #'default) opts)
               (cons (list #'id #'default '#:opt) specs)
               kws
               req-kws)]
        [(kw id . rest)
         (and (identifier? #'id) (keyword-literal? #'kw))
         (loop #'rest needs-default?
               plain
               opts
               (cons (list #'id #f '#:kw-req) specs)
               (cons (list #'kw #'id #t #f) kws)
               (cons #'kw req-kws))]
        [(kw [id default] . rest)
         (and (identifier? #'id) (keyword-literal? #'kw))
         (loop #'rest needs-default?
               plain
               opts
               (cons (list #'id #'default '#:kw-opt) specs)
               (cons (list #'kw #'id #f #'default) kws)
               req-kws)]
        [(kw)
         (keyword-literal? #'kw)
         (syntax-violation #f "missing argument identifier after keyword" stx #'kw)]
        [(kw bad . rest)
         (keyword-literal? #'kw)
         (syntax-violation #f "after keyword, not an identifier or identifier with default"
                           stx #'bad)]
        [(bad . rest)
         (syntax-violation #f "not an identifier, identifier with default, or keyword"
                           stx #'bad)]
        [_ (syntax-violation #f "bad argument sequence" stx args)]))
    (let-values ([(plain opts specs kws req-kws rest)
                  (loop args #f '() '() '() '() '())])
      (let ([dup-id (find-duplicate-identifier
                     (append (map car specs) (if rest (list rest) '())))])
        (when dup-id
          (syntax-violation #f "duplicate argument identifier" stx dup-id)))
      (values plain opts specs kws req-kws rest)))

  (define (normalize-definition stx lambda-op)
    (define (clause-args args)
      (let loop ([args args] [need-def? #f])
        (syntax-case args ()
          [() (values '() '())]
          [(id . more)
           (identifier? #'id)
           (begin
             (when need-def?
               (syntax-violation #f "default-value expression missing" stx #'id))
             (let-values ([(ids kws) (loop #'more #f)])
               (values (cons #'id ids) kws)))]
          [([id default] . more)
           (identifier? #'id)
           (let-values ([(ids kws) (loop #'more #t)])
             (values (cons #'id ids) kws))]
          [(kw id . more)
           (and (identifier? #'id) (keyword-literal? #'kw))
           (let-values ([(ids kws) (loop #'more need-def?)])
             (values (cons #'id ids) (cons #'kw kws)))]
          [(kw [id default] . more)
           (and (identifier? #'id) (keyword-literal? #'kw))
           (let-values ([(ids kws) (loop #'more need-def?)])
             (values (cons #'id ids) (cons #'kw kws)))]
          [(kw . more)
           (keyword-literal? #'kw)
           (syntax-violation #f "missing argument identifier after keyword" stx #'kw)]
          [(x . more)
           (syntax-violation #f "not an identifier, identifier with default, or keyword for procedure argument"
                             stx #'x)]
          [_ (syntax-violation #f "bad argument sequence" stx args)])))
    (define (check-clause ids kw-stxs rest)
      (let ([dup (find-duplicate-identifier (if rest (cons rest ids) ids))])
        (when dup
          (syntax-violation #f "duplicate argument identifier" stx dup)))
      (let ([sorted+dup (sort-by-keyword kw-stxs (lambda (x) x))])
        (when (cdr sorted+dup)
          (syntax-violation #f "duplicate keyword for argument" stx (cdr sorted+dup)))))

    (define (simple-proto proto)
      (syntax-case proto ()
        [(id arg ...)
         (let-values ([(ids kws) (clause-args #'(arg ...))])
           (check-clause ids kws #f)
           (lambda (body)
             (with-syntax ([lam lambda-op] [(a ...) #'(arg ...)] [(b ...) body])
               #'(lam (a ...) b ...))))]
        [(id arg ... . rest)
         (identifier? #'rest)
         (let-values ([(ids kws) (clause-args #'(arg ...))])
           (check-clause ids kws #'rest)
           (lambda (body)
             (with-syntax ([lam lambda-op] [(a ...) #'(arg ...)] [r #'rest]
                           [(b ...) body])
               #'(lam (a ... . r) b ...))))]
        [_ (syntax-violation #f "bad prototype" stx proto)]))
    (define (general-proto proto)
      (syntax-case proto ()
        [(id . _)
         (identifier? #'id)
         (values #'id (simple-proto proto))]
        [((inner-id . inner-args) . _)
         (let-values ([(id mk-outer) (general-proto #'(inner-id . inner-args))])
           (let ([mk-inner (simple-proto proto)])
             (values id (lambda (body) (mk-outer (list (mk-inner body)))))))]
        [(other . _)
         (syntax-violation #f "bad syntax (not an identifier for procedure name, and not a nested procedure form)"
                           stx #'other)]
        [_ (syntax-violation #f "bad syntax" stx proto)]))
    (syntax-case stx ()
      [(_ id expr)
       (identifier? #'id)
       (values #'id #'expr)]
      [(_ id . _)
       (identifier? #'id)
       (syntax-violation
        #f
        (syntax-case stx ()
          [(_ id expr0 expr ...) "bad syntax (multiple expressions after identifier)"]
          [(_ id) "bad syntax (missing expression after identifier)"]
          [_ "bad syntax (illegal use of `.')"])
        stx)]
      [(_ proto . body)
       (let-values ([(id mk-rhs) (general-proto #'proto)])
         (unless (syntax->list #'body)
           (syntax-violation #f "bad syntax (illegal use of `.' for procedure body)" stx))
         (syntax-case #'body ()
           [() (syntax-violation #f "bad syntax (missing body)" stx)]
           [_ (values id (mk-rhs #'body))]))]
      [_ (syntax-violation #f "bad syntax" stx)]))

  (define (opt-cases-call fn leadings opt-ids opt-temps opt-ns base-ids rest-temp)
    (if rest-temp
        (with-syntax ([f fn] [(lead ...) leadings]
                      [(o ...) opt-ids] [(t ...) opt-temps] [(ns ...) opt-ns]
                      [(b ...) base-ids]
                      [re #'()] [rid rest-temp] [(r1) (list rest-temp)])
          #'(opt-cases (f lead ...) ((o t ns) ...) (b ...) () (re rid . (r1)) ()))
        (with-syntax ([f fn] [(lead ...) leadings]
                      [(o ...) opt-ids] [(t ...) opt-temps] [(ns ...) opt-ns]
                      [(b ...) base-ids]
                      [re #'()] [rid #'()] [rst #'()])
          #'(opt-cases (f lead ...) ((o t ns) ...) (b ...) () (re rid . rst) ()))))

  (define (fail-opt-cases-call fail-expr opt-ids base-ids rest-temp)
    (if rest-temp
        (with-syntax ([f fail-expr] [(o ...) opt-ids] [(b ...) base-ids]
                      [rid rest-temp] [(r1) (list rest-temp)])
          #'(fail-opt-cases (f) (o ...) (b ...) () (rid . (r1)) ()))
        (with-syntax ([f fail-expr] [(o ...) opt-ids] [(b ...) base-ids]
                      [rid #'()] [rst #'()])
          #'(fail-opt-cases (f) (o ...) (b ...) () (rid . rst) ()))))

  (define (parse-application stx rator rands plain-rator generate-direct)
    (if (or (keyword-literal? rator)
            (not (exists keyword-literal? rands)))
        (generate-direct
         rands '() #f
         (with-syntax ([op plain-rator] [(arg ...) rands])
           #'(op arg ...)))
        (let ([temps (generate-temporaries (cons rator rands))])
          (define (finish binds pos-ids kw-pairs)
            (let ([sorted+dup (sort-by-keyword kw-pairs car)])
              (when (cdr sorted+dup)
                (syntax-violation 'application "duplicate keyword in application"
                                  stx (car (cdr sorted+dup))))
              (let* ([sorted (car sorted+dup)]
                     [rator-temp (car pos-ids)]
                     [arg-temps (cdr pos-ids)]
                     [val-temps (map cdr sorted)]
                     [kw-stxs (map car sorted)]
                     [cnt (datum->syntax stx (+ 2 (length arg-temps)))])
                (with-syntax ([(bind ...) binds]
                              [rator-tmp rator-temp]
                              [(arg ...) arg-temps]
                              [(kw ...) kw-stxs]
                              [(val ...) val-temps]
                              [n cnt]
                              [body (generate-direct
                                     arg-temps sorted #t
                                     (with-syntax ([rator-tmp rator-temp]
                                                   [(arg ...) arg-temps]
                                                   [(kw ...) kw-stxs]
                                                   [(val ...) val-temps]
                                                   [n cnt])
                                       #'((kw-extract (quote (kw ...)) n rator-tmp)
                                          (quote (kw ...))
                                          (list val ...)
                                          arg ...)))])
                  #'(let* (bind ...) body)))))
          (let loop ([xs (cons rator rands)]
                     [ts temps]
                     [binds '()]
                     [pos '()]
                     [kws '()])
            (cond [(null? xs)
                   (finish (reverse binds) (reverse pos) (reverse kws))]
                  [(keyword-literal? (car xs))
                   (let ([kw (car xs)])
                     (when (null? (cdr xs))
                       (syntax-violation 'application
                                         "missing argument expression after keyword"
                                         stx kw))
                     (when (keyword-literal? (cadr xs))
                       (syntax-violation 'application
                                         "keyword in expression position (immediately after another keyword)"
                                         stx (cadr xs)))
                     (loop (cddr xs) (cdr ts)
                           (cons (list (car ts) (cadr xs)) binds)
                           pos
                           (cons (cons kw (car ts)) kws)))]
                  [else
                   (loop (cdr xs) (cdr ts)
                         (cons (list (car ts) (car xs)) binds)
                         (cons (car ts) pos)
                         kws)])))))

  (define (simple-args? args)
    (syntax-case args ()
      [id (identifier? #'id) #t]
      [() #t]
      [(a . d) (and (identifier? #'a) (simple-args? #'d))]
      [_ #f]))

  (define (parse-lambda stx ctx local-name non-kw-k kw-k)
    (syntax-case stx ()
      [(_ args body1 body ...)
       (if (simple-args? #'args)
           (non-kw-k (with-syntax ([as #'args] [(b ...) #'(body1 body ...)])
                       #'(lambda as b ...)))
           (parse-lambda* stx ctx local-name #'args #'(body1 body ...) non-kw-k kw-k))]
      [_ (syntax-violation #f "bad lambda form" stx)]))

  (define (parse-lambda* stx ctx local-name args body non-kw-k kw-k)
    (let-values ([(plain-ids opts specs kws req-kw-stxs rest-id)
                  (parse-formals stx args)])
      (let* ([opt-ids (map car opts)]
             [opt-defaults (map cadr opts)]
             [kw-stxs (map car kws)]
             [kw-ids (map cadr kws)]
             [kw-reqs (map caddr kws)]
             [kw-defaults (map cadddr kws)]
             [undef-marker #'kw-undefined]
             [kw-temps (generate-temporaries kw-ids)]
             [kw-seen-temps (generate-temporaries kw-ids)]
             [opt-temps (generate-temporaries opt-ids)]
             [plain-temps (generate-temporaries plain-ids)]
             [rest-temp (and rest-id (car (generate-temporaries (list rest-id))))]
             [core-id (car (generate-temporaries
                            (list (datum->syntax ctx (or local-name 'core)))))]
             [unpack-id (car (generate-temporaries
                              (list (datum->syntax ctx 'unpack))))]
             [opt-ns (map (lambda (d)
                            (if (immediate-default? d) d undef-marker))
                          opt-defaults)]
             [kw-ns (map (lambda (d)
                           (if (and d (immediate-default? d)) d undef-marker))
                         kw-defaults)]
             [sorted+d (sort-by-keyword
                        (map list kw-stxs kw-temps kw-seen-temps kw-reqs kw-ns)
                        car)]
             [sorted-kw-entries (car sorted+d)]
             [req-sorted+d (sort-by-keyword req-kw-stxs (lambda (x) x))]
             [req-kws-datums (map syntax->datum (car req-sorted+d))]
             [all-kws-datums
              (map (lambda (e)
                     (let ([ns (list-ref e 4)])
                       (list (syntax->datum (car e))
                             (list-ref e 3)
                             (eq? ns undef-marker)
                             (if (eq? ns undef-marker) #f ns))))
                   sorted-kw-entries)]
             [opt-ns-datums
              (map (lambda (d ns)
                     (cons (eq? ns undef-marker)
                           (if (eq? ns undef-marker) #f d)))
                   opt-defaults opt-ns)]
             [n-req (length plain-ids)]
             [n-opt (length opts)]
             [has-rest? (and rest-id #t)]
             [arity-datum (cons (+ 2 n-req)
                                (if rest-id #f (+ 2 n-req n-opt)))]
             [given-kws-id (car (generate-temporaries
                                 (list (datum->syntax ctx 'given-kws))))]
             [given-args-id (car (generate-temporaries
                                  (list (datum->syntax ctx 'given-args))))]
             [given-kws2-id (car (generate-temporaries
                                  (list (datum->syntax ctx 'given-kws))))]
             [given-argc-id (car (generate-temporaries
                                  (list (datum->syntax ctx 'given-argc))))]
             [dup-kw (cdr sorted+d)]
             [dup-req (cdr req-sorted+d)])
        (define (mk-core with-kws?)
          ;; Parameters stay sorted (callers pass sorted keyword values),
          ;; but the specs run in source order, so it receives the
          ;; same temps in source order.
          (let ([kf-temps (if with-kws? (map cadr sorted-kw-entries) '())]
                [kf-src (if with-kws? kw-temps '())])
            (with-syntax ([(kf ...) kf-temps]
                          [(kfs ...) kf-src]
                          [(pf ...) plain-temps]
                          [(of ...) opt-temps]
                          [(rf ...) (if rest-temp (list rest-temp) '())]
                          [(spec ...) (append specs
                                              (if rest-id (list rest-id) '()))]
                          [(req ...) (append plain-temps
                                              (if rest-temp (list rest-temp) '()))]
                          [(b ...) body])
              #'(lambda (kf ... pf ... of ... rf ...)
                  (let-maybe (spec ...) (kfs ...) (of ...) (req ...) b ...)))))
        (define (mk-unpack)
          (with-syntax ([(pf ...) plain-temps]
                        [(of ...) opt-temps]
                        [(rf ...) (if rest-temp (list rest-temp) '())]
                        [(kf ...) (map cadr sorted-kw-entries)]
                        [(entry ...) sorted-kw-entries]
                        [gk given-kws-id]
                        [ga given-args-id]
                        [core core-id])
            #'(lambda (gk ga pf ... of ... rf ...)
                (let-kws gk ga (entry ...)
                         (core kf ... pf ... of ... rf ...)))))
        (define (mk-no-kws with-kws?)
          (opt-cases-call (if with-kws? unpack-id core-id)
                          (if with-kws? (list #'() #'()) '())
                          opt-ids opt-temps opt-ns plain-ids rest-temp))
        (define (mk-with-kws)
          (opt-cases-call unpack-id '()
                          opt-ids opt-temps opt-ns
                          (cons given-kws-id (cons given-args-id plain-ids))
                          rest-temp))
        (define (mk-checker has-required?)
          (with-syntax ([ar arity-datum]
                        [(need ...) (car req-sorted+d)]
                        [(allow ...) (map car sorted-kw-entries)]
                        [gk given-kws2-id]
                        [gc given-argc-id])
            (if has-required?
                #'(lambda (gk gc)
                    (and (arity-matches?/static gc ar)
                         (subsets?/static (quote (need ...)) gk (quote (allow ...)))))
                #'(lambda (gk gc)
                    (and (arity-matches?/static gc ar)
                         (subset?/static gk (quote (allow ...))))))))
        (define (mk-fail-wrap)
          (let ([fail-expr
                 (with-syntax ([nm (or local-name #f)] [rq req-kws-datums])
                   #'(lambda args
                       (raise-missing-kw (quote nm) (quote rq) args)))])
            (fail-opt-cases-call fail-expr opt-ids plain-ids rest-temp)))
        ;; Duplicate keywords are reported here, on the `new-lambda` form.
        (when dup-kw
          (syntax-violation #f "duplicate keyword for argument" stx (car dup-kw)))
        (when dup-req
          (syntax-violation #f "duplicate keyword for argument" stx dup-req))
        (cond [(null? kws)
               ;; Optional-only: plain core + dispatch, no keyword machinery.
               (non-kw-k
                (with-syntax ([core core-id]
                              [core-e (mk-core #f)]
                              [dispatch (mk-no-kws #f)])
                  #'(let ((core core-e)) dispatch)))]
              [else
               (let ([has-required? (pair? req-kws-datums)])
                 (kw-k (mk-core #t)
                       (mk-unpack)
                       (if has-required? (mk-fail-wrap) (mk-no-kws #t))
                       (mk-checker has-required?)
                       core-id
                       unpack-id
                       n-req
                       opt-ns-datums
                       has-rest?
                       arity-datum
                       req-kws-datums
                       all-kws-datums))]))))

  ;; Checks supplied keyword pairs `((kw . val) ...)` (sorted) against the
  ;; required list and full sorted `all-data` rows. Returns #t when the call
  ;; may go direct.
  (define (kw-args-match? kw-pairs req-data all-data)
    (let loop ([kw-pairs kw-pairs] [req req-data] [all all-data])
      (cond [(null? kw-pairs) (null? req)]
            [else
             (let ([kw (syntax->datum (caar kw-pairs))])
               (let ([all (let drop ([all all])
                            (cond [(null? all) '()]
                                  [(keyword<? (caar all) kw) (drop (cdr all))]
                                  [else all]))])
                 (cond [(or (null? all) (not (eq? kw (caar all)))) #f]
                       [(and (pair? req) (eq? kw (car req)))
                        (loop (cdr kw-pairs) (cdr req) (cdr all))]
                       [(and (pair? req) (keyword<? (car req) (caar all))) #f]
                       [else (loop (cdr kw-pairs) req (cdr all))])))])))

  ;; Builds the direct `core` call for a matching application: keyword values
  ;; in `all-data` order (filling not-supplied slots), then required
  ;; positionals, optional positionals (filling), then the rest list when the
  ;; procedure takes one. Unless `lifted?` (temporaries already bound by
  ;; `parse-application`), binds the positional arguments first so each
  ;; evaluates once, in order.
  (define (direct-call stx core-id pos-args kw-pairs all-data opt-ns-data n-req rest? lifted?)
    ;; Rebuilds one not-supplied form: the undefined marker, `eof-object`,
    ;; or the immediate default as caller-context syntax.
    (define (convert-default undef? datum)
      (cond [undef? #'kw-undefined]
            [(equal? datum 'eof-object) #'eof-object]
            [(equal? datum '(eof-object)) (with-syntax () #'(eof-object))]
            [else (datum->syntax stx datum)]))
    ;; Keyword values in `all-data` order, supplied or filled.
    (define (kw-values)
      (let loop ([pairs kw-pairs] [all all-data])
        (cond [(null? all) '()]
              [(and (pair? pairs)
                    (eq? (syntax->datum (caar pairs)) (caar all)))
               (cons (cdar pairs) (loop (cdr pairs) (cdr all)))]
              [else (cons (convert-default (caddar all) (cadddr (car all)))
                          (loop pairs (cdr all)))])))
    (define (split-at ids n)
      (if (or (zero? n) (null? ids))
          (values '() ids)
          (let-values ([(head tail) (split-at (cdr ids) (- n 1))])
            (values (cons (car ids) head) tail))))
    (define (build pos-ids)
      (let-values ([(req-ids opt-ids) (split-at pos-ids n-req)])
        (let ([opt-forms
               (let loop ([ns opt-ns-data] [ids opt-ids])
                 (cond [(null? ns) '()]
                       [(null? ids)
                        (cons (convert-default (caar ns) (cdar ns))
                              (loop (cdr ns) '()))]
                       [else (cons (car ids) (loop (cdr ns) (cdr ids)))]))]
              [rest-ids (if rest?
                            (list-tail pos-ids (min (length pos-ids)
                                                    (+ n-req (length opt-ns-data))))
                            '())])
          (if rest?
              (with-syntax ([core core-id]
                            [(kval ...) (kw-values)]
                            [(req ...) req-ids]
                            [(opt ...) opt-forms]
                            [(rst ...) rest-ids])
                #'(core kval ... req ... opt ... (list rst ...)))
              (with-syntax ([core core-id]
                            [(kval ...) (kw-values)]
                            [(req ...) req-ids]
                            [(opt ...) opt-forms])
                #'(core kval ... req ... opt ...))))))
    (if lifted?
        (build pos-args)
        (let ([temps (generate-temporaries pos-args)])
          (with-syntax ([(t ...) temps]
                        [(arg ...) pos-args]
                        [body (build temps)])
            #'(let* ((t arg) ...) body)))))

  ;; Builds the operator macro for a `new-define` name: calls with literal
  ;; keywords and matching arity expand to direct `core` calls; anything else
  ;; falls back to the generic expansion.
  (define (make-kw-operator core-id wrap-id n-req opt-ns-data rest? req-data all-data)
    (make-variable-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! self rhs)
          (syntax-violation 'set! "cannot assign to a keyword-procedure binding" stx #'self)]
         [(self arg ...)
          (parse-application
           stx #'self (syntax->list #'(arg ...)) wrap-id
           (lambda (pos-args kw-pairs lifted? fallback)
             (let ([n-pos (length pos-args)]
                   [n-opt (length opt-ns-data)])
               (if (and (not (or (< n-pos n-req)
                                 (and (not rest?) (> n-pos (+ n-req n-opt)))))
                        (kw-args-match? kw-pairs req-data all-data))
                   (direct-call stx core-id pos-args kw-pairs all-data
                                opt-ns-data n-req rest? lifted?)
                   fallback))))]
         [self
          (identifier? #'self)
          wrap-id]
         [_ (syntax-violation #f "bad keyword-procedure use" stx)]))))

  ;; Builds the "evaluate the default if missing" check for one optional.
  ;; Immediate defaults use a constant-#f test (the value is already in hand;
  ;; the test keeps the default expression in the code for tools to see).
  (define (wrap-default-check arg-id expr)
    (if (immediate-default? expr)
        (with-syntax ([arg arg-id] [e expr] [t (datum->syntax arg-id #f)])
          #'(if t e arg))
        (with-syntax ([arg arg-id] [e expr])
          #'(if (eq? arg kw-undefined) e arg))))

  ) ; end eval-when for expansion helpers


  ;; Steps through the keyword/value lists bound to `kws`/`kw-args`,
  ;; extracting each declared keyword's value. Entries are
  ;; `([kw arg seen? required? not-supplied] ...)` in sorted order; required
  ;; entries skip the checks the checker already guaranteed.
  (define-syntax let-kws
    (lambda (stx)
      (syntax-case stx ()
        [(_ kws kw-args () . body)
         #'(begin . body)]
        [(_ kws kw-args ([kw arg seen? #f not-supplied]) . body)
         ;; Last optional: no need to advance the lists.
         #'(let ((seen? (pair? kws)))
             (let ((arg (if seen? (car kw-args) not-supplied)))
               . body))]
        [(_ kws kw-args ([kw arg seen? #f not-supplied] . rest) . body)
         (with-syntax ([next-kws (datum->syntax stx (gensym "kws"))]
                       [next-vals (datum->syntax stx (gensym "kw-args"))])
           #'(let ((seen? (and (pair? kws) (eq? (quote kw) (car kws)))))
               (let ((arg (if seen? (car kw-args) not-supplied))
                     (next-kws (if seen? (cdr kws) kws))
                     (next-vals (if seen? (cdr kw-args) kw-args)))
                 (let-kws next-kws next-vals rest . body))))]
        [(_ kws kw-args ([kw arg seen? #t ignored]) . body)
         ;; Last required: no need to advance the lists. This must precede
         ;; the mid-required clause, so single entries do not advance past
         ;; the list end.
         #'(let ((arg (car kw-args)))
             . body)]
        [(_ kws kw-args ([kw arg seen? #t ignored] . rest) . body)
         ;; Required: presence guaranteed by the checker.
         (with-syntax ([next-kws (datum->syntax stx (gensym "kws"))]
                       [next-vals (datum->syntax stx (gensym "kw-args"))])
           #'(let ((arg (car kw-args))
                   (next-kws (cdr kws))
                   (next-vals (cdr kw-args)))
               (let-kws next-kws next-vals rest . body)))])))

  ;; Binds user argument names from the `core` parameters in source order,
  ;; evaluating defaults for missing optionals. Specs are
  ;; `([id default kind] ...)` (`kind` a keyword: `#:plain`, `#:opt`,
  ;; `#:kw-req`, or `#:kw-opt`) plus an optional bare rest identifier.
  ;; Keywords (not symbols) because macro output must not contain raw symbols.
  (define-syntax let-maybe
    (lambda (stx)
      (syntax-case stx ()
        [(_ () () () () . body)
         #'(let () . body)]
        [(_ ([id ignored kind] . more) kw-args opt-args (req-id . req-ids) . body)
         (eq? (syntax->datum #'kind) '#:plain)
         #'(let ((id req-id))
             (let-maybe more kw-args opt-args req-ids . body))]
        [(_ ([id expr kind] . more) kw-args (opt-arg . opt-args) req-ids . body)
         (eq? (syntax->datum #'kind) '#:opt)
         (with-syntax ([check (wrap-default-check #'opt-arg #'expr)])
           #'(let ((id check))
               (let-maybe more kw-args opt-args req-ids . body)))]
        [(_ ([id ignored kind] . more) (kw-arg . kw-args) opt-args req-ids . body)
         (eq? (syntax->datum #'kind) '#:kw-req)
         #'(let ((id kw-arg))
             (let-maybe more kw-args opt-args req-ids . body))]
        [(_ ([id expr kind] . more) (kw-arg . kw-args) opt-args req-ids . body)
         (eq? (syntax->datum #'kind) '#:kw-opt)
         (with-syntax ([check (wrap-default-check #'kw-arg #'expr)])
           #'(let ((id check))
               (let-maybe more kw-args opt-args req-ids . body)))]
        [(_ (rest-id) () () (req-id) . body)
         (identifier? #'rest-id)
         #'(let ((rest-id req-id)) . body)]
        [_ (syntax-violation #f "bad let-maybe form" stx)])))

  ;; Builds the `case-lambda` dispatching on positional arity; each clause
  ;; jumps to the target with missing optionals filled by their not-supplied
  ;; forms. Processed optionals accumulate in `done`/`done-ns`.
  (define-syntax opt-cases
    (lambda (stx)
      (syntax-case stx ()
        ;; No optionals left: the all-given clause plus accumulated prefixes.
        [(_ (target ...) () (base ...) (done ...) (rest-empty rest-id . rest)
            (clause ...))
         ;; Longest clause first: with a rest argument every prefix clause
         ;; matches any longer call, so prefixes run longest to shortest.
         #'(case-lambda
             ((base ... done ... . rest-id)
              (target ... base ... done ... . rest))
             clause ...)]
        ;; Emit this prefix clause (missing tail filled from the remaining
        ;; not-supplieds, all visible here) and continue with the rest.
        [(_ (target ...) ((opt opt-arg not-supplied) . ((o2 t2 ns2) ...))
            (base ...) (done ...) (rest-empty rest-id . rest) (clause ...))
         #'(opt-cases (target ...) ((o2 t2 ns2) ...)
                      (base ...) (done ... opt)
                      (rest-empty rest-id . rest)
                      (((base ... done ... . rest-id)
                        (target ... base ... done ...
                                not-supplied ns2 ... . rest))
                       clause ...))])))

  ;; Like `opt-cases`, but every clause forwards to the failure procedure.
  (define-syntax fail-opt-cases
    (lambda (stx)
      (syntax-case stx ()
        [(_ (fail ...) () (base ...) () (rest-id . rest) ())
         #'(case-lambda ((base ... . rest-id) (apply fail ... base ... . rest)))]
        [(_ (fail ...) (opt-id) (base ...) (done ...) (rest-id . rest) clauses)
         #'(case-lambda
             ((base ...) (fail ... base ...))
             ((base ... done ... opt-id . rest-id)
              (apply fail ... base ... done ... opt-id . rest))
             . clauses)]
        [(_ (fail ...) (opt-id . more) (base ...) (done ...) (rest-id . rest) clauses)
         #'(fail-opt-cases (fail ...) more (base ...) (done ... opt-id) (rest-id . rest)
                           (((base ... done ... opt-id) (fail ... base ... done ... opt-id))
                            . clauses))])))

  ;; Statically unrolled keyword-subset checks for generated checkers.
  (define-syntax subset?/static
    (lambda (stx)
      (syntax-case stx (quote)
        [(_ l1-expr (quote ()))
         #'(null? l1-expr)]
        [(_ (quote ()) l2-expr)
         #'#t]
        [(_ l1-expr (quote (kw . kws)))
         (with-syntax ([l1 (datum->syntax stx (gensym "l1"))])
           #'(let ((l1 l1-expr))
               (let ((l1 (if (null? l1)
                             l1
                             (if (eq? (car l1) (quote kw))
                                 (cdr l1)
                                 l1))))
                 (subset?/static l1 (quote kws)))))]
        [(_ l1-expr l2-expr)
         #'(subset? l1-expr l2-expr)])))

  (define-syntax subsets?/static
    (lambda (stx)
      (syntax-case stx (quote)
        [(_ (quote ()) l2-expr l3-expr)
         #'(subset?/static l2-expr l3-expr)]
        [(_ l1-expr l2-expr (quote ()))
         #'(subset?/static l1-expr l2-expr)]
        [(_ (quote l1-elems) l2-expr (quote l3-elems))
         (if (equal? (map syntax->datum (or (syntax->list #'l1-elems) '()))
                     (map syntax->datum (or (syntax->list #'l3-elems) '())))
             #'(equal?/static (quote l1-elems) l2-expr)
             #'(subsets? (quote l1-elems) l2-expr (quote l3-elems)))])))

  (define-syntax equal?/static
    (lambda (stx)
      (syntax-case stx (quote)
        [(_ (quote ()) l2-expr)
         #'(null? l2-expr)]
        [(_ (quote (kw . kw-rest)) l2-expr)
         (with-syntax ([l2 (datum->syntax stx (gensym "l2"))])
           #'(let ((l2 l2-expr))
               (and (pair? l2)
                    (eq? (car l2) (quote kw))
                    (equal?/static (quote kw-rest) (cdr l2)))))])))

  ;; Statically unrolled arity check: `#t` accepts anything, a proper list
  ;; accepts exactly its members, and an improper `(min . max)` pair accepts
  ;; a range (`#f` max is unbounded).
  (define-syntax arity-matches?/static
    (lambda (stx)
      (syntax-case stx ()
        [(_ v #t)
         (datum->syntax stx #t)]
        [(_ v (a ...))
         (with-syntax ([vv #'v] [(a ...) #'(a ...)])
           #'(or (= vv a) ...))]
        [(_ v (min . max))
         (if (eq? (syntax->datum #'max) #f)
             (with-syntax ([vv #'v] [mn #'min])
               #'(>= vv mn))
             (with-syntax ([vv #'v] [mn #'min] [mx #'max])
               #'(and (>= vv mn) (<= vv mx))))])))

  (define-syntax new-lambda
    (lambda (stx)
      (parse-lambda stx stx #f
                    (lambda (expanded) expanded)
                    (lambda (core unpack wrap checker core-id unpack-id
                             n-req opt-ns-data rest? arity req-data all-data)
                      (with-syntax ([core core-id]
                                    [unpack unpack-id]
                                    [core-e core]
                                    [unpack-e unpack]
                                    [wrap-e wrap]
                                    [checker-e checker]
                                    [req req-data]
                                    [allow (map car all-data)])
                        #'(let ((core core-e))
                            (let ((unpack unpack-e))
                              (let ((w wrap-e))
                                (attach-kwinfo!
                                 w
                                 (make-kwinfo checker-e unpack
                                              (quote req) (quote allow) #f)
                                 #f)))))))))

  (define-syntax new-define
    (lambda (stx)
      (let-values ([(id rhs) (normalize-definition stx #'new-lambda)])
        (define (plain rhs)
          (with-syntax ([i id] [r rhs])
            #'(define i r)))
        (syntax-case rhs ()
          [(head . _)
           (and (identifier? #'head) (free-identifier=? #'head #'new-lambda))
           (parse-lambda rhs stx id
                           plain
                           (lambda (core unpack wrap checker core-id unpack-id
                                    n-req opt-ns-data rest? arity req-data all-data)
                             (let* ([w-id (car (generate-temporaries (list id)))]
                                    [out
                                     (with-syntax ([f id]
                                           [core core-id]
                                           [core-stx
                                            (with-syntax ([c core-id])
                                              #'(syntax c))]
                                           [unpack unpack-id]
                                           [w w-id]
                                           [w-stx
                                            (with-syntax ([x w-id])
                                              #'(syntax x))]
                                           [core-e core]
                                           [unpack-e unpack]
                                           [wrap-e wrap]
                                           [checker-e checker]
                                           [n n-req]
                                           [opts opt-ns-data]
                                           [rst-stx
                                            (if rest?
                                                (with-syntax ([r rest?])
                                                  #'(syntax r))
                                                #f)]
                                           [req req-data]
                                           [allow (map car all-data)]
                                           [all all-data]
                                           [nm id])
                               #'(begin
                                   (define-syntax f
                                     (make-kw-operator core-stx w-stx n (quote opts) rst-stx
                                                       (quote req) (quote all)))
                                   (define core core-e)
                                   (define unpack unpack-e)
                                   (define w
                                     (let ((tmp wrap-e))
                                       (attach-kwinfo!
                                        tmp
                                        (make-kwinfo checker-e unpack
                                                     (quote req) (quote allow)
                                                     (quote nm))
                                        (quote nm))))))])
                                   (when (getenv "KW_TRACE")
                                     (write (syntax->datum out)
                                            (current-error-port))
                                     (newline (current-error-port)))
                                   out)))]
          [_ (plain rhs)]))))

  ;; Application with keyword arguments: calls carrying
  ;; literal keywords are parsed and sorted at expansion time, then checked
  ;; at run time; plain calls pass through to the core application form.
  (define-syntax new-app
    (lambda (stx)
      (syntax-case stx ()
        [(_ rator rand ...)
         (let ([rands (syntax->list #'(rand ...))])
           (if (and (exists keyword-literal? rands)
                    (not (interleaved-keyword-operator? #'rator)))
               (parse-application stx #'rator rands #'rator
                                  (lambda (args kw-args lifted? orig) orig))
               (with-syntax ([r #'rator] [(a ...) rands])
                 #'(|#%plain-app| r a ...))))]
        [(_) (syntax-violation '|#%app| "bad application form: missing operator" stx)]
        [_ (syntax-violation '|#%app| "bad application form" stx)])))

  ;; Keyword-aware `apply`: keyword
  ;; arguments to `apply` are forwarded to the target through
  ;; `keyword-apply`; without keywords it is the primitive `apply`.
  (define new-apply-proc
    (make-keyword-procedure
     (lambda (kws kw-args proc args . rest)
       (keyword-apply proc kws kw-args (apply cons* args rest)))
     apply))

  (define-syntax new-apply
    (lambda (stx)
      (syntax-case stx ()
        [(_ . args)
         (if (exists keyword-literal? (syntax->list #'args))
             #'(new-app new-apply-proc . args)
             #'(apply . args))]
        [_ #'new-apply-proc])))
  )
