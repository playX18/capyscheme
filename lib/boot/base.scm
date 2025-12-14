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
                       docstring ...        ; optional docstring
                       #((macro-type . syntax-rules)
                         (patterns pattern ...)) ; embed patterns as procedure metadata
                       (syntax-case x (k ...)
                         clause ...))))
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
         (let f ((c1  (syntax c1))
                 (c2* (syntax (c2 ...))))
           (syntax-case c2* ()
             (()
              (syntax-case c1 (else =>)
                ((else e1 e2 ...) (syntax (let () e1 e2 ...)))
                ((e0)             (syntax (let ((t e0)) (if t t))))
                ((e0 => e1)       (syntax (let ((t e0)) (if t (e1 t)))))
                ((e0 e1 e2 ...)   (syntax (if e0 (let () e1 e2 ...))))
                (_                (syntax-violation
                                   'cond "Invalid expression" x))))
             ((c2 c3 ...)
              (with-syntax ((rest (f (syntax c2)
                                     (syntax (c3 ...)))))
                (syntax-case c1 (else =>)
                  ((e0)           (syntax (let ((t e0)) (if t t rest))))
                  ((e0 => e1)     (syntax (let ((t e0)) (if t (e1 t) rest))))
                  ((e0 e1 e2 ...) (syntax (if e0 (let () e1 e2 ...) rest)))
                  (_              (syntax-violation
                                   'cond "Invalid expression" x)))))))))))

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
       clauses ...)
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
       clause clauses ...)
      (if (memv key '(atoms ...))
          (result key)
          (case key clause clauses ...)))

     ((case key
       ((atoms ...) result1 result2 ...))
      (if (memv key '(atoms ...))
          (let () result1 result2 ...)))

     ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
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
           (with-syntax (((k _)     x) ;; original identifier must be copied
                         ((e* reps) (expand (syntax e) (+ level 1))))
             (syntax ((k e*) reps))))                                  
          ((unsyntax e)
           (= level 0)
           (with-syntax (((t) (generate-temporaries '(t))))
             (syntax (t ((t e))))))
          (((unsyntax e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)     (generate-temporaries (syntax (e ...)))))
             (syntax ((t ... . r*)
                      ((t e) ... rep ...)))))
          (((unsyntax-splicing e ...) . r)
           (= level 0)
           (with-syntax (((r* (rep ...)) (expand (syntax r) 0))
                         ((t ...)     (generate-temporaries (syntax (e ...)))))
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

(define-syntax define-module 
    (lambda (x)
        (syntax-case x () 
            [(_ (name name* ...) body ...)
                #'(begin 
                    (eval-when (expand load eval)
                        (let ([m ((@@ (capy) define-module*) '(name name* ...))])
                            (current-module m)))
                    (@@ @@ (name name* ...) body) ...
                    )])))
(define-syntax define-pure-module 
    (lambda (x)
        (syntax-case x () 
            [(_ (name name* ...) body ...)
                #'(begin 
                    (eval-when (expand load eval)
                        (let ([m ((@@ (capy) define-module*) '(name name* ...) #t)])
                            (current-module m)))
                    (@@ @@ (name name* ...) body) ...
                    )])))


(define (resolve-r6rs-interface import-spec)
  (define (sym? stx)
    (symbol? (syntax->datum stx)))
  (define (n? stx)
    (let ((n (syntax->datum stx)))
      (and (exact-integer? n)
           (not (negative? n)))))

  (define (colon-n? x)
    (let ((sym (syntax->datum x)))
      (and (symbol? sym)
           (let ((str (symbol->string sym)))
             (and (string-prefix? ":" str)
                  (let ((num (string->number (substring str 1))))
                    (and (exact-integer? num)
                         (not (negative? num)))))))))

  (define (srfi-name? stx)
    (syntax-case stx (srfi)
      ((srfi n rest ...)
       (and (and-map sym? #'(rest ...))
            (or (n? #'n)
                (colon-n? #'n))))
      (_ #f)))

  (define (module-name? stx)
    (or (srfi-name? stx)
        (syntax-case stx ()
          ((name name* ...)
           (and-map sym? #'(name name* ...)))
          (_ #f))))

  (define (make-srfi-n context n)
    (datum->syntax
     context
     (string->symbol
      (string-append
       "srfi-"
       (let ((n (syntax->datum n)))
         (if (symbol? n)
             (substring (symbol->string n) 1)
             (number->string n)))))))

  (define (make-custom-interface mod)
    (let ((iface (make-module)))
      (set-module-kind! iface 'custom-interface)
      (set-module-name! iface (module-name mod))
      iface))
  (define (module-for-each/nonlocal f mod)
    (define (module-and-uses mod)
      (let lp ((in (list mod)) (out '()))
        (cond
         ((null? in) (reverse out))
         ((memq (car in) out) (lp (cdr in) out))
         (else (lp (append (module-uses (car in)) (cdr in))
                   (cons (car in) out))))))
    (for-each (lambda (mod)
                (module-for-each f mod))
              (module-and-uses mod)))
  (syntax-case import-spec (library only except prefix rename srfi)
    ;; (srfi :n ...) -> (srfi srfi-n ...)
    ;; (srfi n ...) -> (srfi srfi-n ...)
    ((library (srfi n rest ... (version ...)))
     (srfi-name? #'(srfi n rest ...))
     (let ((srfi-n (make-srfi-n #'srfi #'n)))
       (resolve-r6rs-interface
        (syntax-case #'(rest ...) ()
          (()
           #`(library (srfi #,srfi-n (version ...))))
          ((name rest ...)
           ;; SRFI 97 says that the first identifier after the `n'
           ;; is used for the libraries name, so it must be ignored.
           #`(library (srfi #,srfi-n rest ... (version ...))))))))
   
    
    ((library (name name* ... (version ...)))
      (and-map sym? #'(name name* ...))
      (resolve-interface (syntax->datum #'(name name* ...)) #f '() #f))

    ((library (name name* ...))
     (and-map sym? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... ()))))
     ;(resolve-interface (syntax->datum #'(name name* ...)) #f '() #f))

    
    ((only import-set identifier ...)
     (and-map sym? #'(identifier ...))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (for-each (lambda (sym)
                   (module-add! iface sym
                                (or (module-variable mod sym)
                                    (error 'import (format
                                             #f
                                             "no binding '~a' in module ~a"
                                             sym mod (module-uses mod)))))
                   (if (core-hash-ref (module-replacements mod) sym)
                     (core-hash-put! (module-replacements iface) sym #t)))
                 (syntax->datum #'(identifier ...)))
       iface))
    
    ((except import-set identifier ...)
     (and-map sym? #'(identifier ...))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod)))
       (module-for-each/nonlocal (lambda (sym var)
                                   (module-add! iface sym var))
                                 mod)
       (for-each (lambda (sym)
                   (if (not (module-local-variable iface sym))
                     (error 'import (format #f "no binding '~a' in module ~a ~a" sym mod (module-uses mod) )))
                   (module-remove! iface sym))
                 (syntax->datum #'(identifier ...)))
       iface))

    ((prefix import-set identifier)
     (sym? #'identifier)
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (iface (make-custom-interface mod))
            (pre (syntax->datum #'identifier)))
       (module-for-each/nonlocal
        (lambda (sym var)
          (let ((sym* (symbol-append pre sym)))
            (module-add! iface sym* var)
            (if (core-hash-ref (module-replacements mod) sym)
              (core-hash-put! (module-replacements iface) sym* #t))))
        mod)
       iface))

    ((rename import-set (from to) ...)
     (and (and-map sym? #'(from ...)) (and-map sym? #'(to ...)))
     (let* ((mod (resolve-r6rs-interface #'import-set))
            (replacements (module-replacements mod))
            (iface (make-custom-interface mod)))
       (module-for-each/nonlocal
        (lambda (sym var) (module-add! iface sym var))
        mod)
       (let lp ((in (syntax->datum #'((from . to) ...))) (out '()))
         (cond
          ((null? in)
           (for-each
            (lambda (v)
              (let ((to (vector-ref v 0))
                    (replace? (vector-ref v 1))
                    (var (vector-ref v 2)))
                (if (module-local-variable iface to)
                  (error 'import (format
                           #f
                           "duplicate binding for '~a' in module ~a (rename uses ~a)"
                           to
                           mod
                           (module-uses mod))))
               
                (module-add! iface to var)
                (if replace?
                  (core-hash-put! replacements to #t))))
            out)

           iface)
          (else
           (let* ((from (caar in))
                  (to (cdar in))
                  (var (module-variable mod from))
                  (replace? (core-hash-ref replacements from)))
             (if (not var) (error 'resolve-r6rs-interface
                           (format #f "no binding `~a` in module ~a" from mod)))
             (module-remove! iface from)
             (core-hash-remove! replacements from)
             (lp (cdr in) (cons (vector to replace? var) out))))))))
    
    ((name name* ... (version ... ))
     (module-name? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... (version ...)))))

    ((name name* ...)
     (module-name? #'(name name* ...))
     (resolve-r6rs-interface #'(library (name name* ... ()))))))

(define-syntax export 
    (syntax-rules ()
        [(_ name ...)
            (eval-when (expand load eval)
                (module-export! (current-module) '(name ...)))]))

(define-syntax re-export 
    (syntax-rules ()
        [(_ name ...)
            (eval-when (expand load eval)
                (module-re-export! (current-module) '(name ...)))]))

(define-syntax export! 
    (syntax-rules ()
        [(_ name ...)
            (eval-when (expand load eval)
                (module-replace! (current-module) '(name ...)))]))

(define-syntax export-syntax 
    (syntax-rules ()
        [(_ name ...)
            (export name ...)]))

(define-syntax re-export-syntax 
    (syntax-rules ()
        [(_ name ...)
            (re-export name ...)]))

(define-syntax library
  (lambda (stx)
    (define (sym? stx)
      (symbol? (syntax->datum stx)))

    (define (n? stx)
      (let ((n (syntax->datum stx)))
        (and (exact-integer? n)
             (not (negative? n)))))

    (define (colon-n? x)
      (let ((sym (syntax->datum x)))
        (and (symbol? sym)
             (let ((str (symbol->string sym)))
               (and (string-prefix? ":" str)
                    (let ((num (string->number (substring str 1))))
                      (and (exact-integer? num)
                           (not (negative? num)))))))))

    (define (srfi-name? stx)
      (syntax-case stx (srfi)
        ((srfi n rest ...)
         (and (and-map sym? #'(rest ...))
              (or (n? #'n)
                  (colon-n? #'n))))
        (_ #f)))

    (define (module-name? stx)
      (or (srfi-name? stx)
          (syntax-case stx ()
            ((name name* ...)
             (and-map sym? #'(name name* ...)))
            (_ #f))))

    (define (make-srfi-n context n)
      (datum->syntax
       context
       (string->symbol
        (string-append
         "srfi-"
         (let ((n (syntax->datum n)))
           (if (symbol? n)
               (substring (symbol->string n) 1)
               (number->string n)))))))

    (define (compute-exports ifaces specs)
      (define (re-export? sym)
        (or-map (lambda (iface) 
          (module-variable iface sym)) ifaces))
      (define (replace? sym)
        (module-variable the-scm-module sym))
      
      (let lp ((specs specs) (e '()) (r '()) (x '()))
        (syntax-case specs (rename)
          (() (values e r x))
          (((rename (from to) ...) . rest)
           (and (and-map identifier? #'(from ...))
                (and-map identifier? #'(to ...)))
           (let lp2 ((in #'((from . to) ...)) (e e) (r r) (x x))
             (syntax-case in ()
               (() (lp #'rest e r x))
               (((from . to) . in)
                (cond
                 ((re-export? (syntax->datum #'from))
                  (lp2 #'in e (cons #'(from . to) r) x))
                 ((replace? (syntax->datum #'from))
                  (lp2 #'in e r (cons #'(from . to) x)))
                 (else
                  (lp2 #'in (cons #'(from . to) e) r x)))))))
          ((id . rest)
           (identifier? #'id)
           (let ((sym (syntax->datum #'id)))
             (cond
              ((re-export? sym)
               (lp #'rest e (cons #'id r) x))
              ((replace? sym)
               (lp #'rest e r (cons #'id x)))
              (else
               (lp #'rest (cons #'id e) r x))))))))
    (syntax-case stx (export import srfi)
      [(_ (srfi n rest ...)
          (export espec ...)
          (import ispec ...))
        (srfi-name? #'(srfi n rest ...))
        (let ((srfi-n (make-srfi-n #'srfi #'n)))
          #`(library (srfi #,srfi-n rest ... ())
              (export espec ...)
              (import ispec ...)))]
      ((_ (srfi n rest ... (version ...))
          (export espec ...)
          (import ispec ...)
          body ...)
       (srfi-name? #'(srfi n rest ...))
       (let ((srfi-n (make-srfi-n #'srfi #'n)))
         #`(library (srfi #,srfi-n rest ...)
             (export espec ...)
             (import ispec ...)
             body ...)))
      [(_ (name name* ...)
          (export espec ...)
          (import ispec ...)
          body ...)
          (module-name? #'(name name* ...))
          #`(library (name name* ... ())
              (export espec ...)
              (import ispec ...)
              body ...)]

      ((_ (name name* ... (version ...))
          (export espec ...)
          (import ispec ...)
	  body ...)
       (module-name? #'(name name* ...))
       (call-with-values
           (lambda ()
             (compute-exports 
              (map (lambda (im)
                     (syntax-case im (for)
                       ((for import-set import-level ...)
                        (resolve-r6rs-interface #'import-set))
                       (import-set (resolve-r6rs-interface #'import-set))))
                   #'(ispec ...))
              #'(espec ...)))
         (lambda (exports re-exports replacements)
           (with-syntax (((e ...) exports)
                         ((r ...) re-exports)
                         ((x ...) replacements))
             ;; It would be nice to push the module that was current before the
             ;; definition, and pop it after the library definition, but I
             ;; actually can't see a way to do that. Helper procedures perhaps,
             ;; around a fluid that is rebound in save-module-excursion? Patches
             ;; welcome!
             #'(begin
                 (define-pure-module (name name* ...))
                 
                 (import ispec)
                 ...
                 ;; (capy prelims) exports "plain" #%app form which must be imported
                 ;; by any library in order to correctly run code. 
                 (import (capy prelims))
                 (export e ...)
                 (re-export r ...)
                 (export! x ...)
                 (@@ @@ (name name* ...) body)
                 ...))))))))
    
(define-syntax import
  (lambda (stx)
    (define (strip-for import-set)
      (syntax-case import-set (for)
        ((for import-set import-level ...)
         #'import-set)
        (import-set
         #'import-set)))
    (syntax-case stx ()
      ((_ import-set ...)
       (with-syntax (((library-reference ...) (map strip-for #'(import-set ...))))
         #'(eval-when (expand load eval)
             (let ((iface (resolve-r6rs-interface 'library-reference)))
                   (module-use-interfaces! (current-module) (list iface)))
             ...
             (if #f #f)))))))


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
                 (set! dummy #f)  ; blackhole dummy
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
                 (set! dummy #f)  ; blackhole dummy
                 v))))))))


  (define-syntax do
    (lambda (orig-x)
      (syntax-case orig-x ()
        ((_ ((var init . step) ...) (e0 e1 ...) c ...)
         (with-syntax (((step ...)
                        (map (lambda (v s)
                               (syntax-case s ()
                                 (()  v)
                                 ((e) (syntax e))
                                 (_   (syntax-violation
                                       'do "Invalid step" orig-x s))))
                             (syntax (var ...))
                             (syntax (step ...)))))
           (syntax-case (syntax (e1 ...)) ()
             (()          (syntax (let do ((var init) ...)
                                    (if (not e0)
                                        (let () c ... (do step ...))))))
             ((e1 e2 ...) (syntax (let do ((var init) ...)
                                    (if e0
                                        (let () e1 e2 ...)
                                        (let () c ... (do step ...))))))))))))
  
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
                         b0 b1 ...)))
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
           body ...))))))
(define-syntax with-implicit
    (lambda (x)
      (syntax-case x ()
        [(_ (k x ...) e1 ... e2)
         #'(with-syntax ([x (datum->syntax #'k 'x)] ...)
             e1 ... e2)]
        [_ (syntax-violation 'with-implicit "invalid syntax" x)])))

(define-syntax false-if-exception 
  (lambda (stx)
    (syntax-case stx ()
      [(_ expr)
       #'(with-exception-handler 
        (lambda ex #f)
        (lambda () expr))])))

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
    exact-closed ieee-float full-unicode ratios ;; R7RS features.
    srfi-0   ;; cond-expand itself
    srfi-6   ;; string ports
    srfi-8   ;; `receive` syntax
    srfi-9   ;; define-record-type
    srfi-11  ;; Syntax for receiving multiple values
    srfi-13  ;; string library
    srfi-14  ;; character sets
    srfi-16  ;; case-lambda
    srfi-23  ;; `error` procedure
    srfi-24  ;; define-syntax in local lexical scope
    srfi-27  ;; random sources
    srfi-28  ;; basic format strings
    srfi-30  ;; nested multi-line comments
    srfi-34  ;; exception handling for programs
    srfi-36  ;; I/O conditions
    srfi-39  ;; parameterize
    srfi-46  ;; basic syntax-rules extensions
    srfi-48  ;; Intermediate format strings 
    srfi-55  ;; require-extension
    srfi-61  ;; general cond clause
    srfi-62  ;; s-expression comments
    srfi-64  ;; A Scheme API for test suites
    srfi-87  ;; => in case clauses
    srfi-105 ;; curly infix expressions
    srfi-124 ;; ephemerons
    srfi-130 ;; Cursor-based string library
    srfi-132 ;; sorting
    srfi-157 ;; Continuation marks
    srfi-180 ;; JSON
    ; srfi-226 ;; Control features (TODO: BROKEN!)
    srfi-259 ;; Tagged procedures with type safety
    ))

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
      (syntax-case condition (and or not)
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

(define-syntax define-library 
  (lambda (stx)
    (define (handle-includes filenames)
      (syntax-case filenames ()
        (() #'())
        ((filename . filenames)
         (append (call-with-include-port
                  #'filename
                  (lambda (p)
                    (let lp ()
                      (let ((x (read-syntax p)))
                        (if (eof-object? x)
                            #'()
                            (cons (datum->syntax #'filename x) (lp)))))))
                 (handle-includes #'filenames)))))
    (define (handle-cond-expand clauses)
      (define (has-req? req)
        (syntax-case req (and or not library)
          ((and req ...)
           (and-map has-req? #'(req ...)))
          ((or req ...)
           (or-map has-req? #'(req ...)))
          ((not req)
           (not (has-req? #'req)))
          ((library lib-name)
           (->bool
            (false-if-exception
             (resolve-r6rs-interface
              (syntax->datum #'lib-name)))))
          (id
           (identifier? #'id)
           (memq (syntax->datum #'id) %cond-expand-features))))
      (syntax-case clauses (else)
        (() #'())  ; R7RS says this is not specified :-/
        (((else decl ...))
         #'(decl ...))
        (((test decl ...) . clauses)
         (if (has-req? #'test)
             #'(decl ...)
             (handle-cond-expand #'clauses)))))
    (define (partition-decls decls exports imports code)
      (syntax-case decls (export import begin include include-ci
                                 include-library-declarations cond-expand)
        (() (values exports imports (reverse code)))
        (((export clause ...) . decls)
         (partition-decls #'decls (append exports #'(clause ...)) imports code))
        (((import clause ...) . decls)
         (partition-decls #'decls exports (append imports #'(clause ...)) code))
        (((begin expr ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin expr ...) code)))
        (((include filename ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin (include filename) ...) code)))
        (((include-ci filename ...) . decls)
         (partition-decls #'decls exports imports
                          (cons #'(begin (include-ci filename) ...) code)))
        (((include-library-declarations filename ...) . decls)
         (syntax-case (handle-includes #'(filename ...)) ()
           ((decl ...)
            (partition-decls #'(decl ... . decls) exports imports code))))
        (((cond-expand clause ...) . decls)
         (syntax-case (handle-cond-expand #'(clause ...)) ()
           ((decl ...)
            (partition-decls #'(decl ... . decls) exports imports code))))))

    (syntax-case stx ()
      ((_ name decl ...)
       (call-with-values (lambda ()
                           (partition-decls #'(decl ...) '() '() '()))
         (lambda (exports imports code)
           #`(library name
               (export . #,exports)
               (import . #,imports)
               . #,code)))))))

(define-syntax include-library-declarations
  (lambda (x)
    (syntax-violation
     'include-library-declarations
     "use of 'include-library-declarations' outside define-library" x x)))

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
        key value 
        (with-continuation-marks 
          (rest ...)
          body body* ...))])
)

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
        body1 body ...))]
    [(_ name value)
      (eval-when (expand load eval)
        (define name value))]))