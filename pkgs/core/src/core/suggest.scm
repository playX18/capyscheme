(library (core suggest)
  (export install-import-suggestions
    suggest-module-name
    suggest-binding
    suggest-unbound
    find-modules-exporting
    loaded-modules
    levenshtein)
  (import (capy)
    (core files)
    (core exceptions))

  (define (name->string name)
    (format #f "~a" name))

  (define (normalize-name name)
    (let lp ([name name] [out '()])
      (cond
        [(null? name) (reverse out)]
        [(pair? (car name)) (lp (cdr name) out)]
        [else (lp (cdr name) (cons (car name) out))])))

  (define (insert-sorted lst item score)
    (cond
      [(null? lst) (list item)]
      [(<= (score item) (score (car lst))) (cons item lst)]
      [else (cons (car lst) (insert-sorted (cdr lst) item score))]))

  (define (dedupe lst)
    (let lp ([lst lst] [seen '()] [out '()])
      (cond
        [(null? lst) (reverse out)]
        [(member (car lst) seen) (lp (cdr lst) seen out)]
        [else (lp (cdr lst) (cons (car lst) seen) (cons (car lst) out))])))

  (define (levenshtein a b)
    (let* ([m (string-length a)]
           [n (string-length b)]
           [w (+ m 1)]
           [d (make-vector (* w (+ n 1)) 0)])
      (do ([i 0 (+ i 1)]) ((> i m))
        (vector-set! d i i))
      (do ([j 0 (+ j 1)]) ((> j n))
        (vector-set! d (* j w) j))
      (do ([j 1 (+ j 1)]) ((> j n))
        (do ([i 1 (+ i 1)]) ((> i m))
          (let ([cost (if (char=? (string-ref a (- i 1)) (string-ref b (- j 1)))
                        0
                        1)])
            (vector-set! d (+ i (* j w))
              (min (+ (vector-ref d (+ (- i 1) (* j w))) 1)
                (+ (vector-ref d (+ i (* (- j 1) w))) 1)
                (+ (vector-ref d (+ (- i 1) (* (- j 1) w))) cost))))))
      (vector-ref d (+ m (* n w)))))

  ; The library keys whose instance exports something: the loaded libraries.
  (define (loaded-modules)
    (let lp ([keys (registered-library-keys (current-library-registry))] [out '()])
      (cond
        [(null? keys) (reverse out)]
        [else
         (let ([inst (library-instance (current-library-registry) (car keys))])
           (lp (cdr keys)
             (if (and inst (pair? (instance-symbols inst)))
               (cons (car keys) out)
               out)))])))

  (define (module-name-of key)
    (normalize-name key))

  (define (find-modules-exporting sym)
    (let lp ([keys (loaded-modules)] [out '()])
      (cond
        [(null? keys)
         (let ([scan (ensure-scan!)])
           (dedupe (append (reverse out)
                     (or (core-hash-ref (cdr scan) sym) '()))))]
        [else
         (let ([inst (library-instance (current-library-registry) (car keys))])
           (if (memq sym (instance-symbols inst))
             (lp (cdr keys) (cons (module-name-of (car keys)) out))
             (lp (cdr keys) out)))])))

  ;;; Views
  ;;;
  ;;; The names a program can import are the ones its package's view shows (see "Library keys
  ;;; of packages, and views" in boot/linker.scm), so a suggestion is a name the view maps to a
  ;;; library, never a key. Without a view the names are the keys.

  (define (boot-ref key name)
    (instance-ref (library-instance (root-library-registry) key) name))

  (define (current-view)
    ((boot-ref '(boot environments) 'current-library-view)))

  ;; The names VIEW shows the library KEY under.
  (define (view-names view key)
    ((boot-ref '(boot linker) 'linker-view-names) view key))

  (define (libmap-keys)
    ((boot-ref '(boot linker) 'linker-libmap-keys)))

  (define (loaded-module-names)
    (let ([view (current-view)])
      (if view
        (apply append (map (lambda (key) (view-names view key)) (loaded-modules)))
        (map module-name-of (loaded-modules)))))

  ;; The names of the libraries the libmap lists, as the current view shows them.
  (define (libmap-module-names)
    (let ([view (current-view)])
      (apply append
        (map (lambda (key)
              (if view
                (view-names view key)
                (if ((boot-ref '(boot linker) 'linker-package-key?) key) '() (list (module-name-of key)))))
          (libmap-keys)))))

  ;; Cache: (load-path-copy . (module-names . exports-hashtable))
  (define %scan-state #f)

  (define (library-file? entry)
    (let ([len (string-length entry)])
      (and (>= len 4)
        (let find-dot ([i 0])
          (cond
            [(>= i len) #f]
            [(char=? (string-ref entry i) #\.)
             (member (substring entry i len)
               '(".scm" ".sls" ".sld" ".sps" ".sch" ".ss"))]
            [else (find-dot (+ i 1))])))))

  (define (export-spec-name spec)
    ;; (export (rename internal external) ...): the visible name is the last.
    (if (pair? spec)
      (let lp ([x spec])
        (if (pair? (cdr x)) (lp (cdr x)) (car x)))
      spec))

  (define (extract-exports clauses)
    (let lp ([clauses clauses] [out '()])
      (cond
        [(null? clauses) (reverse out)]
        [(and (pair? (car clauses)) (eq? (caar clauses) 'export))
         (lp (cdr clauses)
           (append (map export-spec-name (cdar clauses)) out))]
        [else (lp (cdr clauses) out)])))

  (define (read-library-declaration path)
    ;; Reads the leading (library (name) ...) or (define-library (name) ...)
    ;; form of a library file; returns (name exports) or #f.
    (guard (e [else #f])
      (call-with-input-file
        path
        (lambda (in)
          (let lp ([n 0])
            (if (> n 8)
              #f
              (let ([form (read in)])
                (cond
                  [(eof-object? form) #f]
                  [(and (pair? form)
                     (memq (car form) '(library define-library))
                     (pair? (cdr form))
                     (list? (cadr form)))
                   (list (cadr form) (extract-exports (cddr form)))]
                  [else (lp (+ n 1))]))))))))

  (define (scan-directory dir names exports)
    ;; Returns the accumulated module names; exports is extended in place.
    (let lp ([entries (directory-list dir)] [names names])
      (cond
        [(null? entries) names]
        [else
         (let ([path (in-vicinity dir (car entries))])
           (cond
             [(file-directory? path) (lp (cdr entries) (scan-directory path names exports))]
             [(file-regular? path)
              (if (library-file? (car entries))
                (let ([decl (read-library-declaration path)])
                  (if decl
                    (let ([name (normalize-name (car decl))])
                      (for-each
                        (lambda (sym)
                          (core-hash-set! exports sym
                            (cons name (or (core-hash-ref exports sym) '()))))
                        (cadr decl))
                      (lp (cdr entries) (cons name names)))
                    (lp (cdr entries) names)))
                (lp (cdr entries) names))]
             [else (lp (cdr entries) names)]))])))

  (define (ensure-scan!)
    (if (and %scan-state (equal? (car %scan-state) %load-path))
      (cdr %scan-state)
      (let ([exports (make-core-hashtable 'eqv?)])
        (let lp ([dirs %load-path] [names '()])
          (cond
            [(null? dirs)
             (let ([result (cons names exports)])
               (set! %scan-state (cons %load-path result))
               result)]
            [else
             (lp (cdr dirs)
               (guard (e [else names])
                 (scan-directory (car dirs) names exports)))])))))

  (define (scan-module-names)
    (car (ensure-scan!)))

 
  (define (name-list->string names)
    (let lp ([names names] [out ""])
      (cond
        [(null? names) out]
        [(null? (cdr names)) (string-append out (name->string (car names)))]
        [else (lp (cdr names) (string-append out (name->string (car names)) ", "))])))

  ;; The names the interface MOD binds.
  (define (interface-names mod)
    (let ([out '()])
      ((boot-ref '(boot environments) 'env-for-each) (lambda (sym var) (set! out (cons sym out))) mod)
      out))

  ;; The hint for the name SYM that the library interface MOD does not export: the libraries
  ;; that export it, or else the names MOD exports that are close to it.
  (define (suggest-binding sym mod)
    (let ([found (find-modules-exporting sym)])
      (if (null? found)
        (let ([similar (take-up-to (similar-names (symbol->string sym)
                                     (guard (e [else '()]) (interface-names mod)))
                         3)])
          (cond
            [(null? similar) #f]
            [(null? (cdr similar)) (format #f "did you mean `~a`?" (car similar))]
            [else (format #f "did you mean one of ~a?" (quote-names similar))]))
        (format #f "~a is exported by ~a; did you mean (import ~a)?"
          sym (name-list->string found) (name->string (car found))))))

  ;;; Unbound variables
  ;;;
  ;;; The help of the diagnostic of a name that nothing binds (see `unbound-suggester` in
  ;;; boot/units.scm): the names in scope that are close to it, and the libraries that export it.

  ;; How far a name may be from SYM to be a likely typo for it.
  (define (typo-distance len)
    (max 1 (quotient len 3)))

  ;; The names of CANDIDATES that look like a typo of the name NAME, closest first.
  (define (similar-names name candidates)
    (let* ([len (string-length name)]
           [limit (typo-distance len)])
      (let lp ([cands candidates] [found '()])
        (cond
          [(null? cands)
           (map cdr
             (list-sort (lambda (a b)
                         (or (< (car a) (car b))
                           (and (= (car a) (car b)) (string<? (cdr a) (cdr b)))))
               found))]
          [else
           (let ([cand (symbol->string (car cands))])
             (if (and (<= (abs (- (string-length cand) len)) limit)
                  (not (string=? cand name)))
               (let ([d (levenshtein name cand)])
                 (lp (cdr cands) (if (<= d limit) (cons (cons d cand) found) found)))
               (lp (cdr cands) found)))]))))

  (define (take-up-to l n)
    (if (or (null? l) (= n 0)) '() (cons (car l) (take-up-to (cdr l) (- n 1)))))

  (define (quote-names names)
    (let lp ([names names] [out ""])
      (cond
        [(null? names) out]
        [else (lp (cdr names)
                (string-append out (if (string=? out "") "" ", ") "`" (car names) "`"))])))

  ;; The libraries of the load path and the loaded ones that export SYM, as the strings that name
  ;; them, ordered by name. Internal libraries ((boot ...) and (core ...)) are left out
  ;; unless they are all there is.
  (define (exporting-library-names sym)
    (let* ([all (dedupe (map (lambda (name) (name->string (srfi-plain-name name)))
                          (find-modules-exporting sym)))]
           [public (filter (lambda (n) (not (or (starts-with? "(boot " n) (starts-with? "(core " n)
                                              (string=? n "(capy)") (string=? n "(core)"))))
                     all)])
      (list-sort string<? (if (null? public) all public))))

  ;; NAME with an R6RS SRFI number `:N` written as `N`: (srfi :1) and (srfi 1) are one library.
  (define (srfi-plain-name name)
    (if (and (pair? name) (eq? (car name) 'srfi) (pair? (cdr name)) (symbol? (cadr name)))
      (let* ([s (symbol->string (cadr name))]
             [n (and (> (string-length s) 1) (char=? (string-ref s 0) #\:)
                     (string->number (substring s 1 (string-length s)) 10))])
        (if (and n (exact-integer? n) (>= n 0))
          (cons 'srfi (cons n (cddr name)))
          name))
      name))

  (define (starts-with? prefix s)
    (and (>= (string-length s) (string-length prefix))
      (string=? (substring s 0 (string-length prefix)) prefix)))

  ;; The help lines for the unbound variable SYM, VISIBLE being the symbols bound where it is used.
  (define (suggest-unbound sym visible)
    (let ([similar (take-up-to (similar-names (symbol->string sym) visible) 3)]
          [libraries (take-up-to (exporting-library-names sym) 3)])
      (append
        (cond
          [(null? similar) '()]
          [(null? (cdr similar))
           (list (format #f "a binding with a similar name exists: `~a`" (car similar)))]
          [else
           (list (format #f "bindings with similar names exist: ~a" (quote-names similar)))])
        (if (null? libraries)
          '()
          (list (format #f "`~a` is exported by ~a; add it to the imports"
                  sym
                  (let lp ([ls libraries] [out ""])
                    (cond
                      [(null? ls) out]
                      [else (lp (cdr ls) (string-append out (if (string=? out "") "" ", ") (car ls)))]))))))))

  (define (fuzzy-module-names requested)
    (define req (name->string (normalize-name requested)))
    (define (score cand)
      (let ([cs (name->string cand)])
        (cond
          [(string=? req cs) 0]
          [(let ([d (levenshtein req cs)])
             (if (<= d 2) (+ 2 d) #f))]
          [else #f])))
    (let lp ([cands (dedupe (append (loaded-module-names) (libmap-module-names)
                              (if (current-view) '() (scan-module-names))))]
             [scored '()])
      (cond
        [(null? cands) (reverse scored)]
        [else
         (let ([s (score (car cands))])
           (if s
             (lp (cdr cands) (insert-sorted scored (car cands) score))
             (lp (cdr cands) scored)))])))

  (define (suggest-module-name name)
    (let ([found (fuzzy-module-names name)])
      (if (null? found)
        #f
        (format #f "did you mean (import ~a)?" (name->string (car found))))))

  (define (install-import-suggestions)
    (install-import-suggestion-hook!
      (lambda (kind . args)
        (case kind
          [(module) (suggest-module-name (car args))]
          [(binding) (suggest-binding (car args) (cadr args))]
          [else #f])))))
