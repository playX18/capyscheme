(library (core suggest)
  (export install-import-suggestions
    suggest-module-name
    suggest-binding
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

  (define (module-name-of m)
    (normalize-name (or (raw-module-name m) (module-name m))))

  (define (loaded-modules)
    ;; All modules in the resolve tree that have a public interface.
    (let lp ([pending (list (resolve-module '() #f #t))] [seen '()] [out '()])
      (cond
        [(null? pending) out]
        [(memq (car pending) seen) (lp (cdr pending) seen out)]
        [else
         (let* ([m (car pending)]
                [seen* (cons m seen)]
                [iface (module-public-interface m)]
                [out* (if (and iface (not (eq? iface m)))
                        (cons m out)
                        out)])
           (let lp2 ([entries (core-hash->alist (module-submodules m))]
                     [pending (cdr pending)]
                     [out out*])
             (cond
               [(null? entries) (lp pending seen* out)]
               [(module? (cdar entries))
                (lp2 (cdr entries) (cons (cdar entries) pending) out)]
               [else (lp2 (cdr entries) pending out)])))])))

  (define (find-modules-exporting sym)
    (let lp ([modules (loaded-modules)] [out '()])
      (cond
        [(null? modules)
         (let ([scan (ensure-scan!)])
           (dedupe (append (reverse out)
                     (or (core-hash-ref (cdr scan) sym) '()))))]
        [else
         (let ([m (car modules)])
           (let ([iface (module-public-interface m)])
             (if (and iface (not (eq? iface m))
                  (core-hash-ref (module-obarray iface) sym))
               (lp (cdr modules) (cons (module-name-of m) out))
               (lp (cdr modules) out))))])))

  (define (loaded-module-names)
    (map module-name-of (loaded-modules)))

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

  (define (suggest-binding sym mod)
    (let ([found (find-modules-exporting sym)])
      (if (null? found)
        #f
        (format #f "~a is exported by ~a; did you mean (import ~a)?"
          sym (name-list->string found) (name->string (car found))))))

  (define (fuzzy-module-names requested)
    (define req (name->string (normalize-name requested)))
    (define (score cand)
      (let ([cs (name->string cand)])
        (cond
          [(string=? req cs) 0]
          [(let ([d (levenshtein req cs)])
             (if (<= d 2) (+ 2 d) #f))]
          [else #f])))
    (let lp ([cands (append (loaded-module-names) (scan-module-names))]
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
