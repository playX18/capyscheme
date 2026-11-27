(library (core compiler cps primitives)
  (export
    primitive-known? primitive-branch-fusable? primitive-table-ref
    string-length-offset class-id-string)
  (import (core) (capy))

  (define string-length-offset 16)
  (define class-id-string 28)

  ;; Copied from Primitive::is_branch_fusable in def.rs.
  (define fallback-branch-fusable
    '(< <= > >= = fx<? fx<=? fx>? fx>=? fx=? fl<? fl<=? fl>? fl>=? fl=?
       eqv? null? pair? vector? bytevector? string? symbol? char? boolean?
       procedure? flonum? fixnum? exact-integer? number? complex? real? eq?
       equal? eof-object? list? unspecified? not nan? inexact? exact? integer?
       rational?))

  ;; name -> props alist, built once.  Lookup only, never iterated.
  (define cache #f)

  (define (load-table)
    (let ([entries (guard (e [#t #f]) (eval '(%primitive-table)))]
          [table (make-eq-hashtable)])
      (if (and entries (pair? entries))
        (for-each
          (lambda (entry)
            (when (pair? entry)
              (hashtable-set! table (car entry) (cdr entry))))
          entries)
        (for-each
          (lambda (name)
            (hashtable-set! table name '((id . 0) (branch-fusable . #t))))
          fallback-branch-fusable))
      table))

  (define (table)
    (or cache
      (let ([t (load-table)])
        (set! cache t)
        t)))

  ;; The props alist of primitive NAME, or #f when it is not a known primitive.
  (define (primitive-table-ref name)
    (hashtable-ref (table) name #f))

  ;; Primitive::from_name is Some: an entry with an id (names that only have a
  ;; CPS transformer are in the table without one).
  (define (primitive-known? name)
    (let ([props (and (symbol? name) (primitive-table-ref name))])
      (and props (assq 'id props) #t)))

  (define (primitive-branch-fusable? name)
    (let ([props (and (symbol? name) (primitive-table-ref name))])
      (and props
        (let ([p (assq 'branch-fusable props)])
          (and p (cdr p) #t))))))
