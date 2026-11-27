;;; What the decl of a compiled library says about its exports and replacements: it has to be a
;;; function of the library form and its imports.

(import (srfi 64) (rnrs) (capy))

(define (decl-of library-form)
  (let* ([dir (string-append (or (getenv "TMPDIR") "/tmp") "/capy-library-decls-"
                (number->string (random 1000000000)))]
         [source (string-append dir ".scm")]
         [compiled (string-append dir ".fasl")])
    (call-with-output-file source (lambda (p) (write library-form p)))
    (compile-file source compiled (current-namespace) #f)
    (let ([decl (cadddr (car (%fasl-directory compiled)))])
      (delete-file source)
      (delete-file compiled)
      decl)))

(define (decl-exports decl) (vector-ref decl 3))
(define (decl-replacements decl) (vector-ref decl 6))
(define (decl-interface-hash decl) (vector-ref decl 8))
(define (decl-knowns decl) (vector-ref decl 11))

;; A known without the inline body a small procedure carries: `(procedure mask)`.
(define (known-head known)
  (if (and (pair? known) (eq? (car known) 'procedure))
    (list (car known) (cadr known))
    known))

(test-group "library-decls.scm"
  ;; `filter` and `list-head` are bound in the core environment, `zzz-own` is not
  (let ([d (decl-of '(library (decl-test-a)
                       (export filter list-head zzz-own)
                       (import (rnrs))
                       (define filter (lambda (p l) l))
                       (define list-head (lambda (l n) l))
                       (define zzz-own 1)))])
    (test-equal "a name the core environment binds is not a replacement"
      '() (decl-replacements d))
    (test-equal "exports are listed by name"
      '(filter list-head zzz-own) (map car (decl-exports d)))
    (test-equal "knowns are those of every variable export"
      '(filter list-head zzz-own) (map car (decl-knowns d))))

  (let ([d (decl-of '(library (decl-test-b)
                       (export (replace filter) zzz-own)
                       (import (rnrs))
                       (define filter (lambda (p l) l))
                       (define zzz-own 1)))])
    (test-equal "a replace spec declares replacements" '(filter) (decl-replacements d))
    (test-equal "a replacement is exported" '(filter zzz-own) (map car (decl-exports d)))
    (test-equal "a replacement has a known like any other export"
      '(filter zzz-own) (map car (decl-knowns d))))

  (let ([d1 (decl-of '(library (decl-test-c) (export a b c) (import (rnrs))
                       (define a 1) (define b 2) (define c 3)))]
        [d2 (decl-of '(library (decl-test-c) (export c b a) (import (rnrs))
                       (define a 1) (define b 2) (define c 3)))])
    (test-equal "the order of the export spec does not matter to the interface hash"
      (decl-interface-hash d1) (decl-interface-hash d2))
    (test-equal "nor to the exports of the decl" (decl-exports d1) (decl-exports d2))
    (test-equal "nor to the knowns" (decl-knowns d1) (decl-knowns d2)))

  ;; a re-export carries the known of its origin
  (let ([d (decl-of '(library (decl-test-d) (export car map vector-map cons list)
                       (import (rnrs))))])
    (test-equal "a primitive re-exported from the runtime has its arity"
      '(procedure 2) (cdr (assq 'car (decl-knowns d))))
    (test-equal "a fixed arity of two" '(procedure 4) (cdr (assq 'cons (decl-knowns d))))
    (test-assert "a procedure that takes any number of arguments says nothing (mask -1 is left out)"
      (not (assq 'list (decl-knowns d))))
    (test-equal "a procedure of the runtime written in Scheme"
      '(procedure -4) (cdr (assq 'map (decl-knowns d))))
    (test-assert "a procedure of a library re-exported by (rnrs)"
      (eq? 'procedure (car (cdr (assq 'vector-map (decl-knowns d)))))))

  (let* ([a (decl-of '(library (decl-test-e-a) (export one two zzz)
                      (import (rnrs))
                      (define (one x) x)
                      (define (two x . rest) x)
                      (define zzz 5)))]
         [b (decl-of '(library (decl-test-e-b) (export one two zzz own)
                      (import (rnrs) (decl-test-e-a))
                      (define (own a b) a)))])
    (test-equal "a library that re-exports a procedure has the known of its origin"
      (list (assq 'one (decl-knowns a)) (assq 'two (decl-knowns a)) (assq 'zzz (decl-knowns a)))
      (list (assq 'one (decl-knowns b)) (assq 'two (decl-knowns b)) (assq 'zzz (decl-knowns b))))
    (test-equal "and its own"
      '(procedure 4) (known-head (cdr (assq 'own (decl-knowns b)))))
    (test-assert "a small procedure carries its body"
      (pair? (cddr (cdr (assq 'own (decl-knowns b))))))
    (test-equal "a known of the origin is a known of the re-export"
      '(procedure 2) (known-head (cdr (assq 'one (decl-knowns b)))))))

