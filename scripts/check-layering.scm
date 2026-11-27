;;; Checks the layering of the packages of the tree.

(import (rnrs) (only (capy) directory-list file-directory?) (capy pkg manifest))

(define violations 0)

(define (violation! file fmt . args)
  (set! violations (+ violations 1))
  (display file (current-error-port))
  (display ": " (current-error-port))
  (display fmt (current-error-port))
  (for-each (lambda (a) (display " " (current-error-port)) (write a (current-error-port))) args)
  (newline (current-error-port)))

(define (scheme-file? name)
  (let ([n (string-length name)])
    (and (> n 4) (string=? (substring name (- n 4) n) ".scm"))))

;; Every .scm file under DIR, sorted.
(define (source-files dir)
  (let loop ([names (list-sort string<? (directory-list dir))] [out '()])
    (cond
      [(null? names) (reverse out)]
      [else
        (let ([path (string-append dir "/" (car names))])
          (cond
            [(file-directory? path) (loop (cdr names) (append (reverse (source-files path)) out))]
            [(scheme-file? (car names)) (loop (cdr names) (cons path out))]
            [else (loop (cdr names) out)]))])))

(define (read-datums path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([out '()])
        (let ([x (read port)])
          (if (eof-object? x) (reverse out) (loop (cons x out))))))))

;; The library name an import spec refers to.
(define (spec-library spec)
  (cond
    [(and (pair? spec) (memq (car spec) '(only except prefix rename for library)))
      (spec-library (cadr spec))]
    [(and (pair? spec) (list? spec))
      ;; a trailing version reference is not part of the name
      (let loop ([s spec] [out '()])
        (cond
          [(null? s) (reverse out)]
          [(pair? (car s)) (reverse out)]
          [else (loop (cdr s) (cons (car s) out))]))]
    [else #f]))

;; (name . imports) for every form of a file that imports: the library forms (NAME is their name)
;; and the top-level `|#%require|` and `import` forms of a boot file (NAME is #f).
(define (file-facts datums)
  (define (import-clauses clauses)
    (let loop ([clauses clauses] [out '()])
      (cond
        [(null? clauses) (reverse out)]
        [(and (pair? (car clauses)) (eq? (car (car clauses)) 'import))
          (loop (cdr clauses) (append (reverse (cdr (car clauses))) out))]
        [else (loop (cdr clauses) out)])))
  (let loop ([datums datums] [out '()])
    (cond
      [(null? datums) (reverse out)]
      [(not (pair? (car datums))) (loop (cdr datums) out)]
      [else
        (let ([form (car datums)])
          (case (car form)
            [(library define-library)
              (loop (cdr datums) (cons (cons (cadr form) (import-clauses (cddr form))) out))]
            [(|#%require| import)
              (loop (cdr datums) (cons (cons #f (cdr form)) out))]
            [else (loop (cdr datums) out)]))])))

(define (check-package manifest manifests)
  (let* ([deps (map cdr (manifest-path-dependencies manifest manifests))]
         [core (find (lambda (m) (eq? (manifest-name m) 'core)) manifests)]
         [visible (append (list manifest) deps (if core (list core) '()))]
         [own (manifest-provides manifest)])
    (for-each
      (lambda (file)
        (guard (e [#t (violation! file "cannot read the file:" (condition-message* e))])
          (for-each
            (lambda (fact)
              (let ([name (car fact)])
                (when (and name (not (provides-match? own name)))
                  (violation! file "defines a library the package does not provide:" name))
                (for-each
                  (lambda (spec)
                    (let ([lib (spec-library spec)])
                      (when lib
                        (unless (exists (lambda (m) (provides-match? (manifest-provides m) lib)) visible)
                          (violation! file
                            (string-append "the package " (symbol->string (manifest-name manifest))
                              " imports a library of no package it depends on:")
                            lib
                            (let ([owner (find (lambda (m) (provides-match? (manifest-provides m) lib)) manifests)])
                              (if owner (list 'from (manifest-name owner)) '(from nowhere))))))))
                  (cdr fact))))
            (file-facts (read-datums file)))))
      (source-files (manifest-source-directory manifest)))))

(define (condition-message* e)
  (if (message-condition? e) (condition-message e) e))

(let ([args (cdr (command-line))])
  (if (not (= (length args) 1))
    (begin
      (display "usage: check-layering.scm PKGS-DIR\n" (current-error-port))
      (exit 2)))
  (let ([manifests (read-manifest-tree (car args))])
    (for-each (lambda (m) (check-package m manifests)) manifests)
    (cond
      [(> violations 0)
        (display (string-append "layering: " (number->string violations) " violation(s)\n")
          (current-error-port))
        (exit 1)]
      [else (display "layering: ok\n")])))
