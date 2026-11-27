;; A script to compile psyntax into R5RS code.
;;
;; pkgs/core/src/boot/psyntax.scm is the source of pkgs/core/src/boot/psyntax-exp.scm, which is its expansion and
;; is what the boot loads (the expander cannot expand itself while it runs). Never edit
;; psyntax-exp.scm by hand: change psyntax.scm and regenerate it with
;;
;;   stage-1/capy -s pkgs/core/tools/compile-psyntax.scm pkgs/core/src/boot/psyntax.scm pkgs/core/src/boot/psyntax-exp.scm
;;
;; (with a stage: `stage-1/capy-rt -s pkgs/core/tools/compile-psyntax.scm pkgs/core/src/boot/psyntax.scm pkgs/core/src/boot/psyntax-exp.scm`).

(import (core compiler tree-il)
  (core compiler tree-il terms)
  (core compiler tree-il fold)
  (core compiler tree-il primitives)
  (core pretty-print)
  (core)
  (capy))

(define (squeeze-syntax-object syn)
  (define (ensure-list x)
    (if (vector? x)
      (vector->list x)
      x))

  (define x (syntax-expression syn))
  (define wrap (syntax-wrap syn))
  (define mod (syntax-module syn))

  (define marks (car wrap))
  (define subst (cdr wrap))

  (define (squeeze-wrap marks subst)
    (make-syntax x (cons marks subst) mod))

  (cond
    [(symbol? x)
      (let loop ((marks marks) (subst subst))
        (cond
          [(null? subst) (squeeze-wrap marks subst)]
          [(eq? 'shift (car subst)) (loop (cdr marks) (cdr subst))]
          [(find (lambda (entry) (and (eq? x (car entry))
                                  (equal? marks (cadr entry))))
              (apply map list (map ensure-list (cdr (vector->list (car subst))))))
            =>
            (lambda (entry)
              (squeeze-wrap marks
                (list (list->vector
                       (cons 'ribcage
                         (map vector entry))))))]
          [else (loop marks (cdr subst))]))]
    [(or (pair? x) (vector? x)) syn]
    [else x]))

(define (squeeze-constant x)
  (cond
    [(syntax? x) (squeeze-syntax-object x)]
    [(pair? x)
      (cons (squeeze-constant (car x))
        (squeeze-constant (cdr x)))]
    [(vector? x)
      (list->vector (map squeeze-constant (vector->list x)))]
    [else x]))

;; The binders of `let`, `letrec` and `letrec*` keep their identifiers in the terms (syntax
;; objects, for tools that look for the binder by source); the generated code names them by symbol.
(define (symbol-id id)
  (if (syntax? id) (syntax->datum id) id))

(define (squeeze-tree-il x)
  (post-order
    (lambda (x)
      (cond
        [(constant? x)
          (make-constant (term-src x)
            (squeeze-constant (constant-value x)))]
        [(and (let? x) (exists syntax? (let-ids x)))
          (make-let (term-src x) (let-style x) (map symbol-id (let-ids x)) (let-lhs x)
            (let-rhs x) (let-body x))]
        [else x]))
    x))

(define (translate-literal-syntax-objects x)
  (define (find-make-syntax-lexical-binding x)
    (call/cc
      (lambda (return)
        (pre-order
          (lambda (x)
            (when (let? x)
              (for-each (lambda (name sym)
                         (when (eq? name 'make-syntax)
                           (return sym)))
                (let-ids x)
                (let-lhs x)))
            x)
          x)
        #f)))

  (define make-syntax-gensym (find-make-syntax-lexical-binding x))
  (define retry #f)
  (define (translate-constant x)
    (define src (term-src x))
    (define exp (constant-value x))
    (cond
      [(list? exp)
        (define exp* (map (lambda (x) (translate-constant (make-constant src x))) exp))
        (if (and-map constant? exp*)
          x
          (make-primcall src 'list exp*))]
      [(pair? exp)
        (define car* (translate-constant (make-constant src (car exp))))
        (define cdr* (translate-constant (make-constant src (cdr exp))))
        (if (and (constant? car*) (constant? cdr*))
          x
          (make-primcall src 'cons (list car* cdr*)))]
      [(vector? exp)
        (define exp* (map (lambda (x) (translate-constant (make-constant src x))) (vector->list exp)))
        (if (and-map constant? exp*)
          x
          (make-primcall src 'vector exp*))]
      [(syntax? exp)
        (make-application src
          (if make-syntax-gensym
            (make-lref src 'make-syntax make-syntax-gensym)
            (retry #t))
          (list
            (translate-constant
              (make-constant src (syntax-expression exp)))
            (translate-constant
              (make-constant src (syntax-wrap exp)))
            (translate-constant
              (make-constant src (syntax-module exp)))))]
      [else x]))

  (let loop ()
    (define res (call/cc
                 (lambda (return)
                   (set! retry return)
                   (post-order
                     (lambda (x)
                       (if (constant? x)
                         (translate-constant x)
                         x))
                     x))))
    (cond
      [(boolean? res)
        (translate-literal-syntax-objects
          (make-global-set
            (term-src x)
            (global-set-key x)
            (global-set-name x)
            (global-set-slot x)
            (make-let
              (term-src x)
              'let
              (list 'make-syntax)
              (list (env-gensym))
              (list (make-global-ref #f (global-set-key x) 'make-syntax #f))
              (global-set-value x))))]
      [else res])))

(define source (list-ref (program-arguments) 1))
(define target (list-ref (program-arguments) 2))

(define unit (make-unit 'boot '(boot psyntax-exp) the-core-env))
(current-expansion-env the-core-env)

(define (mark-definitions t)
  (cond
    [(sequence? t)
      (make-sequence (term-src t) (mark-definitions (sequence-head t))
        (mark-definitions (sequence-tail t)))]
    [(and (global-set? t) (equal? (global-set-key t) (unit-key unit)))
      (make-global-set (term-src t) (global-set-key t) (global-set-name t) 'define
        (global-set-value t))]
    [else t]))

(define in (open-input-file source))
(define out (open-output-file target))


(define boot-linklet-requires
  '((boot prim) (boot control) (boot records) (boot conditions)
    (boot violations) (boot raise) (boot exceptions) (boot expand) (boot interpreter)
    (boot enums) (boot sys) (boot osdep) (boot ports-core) (boot transcoder)
    (boot binary-ports) (boot textual-ports) (boot custom-ports) (boot file-ports)
    (boot console-ports) (boot port-api) (boot print) (boot format) (boot log)
    (boot match-syntax) (boot units) (boot environments) (boot library-manager)
    (boot reader)))

(write (cons '|#%require| boot-linklet-requires) out)
(newline out)
(write '(|#%provide| all-defined) out)
(newline out)
(newline out)
(write-string ";; Auto-generated by compile-psyntax.scm. Do not edit.\n\n" out)

(call-with-unit unit
  (lambda ()
    (let loop ((x (read in)))
      (if (eof-object? x)
        (begin
          (close-port in)
          (close-port out))
        (begin
          (pretty-print
            (tree-il->scheme
              (translate-literal-syntax-objects
                (squeeze-tree-il
                  (mark-definitions
                    (resolve-primitives (macroexpand x 'c '(compile load eval)) #f))))
              '(use-case? denoise-lexicals?))
            out)
          (newline out)
          (loop (read in)))))))
