;; A script to compile psyntax into R5RS code.

(import (capy compiler tree-il)
        (capy pretty-print)
        (capy)
        (scheme base)
        (scheme process-context))



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
            => (lambda (entry)
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

(define (squeeze-tree-il x)
  (post-order 
    (lambda (x)
      (if (constant? x)
        (make-constant (term-src x)
                       (squeeze-constant (constant-value x)))
        x))
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
                (let-lhs x)))))
        #f)))
  
  (define make-syntax-gensym (find-make-syntax-lexical-binding x))
  #f)

(define source (list-ref (command-line) 1))
(define target (list-ref (command-line) 2))

(define in (open-input-file source))
(define out (open-output-file target))

;(write '(eval-when (compile) (current-module (resolve-module '(capy) #f #f))) out)
;(newline out)

(let loop ((x (read in)))
  (if (eof-object? x)
    (begin 
      (close-port in)
      (close-port out))
    (begin 
      (pretty-print 
        (squeeze-tree-il 
          (tree-il->scheme 
            (macroexpand x 'c '(compile load eval))))
        out)
      (newline out)
      (loop (read in)))))

      