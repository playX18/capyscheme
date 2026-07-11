(import (core)
        (srfi 64)
        (capy)
        (capy compiler tree-il)
        (capy compiler tree-il terms))

(test-begin "phase0 tree-il shape characterization")

(define (any pred xs)
  (cond
    [(null? xs) #f]
    [(pred (car xs)) #t]
    [else (any pred (cdr xs))]))

(define (tree-contains? pred term)
  (or (pred term)
      (cond
        [(lset? term)
         (tree-contains? pred (lset-value term))]
        [(module-set? term)
         (tree-contains? pred (module-set-value term))]
        [(toplevel-set? term)
         (tree-contains? pred (toplevel-set-value term))]
        [(toplevel-define? term)
         (tree-contains? pred (toplevel-define-value term))]
        [(if? term)
         (or (tree-contains? pred (if-test term))
             (tree-contains? pred (if-then term))
             (tree-contains? pred (if-else term)))]
        [(let? term)
         (or (any (lambda (x) (tree-contains? pred x)) (let-rhs term))
             (tree-contains? pred (let-body term)))]
        [(fix? term)
         (or (any (lambda (x) (tree-contains? pred x)) (fix-rhs term))
             (tree-contains? pred (fix-body term)))]
        [(receive? term)
         (or (tree-contains? pred (receive-producer term))
             (tree-contains? pred (receive-consumer term)))]
        [(application? term)
         (or (tree-contains? pred (application-operator term))
             (any (lambda (x) (tree-contains? pred x)) (application-operands term)))]
        [(primcall? term)
         (any (lambda (x) (tree-contains? pred x)) (primcall-args term))]
        [(proc? term)
         (tree-contains? pred (proc-body term))]
        [(values? term)
         (any (lambda (x) (tree-contains? pred x)) (values-values term))]
        [(sequence? term)
         (or (tree-contains? pred (sequence-head term))
             (tree-contains? pred (sequence-tail term)))]
        [(wcm? term)
         (or (tree-contains? pred (wcm-key term))
             (tree-contains? pred (wcm-mark term))
             (tree-contains? pred (wcm-result term)))]
        [else #f])))

(define (datum-contains? pred datum)
  (or (pred datum)
      (cond
        [(pair? datum)
         (or (datum-contains? pred (car datum))
             (datum-contains? pred (cdr datum)))]
        [(vector? datum)
         (any (lambda (x) (datum-contains? pred x)) (vector->list datum))]
        [else #f])))

(define expanded-library
  (expand
    '(library (tests phase0 tree-il-snapshot)
       (export exported-value)
       (import (core))
       (define exported-value (+ 40 2)))))

(define residual-scheme (tree-il->scheme expanded-library))

(test-assert "expanded library contains current toplevel-define nodes"
  (tree-contains? toplevel-define? expanded-library))

(test-assert "expanded library contains current module-ref nodes"
  (tree-contains? module-ref? expanded-library))

(test-assert "scheme residual includes define forms"
  (datum-contains? (lambda (x) (eq? x 'define)) residual-scheme))

(test-assert "scheme residual includes module reference forms"
  (datum-contains? (lambda (x) (or (eq? x '@) (eq? x '@@))) residual-scheme))

(test-end "phase0 tree-il shape characterization")
