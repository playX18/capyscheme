;; The first file that gets loaded during Scheme boot process. This file defines
;; various core functions, records, module system and then loads rest of the 
;; standard library. After this file is loaded, it's possible to use CLI.



(define (eq? x y) (eq? x y))
(define (eqv? x y) (eqv? x y))
(define (equal? x y) (equal? x y))
(define (null? x) (null? x))
(define (char? x) (char? x))
(define (char>=? x . ys)
  (let loop ([ys ys])
    (cond
      [(null? ys) #t]
      [else (and (char>=? x (car ys))
                 (loop (cdr ys)))])))

(define (char<=? x . ys)  
  (let loop ([ys ys])
    (cond
      [(null? ys) #t]
      [else (and (char<=? x (car ys))
                 (loop (cdr ys)))])))

(define (char>? x . ys)
  (let loop ([ys ys])
    (cond
      [(null? ys) #t]
      [else (and (char>? x (car ys))
                 (loop (cdr ys)))])))

(define (char<? x . ys)
  (let loop ([ys ys])
    (cond
      [(null? ys) #t]
      [else (and (char<? x (car ys))
                 (loop (cdr ys)))])))

(define (char=? x . ys)
  (let loop ([ys ys])
    (cond
      [(null? ys) #t]
      [else (and (char=? x (car ys))
                 (loop (cdr ys)))])))

(define (integer->char i)
  (integer->char i))
(define (char->integer c)
  (char->integer c))

(define (real? x) (real? x))
(define (inexact? x) (inexact? x))


(define (tuple->list x)
  (let lp ([i 0] [n (tuple-size x)] [acc '()])
    (if (< i n)
        (lp (+ i 1) n (cons (tuple-ref x i) acc))
        (reverse acc))))

(define (tuple-index x v)
  (let lp ([i 0] [n (tuple-size x)])
    (cond ((= i n) -1)
          ((equal? (tuple-ref x i) v) i)
          (else (lp (+ i 1) n)))))
  
(define min
  (letrec ((min (lambda (x . y)
                  (if (<= x x)
                      (loop y x (exact? x))
                      x)))
           (loop (lambda (y x exact)
                   (if (null? y)
                       x
                       (let ((y1 (car y)))
                         (cond ((< y1 x)
                                (if exact
                                    (loop (cdr y) y1 (exact? y1))
                                    (loop (cdr y) (inexact y1) exact)))
                               ((and exact (not (exact? y1)))
                                (loop (cdr y) (inexact x) #f))
                               (else (loop (cdr y) x exact))))))))
    min))
 
(define max
  (letrec ((max (lambda (x . y)
                  (if (<= x x)
                      (loop y x (exact? x))
                      x)))
           (loop (lambda (y x exact)
                   (if (null? y)
                       x
                       (let ((y1 (car y)))
                         (cond ((> y1 x)
                                (if exact
                                    (loop (cdr y) y1 (exact? y1))
                                    (loop (cdr y) (inexact y1) exact)))
                               ((and exact (not (exact? y1)))
                                (loop (cdr y) (inexact x) #f))
                               (else (loop (cdr y) x exact))))))))
    max))
(define (boolean? x) (boolean? x))
(define (alist-cons key datum alist) (cons (cons key datum) alist))
(define (flatten lst)
  (if (null? lst) '()
    (let ((first (car lst))
          (rest (flatten (cdr lst))))
      (if (list? first)
          (append (flatten first) rest)
          (cons first rest)))))

(define (remove* l r)
  (if (not (list? l))
    (assertion-violation 'remove* "expected a list" l))
  (if (not (list? r))
    (assertion-violation 'remove* "expected a list" r))

  (let rloop ([r r])
    (cond
      [(null? r) r]
      [else (let ([first-r (car r)])
              (let loop ([l-rest l])
                (cond
                  [(null? l-rest)
                    (let ([next (rloop (cdr r))])
                      (if (eq? next (cdr r))
                        r
                        (cons first-r next)))]
                  [(equal? (car l-rest) first-r) (rloop (cdr r))]
                  [else (loop (cdr l-rest))])))])))

(define (member key alist)
  (let loop ([alist alist])
    (if (null? alist)
        #f
        (if (equal? key (car alist))
            alist
            (loop (cdr alist))))))

(define (memq key alist)
  (let loop ([alist alist])
    (if (null? alist)
        #f
        (if (eq? key (car alist))
            alist
            (loop (cdr alist))))))

(define (memv key alist)
  (let loop ([alist alist])
    (if (null? alist)
        #f
        (if (eqv? key (car alist))
            alist
            (loop (cdr alist))))))
(define (assq key alist)
  (if (null? alist)
      #f
      (if (eq? key (caar alist))
          (car alist)
          (assq key (cdr alist)))))
(define (alist? x)
  (and (list? x) (every1 pair? x)))
(define (assoc key alist)
  (if (null? alist)
      #f
      (if (equal? key (caar alist))
          (car alist)
          (assoc key (cdr alist)))))
(define (assv key alist)
  (if (null? alist)
      #f
      (if (eqv? key (caar alist))
          (car alist)
          (assv key (cdr alist)))))

(define (assq-ref alist key)
  (let ((pair (assq key alist)))
    (if (pair? pair) (cdr pair) #f)))
(define (assoc-ref alist key)
  (let ((pair (assoc key alist)))
    (if (pair? pair) (cdr pair) #f)))

(define (assoc-remove alist key)
  (cond ((null? alist) '())
        ((equal? key (caar alist)) (cdr alist))
        (else (cons (car alist) (assoc-remove (cdr alist) key)))))

(define (append-map proc lst)
  (if (null? lst) '()
    (let ((res (proc (car lst))))
      (if (null? res)
          (append-map proc (cdr lst))
          (append res (append-map proc (cdr lst)))))))

(define every1
  (lambda (pred lst)
    (or (null? lst)
        (let loop ((head (car lst)) (rest (cdr lst)))
          (and (pred head)
               (or (null? rest)
                   (loop (car rest) (cdr rest))))))))


(define every2
  (lambda (pred lst1 lst2)
    (or (null? lst1)
        (null? lst2)
        (let loop ((head1 (car lst1)) (rest1 (cdr lst1)) (head2 (car lst2)) (rest2 (cdr lst2)))
          (and (pred head1 head2)
               (or (null? rest1)
                   (null? rest2)
                   (loop (car rest1) (cdr rest1) (car rest2) (cdr rest2))))))))

(define any1
  (lambda (pred lst)
    (if (null? lst) #f
      (let ([r (pred (car lst))])
        (if r r (any1 pred (cdr lst)))))))

(define any2
  (lambda (pred lst1 lst2)
    (and (not (null? lst1))
         (not (null? lst2))
         (or (pred (car lst1) (car lst2))
             (any2 pred (cdr lst1) (cdr lst2))))))

(define filter
  (lambda (pred lst)
    (let loop ((lst lst))
      (cond ((null? lst) '())
            ((pred (car lst)) (cons (car lst) (loop (cdr lst))))
            (else (loop (cdr lst)))))))

(define partition
  (lambda (pred lst)
    (let loop ((lst lst) (acc1 '()) (acc2 '()))
      (cond ((null? lst) (values (reverse acc1) (reverse acc2)))
            ((pred (car lst)) (loop (cdr lst) (cons (car lst) acc1) acc2))
            (else (loop (cdr lst) acc1 (cons (car lst) acc2)))))))

(define for-all
  (lambda (pred lst1 . lst2)
    (cond ((null? lst2)
            (for-all-1 pred lst1))
          ((apply list-transpose+ lst1 lst2)
            => (lambda (lst) (for-all-n-quick pred lst)))
          (else
            (for-all-n pred (cons lst1 lst2))))))

(define for-all-1
  (lambda (pred lst)
    (cond ((null? lst) #t)
          ((pair? lst)
            (let loop ((head (car lst)) (rest (cdr lst)))
              (cond ((null? rest) (pred head))
                    ((pair? rest)
                    (and (pred head)
                          (loop (car rest) (cdr rest))))
                    (else
                    (and (pred head)
                          (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" rest) (list pred lst)))))))
          (else
            (assertion-violation 'for-all (format "expected chain of pairs, but got ~r, as argument 2" lst) (list pred lst))))))

(define for-all-n
  (lambda (pred list-of-lists)
    (let ((argc (length list-of-lists)))

      (define collect-car
        (lambda (lst)
          (let loop ((lst lst))
            (cond ((null? lst) '())
                  ((pair? (car lst))
                    (cons (caar lst) (loop (cdr lst))))
                  (else
                    (assertion-violation 'for-all (format "traversal reached to non-pair element ~s" (car lst)) list-of-lists))))))

      (let loop ((head (collect-car list-of-lists)) (rest (collect-cdr list-of-lists)))
        (or (= (length head) argc)
            (assertion-violation 'for-all "expected same length chains of pairs" list-of-lists))
        (if (null? rest)
            (apply pred head)
            (and (apply pred head)
                  (loop (collect-car rest) (collect-cdr rest))))))))

(define for-all-n-quick
  (lambda (pred lst)
    (or (null? lst)
        (let loop ((head (car lst)) (rest (cdr lst)))
          (if (null? rest)
              (apply pred head)
              (and (apply pred head)
                    (loop (car rest) (cdr rest))))))))
(define safe-length
  (lambda (lst)
    (let loop ((lst lst) (n 0))
      (if (pair? lst)
          (loop (cdr lst) (+ n 1))
          (or (and (null? lst) n) -1)))))

(define split-at
  (lambda (lst n)
    (values (take lst n) (drop lst n))))

(define (map f x . rest)
  (define (map1 f x)
    (if (pair? x)
      (let* ([a (f (car x))]
             [b (map1 f (cdr x))])
        (cons a b))
      (if (null? x) '()
        (assertion-violation 'map "expected a proper list" x))))
  (define (map2 f x y)
    (if (and (pair? x) (pair? y))
      (let* ([a (f (car x) (car y))]
             [b (map2 f (cdr x) (cdr y))])
        (cons a b))
      (if (or (null? x) (null? y)) '()
        (assertion-violation 'map "expected proper lists" (list x y)))))
  (define (map3 f x y z)
    (if (and (pair? x) (pair? y) (pair? z))
      (let* ([a (f (car x) (car y) (car z))]
             [b (map3 f (cdr x) (cdr y) (cdr z))])
        (cons a b))
      (if (or (null? x) (null? y) (null? z)) '()
        (assertion-violation 'map "expected proper lists" (list x y z)))))
  (let ([l (length rest)])
    (cond
      [(zero? l) (map1 f x)]
      [(= l 1) (map2 f x (car rest))]
      [(= l 2) (map3 f x (car rest) (car (cdr rest)))]
    )))

(define for-each
  (lambda (proc lst1 . lst2)
    (define for-each-1
      (lambda (proc lst)
        (if (null? lst)
            (unspecified)
            (begin (proc (car lst)) (for-each-1 proc (cdr lst))))))
    (define for-each-n
      (lambda (proc lst)
        (cond ((null? lst) (unspecified))
              (else (apply proc (car lst)) (for-each-n proc (cdr lst))))))
    (if (null? lst2)
        (if (list? lst1)
            (for-each-1 proc lst1)
            (assertion-violation 'for-each "not a proper list" (cons* proc lst1 lst2)))
        (cond ((apply list-transpose+ lst1 lst2) => (lambda (lst) (for-each-n proc lst)))
              (else (assertion-violation 'for-each "expected same length proper lists" (cons* proc lst1 lst2)))))))


(define (and-map pred lst)
  (let loop ([lst lst])
    (if (null? lst) #t
      (if (pred (car lst))
          (loop (cdr lst))
          #f))))

(define (or-map pred lst)
  (let loop ([lst lst])
    (if (null? lst) #f
      (if (pred (car lst))
          #t
          (loop (cdr lst))))))

(define (every? pred lst)
  (and (list? lst) (every1 pred lst)))

(define (make-parameter init . filter)
  (let ([f (make-fluid init)]
        [conv (if (null? filter) (lambda (x) x)  (car filter))])
      (unless (procedure? conv)
        (assertion-violation 'make-parameter "expected a procedure as filter" conv))
      (lambda args
        (if (null? args)
            (fluid-ref f)
            (let ([old (fluid-ref f)])
              (if (not (conv (car args)))
                (assertion-violation 'parameter "bad value for parameter" (car args)))
              (fluid-set! f (car args))
              old)))))

(define (make-thread-parameter init . filter)
  (let ([f (make-thread-local-fluid init)]
        [conv (if (null? filter) (lambda (x) x)  (car filter))])
      (unless (procedure? conv)
        (assertion-violation 'make-thread-parameter "expected a procedure as filter" conv))
      (lambda args
        (if (null? args)
            (thread-fluid-ref f)
            (let ([old (thread-fluid-ref f)])
              (if (not (conv (car args)))
                (assertion-violation 'thread-parameter "bad value for parameter" (car args)))
              (thread-fluid-set! f (car args))
              old)))))

(define tuple-printer (make-parameter #f))

; 0 - no logging, 1 - error, 2 - warning, 3 - info, 4 - debug, 5 - trace
(define *log-level* 0)



(define current-exception-handler
  (let ([f (make-thread-local-fluid #f)])
    (lambda args
      (if (null? args)
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))

(define parent-exception-handler
  (let ([f (make-thread-local-fluid #f)])
    (lambda args
      (if (null? args)
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))


(define current-dynamic-wind-record 
  (let ([f (make-thread-local-fluid '())])
    (lambda args
      (if (null? args)
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))

(define (perform-dynamic-wind new cont args)
  (define common-tail
      (lambda (x y)
        (let ((nx (length x)) (ny (length y)))
          (do
            ((x (if (> nx ny) (list-tail x (- nx ny)) x) (cdr x))
             (y (if (> ny nx) (list-tail y (- ny nx)) y) (cdr y)))
            ((eq? x y) x)))))
    (let ((tail (common-tail new (current-dynamic-wind-record))))
      (let loop ((rec (current-dynamic-wind-record)))
        (cond ((not (eq? rec tail)) (current-dynamic-wind-record (cdr rec)) ((cdar rec)) (loop (cdr rec)))))
      (let loop ((rec new))
        (cond ((not (eq? rec tail)) (loop (cdr rec)) ((caar rec)) (current-dynamic-wind-record rec)))))
    (apply cont args))

(define (dynamic-wind in body out)
  (in)
  (current-dynamic-wind-record (cons (cons in out) (current-dynamic-wind-record)))
  (call-with-values 
    body
    (lambda ans 
      (current-dynamic-wind-record (cdr (current-dynamic-wind-record)))
      (out)
      (apply values ans))))

(define (call/cc f)
  (define saved-record (current-dynamic-wind-record))
  (.call/cc-unsafe 
    (lambda (k)
      (f (lambda args 
        (perform-dynamic-wind saved-record k args))))))
(define call-with-current-continuation call/cc)

(define (unhandled-exception-error val)
  (.return-error val))

(define *basic-exception-handlers* (list unhandled-exception-error))

(define *current-exception-handlers*
  (let ([f (make-thread-local-fluid *basic-exception-handlers*)])
    (lambda args
      (if (null? args)
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))


(define (raise obj)
  (.raise obj))

(define (raise-continuable obj)
  (let ((handlers (*current-exception-handlers*)))
    (with-exception-handler (cadr handlers)
      (lambda () ((car handlers) obj)))))


(define (with-exception-handler handler thunk)
  (let* ([previous-handlers (*current-exception-handlers*)]
        [new-handlers (if handler
                          (cons handler previous-handlers)
                          previous-handlers)])
    (dynamic-wind
      (lambda ()
        (*current-exception-handlers* new-handlers))
      (lambda() (.with-handler handler thunk))
      (lambda ()
        (*current-exception-handlers* previous-handlers)))))



(define (filter pred lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
      (reverse acc)
      (if (pred (car lst))
        (loop (cdr lst) (cons (car lst) acc))
        (loop (cdr lst) acc)))))


;; These functions are defined in `boot/eval.scm`. Due to the fact that
;; we now have letrectification overriding them also requires overriding
;; `resolve-module` so we just keep it simple and make `resolve-module` 
;; do a global lookup of `load` and `load-in-vicinity`. `primitive-load`
;; is defined here because we do not depend on its overloaded version from 
;; here.

;(define (load-in-vicinity filename directory)
;  (let ([thunk (load-thunk-in-vicinity filename #t directory )])
;    (thunk)))
;
;(define (load filename)
;  (let ([thunk (load-thunk-in-vicinity filename #t)])
;    (thunk)))
;
(define (primitive-load filename)
  "Loads file by searching only load path or by its absolute path."
  (let ([thunk (load-thunk-in-vicinity filename #f)])
    (thunk)))

(define (module-search fn m v)
  (or (fn m v)
    (let loop ([pos (module-uses m)])
      (if (null? pos)
        #f
        (or (fn (car pos) v)
          (loop (cdr pos)))))))



(define (core-hash-map proc ht)
  (map proc (core-hash->list ht)))

(define (core-hash-for-each proc ht)
  (for-each proc (core-hash->list ht)))

(define (core-hash-copy ht)
  (let ([new (make-core-hash)])
    (core-hash-for-each
      (lambda (pair)
        (core-hash-put! new (car pair) (cdr pair)))
      ht)
    new))

(define (module-for-each proc module)
  (for-each (lambda (kv) (proc (car kv) (cdr kv))) (core-hash->list (module-obarray module))))


(define (module-map proc module)
  (map proc (core-hash->list (module-obarray module))))

(define (module-ref-submodule module name)
  (core-hash-ref (module-submodules module) name))

(define (module-define-submodule! module name submodule)
  (core-hash-put! (module-submodules module) name submodule))

(define (save-module-excursion thunk)

  (let ([inner-module (current-module)]
        [outer-module #f])
    (dynamic-wind
      (lambda ()
        (set! outer-module (current-module))
        (current-module inner-module)
      
        (set! inner-module #f))
      thunk
      (lambda ()
        (set! inner-module (current-module))
     
        (current-module outer-module)
        (set! outer-module #f)))))

(define (module-ref module name . rest)
  (let ([var (module-variable module name)])
    (if (and var (variable-bound? var))
      (variable-ref var)
      (if (null? rest)
        (assertion-violation 'module-ref "unbound variable" module name)
        (car rest)))))

(define (module-set! module name value)
  (let ([var (module-variable module name)])
    (if var
      (variable-set! var value)
      (assertion-violation 'module-set! "unbound variable" module name))))


(define (module-defined? module name)
  (let ([var (module-variable module name)])
    (and var (variable-bound? var))))

(define (module-use! module interface)
  (if (not (or (eq? module interface)
               (memq interface (module-uses module))))
    (begin
      (set-module-uses! module (append (module-uses module) (list interface)))
      (core-hash-clear! (module-import-obarray module)))))

(define (module-use-interfaces! module interfaces)
 
  (let* ([cur (module-uses module)]
         [new (let loop ([in interfaces] [out '()])
                (if (null? in)
                  (reverse out)
                  (loop (cdr in)
                    (let ([iface (car in)])
                      (if (or (memq iface cur) (memq iface out))
                        out
                        (cons iface out))))))])

    (set-module-uses! module (append cur new))
    (core-hash-clear! (module-import-obarray module))))

(define (module-define! module name value)
    (let ([variable (module-local-variable module name)])
        (if variable
            (begin
                (variable-set! variable value))
            (let ([variable (make-variable value)])
                (module-add! module name variable)))))


(define (nested-ref root names)
  (if (null? names)
    root
    (let loop ([cur root]
               [head (car names)]
               [tail (cdr names)])
      (if (null? tail)
        (module-ref cur head #f)
        (let ([cur (module-ref-submodule cur head)])
          (and cur
            (loop cur (car tail) (cdr tail))))))))

(define (nested-set! root names val)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-set! cur head val)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (assertion-violation 'nested-set! "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-remove! root names)
  (let loop ((cur root)
             (head (car names))
             (tail (cdr names)))
    (if (null? tail)
        (module-remove! cur head)
        (let ((cur (module-ref-submodule cur head)))
          (if (not cur)
              (assertion-violation 'nested-remove! "failed to resolve module" names)
              (loop cur (car tail) (cdr tail)))))))


(define (nested-ref-module root names)
  (let loop ((cur root)
             (names names))
    (if (null? names)
        cur
        (let ((cur (module-ref-submodule cur (car names))))
          (and cur
               (loop cur (cdr names)))))))

(define (nested-define-module! root names module)
  (if (null? names)
      (assertion-violation 'nested-define-module! "can't redefine root module" module)
      (let loop ((cur root)
                 (head (car names))
                 (tail (cdr names)))
        (if (null? tail)
            (module-define-submodule! cur head module)
            (let ((cur (or (module-ref-submodule cur head)
                           (let ((m (make-module)))
                             (set-module-kind! m 'directory)
                             (set-module-name! m (append (module-name cur)
                                                         (list head)))
                             (module-define-submodule! cur head m)
                             m))))
              (loop cur (car tail) (cdr tail)))))))


(define (local-ref names)
  (nested-ref (current-module) names))

(define (local-set! names val)
  (nested-set! (current-module) names val))

(define (local-define names val)
  (nested-define! (current-module) names val))

(define (local-remove names)
  (nested-remove! (current-module) names))

(define (local-ref-module names)
  (nested-ref-module (current-module) names))

(define (local-define-module names mod)
  (nested-define-module! (current-module) names mod))


(define (module-name mod)
  (or (raw-module-name mod)
    (let ([name (list (gensym))])
      (set-module-name! mod name)
      (nested-define-module! (resolve-module '() #f #t) name mod)
      (raw-module-name mod))))

(define (make-modules-in module name)
  (or (nested-ref-module module name)
    (let ([m (make-module)])
      (set-module-kind! m 'directory)
      (set-module-name! m (append (module-name module) name))
      (nested-define-module! module name m)
      m)))

(define (beautify-user-module! module)
  (let ([interface (module-public-interface module)])
    (if (or (not interface)
            (eq? interface module))
      (let ([interface (make-module)])
        (set-module-name! interface (module-name module))
        (set-module-kind! interface 'interface)
        (set-module-public-interface! module interface))))
  (if (and (not (memq the-scm-module (module-uses module)))
           (not (eq? module the-root-module)))
    (module-use! module the-scm-module)))

(define (make-fresh-user-module)
  (let ([m (make-module)])
    (beautify-user-module! m)
    (set-module-declarative! m #f)
    m))

(define (purify-module! module)
  (let ([use-list (module-uses module)])
    (if (and (pair? use-list)
             (eq? (car (last-pair use-list)) the-scm-module))
        (set-module-uses! module (reverse (cdr (reverse use-list)))))))

(define resolve-module
  (let ([root *resolve-module-root*])
    (lambda (name autoload ensure)
      (let ([already (nested-ref-module root name)])
        (if (and already
                 (or (not autoload) (module-public-interface already)))
          already
          (if autoload
            (begin
              (try-module-autoload name)
              (resolve-module name #f ensure))
            (or already
              (and ensure
                (make-modules-in root name)))))))))


(define (->bool x) (not (not x)))

(define autoloads-in-progress '())
(define autoloads-done '((capy . capy)))

(define (autoload-done-or-in-progress? p m)
  (let ((n (cons p m)))
    (->bool (or (member n autoloads-done)
                (member n autoloads-in-progress)))))

(define (autoload-done! p m)
  (let ((n (cons p m)))
    (set! autoloads-in-progress
          (delete! n autoloads-in-progress))
    (or (member n autoloads-done)
        (set! autoloads-done (cons n autoloads-done)))))

(define (autoload-in-progress! p m)
  (let ((n (cons p m)))
    (set! autoloads-done
          (delete! n autoloads-done))
    (set! autoloads-in-progress (cons n autoloads-in-progress))))

(define (set-autoloaded! p m done?)
  (if done?
      (autoload-done! p m)
      (let ((n (cons p m)))
        (set! autoloads-done (delete! n autoloads-done))
        (set! autoloads-in-progress (delete! n autoloads-in-progress)))))

(define (try-module-autoload module-name)
  (let* ([reverse-name (reverse module-name)]
         [name (symbol->string (car reverse-name))]
         [dir-hint-module-name (reverse (cdr reverse-name))]
         [dir-hint (apply string-append
          (map (lambda (elt)
            (string-append (symbol->string elt) "/"))
            dir-hint-module-name))])
    (resolve-module dir-hint-module-name #f #t)

    (and (not (autoload-done-or-in-progress? dir-hint name))
         (let ([didit #f])
          (dynamic-wind
            (lambda () (autoload-in-progress! dir-hint name))
            (lambda ()
              (save-module-excursion
                (lambda ()
                  (current-module (make-fresh-user-module))
                  (call/cc (lambda (return)
                    (with-exception-handler
                      (lambda (_x) ((current-exception-printer) _x) (return #f))
                      (lambda ()
                        (load (string-append dir-hint name))
                        (set! didit #t)
                        )))))))
            (lambda () (set-autoloaded! dir-hint name didit)))
          didit))))

(define (identity x) x)

(define (resolve-interface name select hide prefix)
  (let* ([mod (resolve-module name #t #f)]
         [public-i (and mod (module-public-interface mod))]
         [renamer (if prefix (lambda (symbol) (symbol-append prefix symbol)) identity)])
    (if (not public-i)
      (assertion-violation 'resolve-interface "no code for module" name))
   
    (if (and (not select) (null? hide) (eq? renamer identity))
      public-i
      (let ([custom-i (make-module)])
        (define (maybe-export! src dst var)
          (if (not (memq src hide))
            (begin 
              (let ([name (renamer dst)])
                (if (core-hash-ref (module-replacements public-i) src)
                  (core-hash-put! (module-replacements custom-i) name #t))
                (module-add! custom-i name var)))))
        (set-module-kind! custom-i 'custom-interface)
        (set-module-name! custom-i name)
        (for-each (lambda (binding)
          (if (not (module-local-variable public-i binding))
            (assertion-violation #f "no binding to hide in module" name binding)))
          hide)
        
        (cond 
          [select 
            (for-each (lambda (bspec)
              (let* ([direct? (symbol? bspec)]
                     [orig (if direct? bspec (car bspec))]
                     [seen (if direct? bspec (cdr bspec))]
                     [var (module-local-variable public-i orig)])
                (if (not var)
                  (assertion-violation 'unbound-variable "no binding to select in module" orig name))
                (maybe-export! orig seen var))
            ) select)]
          [else (module-for-each (lambda (sym var)
            (maybe-export! sym sym var)) public-i)])
          custom-i))))


(define (define-module* name)
  (let ([module (resolve-module name #f #t)])
    (beautify-user-module! module)
    module))

(define (module-export! m names . replace?)

  (let ([replace? (if (null? replace?) #f (car replace?))]
        [public-i (module-public-interface m)])
    (for-each (lambda (name)
      (let* ([internal-name (if (pair? name) (car name) name)]
             [external-name (if (pair? name) (cdr name) name)]
             [var (module-ensure-local-variable! m internal-name)])
        (if replace? 
          (core-hash-put! (module-replacements public-i) external-name #t))
        (module-add! public-i external-name var))) names)))

(define (module-replace! m names)
  (module-export! m names #t))

(define (module-export-all! mod)
  (define (fresh-interface!)
    (let ((iface (make-module)))
      (set-module-name! iface (module-name mod))
      (set-module-version! iface (module-version mod))
      (set-module-kind! iface 'interface)
      (set-module-public-interface! mod iface)
      iface))
  (let ((iface (or (module-public-interface mod)
                   (fresh-interface!))))
    (set-module-obarray! iface (module-obarray mod))))

(define (process-use-modules module-iface-args)
  (let ([interfaces (map (lambda (mif-args)
    (or (apply resolve-interface mif-args)
      (assertion-violation 'use "failed to resolve module" mif-args))) module-iface-args)])
    (module-use-interfaces! (current-module) interfaces)))


(define (call-with-values producer consumer)
  (receive results (producer)
    (apply consumer results)))

(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cdar x) (cdr (car x)))
(define (caar x) (car (car x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cdaadr x) (cdr (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (car x)))))
(define (caaaddr x) (car (car (car (cdr x)))))
(define (cdaaddr x) (cdr (car (car (cdr x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (cdr (car x)))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caadr x) (car (cdr (car x))))
(define (cdaar x) (cdr (car (car x))))
(define (caaar x) (car (car (car x))))

(define (string->number x . radix) 
  (let ([radix (if (null? radix) 10 (car radix))])
    (string->number x radix)))

(define (string-any pred str)
  (let loop ((i 0) (n (string-length str)))
    (if (>= i n)
        #f
        (if (pred (string-ref str i))
            #t
            (loop (+ i 1) n)))))

(define (lookup-bound module name public?)
  (let ([mod (resolve-module module #f #f)])
    (if (not mod)
      (assertion-violation 'lookup-bound "module not found" module))

    (let* ([iface (if public? (module-public-interface mod) mod)]
           [var (module-variable iface name)])
      (if (or (not var) (not (variable-bound? var)))
        (assertion-violation 'lookup-bound "unbound variable" module name))
      var)))


(define (module-re-export! m names . replace?)
  (let ([replace? (if (null? replace?) #f (car replace?))])
    (let ([public-i (module-public-interface m)])
      (for-each 
        (lambda (name)
          (let* ([internal-name (if (pair? name) (car name) name)]
                 [external-name (if (pair? name) (cdr name) name)]
                 [var (module-variable m internal-name)])
              (cond 
                [(not var)
                  (assertion-violation 'unbound-variable "undefined variable" internal-name)]
                [(eq? var (module-local-variable m internal-name))
                  (assertion-violation 'export "re-exporting local variable" internal-name)]
                [else 
                  (if replace? 
                    (core-hash-put! (module-replacements public-i) external-name #t))
                  (module-add! public-i external-name var)])))
        names))))


(define current-exception-printer
  (make-parameter
    (lambda (exn . port)
      (define p (if (null? port) (current-error-port) (car port)))
   
    
      (format p "Unhandled exception: ~a~!: ~a~%~!" (condition-who exn)))))


(define (r6rs:bytevector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
      (let loop ((i 0))
        (if (< i count)
            (begin
              (bytevector-u8-set! target
                                  (+ target-start i)
                                  (bytevector-u8-ref source (+ source-start i)))
              (loop (+ i 1)))))
      (let loop ((i (- count 1)))
        (if (>= i 0)
            (begin
              (bytevector-u8-set! target
                                  (+ target-start i)
                                  (bytevector-u8-ref source (+ source-start i)))
              (loop (- i 1)))))))

;;; Generalized from one argument for R7RS.

(define (bytevector-copy b . rest)
  (let* ((n (bytevector-length b))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (k (- end start))
         (b2 (make-bytevector k)))
    (r6rs:bytevector-copy! b start b2 0 k)
    b2))

(define (bytevector-copy/nonmoving b . rest)
  (let* ((n (bytevector-length b))
         (start (if (null? rest) 0 (car rest)))
         (end (if (or (null? rest) (null? (cdr rest))) n (cadr rest)))
         (k (- end start))
         (b2 (make-bytevector/nonmoving k)))
    (r6rs:bytevector-copy! b start b2 0 k)
    b2))

(define bytevector-copy! r6rs:bytevector-copy!)

(define (assert cond . rest)
  (if (not cond)
      (apply assertion-violation 'assert "assertion failed" rest)
      #t))

(define (procedure-property proc key)
  (cond 
    [(procedure-properties proc)
      (let ([p (assq key (procedure-properties proc))])
        (and p (cdr p)))]
    [else #f]))

(define (set-procedure-property! proc key value)
  (let ([props (procedure-properties proc)])
    (if props
      (let ([p (assq key props)])
        (if p
          (set-cdr! p value)
          (set-procedure-properties! proc (cons (cons key value) props))))
      (set-procedure-properties! proc (list (cons key value))))))

(define (procedure-name proc)
  (procedure-property proc 'name))

(define (procedure-documentation proc)
  (procedure-property proc 'documentation))

(define (procedure-sourcev proc)
  (let ([src (procedure-property proc 'source)])
    (cond 
      [(vector? src) src]
      [(list? src)
        (let ([file (assq 'filename src)]
              [line (assq 'line src)]
              [col (assq 'column src)])
          (vector file line col))]
      [else #f])))

(define (call-without-interrupts thunk)
  (thunk))

(define mod
  (lambda (x y)
    (- x (* (div x y) y))))

(define div-and-mod
  (lambda (x y)
    (let ((d (div x y)))
      (values d (- x (* d y))))))

(define mod0
  (lambda (x y)
    (- x (* (div0 x y) y))))


(define div0-and-mod0
  (lambda (x y)
    (let ((d0 (div0 x y)))
      (values d0 (- x (* d0 y))))))


(define current-jiffy microsecond)
(define jiffies-per-second (lambda () 1000000))
(define current-second (lambda () (/ (microsecond) 1000000.0)))

(define gcd2
  (lambda (a b)
    (if (= b 0)
        (abs (if (inexact? b) (inexact a) a))
        (gcd2 b (remainder a b)))))

(define gcd
  (lambda args
    (for-each
      (lambda (a)
        (or (integer-valued? a) (assertion-violation 'gcd (format "expected integer, but got ~s" a) args)))
      args)
    (let loop ((lst args))
      (case (length lst)
            ((2) (gcd2 (car lst) (cadr lst)))
            ((1) (abs (car lst)))
            ((0) 0)
            (else (loop (cons (gcd2 (car lst) (cadr lst)) (cddr lst))))))))

(define lcm
  (lambda args
    (define lcm2
      (lambda (a b)
        (if (or (= a 0) (= b 0))
            (if (and (exact? a) (exact? b)) 0 0.0)
            (abs (* (quotient a (gcd2 a b)) b)))))
    (for-each
      (lambda (a) (or (integer-valued? a) (assertion-violation 'lcm (format "expected integer, but got ~s" a) args)))
      args)
    (let loop ((lst args))
      (case (length lst)
            ((2) (lcm2 (car lst) (cadr lst)))
            ((1) (abs (car lst)))
            ((0) 1)
            (else (loop (cons (lcm2 (car lst) (cadr lst)) (cddr lst))))))))

(define rationalize
  (lambda (x e)
    (or (real? x) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 1" x) (list x e)))
    (or (real? e) (assertion-violation 'rationalize (format "expected real, but got ~s as argument 2" e) (list x e)))
    (cond ((infinite? e) (if (infinite? x) +nan.0 0.0))
          ((= x 0) x)
          ((= x e) (- x e))
          ((negative? x) (- (rationalize (- x) e)))
          (else
            (let ((e (abs e)))
              (let loop ((bottom (- x e)) (top (+ x e)))
                (cond ((= bottom top) bottom)
                      (else
                        (let ((x (ceiling bottom)))
                          (cond ((< x top) x)
                                (else
                                  (let ((a (- x 1)))
                                    (+ a (/ 1 (loop (/ 1 (- top a)) (/ 1 (- bottom a)))))))))))))))))

(define string->list
  (lambda (s)
    (define n (string-length s))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (reverse acc)
          (loop (+ i 1) (cons (string-ref s i) acc))))))

(define string-for-each
  (lambda (proc str1 . str2)
    (apply for-each proc (string->list str1)
           (map string->list str2))))

(define vector-map
  (lambda (proc vec1 . vec2)
    (list->vector
     (apply map proc (vector->list vec1)
            (map vector->list vec2)))))

(define vector-for-each
  (lambda (proc vec1 . vec2)
    (apply for-each proc (vector->list vec1)
           (map vector->list vec2))))

(define (bytevector->u8-list bv) (bytevector->list bv))

(define drop-last-cdr
  (lambda (lst)
    (cond ((null? lst) '())
          (else
            (let loop ((lst lst))
              (cond ((pair? lst) (cons (car lst) (loop (cdr lst))))
                    (else '())))))))
(define drop-last-pair
  (lambda (lst)
    (cond ((null? lst) '())
          (else
            (let loop ((lst lst))
              (cond ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst))))
                    (else '())))))))
(define last-pair
  (lambda (lst)
    (cond ((null? lst) '())
          (else
            (let loop ((lst lst))
              (cond ((pair? (cdr lst)) (loop (cdr lst)))
                    (else lst)))))))
(define last-cdr
  (lambda (lst)
    (cond ((pair? lst)
            (let loop ((lst lst))
              (cond ((pair? (cdr lst)) (loop (cdr lst)))
                    (else (cdr lst)))))
          (else lst))))
(define count-pair
  (lambda (lst)
    (let loop ((lst lst) (n 0))
      (cond ((pair? lst) (loop (cdr lst) (+ n 1)))
            (else n)))))
(define last-n-pair
  (lambda (n lst)
    (let ((m (count-pair lst)))
      (cond ((< m n) '())
            (else (list-tail lst (- m n)))))))
(define drop-last-n-pair
  (lambda (n lst)
    (cond ((null? lst) '())
          (else
            (let loop ((lst lst) (m (- (count-pair lst) n)))
              (cond ((<= m 0) '())
                    ((pair? (cdr lst)) (cons (car lst) (loop (cdr lst) (- m 1))))
                    (else '())))))))

(define (cyclic-object? c)
  (define ht (make-core-hash-eq))
  
  (define (rec x)
    (cond 
      [(pair? x) 
        (if (core-hash-ref ht x)
          #t
          (begin
            (core-hash-put! ht x #t)
            (or (rec (car x)) (rec (cdr x)))))]
      [(vector? x)
        (if (core-hash-ref ht x)
          #t
          (begin
            (core-hash-put! ht x #t)
            (let loop ((i 0) (n (vector-length x))) 
              (if (>= i n)
                #f
                (or (rec (vector-ref x i))
                    (loop (+ i 1) n))))))]
      [(syntax? x)
        (if (core-hash-ref ht x)
          #t
          (begin
            (core-hash-put! ht x #t)
            (rec (syntax-expression x)
            (rec (syntax-wrap x)))))]
      [(core-hashtable? x)
        (if (core-hash-ref ht x)
          #t
          (begin
            (core-hash-put! ht x #t)
            (let loop ((lst (core-hash->list x)))
              (if (null? lst)
                #f
                (or (rec (caar lst))
                    (rec (cdar lst))
                    (loop (cdr lst)))))))]
      [else #f]))
  (rec c))

(define (inexact->exact num)
  (inexact->exact))

(define (exact->inexact num)
  (exact->inexact num))

(set! %load-extensions 
  (append '("capy.sls" "capy.sld" "capy.scm" "sls" "sld" ".sch" "sps")
          %load-extensions))

(let* (
      [host-arch (host-arch)]
      [host-os (host-os)]
      [host-family (host-family)]
      [host-os-sld (string-append host-os ".sld")]
      [host-family-sld (string-append host-family ".sld")]
      [arch-sld (string-append host-arch ".sld")]
      [host-os-sls (string-append host-os ".sls")]
      [host-family-sls (string-append host-family ".sls")]
      [arch-sls (string-append host-arch ".sls")])
  (set! %load-extensions
    (append (list host-os-sld host-family-sld arch-sld
                  host-os-sls host-family-sls arch-sls)
            %load-extensions)))



(primitive-load "boot/records.scm")
(primitive-load "boot/exceptions.scm")
(primitive-load "boot/expand.scm")
(primitive-load "boot/interpreter.scm")
(primitive-load "boot/psyntax.scm")
(primitive-load "boot/sys.scm")
(primitive-load "boot/osdep.scm")
(primitive-load "boot/iosys.scm")
(primitive-load "boot/portio.scm")
(primitive-load "boot/bytevectorio.scm")
(primitive-load "boot/fileio.scm")
(primitive-load "boot/conio.scm")
(primitive-load "boot/stringio.scm")
(primitive-load "boot/stdio.scm")
(primitive-load "boot/utf16.scm")
(primitive-load "boot/customio.scm")
(primitive-load "boot/print.scm")
(primitive-load "boot/format.scm")
(primitive-load "boot/log.scm")
(initialize-io-system)
(primitive-load "boot/reader.scm")
(primitive-load "boot/eval.scm")

(let ([user-module (define-module* '(capy user))])
  (current-module user-module))
