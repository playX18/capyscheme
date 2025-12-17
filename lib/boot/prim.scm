;; Primitive functions to make bootstrap easier.


(define (filter pred lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
      (reverse acc)
      (if (pred (car lst))
        (loop (cdr lst) (cons (car lst) acc))
        (loop (cdr lst) acc)))))

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
  (define (map-n proc lst)
    (cond 
      [(null? lst) '()]
      [else (cons (apply proc (car lst))
                  (map-n proc (cdr lst)))]))

  (let ([l (length rest)])
    (cond
      [(zero? l) (map1 f x)]
      [else (map-n f (apply list-transpose* x rest))]
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


(define (core-hash-map proc ht)
  (map proc (core-hash->list ht)))

(define (core-hash-for-each proc ht)
  (for-each proc (core-hash->list ht)))



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



(define (string-any pred str)
  (let loop ((i 0) (n (string-length str)))
    (if (>= i n)
        #f
        (if (pred (string-ref str i))
            #t
            (loop (+ i 1) n)))))


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
  (inexact->exact num))

(define (exact->inexact num)
  (exact->inexact num))


(define list-sort
  (lambda (proc lst)

    (define merge
      (lambda (lst1 lst2)
        (cond
         ((null? lst1) lst2)
         ((null? lst2) lst1)
         (else
          (if (proc (car lst2) (car lst1))
              (cons (car lst2) (merge lst1 (cdr lst2)))
              (cons (car lst1) (merge (cdr lst1) lst2)))))))

    (define sort
      (lambda (lst n)
        (cond ((= n 1)
               (list (car lst)))
              ((= n 2)
               (if (proc (cadr lst) (car lst))
                   (list (cadr lst) (car lst))
                   (list (car lst) (cadr lst))))
              (else
               (let ((n/2 (div n 2)))
                 (merge (sort lst n/2)
                        (sort (list-tail lst n/2) (- n n/2))))))))

    (define divide
      (lambda (lst)
        (let loop ((acc 1) (lst lst))
          (cond ((null? (cdr lst)) (values acc '()))
                (else
                 (if (proc (car lst) (cadr lst))
                     (loop (+ acc 1) (cdr lst))
                     (values acc (cdr lst))))))))

    (cond ((null? lst) '())
          (else
           (let ((len (length lst)))
             (receive (n rest) (divide lst)
               (cond ((null? rest) lst)
                     (else
                      (merge (list-head lst n)
                             (sort rest (- len n)))))))))))

(define (foldl proc init lst)
  (let loop ((acc init) (lst lst))
    (if (null? lst)
        acc
        (loop (proc acc (car lst)) (cdr lst)))))

(define (foldr proc init lst)
  (if (null? lst)
      init
      (proc (car lst) (foldr proc init (cdr lst)))))

(define (keyword<? kw1 kw2)
  (define s1 (keyword->symbol kw1))
  (define s2 (keyword->symbol kw2))

  (string<? (symbol->string s1) (symbol->string s2)))

(define capy:execution-mode (make-parameter 'rnrs))