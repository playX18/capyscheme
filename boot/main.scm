(define (eq? x y) (eq? x y))
(define (eqv? x y) (eqv? x y))
(define (equal? x y) (equal? x y))
(define (null? x) (null? x))

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

(define (for-all pred lst)
  (if (null? lst) #t
    (and (pred (car lst)) (for-all pred (cdr lst)))))

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

(define (for-each proc lst)
  (let loop ([lst lst])
    (if (pair? lst)
      (begin (proc (car lst)) (loop (cdr lst)))
      (if (null? lst) '()
        (assertion-violation 'for-each "expected a proper list" lst)))))

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

(define (make-parameter init . converter)
  (let ([f (make-fluid init)]
        [conv (if (null? converter) (lambda (x) x)  (car converter))])
      (lambda args
        (if (null? args)
            (fluid-ref f)
            (let ([old (fluid-ref f)])
              (if (not (conv (car args)))
                (assertion-violation 'parameter "bad value for parameter" (car args)))
              (fluid-set! f (car args))
              old)))))


(define tuple-printer (make-parameter #f))

(define (make-rtd name parent uid sealed? opaque? fields)
    (tuple 'type:record-type-descriptor name parent uid sealed? opaque? fields #f))

(define record-type-descriptor?
  (lambda (obj)
    (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:record-type-descriptor))))


(define rtd-name        (lambda (rtd) (tuple-ref rtd 1)))
(define rtd-parent      (lambda (rtd) (tuple-ref rtd 2)))
(define rtd-uid         (lambda (rtd) (tuple-ref rtd 3)))
(define rtd-sealed?     (lambda (rtd) (tuple-ref rtd 4)))
(define rtd-opaque?     (lambda (rtd) (tuple-ref rtd 5)))
(define rtd-fields      (lambda (rtd) (tuple-ref rtd 6)))
(define rtd-printer     (lambda (rtd) (tuple-ref rtd 7)))
(define set-rtd-printer! (lambda (rtd printer) (tuple-set! rtd 7 printer)))


(define (rtd-ancestor? parent rtd)
  (let loop ((rtd rtd))
    (or (eq? rtd parent)
      (and (rtd-parent rtd)
           (loop (rtd-parent rtd))))))

(define (rtd-inherited-field-count rtd)
  (let loop ([rtd (rtd-parent rtd)] [count 0])
    (if rtd
      (loop (rtd-parent rtd) (+ count (length (rtd-fields rtd))))
      count)))



(define (rtd-total-field-count rtd)
  (+ (rtd-inherited-field-count rtd) (length (rtd-fields rtd))))

(define (record-type-name rtd)
  (rtd-name rtd))

(define (record-type-parent rtd)
  (rtd-parent rtd))

(define (record-type-uid rtd)
  (rtd-uid rtd))

(define (record-type-sealed? rtd)
  (rtd-sealed? rtd))

(define (record-type-opaque? rtd)
  (rtd-opaque? rtd))

(define (record-type-fields rtd)
  (rtd-fields rtd))

(define (record-type-field-names rtd)
  (list->vector (map cdr (rtd-fields rtd))))

(define (record-field-mutable? rtd k)
  (car (list-ref (rtd-fields rtd) k)))

(define (cadr x) (car (cdr x)))

(define (make-record-type-descriptor name parent uid sealed? opaque? fields)
  (or (symbol? name)
    (assertion-violation 'make-record-type-descriptor "expected a symbol for name" name))
  (or (vector? fields)
    (assertion-violation 'make-record-type-descriptor "expected a vector for fields" fields))
  (and parent 
    (or (record-type-descriptor? parent)
        (assertion-violation 'make-record-type-descriptor "expected a record-type descriptor or #f for parent" parent))
    (and (rtd-sealed? parent)
      (assertion-violation 'make-record-type-descriptor "cannot extend a sealed record type" parent)))
  
  (let ([opaque? (or opaque? (and parent (rtd-opaque? parent)))]
        [fields
          (map (lambda (field)
                (if (eq? (car field) 'mutable)
                  (cons #t (cadr field))
                  (cons #f (cadr field)))) 
               (vector->list fields))])
    (cond
      [(not uid) (make-rtd name parent #f sealed? opaque? fields)]
      [else 
        (let ([current (core-hash-ref nongenerative-record-types uid #f)])
          (cond 
            [current
              (if (and (eqv? (rtd-uid current) uid)
                       (eqv? parent (rtd-parent current))
                       (equal? fields (rtd-fields current)))
                current
                (assertion-violation 'make-record-type-descriptor
                                     "mismatched subsequent call for nongenerative record-type"
                                     (list name parent uid sealed? opaque? fields)))]
            [else 
              (let ([new (make-rtd name parent uid sealed? opaque? fields)])
                (core-hash-put! nongenerative-record-types uid new)
                new)]))])))


(define (make-rcd rtd protocol custom-protocol? parent)
  (tuple 'type:record-constructor-descriptor rtd protocol custom-protocol? parent))

(define (record-constructor-descriptor? obj)
  (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:record-constructor-descriptor)))

(define (rcd-rtd rcd)
  (tuple-ref rcd 1))
(define (rcd-protocol rcd)
  (tuple-ref rcd 2))
(define (rcd-custom-protocol? rcd)
  (tuple-ref rcd 3))
(define (rcd-parent rcd)
  (tuple-ref rcd 4))


(define default-protocol
  (lambda (rtd)
    (let ([parent (rtd-parent rtd)])

      (if parent
        (let ([parent-field-count (rtd-total-field-count parent)])
          (lambda (p)

            (lambda field-values
              (receive (parent-field-values this-field-values) (split-at field-values parent-field-count)
                (apply (apply p parent-field-values) this-field-values)))))
        (lambda (p)
          (lambda field-values
             (apply p field-values)))))))

(define (make-simple-conser desc rtd argc)
  ((rcd-protocol desc)
    (lambda field-values
      (apply tuple rtd field-values))))

(define (make-nested-conser desc rtd argc)
  ((rcd-protocol desc)
    ((let loop ([desc desc])
      (if (rcd-parent desc)
        (let ([parent (rcd-parent desc)])
          (lambda extra-field-values
            (lambda protocol-args
              (lambda this-field-values
                (apply
                  ((rcd-protocol parent) (apply (loop parent) (append this-field-values extra-field-values)))
                  protocol-args)))))
        (lambda extra-field-values
          (lambda this-field-values
              (let ([field-values (append this-field-values extra-field-values)])
                (apply tuple rtd field-values)))))))))

(define (make-record-constructor-descriptor rtd parent protocol)
  (let ([custom-protocol? (and protocol #t)]
        [protocol (or protocol (default-protocol rtd))]
        [parent
          (or parent
            (if (rtd-parent rtd)
              (make-record-constructor-descriptor (rtd-parent rtd) #f #f)
              #f))])
      (make-rcd rtd protocol custom-protocol? parent)))

(define (flat-field-offset rtd k)
  (+ k (rtd-inherited-field-count rtd) 1))

(define make-accessor
  (lambda (rtd k)
    (lambda (obj)
      (if (not (record? obj))
        (assertion-violation 'record-accessor "expected a record" obj))
      (if (or (eq? rtd (tuple-ref obj 0))
              (rtd-ancestor? rtd (tuple-ref obj 0)))
          (tuple-ref obj k)
          (assertion-violation "record accessor" "invalid record type" obj rtd)))))

(define (wrong-type-argument-message . args) args)

(define make-mutator
  (lambda (rtd k)
    (lambda (obj datum)
      (if (not (record? obj))
        (assertion-violation "record mutator" "expected a record" obj))
      (if (or (eq? rtd (tuple-ref obj 0))
              (rtd-ancestor? rtd (tuple-ref obj 0)))
          (tuple-set! obj k datum)
          (assertion-violation "record mutator" (wrong-type-argument-message (format "record of type ~a" (rtd-name rtd)) (list obj datum)))))))

(define (make-predicate rtd)
  (lambda (obj)
    "Predicate for object"
    (and (tuple? obj) (or (eq? rtd (tuple-ref obj 0))
        (rtd-ancestor? rtd (tuple-ref obj 0))))))

(define (record-constructor desc)
  (or (record-constructor-descriptor? desc)
        (assertion-violation 'record-constructor (wrong-type-argument-message "record-constructor-descriptor" desc)))
  (let ([rtd (rcd-rtd desc)])
    (if (rcd-parent desc)
      (make-nested-conser desc rtd (rtd-total-field-count rtd))
      (make-simple-conser desc rtd (length (rtd-fields rtd))))))


(define (record-predicate rtd)
  (or (record-type-descriptor? rtd)
        (assertion-violation 'record-predicate (wrong-type-argument-message "record-type-descriptor" rtd)))
  (make-predicate rtd))

(define (record-accessor rtd k)
  (or (record-type-descriptor? rtd)
        (assertion-violation 'record-accessor (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd k)))
  (or (< -1 k (length (rtd-fields rtd)))
      (assertion-violation 'record-accessor "field index out of range"))
  (make-accessor rtd (flat-field-offset rtd k)))

(define (record-mutator rtd k)
  (or (record-type-descriptor? rtd)
        (assertion-violation 'record-mutator (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd k)))
  (or (< -1 k (length (rtd-fields rtd)))
      (assertion-violation 'record-mutator "field index out of range" (list rtd k)))
  (or (record-field-mutable? rtd k)
      (assertion-violation 'record-mutator "specified field is immutable" (list rtd k)))
  (make-mutator rtd (flat-field-offset rtd k)))

(define (make-record-type name rtd rcd)
  (tuple 'type:record-type name rtd rcd))


(define (record-type? obj)
  (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:record-type)))
(define (record-type-rcd obj)
  (or (record-type? obj)
        (assertion-violation 'record-type-rcd "not a record-type" obj))
  (tuple-ref obj 3))
(define (record-type-rtd obj)
  (or (record-type? obj)
        (assertion-violation 'record-type-rtd (wrong-type-argument-message "record-type" obj)))
  (tuple-ref obj 2))
(define (record? obj)
  (and
    (tuple? obj)
    (record-type-descriptor? (tuple-ref obj 0))))
(define (record-type-descriptor obj)
  (record-rtd obj))
(define (record-rtd obj)
  (if (record? obj)
    (tuple-ref obj 0)
    (assertion-violation
      'record-rtd
      (format "expected a record, but got ~r" obj) obj)))




(define &condition
  (let* ([rtd (make-record-type-descriptor '&condition #f #f #f #f '#())]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&condition rtd rcd)))

(define (compound-condition-component obj) (tuple-ref obj 1))

(define condition
  (lambda components
    (tuple
      'type:condition
      (apply append
        (map (lambda (component) (simple-conditions component)) components)))))

(define (compound-condition? obj)
  (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:condition)))

(define (simple-condition? obj)
  (and (record? obj) (rtd-ancestor? (record-type-rtd &condition) (record-rtd obj))))

(define (condition? obj)
  (or (simple-condition? obj) (compound-condition? obj)))

(define (simple-conditions c)
  (if (simple-condition? c)
    (list c)
    (if (compound-condition? c)
      (compound-condition-component c)
      #f)))

(define (condition-predicate rtd)
  (lambda (obj)
    (if (simple-condition? obj)
      (rtd-ancestor? rtd (record-rtd obj))
      (if (compound-condition? obj)
        (any1 (lambda (c) (rtd-ancestor? rtd (record-rtd c))) (compound-condition-component obj))
      #f))))

(define (condition-accessor rtd proc)
  (define wrong-type
    (lambda (rtd obj)
      (assertion-violation
        "condition accessor"
        (format "expected condition of a subtype of ~s, but got ~r" rtd obj) rtd obj)))

  (or (rtd-ancestor? (record-type-rtd &condition) rtd)
      (assertion-violation
        'condition-accessor
        (format "expected record-type-descriptor of a subtype of &condition, but got ~s" rtd) rtd proc))
  (lambda (obj)

    (if (simple-condition? obj)
      (begin
        (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj))
        (proc obj))
      (if (compound-condition? obj)
        (let ([res (any1 (lambda (c) (and (rtd-ancestor? rtd (record-rtd c)) c)) (compound-condition-component obj))])
          (if res (proc res) (wrong-type rtd obj)))
        (wrong-type rtd obj)))))



(define &message
  (let ([rtd (make-record-type-descriptor '&message (record-type-rtd &condition) #f #f #f '#((immutable message)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&message rtd rcd))))

(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))

(define &source
  (let ([rtd (make-record-type-descriptor '&source (record-type-rtd &condition) #f #f #f '#((immutable file) (immutable line) (immutable column)))])
    (let ([rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)])
      (make-record-type '&source rtd rcd))))

(define make-source-condition (record-constructor (record-type-rcd &source)))
(define source-condition? (condition-predicate (record-type-rtd &source)))
(define condition-source-file (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 0)))
(define condition-source-line (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 1)))
(define condition-source-column (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 2)))

(define (make-condition-uid) #f)
(define &warning
  (let ((rtd (make-record-type-descriptor '&warning (record-type-rtd &condition) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&warning rtd rcd))))
(define make-warning (record-constructor (record-type-rcd &warning)))
(define warning? (condition-predicate (record-type-rtd &warning)))

(define &serious
  (let ((rtd (make-record-type-descriptor '&serious (record-type-rtd &condition) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&serious rtd rcd))))
(define make-serious-condition (record-constructor (record-type-rcd &serious)))
(define serious-condition? (condition-predicate (record-type-rtd &serious)))

(define &error
  (let ((rtd (make-record-type-descriptor '&error (record-type-rtd &serious) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &serious) #f)))
      (make-record-type '&error rtd rcd))))
(define make-error (record-constructor (record-type-rcd &error)))
(define error? (condition-predicate (record-type-rtd &error)))

(define &violation
  (let ((rtd (make-record-type-descriptor '&violation (record-type-rtd &serious) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &serious) #f)))
      (make-record-type '&violation rtd rcd))))
(define make-violation (record-constructor (record-type-rcd &violation)))
(define violation? (condition-predicate (record-type-rtd &violation)))

(define &assertion
  (let ((rtd (make-record-type-descriptor '&assertion (record-type-rtd &violation) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&assertion rtd rcd))))
(define make-assertion-violation (record-constructor (record-type-rcd &assertion)))
(define assertion-violation? (condition-predicate (record-type-rtd &assertion)))

(define &irritants
  (let ((rtd (make-record-type-descriptor '&irritants (record-type-rtd &condition) (make-condition-uid) #f #f '#((immutable irritants)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&irritants rtd rcd))))
(define &irritants-irritants (record-accessor (record-type-rtd &irritants) 0))
(define make-irritants-condition (record-constructor (record-type-rcd &irritants)))
(define irritants-condition? (condition-predicate (record-type-rtd &irritants)))
(define condition-irritants (condition-accessor (record-type-rtd &irritants) &irritants-irritants))

(define &who
  (let ((rtd (make-record-type-descriptor '&who (record-type-rtd &condition) (make-condition-uid) #f #f '#((immutable who)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&who rtd rcd))))
(define &who-who (record-accessor (record-type-rtd &who) 0))
(define make-who-condition (record-constructor (record-type-rcd &who)))
(define who-condition? (condition-predicate (record-type-rtd &who)))
(define condition-who (condition-accessor (record-type-rtd &who) &who-who))

(define &non-continuable
  (let ((rtd (make-record-type-descriptor '&non-continuable (record-type-rtd &violation) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&non-continuable rtd rcd))))
(define make-non-continuable-violation (record-constructor (record-type-rcd &non-continuable)))
(define non-continuable-violation? (condition-predicate (record-type-rtd &non-continuable)))

(define &implementation-restriction
  (let ((rtd (make-record-type-descriptor '&implementation-restriction (record-type-rtd &violation) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&implementation-restriction rtd rcd))))
(define make-implementation-restriction-violation
  (record-constructor (record-type-rcd &implementation-restriction)))
(define implementation-restriction-violation?
  (condition-predicate (record-type-rtd &implementation-restriction)))

(define &lexical
  (let ((rtd (make-record-type-descriptor '&lexical (record-type-rtd &violation) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&lexical rtd rcd))))
(define make-lexical-violation (record-constructor (record-type-rcd &lexical)))
(define lexical-violation? (condition-predicate (record-type-rtd &lexical)))

(define &syntax
  (let ((rtd (make-record-type-descriptor '&syntax (record-type-rtd &violation) (make-condition-uid) #f #f '#((immutable form) (immutable subform)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&syntax rtd rcd))))
(define &syntax-form (record-accessor (record-type-rtd &syntax) 0))
(define &syntax-subform (record-accessor (record-type-rtd &syntax) 1))
(define make-syntax-violation (record-constructor (record-type-rcd &syntax)))
(define syntax-violation? (condition-predicate (record-type-rtd &syntax)))
(define syntax-violation-form (condition-accessor (record-type-rtd &syntax) &syntax-form))
(define syntax-violation-subform (condition-accessor (record-type-rtd &syntax) &syntax-subform))

(define &undefined
  (let ((rtd (make-record-type-descriptor '&undefined (record-type-rtd &violation) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &violation) #f)))
      (make-record-type '&undefined rtd rcd))))
(define make-undefined-violation (record-constructor (record-type-rcd &undefined)))
(define undefined-violation? (condition-predicate (record-type-rtd &undefined)))

(define &i/o
  (let ((rtd (make-record-type-descriptor '&i/o (record-type-rtd &error) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &error) #f)))
      (make-record-type '&i/o rtd rcd))))
(define make-i/o-error (record-constructor (record-type-rcd &i/o)))
(define i/o-error? (condition-predicate (record-type-rtd &i/o)))

(define &i/o-read
  (let ((rtd (make-record-type-descriptor '&i/o-read (record-type-rtd &i/o) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-read rtd rcd))))
(define make-i/o-read-error (record-constructor (record-type-rcd &i/o-read)))
(define i/o-read-error? (condition-predicate (record-type-rtd &i/o-read)))

(define &i/o-write
  (let ((rtd (make-record-type-descriptor '&i/o-write (record-type-rtd &i/o) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-write rtd rcd))))
(define make-i/o-write-error (record-constructor (record-type-rcd &i/o-write)))
(define i/o-write-error? (condition-predicate (record-type-rtd &i/o-write)))

(define &i/o-invalid-position
  (let ((rtd (make-record-type-descriptor '&i/o-invalid-position (record-type-rtd &i/o) (make-condition-uid) #f  #f '#((immutable position)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-invalid-position rtd rcd))))
(define &i/o-invalid-position-position
  (record-accessor (record-type-rtd &i/o-invalid-position) 0))
(define make-i/o-invalid-position-error
  (record-constructor (record-type-rcd &i/o-invalid-position)))
(define i/o-invalid-position-error? (condition-predicate (record-type-rtd &i/o-invalid-position)))
(define i/o-error-position
  (condition-accessor (record-type-rtd &i/o-invalid-position) &i/o-invalid-position-position))

(define &i/o-filename
  (let ((rtd (make-record-type-descriptor '&i/o-filename (record-type-rtd &i/o) (make-condition-uid) #f #f '#((immutable filename)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-filename rtd rcd))))
(define &i/o-filename-filename (record-accessor (record-type-rtd &i/o-filename) 0))
(define make-i/o-filename-error (record-constructor (record-type-rcd &i/o-filename)))
(define i/o-filename-error? (condition-predicate (record-type-rtd &i/o-filename)))
(define i/o-error-filename (condition-accessor (record-type-rtd &i/o-filename) &i/o-filename-filename))

(define &i/o-file-protection
  (let ((rtd (make-record-type-descriptor '&i/o-file-protection (record-type-rtd &i/o-filename) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-protection rtd rcd))))
(define make-i/o-file-protection-error
  (record-constructor (record-type-rcd &i/o-file-protection)))
(define i/o-file-protection-error? (condition-predicate (record-type-rtd &i/o-file-protection)))

(define &i/o-file-is-read-only
  (let ((rtd (make-record-type-descriptor '&i/o-file-is-read-only (record-type-rtd &i/o-file-protection) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-file-protection) #f)))
      (make-record-type '&i/o-file-is-read-only rtd rcd))))
(define make-i/o-file-is-read-only-error (record-constructor (record-type-rcd &i/o-file-is-read-only)))
(define i/o-file-is-read-only-error? (condition-predicate (record-type-rtd &i/o-file-is-read-only)))

(define &i/o-file-already-exists
  (let ((rtd (make-record-type-descriptor '&i/o-file-already-exists (record-type-rtd &i/o-filename) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-already-exists rtd rcd))))
(define make-i/o-file-already-exists-error (record-constructor (record-type-rcd &i/o-file-already-exists)))
(define i/o-file-already-exists-error? (condition-predicate (record-type-rtd &i/o-file-already-exists)))

(define &i/o-file-does-not-exist
  (let ((rtd (make-record-type-descriptor '&i/o-file-does-not-exist (record-type-rtd &i/o-filename) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-filename) #f)))
      (make-record-type '&i/o-file-does-not-exist rtd rcd))))
(define make-i/o-file-does-not-exist-error (record-constructor (record-type-rcd &i/o-file-does-not-exist)))
(define i/o-file-does-not-exist-error? (condition-predicate (record-type-rtd &i/o-file-does-not-exist)))

(define &i/o-port
  (let ((rtd (make-record-type-descriptor '&i/o-port (record-type-rtd &i/o) (make-condition-uid) #f #f '#((immutable port)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o) #f)))
      (make-record-type '&i/o-port rtd rcd))))
(define &i/o-port-port (record-accessor (record-type-rtd &i/o-port) 0))
(define make-i/o-port-error (record-constructor (record-type-rcd &i/o-port)))
(define i/o-port-error? (condition-predicate (record-type-rtd &i/o-port)))
(define i/o-error-port (condition-accessor (record-type-rtd &i/o-port) &i/o-port-port))

(define &i/o-decoding
  (let ((rtd (make-record-type-descriptor '&i/o-decoding (record-type-rtd &i/o-port) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-port) #f)))
      (make-record-type '&i/o-decoding rtd rcd))))
(define make-i/o-decoding-error (record-constructor (record-type-rcd &i/o-decoding)))
(define i/o-decoding-error? (condition-predicate (record-type-rtd &i/o-decoding)))

(define &i/o-encoding
  (let ((rtd (make-record-type-descriptor '&i/o-encoding (record-type-rtd &i/o-port) (make-condition-uid) #f #f '#((immutable char)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &i/o-port) #f)))
      (make-record-type '&i/o-encoding rtd rcd))))
(define &i/o-encoding-char (record-accessor (record-type-rtd &i/o-encoding) 0))
(define make-i/o-encoding-error (record-constructor (record-type-rcd &i/o-encoding)))
(define i/o-encoding-error? (condition-predicate (record-type-rtd &i/o-encoding)))
(define i/o-encoding-error-char (condition-accessor (record-type-rtd &i/o-encoding) &i/o-encoding-char))

(define &no-infinities
  (let ((rtd (make-record-type-descriptor '&no-infinities (record-type-rtd &implementation-restriction) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &implementation-restriction) #f)))
      (make-record-type '&no-infinities rtd rcd))))
(define make-no-infinities-violation (record-constructor (record-type-rcd &no-infinities)))
(define no-infinities-violation? (condition-predicate (record-type-rtd &no-infinities)))

(define &no-nans
  (let ((rtd (make-record-type-descriptor '&no-nans (record-type-rtd &implementation-restriction) (make-condition-uid) #f #f '#())))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &implementation-restriction) #f)))
      (make-record-type '&no-nans rtd rcd))))
(define make-no-nans-violation (record-constructor (record-type-rcd &no-nans)))
(define no-nans-violation? (condition-predicate (record-type-rtd &no-nans)))


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


(define (assertion-violation who message . irritants)
 ;(print-stacktrace)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-assertion-violation)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)))))
      (raise #f))
    (raise #f)))

(define (syntax-violation who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-syntax-violation)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)))))
      #f)
    #f))

(define (error who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-error)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)))))
      #f)
    #f))
(define (implementation-restriction-violation who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (raise
        (apply
          condition
          (filter
            values
            (list
              (make-implementation-restriction-violation)
              (and who (make-who-condition who))
              (make-message-condition message)
              (make-irritants-condition irritants)))))
      #f)
    #f))

(define undefined-violation
  (lambda (who . message)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-undefined-violation)
            (and who (make-who-condition who))
            (and (pair? message) (make-message-condition (car message)))))))))

(define (.make-undefined-violation who . message)
  ;(print-stacktrace)
  (if (or (not who) (string? who) (symbol? who))
    (apply
      condition
      (filter
        values
        (list
          (make-undefined-violation)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))
    #f))



(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-filename-error filename)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-i/o-error
  (lambda (who message . irritants)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-error)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))))))))

(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (apply constructor options)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and port (make-i/o-port-error port))
            (make-irritants-condition (cons* port options))))))))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (raise
      (apply
        condition
        (filter
          values
          (list
            (apply constructor options)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? options) (make-irritants-condition options))))))))

(define raise-i/o-read-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-read-error who message port)))

(define raise-i/o-write-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-write-error who message port)))

(define raise-i/o-file-protection-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-protection-error who message filename)))

(define raise-i/o-file-is-read-only-error
  (lambda (who message port)
    (raise-misc-i/o-error-with-port make-i/o-file-is-read-only-error who message port)))

(define raise-i/o-file-already-exists-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-already-exists-error who message filename)))

(define raise-i/o-file-does-not-exist-error
  (lambda (who message filename)
    (raise-misc-i/o-error make-i/o-file-does-not-exist-error who message filename)))

(define raise-i/o-invalid-position-error
  (lambda (who message port position)
    (raise-misc-i/o-error-with-port make-i/o-invalid-position-error who message port position)))

(define raise-i/o-decoding-error
  (lambda (who message port)
    (raise-misc-i/o-error make-i/o-decoding-error who message port)))

(define raise-i/o-encoding-error
  (lambda (who message port char)
    (raise-misc-i/o-error make-i/o-encoding-error who message port char)))
(define .make-io-error
  (lambda (who message . irritants)
    (if (or (not who) (string? who) (symbol? who))
      (if (string? message)
        (apply
          condition
          (filter
            values
            (list
              (make-i/o-error)
              (and who (make-who-condition who))
              (make-message-condition message)
              (and (pair? irritants) (make-irritants-condition irritants)))))
        #f)
      #f)))


(define (.make-assertion-violation who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-assertion-violation)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants))))
      #f)
    #f))


(define (.make-error who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-error)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants))))
      #f)
    #f))

(define (.make-implementation-restriction-violation who message . irritants)
  (if (or (not who) (string? who) (symbol? who))
    (if (string? message)
      (apply
        condition
        (filter
          values
          (list
            (make-implementation-restriction-violation)
            (and who (make-who-condition who))
            (make-message-condition message)
            (make-irritants-condition irritants))))
      #f)
    #f))

(define (lexical-violation who . message)
  (raise
    (apply
      condition
      (filter
        values
        (list
          (make-lexical-violation)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))))

(define .make-lexical-violation
  (lambda (who . message)
    (if (or (not who) (string? who) (symbol? who))
      (apply
        condition
        (filter
          values
          (list
            (make-lexical-violation)
            (and who (make-who-condition who))
            (and (pair? message) (make-message-condition (car message))))))
      #f)))

(define (filter pred lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
      (reverse acc)
      (if (pred (car lst))
        (loop (cdr lst) (cons (car lst) acc))
        (loop (cdr lst) acc)))))



(define (load-in-vicinity filename directory)
  (let ([thunk (load-thunk-in-vicinity filename #t directory )])
    (thunk)))

(define (load filename)
  (let ([thunk (load-thunk-in-vicinity filename #t)])
    (thunk)))

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

    (set-module-uses! module (append new cur))
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
      (:print "exception: " p)
      (format p "Unhandled exception: ~a: ~a" (condition-who exn) (condition-message exn)))))


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
(primitive-load "boot/print.scm")
(primitive-load "boot/format.scm")
(primitive-load "boot/log.scm")
(initialize-io-system)
(primitive-load "boot/reader.scm")
(primitive-load "boot/eval.scm")
(set! %load-extensions 
  (append '("capy.sls" "capy.sld" "capy.scm" "sls" "sld" ".sch")
          %load-extensions))

(let* ([host-arch (host-arch)]
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

(let ([user-module (define-module* '(capy user))])
  (current-module user-module))