
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

(define (assq-ref key alist . opt)
  (let ((pair (assq key alist)))
    (if pair (cdr pair) (if (null? opt) #f (car opt)))))

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
  (if (null? rest)
    (map1 f x)
    (map2 f x (car rest))))

(define (for-each proc lst)
  (let loop ([lst lst])
    (if (pair? lst)
      (begin (proc (car lst)) (loop (cdr lst)))
      (if (null? lst) '()
        (assertion-violation 'for-each "expected a proper list" lst)))))

(define (make-parameter init . converter)
  (let ([f (make-fluid init)]
        [conv (if (null? converter) (lambda (x) x)  (car converter))])
      (lambda args
        (if (null? args) 
            (fluid-ref f)
            (let ([old (fluid-ref f)])
              (fluid-set! f (conv (car args)))
              old)))))
          

(define (make-rtd name parent uid sealed? opaque? fields)
    (tuple 'type:record-type-descriptor name parent uid sealed? opaque? fields))

(define record-type-descriptor?
  (lambda (obj)
    (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:record-type-descriptor))))


(define rtd-name        (lambda (rtd) (tuple-ref rtd 1)))
(define rtd-parent      (lambda (rtd) (tuple-ref rtd 2)))
(define rtd-uid         (lambda (rtd) (tuple-ref rtd 3)))
(define rtd-sealed?     (lambda (rtd) (tuple-ref rtd 4)))
(define rtd-opaque?     (lambda (rtd) (tuple-ref rtd 5)))
(define rtd-fields      (lambda (rtd) (tuple-ref rtd 6)))


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
  (let ([opaque? (or opaque? (and parent (rtd-opaque? parent)))]
        [fields 
          (map (lambda (field)
            (if (eq? (car field) 'mutable)
              (cons #t (cadr field))
              (cons #f (cadr field)))
          ) (vector->list fields))])
        
    (make-rtd name parent uid sealed? opaque? fields)))


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
      (if (or (eq? rtd (tuple-ref obj 0))
              (rtd-ancestor? rtd (tuple-ref obj 0)))
          (tuple-ref obj k)
          (assertion-violation "record accessor" (wrong-type-argument-message (format "record of type ~a" (rtd-name rtd)) obj))))))

(define (wrong-type-argument-message . args) args)
(define (format msg . rest) msg)

(define make-mutator
  (lambda (rtd k)
    (lambda (obj datum)
      (if (or (eq? rtd (tuple-ref obj 0))
              (rtd-ancestor? rtd (tuple-ref obj 0)))
          (tuple-set! obj k datum)
          (assertion-violation "record mutator" (wrong-type-argument-message (format "record of type ~a" (rtd-name rtd)) (list obj datum)))))))

(define (make-predicate rtd)
  (lambda (obj)
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
        (assertion-violation 'record-accssor (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd k)))
  (or (< -1 k (length (rtd-fields rtd)))
      (assertion-violation 'record-accssor "field index out of range"))
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
        (assertion-violation 'record-type-rcd (wrong-type-argument-message "record-type" obj)))
  (tuple-ref obj 3))
(define (record-type-rtd obj) 
  (or (record-type? obj)
        (assertion-violation 'record-type-rtd (wrong-type-argument-message "record-type" obj)))
  (tuple-ref obj 2))
(define (record? obj)
  (and 
    (tuple? obj)
    (record-type-descriptor? (tuple-ref obj 0))))
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


(define *here*
  (let ([f (make-thread-local-fluid (list #f))])
    (lambda args
      (if (null? args) 
          (fluid-ref f)
          (let ([old (fluid-ref f)])
            (fluid-set! f (car args))
            old)))))



(define (reroot! there)
    (define (reroot-loop there)
        (if (eq? there (*here*))
            #f
            (begin 
         
                (reroot-loop (cdr there))
                (let ([old-pair (car there)])
                    (let ([before (car old-pair)] [after (cdr old-pair)])
                        (set-car! (*here*) (cons after before))
                        (set-cdr! (*here*) there)
                        (set-car! there #f)
                        (set-cdr! there '()) 
                        (*here* there)
                        (before)
                    )))))
    (reroot-loop there))


(define (dynamic-wind before thunk after)
    (let ([here (*here*)])
        (let ([there (list #f)])
            (before)
            (set-car! (*here*) (cons after before))
            (set-cdr! (*here*) there)
            (*here* there)
            (receive results (thunk)
              (begin
                (reroot! here)
                (apply values results))))))

(define (call-with-current-continuation proc)
  (let ([here (*here*)])
    (.call/cc-unsafe (lambda (cont)
      (proc (lambda results 
        (reroot! here)
        (apply cont results)))))))

(define call/cc call-with-current-continuation)

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
  ((car (*current-exception-handlers*)) obj))

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
(define .make-i/o-error 
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
  (let ([thunk (load-thunk-in-vicinity filename directory)])
    (thunk)))

(define (load filename)
  (let ([thunk (load-thunk-in-vicinity filename)])
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


(define (module-for-each proc module)
  (for-each proc (core-hash->list (module-obarray module))))


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
  (let ([root (make-module)])
    (set-module-name! root '())
    (module-define-submodule! root 'capy the-root-module)
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
                      (lambda (_x) (return #f))
                      (lambda ()
                        (load (string-append dir-hint name))
                        (set! didit #t)
                        )))))))
            (lambda () (set-autoloaded! dir-hint name didit)))
          didit))))

(define (resolve-interface name select hide)
  (let* ([mod (resolve-module name #t #f)]
         [iface (and mod (module-public-interface mod))])
    (if (not iface)
      (assertion-violation 'resolve-interface "no code for module" name))     
    (if (and (not select) (null? hide))
      iface
      #f)))

      
(define (define-module* 
  name
  filename
  imports
  exports 
  replacements
  re-exports
  re-export-replacements
  autoloads)
  (define (list-of pred lst)
    (or (null? lst) (and (pred (car lst)) (list-of pred (cdr lst)))))
  
  (define (valid-import? x)
    (list? x))
  (define (valid-export? x)
    (or (symbol? x) (and (pair? x) (symbol? (car x)) (symbol? (cdr x)))))
  (define (valid-autoload? x)
    (and (pair? x) (list-of symbol? (car x)) (list-of symbol? (cdr x))))

  (let ([module (resolve-module name #f #t)])
    (beautify-user-module! module)
    (if filename 
      (set-module-filename! module filename))
    (if (not (list-of valid-import? imports))
      (assertion-violation 'define-module "invalid imports" imports))
    (if (not (list-of valid-export? exports))
      (assertion-violation 'define-module "invalid exports" exports))
    (if (not (list-of valid-autoload? autoloads))
      (assertion-violation 'define-module "invalid autoloads" autoloads))

    (module-export! module exports)

    (if (not (null? imports))
      (let ([imports (map (lambda (import-spec)
        (resolve-interface import-spec #f '())) imports)])
        (module-use-interfaces! module imports)))

    module))

(define (module-export! m names)
  (let ([public-i (module-public-interface m)])
    (for-each (lambda (name)
      (let* ([internal-name (if (pair? name) (car name) name)]
             [external-name (if (pair? name) (cdr name) name)]
             [var (module-ensure-local-variable! m internal-name)])
        (module-add! public-i external-name var))) names)))

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
    (or (resolve-interface mif-args #f '())
      (assertion-violation 'use "failed to resolve module" mif-args))) module-iface-args)])
    (module-use-interfaces! (current-module) interfaces)))

(define (call-with-values producer consumer)
  (receive results (producer)
    (apply consumer results)))

(load "boot/eval.scm")
(load "boot/expand.scm")
(load "boot/interpreter.scm")
(load "boot/psyntax.scm")

