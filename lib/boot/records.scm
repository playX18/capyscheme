
(define (make-rtd name parent uid sealed? opaque? fields)
    (tuple 'type:record-type-descriptor name parent uid sealed? opaque? fields #f))

(define record-type-descriptor?
  (lambda (obj)
    (and (tuple? obj) (eq? (tuple-ref obj 0) 'type:record-type-descriptor))))


(define rtd-name        (lambda (rtd) (tuple-ref rtd 1)))
(define rtd-parent      (lambda (rtd) (unless (tuple? rtd)
  (error 'rtd-parent "NOT A TUPLE" rtd)) (tuple-ref rtd 2)))
(define rtd-uid         (lambda (rtd) (tuple-ref rtd 3)))
(define rtd-sealed?     (lambda (rtd) (tuple-ref rtd 4)))
(define rtd-opaque?     (lambda (rtd) (tuple-ref rtd 5)))
(define rtd-fields      (lambda (rtd) (tuple-ref rtd 6)))
(define rtd-printer     (lambda (rtd) (tuple-ref rtd 7)))
(define set-rtd-printer! (lambda (rtd printer) (tuple-set! rtd 7 printer)))


(define (rtd-ancestor? parent rtd)
  (let loop ((rtd rtd))
    (or (eq? rtd parent)
      (and rtd
           (tuple? rtd)
           (loop (rtd-parent rtd))))))

(define rtd-inherited-field-count
  (lambda (rtd)
    (let loop ((rtd (rtd-parent rtd)) (count 0))
      (cond (rtd
             (loop (rtd-parent rtd)
                   (+ count (length (rtd-fields rtd)))))
            (else
             count)))))





(define rtd-total-field-count
  (lambda (rtd)
    (+ (rtd-inherited-field-count rtd) (length (rtd-fields rtd)))))


(define (record-type-name rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-name "not a record-type-descriptor" rtd))
  (rtd-name rtd))

(define (record-type-parent rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-parent "not a record-type-descriptor" rtd))
  (rtd-parent rtd))

(define (record-type-uid rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-uid "not a record-type-descriptor" rtd))
  (rtd-uid rtd))

(define (record-type-generative? rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-generative? "not a record-type-descriptor" rtd))
  (not (rtd-uid rtd)))

(define (record-type-sealed? rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-sealed? "not a record-type-descriptor" rtd))
  (rtd-sealed? rtd))

(define (record-type-opaque? rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-opaque? "not a record-type-descriptor" rtd))
  (rtd-opaque? rtd))

(define (record-type-fields rtd)
  (or (record-type-descriptor? rtd)
    (assertion-violation 'record-type-fields "not a record-type-descriptor" rtd))
  (rtd-fields rtd))

(define (record-type-field-names rtd)
  (list->vector (map cdr (rtd-fields rtd))))

(define (record-field-mutable? rtd k)
  (car (list-ref (rtd-fields rtd) k)))


(define nongenerative-record-types (make-core-hash-eq))

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


(define %default-protocol
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
      (cond 
        [(= (length field-values) argc) (apply tuple rtd field-values)]
        [else (assertion-violation 'record-constructor "wrong-number of arguments" field-values)]
      ))))

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
                (if (= (length field-values) argc)
                  (apply tuple rtd field-values)
                  (assertion-violation 'record-constructor "wrong number of arguments" field-values))))))))))

(define (make-record-constructor-descriptor rtd parent protocol)
  (let ([custom-protocol? (and protocol #t)]
        [protocol (or protocol (%default-protocol rtd))]
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

(define (record-accessor-by-name rtd name)
  (or (record-type-descriptor? rtd)
        (assertion-violation 'record-accessor-by-name (wrong-type-argument-message "record-type-descriptor" rtd) (list rtd name)))
  (let loop ((fields (rtd-fields rtd)) (index 0))
    (cond
      ((null? fields)
        (assertion-violation 'record-accessor-by-name (format "no such field ~a in record type ~a" name (rtd-name rtd)) (list rtd name)))
      ((eq? (cdr (car fields)) name)
        (make-accessor rtd (flat-field-offset rtd index)))
      (else
        (loop (cdr fields) (+ index 1))))))


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


(define (record-rtd obj)
  (if (record? obj)
    (tuple-ref obj 0)
    (assertion-violation
      'record-rtd
      (format "expected a record, but got ~r" obj) obj)))
