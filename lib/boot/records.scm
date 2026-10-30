(|#%require| (boot prim) (boot control) (late (boot raise) error) (late (boot format) format))
(|#%provide| all-defined)


(define (rtd-inherited-field-count rtd)
  (- (rtd-field-count rtd) (length (rtd-fields rtd))))

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

;; The protocol of RCD. A descriptor made with the default protocol has none, so the default
;; one is built for its type.
(define (%rcd-protocol rcd)
  (or (rcd-protocol rcd)
    (%default-protocol (rcd-rtd rcd))))

(define %default-protocol
  (lambda (rtd)
    (let ([parent (rtd-parent rtd)])
      (if parent
        (let ([parent-field-count (rtd-field-count parent)])
          (lambda (p)

            (lambda field-values
              (receive (parent-field-values this-field-values) (split-at field-values parent-field-count)
                (apply (apply p parent-field-values) this-field-values)))))
        (lambda (p)
          (lambda field-values
            (apply p field-values)))))))

(define (make-simple-conser desc rtd argc)
  ((%rcd-protocol desc)
    (lambda field-values
      (cond
        [(= (length field-values) argc) (apply tuple rtd field-values)]
        [else (assertion-violation (rtd-name rtd) "wrong-number of arguments" field-values)]))))

(define (make-nested-conser desc rtd argc)
  ((%rcd-protocol desc)
    ((let loop ([desc desc])
        (if (rcd-parent desc)
          (let ([parent (rcd-parent desc)])
            (lambda extra-field-values
              (lambda protocol-args
                (lambda this-field-values
                  (apply
                    ((%rcd-protocol parent) (apply (loop parent) (append this-field-values extra-field-values)))
                    protocol-args)))))
          (lambda extra-field-values
            (lambda this-field-values
              (let ([field-values (append this-field-values extra-field-values)])
                (if (= (length field-values) argc)
                  (apply tuple rtd field-values)
                  (assertion-violation 'record-constructor "wrong number of arguments" field-values))))))))))

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
      (make-nested-conser desc rtd (rtd-field-count rtd))
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
