(|#%require| (boot prim) (boot control) (boot records))
(|#%provide| all-defined)


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
        ($boot-format #f "expected condition of a subtype of ~a, but got ~a" rtd obj)
        rtd
        obj)))

  (or (rtd-ancestor? (record-type-rtd &condition) rtd)
    (assertion-violation
      'condition-accessor
      ($boot-format #f "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd)
      rtd
      proc))
  (lambda (obj)

    (if (simple-condition? obj)
      (begin
        (or (rtd-ancestor? rtd (record-rtd obj)) (wrong-type rtd obj))
        (proc obj))
      (if (compound-condition? obj)
        (let ([res (any1 (lambda (c) (and (rtd-ancestor? rtd (record-rtd c)) c)) (compound-condition-component obj))])
          (if res (proc res) (wrong-type rtd obj)))
        (wrong-type rtd obj)))))


(define make-message-condition (record-constructor (record-type-rcd &message)))
(define message-condition? (condition-predicate (record-type-rtd &message)))
(define condition-message (condition-accessor (record-type-rtd &message) (record-accessor (record-type-rtd &message) 0)))


(define make-source-condition (record-constructor (record-type-rcd &source)))
(define source-condition? (condition-predicate (record-type-rtd &source)))
(define condition-source-file (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 0)))
(define condition-source-line (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 1)))
(define condition-source-column (condition-accessor (record-type-rtd &source) (record-accessor (record-type-rtd &source) 2)))

;; Convert a source vector ([file line col ...]) to an &source condition, or #f.
(define (sourcev->source-condition src)
  (if (and (vector? src) (>= (vector-length src) 3))
    (make-source-condition (vector-ref src 0) (vector-ref src 1) (vector-ref src 2))
    #f))

;; &diagnostic-info: what the diagnostics machinery adds to a condition (see (core diagnostics)).
;;   code         a symbol such as `unbound-variable`
;;   labels       list of (span style text)
;;   notes, helps lists of strings
;;   suggestions  list of (message span replacement applicability)
(define &diagnostic-info
  (let* ([rtd (make-record-type-descriptor '&diagnostic-info (record-type-rtd &condition) #f #f #f
                '#((immutable code) (immutable labels) (immutable notes) (immutable helps)
                   (immutable suggestions)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&diagnostic-info rtd rcd)))
(define make-diagnostic-info-condition (record-constructor (record-type-rcd &diagnostic-info)))
(define diagnostic-info-condition? (condition-predicate (record-type-rtd &diagnostic-info)))
(define (diagnostic-info-accessor i)
  (condition-accessor (record-type-rtd &diagnostic-info)
    (record-accessor (record-type-rtd &diagnostic-info) i)))
(define condition-diagnostic-code (diagnostic-info-accessor 0))
(define condition-diagnostic-labels (diagnostic-info-accessor 1))
(define condition-diagnostic-notes (diagnostic-info-accessor 2))
(define condition-diagnostic-helps (diagnostic-info-accessor 3))
(define condition-diagnostic-suggestions (diagnostic-info-accessor 4))

;; &compilation-errors: a compilation that found errors in `collect` mode raises one condition
;; made of the simple conditions of the first error and this one, which has the diagnostics of
;; all of them (see (core diagnostics)), in source order.
(define &compilation-errors
  (let* ([rtd (make-record-type-descriptor '&compilation-errors (record-type-rtd &condition) #f #f #f
                '#((immutable diagnostics)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&compilation-errors rtd rcd)))
(define make-compilation-errors-condition (record-constructor (record-type-rcd &compilation-errors)))
(define compilation-errors-condition? (condition-predicate (record-type-rtd &compilation-errors)))
(define condition-compilation-diagnostics
  (condition-accessor (record-type-rtd &compilation-errors)
    (record-accessor (record-type-rtd &compilation-errors) 0)))
