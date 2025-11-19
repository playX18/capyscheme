

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
        (format #f "expected condition of a subtype of ~a, but got ~a" rtd obj) rtd obj)))

  (or (rtd-ancestor? (record-type-rtd &condition) rtd)
      (assertion-violation
        'condition-accessor
        (format #f "expected record-type-descriptor of a subtype of &condition, but got ~a" rtd) rtd proc))
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


(define &marks
  (let ((rtd (make-record-type-descriptor '&marks (record-type-rtd &condition) (make-condition-uid) #f #f '#((immutable marks)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &condition) #f)))
      (make-record-type '&marks rtd rcd))))
(define &marks-marks (record-accessor (record-type-rtd &marks) 0))
(define make-marks-condition (record-constructor (record-type-rcd &marks)))
(define marks-condition? (condition-predicate (record-type-rtd &marks)))
(define condition-marks (condition-accessor (record-type-rtd &marks) &marks-marks))



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


(define &continuable-exception
    (let ((rtd (make-record-type-descriptor '&continuable-exception (record-type-rtd &implementation-restriction) (make-condition-uid) #f #f '#((immutable continuation) (immutable base)))))
    (let ((rcd (make-record-constructor-descriptor rtd (record-type-rcd &implementation-restriction) #f)))
      (make-record-type '&continuable-exception rtd rcd))))

(define make-continuable-exception (record-constructor (record-type-rcd &continuable-exception)))
(define continuable-exception? (condition-predicate (record-type-rtd &continuable-exception)))
(define &continuable-exception-continuation (record-accessor (record-type-rtd &continuable-exception) 0))
(define continuable-exception-continuation (condition-accessor (record-type-rtd &continuable-exception) &continuable-exception-continuation))
(define &continuable-exception-base (record-accessor (record-type-rtd &continuable-exception) 1))
(define continuable-exception-base (condition-accessor (record-type-rtd &continuable-exception) &continuable-exception-base))

(define (assertion-violation who message . irritants)
  (define stk (shadow-stack))

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
              (make-irritants-condition irritants)
              (make-marks-condition (current-continuation-marks))))))
      (raise message))
    (raise who)))

(define (unwrap-syntax expr)
  (let loop ([lst expr])
    (cond
      [(pair? lst)
        (let ([a (loop (car lst))] [d (loop (cdr lst))])
          (cond
            [(and (eq? a (car lst)) (eq? d (cdr lst))) lst]
            [else (cons a d)]))]
      [(identifier? lst) lst]
      [(syntax? lst) (loop (syntax-expression lst))]
      [(vector? lst) (list->vector (map loop (vector->list lst)))]
      [else lst])))


(define (syntax-violation who message form . subform)
  (define (get-who-from-form form)
    (define obj (if (syntax? form) (unwrap-syntax form) form))

    (cond
      [(identifier? obj) (make-who-condition (syntax-expression obj))]
      [(and (pair? obj) (identifier? (car obj))) (make-who-condition (syntax-expression (car obj)))]
      [else #f]))

  (if (or (not who) (string? who) (symbol? who) (identifier? who))
        (if (string? message)
            (raise
              (apply
                condition
                (filter
                  values
                  (list
                    (make-syntax-violation form (and (pair? subform) (car subform)))
                    (if who
                        (make-who-condition who)
                        (get-who-from-form form))
                    (make-message-condition message)))))
            (assertion-violation 'syntax-violation "expected string as message" message))
        (assertion-violation 'syntax-violation "expected string or symbol or #f as who" who)))

(define (error who message . irritants)
  (define marks (current-continuation-marks))
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
              (make-irritants-condition irritants)
              (make-marks-condition marks)))))
      #f)
    #f))
(define (implementation-restriction-violation who message . irritants)
  (define marks (current-continuation-marks))
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
              (make-irritants-condition irritants)
              (make-marks-condition marks)))))
      #f)
    #f))

(define undefined-violation
  (lambda (who . message)
    (define marks (current-continuation-marks))

    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-undefined-violation)
            (make-marks-condition marks)
            (and who (make-who-condition who))
            (and (pair? message) (make-message-condition (car message)))))))))

(define (.make-undefined-violation who . message)
  (define marks (current-continuation-marks))
  (if (or (not who) (string? who) (symbol? who))
    (apply
      condition
      (filter
        values
        (list
          (make-undefined-violation)
          (make-marks-condition marks)
          (and who (make-who-condition who))
          (and (pair? message) (make-message-condition (car message))))))
    #f))



(define raise-i/o-filename-error
  (lambda (who message filename . irritants)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-filename-error filename)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))
            (make-marks-condition marks)))))))

(define raise-i/o-error
  (lambda (who message . irritants)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (make-i/o-error)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? irritants) (make-irritants-condition irritants))
            (make-marks-condition marks)))))))

(define raise-misc-i/o-error-with-port
  (lambda (constructor who message port . options)
    (define marks (current-continuation-marks))
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
            (make-irritants-condition (cons* port options))
            (make-marks-condition marks)))))))

(define raise-misc-i/o-error
  (lambda (constructor who message . options)
    (define marks (current-continuation-marks))
    (raise
      (apply
        condition
        (filter
          values
          (list
            (apply constructor options)
            (and who (make-who-condition who))
            (make-message-condition message)
            (and (pair? options) (make-irritants-condition options))
            (make-marks-condition marks)))))))

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
    (define marks (current-continuation-marks))
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
              (and (pair? irritants) (make-irritants-condition irritants))
              (make-marks-condition marks))))
        #f)
      #f)))


(define (.make-assertion-violation who message . irritants)
  (define marks (current-continuation-marks))
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
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
      #f)
    #f))


(define (.make-error who message . irritants)
  (define marks (current-continuation-marks))
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
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
      #f)
    #f))

(define (.make-implementation-restriction-violation who message . irritants)
  (define marks (current-continuation-marks))
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
            (make-irritants-condition irritants)
            (make-marks-condition marks))))
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

(define (print-condition exn p)
    (define (print-syntax form subform)
        (define form-src (if (syntax? form) (syntax-sourcev form) #f))
        (define subform-src (if (and subform (syntax? subform)) (syntax-sourcev subform) #f))
        (define (fmt-source src)
        (define file (vector-ref src 0))
        (define line (vector-ref src 1))
        (define col  (vector-ref src 2))
        (format p "~a:~a:~a" file line col))
        (format p "~a" (syntax->datum form))
        (when form-src 
        (format p "~%       in ")
        (fmt-source form-src))
        (when subform
        (format p "~a" (syntax->datum subform))
        (when subform-src 
            (format p "~%     in")
            (fmt-source subform-src))))

    (cond 
        [(condition? exn)
            (let ([c* (simple-conditions exn)])
                (format p "The condition has ~a components:~%" (length c*))
                (do ([i 1 (+ 1 i)]
                    [c* c* (cdr c*)])
                    [(null? c*)]
                    (let* ([c (car c*)]
                        [rtd (record-rtd c)])
                        (format p " ~a. " i)
                        (let ([supress-type
                                (and (eq? (record-type-parent rtd)
                                    (record-type-rtd &condition))
                                (let ((name (symbol->string (record-type-name rtd)))
                                        (fields (record-type-field-names rtd)))
                                    (and (not (eqv? 0 (string-length name)))
                                        (char=? (string-ref name 0) #\&)
                                        (fx>? (vector-length fields) 0)
                                        (string=? (substring name 1 (string-length name))
                                                (symbol->string (vector-ref fields 0))))))])
                            (if supress-type 
                                (put-char p #\&)
                                (let loop ([rtd rtd])
                                    (format p "~a" (record-type-name rtd))
                                    (cond 
                                        [(record-type-parent rtd) => 
                                            (lambda (parent)
                                                (unless (eq? parent (record-type-rtd &condition))
                                                (format p " ")
                                                (loop parent)))])))
                            (let loop ([rtd rtd])
                                (do ([f* (record-type-field-names rtd)]
                                    [i 0 (+ i 1)])
                                    [(= i (vector-length f*))
                                        (cond [(record-type-parent rtd) => loop])]
                                    (unless (and supress-type (eqv? i 0))
                                        (format p "~%     "))
                                    (format p "~a: " (vector-ref f* i))
                                    (let ([x ((record-accessor rtd i) c)])
                                        (cond 
                                            [(and (eq? rtd (record-type-rtd &syntax)))
                                                (print-syntax (syntax-violation-form c) (syntax-violation-subform c))]
                                            [(and (eq? rtd (record-type-rtd &irritants))
                                                (pair? x)
                                                (list? x))
                                                (display "(" p)
                                                (write (car x) p)
                                                (for-each 
                                                    (lambda (x)
                                                    (display "\n                 " p)
                                                    (write x p))
                                                    (cdr x))
                                                (display ")" p)]
                                            [else (write x p)]))))))
                            (newline p)))]
                    [else 
                        (format p "A non-condition object was raised:~%~s" exn)]))