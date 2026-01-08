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
  (let ((rtd (make-record-type-descriptor '&i/o-invalid-position (record-type-rtd &i/o) (make-condition-uid) #f #f '#((immutable position)))))
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
