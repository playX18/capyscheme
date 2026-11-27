(library (core records)

  (export make-record-type-descriptor
    record-type-descriptor?
    make-record-constructor-descriptor
    record-constructor
    record-predicate
    record-accessor
    record-mutator
    record?
    record-rtd
    record-type-name
    record-type-parent
    record-type-uid
    record-type-generative?
    record-type-sealed?
    record-type-opaque?
    record-type-field-names
    record-field-mutable?
    record-type-descriptor
    record-constructor-descriptor
    define-record-type
    fields
    mutable
    immutable
    parent
    protocol
    sealed
    opaque
    nongenerative
    parent-rtd
    %record-field-offset)

  (import (core primitives)
    (core lists)
    (core syntax-case)
    (only (core arithmetic) fx>?)
    (only (capy) syntax-sourcev tuple tuple? tuple-ref tuple-set! tuple-size
      rtd-ancestor? rtd-field-count rtd-fields %record-ref-slow %record-set-slow!))

  ;; The procedures `define-record-type` expands to. The fast path of an accessor, a mutator or
  ;; a predicate is inline code: the object is a tuple whose slot 0 is the type itself. The
  ;; slow path is the native `%record-ref-slow` / `%record-set-slow!` (record_ops.rs), which
  ;; keep the behaviour and error conditions of the native `record-accessor` and `record-mutator`.
  ;;
  ;; The type and the field indices live in variables named "%rec rtd NAME" and
  ;; "%rec off NAME K": a library exports them from its linklet although its export spec does
  ;; not list them (`inline-exposed-name?` in boot/units.scm), so that the inline body of an
  ;; accessor can refer to them in the libraries that import the accessor.

  ;; The flat tuple index of the K-th own field of RTD.
  (define (%record-field-offset rtd k)
    (+ k 1 (- (rtd-field-count rtd) (length (rtd-fields rtd)))))

  (define-syntax define-record-type
    (lambda (x)
      (let ((stash (make-core-hashtable)))

        (define stash-set!
          (lambda (key value)
            (and (core-hash-ref stash key #f)
              (syntax-violation 'define-record-type (format #f "duplicate ~a clause" key) x))
            (core-hash-set! stash key (list value))))

        (define stash-ref
          (lambda (key default)
            (cond ((core-hash-ref stash key #f) => car)
              (else default))))

        ;; The uid of a record type that asks for a nongenerative type without naming it. It is
        ;; made of where the type is defined, not drawn at random, so that compiling a library
        ;; twice gives the same bytes (./bootstrap.sh compares two builds).
        (define default-uid
          (lambda (name)
            (let ((sv (syntax-sourcev name)))
              (if (and (vector? sv) (string? (vector-ref sv 0)))
                (format #f "~a:~a:~a:~a" (vector-ref sv 0) (vector-ref sv 1) (vector-ref sv 2)
                  (syntax->datum name))
                (make-uuid)))))

        (define parse-record-clauses
          (lambda (first-name record-clauses name-stx)

            (define method-name
              (lambda (spec second-name)
                (datum->syntax second-name (string->symbol (format #f spec first-name (syntax->datum second-name))))))

            (for-each
              (lambda (c)
                (syntax-case c (parent protocol parent-rtd sealed opaque nongenerative fields)
                  ((parent e1)
                    (identifier? #'e1)
                    (stash-set! 'parent #'e1))
                  ((protocol e1)
                    (stash-set! 'protocol #'e1))
                  ((parent-rtd e1 e2)
                    (stash-set! 'parent-rtd (cons #'e1 #'e2)))
                  ((sealed e1)
                    (boolean? (syntax->datum #'e1))
                    (stash-set! 'sealed #'e1))
                  ((opaque e1)
                    (boolean? (syntax->datum #'e1))
                    (stash-set! 'opaque #'e1))
                  ((nongenerative e1)
                    (identifier? #'e1)
                    (stash-set! 'nongenerative #'e1))
                  ((nongenerative)
                    (stash-set! 'nongenerative (datum->syntax #'k (string->symbol (format #f "<~a>" (default-uid name-stx))))))
                  ((fields specs ...)
                    (stash-set!
                      'fields
                      (map (lambda (spec)
                            (syntax-case spec (immutable mutable)
                              ((immutable name accessor)
                                (and (identifier? #'name) (identifier? #'accessor))
                                #'((immutable name) accessor #f))
                              ((mutable name accessor mutator)
                                (and (identifier? #'name) (identifier? #'accessor) (identifier? #'mutator))
                                #'((mutable name) accessor mutator))
                              ((immutable name)
                                (identifier? #'name)
                                (with-syntax ((proc (method-name "~a-~a" #'name)))
                                  #'((immutable name) proc #f)))
                              ((mutable name)
                                (identifier? #'name)
                                (with-syntax
                                  ((proc1 (method-name "~a-~a" #'name))
                                    (proc2 (method-name "~a-~a-set!" #'name)))
                                  #'((mutable name) proc1 proc2)))
                              (name
                                (identifier? #'name)
                                (with-syntax ((proc (method-name "~a-~a" #'name)))
                                  #'((immutable name) proc #f)))
                              (_
                                (syntax-violation 'define-record-type "malformed field spec" x spec))))
                        #'(specs ...))))

                  (_ (syntax-violation 'define-record-type "malformed record clauses" x (syntax->datum c)))))
              record-clauses)))

        (syntax-case x ()
          ((_ (record-name constructor-name predicate-name) record-clauses ...)
            (and (identifier? #'record-name) (identifier? #'constructor-name) (identifier? #'predicate-name))
            (begin
              (parse-record-clauses (syntax->datum #'record-name) #'(record-clauses ...) #'record-name)
              (and (stash-ref 'parent-rtd #f)
                (stash-ref 'parent #f)
                (syntax-violation 'define-record-type "definition have both parent and parent-rtd clause" x))
              (with-syntax
                ((record-type
                    (with-syntax ((parent (stash-ref 'parent #f)))
                      (with-syntax ((rtd-parent (cond ((stash-ref 'parent-rtd #f) => car)
                                                 ((stash-ref 'parent #f) #'(record-type-rtd parent))
                                                 (else #f)))
                                    (rcd-parent (cond ((stash-ref 'parent-rtd #f) => cdr)
                                                 ((stash-ref 'parent #f) #'(record-type-rcd parent))
                                                 (else #f)))
                                    (uid (stash-ref 'nongenerative #f))
                                    (sealed (stash-ref 'sealed #f))
                                    (opaque (stash-ref 'opaque #f))
                                    (protocol (stash-ref 'protocol #f))
                                    (((fields _ _) ...) (datum->syntax #'k (stash-ref 'fields '()))))
                        #'(define record-name
                           (let* ((rtd (make-record-type-descriptor 'record-name rtd-parent 'uid sealed opaque '#(fields ...)))
                                  (rcd (make-record-constructor-descriptor rtd rcd-parent protocol)))
                            (make-record-type 'record-name rtd rcd))))))
                  )
                (let* ((has-parent (and (or (stash-ref 'parent-rtd #f) (stash-ref 'parent #f)) #t))
                       (has-protocol (and (stash-ref 'protocol #f) #t))
                       (sealed? (let ((s (stash-ref 'sealed #f))) (and s (syntax->datum s) #t)))
                       (fields (stash-ref 'fields '()))
                       (base-name (symbol->string (syntax->datum #'record-name)))
                       (rtd-id (datum->syntax #'record-name (string->symbol (string-append "%rec rtd " base-name))))
                       ;; With a parent the number of inherited fields is known only at run time,
                       ;; so the tuple index of a field is a variable; otherwise it is a literal.
                       (offset-ids
                         (let loop ((fs fields) (n 0))
                           (if (null? fs) '()
                             (cons (datum->syntax #'record-name
                                     (string->symbol (string-append "%rec off " base-name " " (number->string n))))
                               (loop (cdr fs) (+ n 1)))))))
                  (define (offset-def id n)
                    (with-syntax ((id id) (n n) (rtd-id rtd-id))
                      #'(define id (%record-field-offset rtd-id n))))
                  (define (offset-of id n)
                    (if has-parent id (datum->syntax #'k (+ n 1))))
                  (define (accessor-def spec id n)
                    (with-syntax (((_ name _) spec) (n n) (off (offset-of id n)) (rtd-id rtd-id))
                      (and (identifier? #'name)
                        #'(define (name obj)
                            (if (and (tuple? obj) (fx>? (tuple-size obj) off)
                                  (eq? (tuple-ref obj 0) rtd-id))
                              (tuple-ref obj off)
                              (%record-ref-slow obj rtd-id off))))))
                  (define (mutator-def spec id n)
                    (with-syntax (((_ _ name) spec) (n n) (off (offset-of id n)) (rtd-id rtd-id))
                      (and (identifier? #'name)
                        #'(define (name obj value)
                            (if (and (tuple? obj) (fx>? (tuple-size obj) off)
                                  (eq? (tuple-ref obj 0) rtd-id))
                              (tuple-set! obj off value)
                              (%record-set-slow! obj value rtd-id off))))))
                  (with-syntax ((rtd-id rtd-id))
                  (with-syntax
                    ((rtd-def #'(define rtd-id (record-type-rtd record-name)))
                      ((offset-def ...)
                        (if has-parent
                          (let loop ((ids offset-ids) (n 0))
                            (if (null? ids) '() (cons (offset-def (car ids) n) (loop (cdr ids) (+ n 1)))))
                          '()))
                      (constructor
                        (if (or has-parent has-protocol)
                          #'(define constructor-name (record-constructor (record-type-rcd record-name)))
                          ;; No parent and no protocol: a plain procedure that allocates, which the
                          ;; compiler can inline. A wrong argument count is an ordinary arity error.
                          (with-syntax (((arg ...) (generate-temporaries (map (lambda (f) 'a) fields))))
                            #'(define (constructor-name arg ...) (tuple rtd-id arg ...)))))
                      (predicate
                        (if sealed?
                          #'(define (predicate-name obj)
                              (and (tuple? obj) (fx>? (tuple-size obj) 0) (eq? (tuple-ref obj 0) rtd-id)))
                          #'(define (predicate-name obj)
                              (and (tuple? obj) (fx>? (tuple-size obj) 0)
                                (let ((t (tuple-ref obj 0)))
                                  (or (eq? t rtd-id) (rtd-ancestor? rtd-id t)))))))
                      ((accessors ...)
                        (let loop ((specs fields) (ids offset-ids) (n 0))
                          (if (null? specs) '()
                            (let ((d (accessor-def (car specs) (car ids) n)))
                              (if d (cons d (loop (cdr specs) (cdr ids) (+ n 1)))
                                (loop (cdr specs) (cdr ids) (+ n 1)))))))
                      ((mutators ...)
                        (let loop ((specs fields) (ids offset-ids) (n 0))
                          (if (null? specs) '()
                            (let ((d (mutator-def (car specs) (car ids) n)))
                              (if d (cons d (loop (cdr specs) (cdr ids) (+ n 1)))
                                (loop (cdr specs) (cdr ids) (+ n 1))))))))
                    #'(begin record-type rtd-def offset-def ... constructor predicate accessors ... mutators ...)))))))
          ((_ record-name record-clauses ...)
            (identifier? #'record-name)
            (let ((base-name (symbol->string (syntax->datum #'record-name))))
              (with-syntax
                ((constructor-name (datum->syntax #'record-name (string->symbol (string-append "make-" base-name))))
                  (predicate-name (datum->syntax #'record-name (string->symbol (string-append base-name "?")))))
                #'(define-record-type (record-name constructor-name predicate-name) record-clauses ...))))))))

  (define-syntax record-type-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) #'(record-type-rtd name)))))

  (define-syntax record-constructor-descriptor
    (lambda (x)
      (syntax-case x ()
        ((_ name) (identifier? #'name) #'(record-type-rcd name)))))

  (define-syntax fields
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax mutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax immutable
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax protocol
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax sealed
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax opaque
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax nongenerative
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))

  (define-syntax parent-rtd
    (lambda (x)
      (syntax-violation #f "misplaced auxiliary syntactic keyword" x)))) ;[end]
