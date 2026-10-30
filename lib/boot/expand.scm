(|#%require| (boot prim) (boot control) (boot records) (boot conditions) (boot violations) (boot raise) (boot exceptions))
(|#%provide| all-defined)

(define &term
  (let* ([rtd (make-record-type-descriptor '&term #f #f #f #f '#((immutable sourcev)))]
         [rcd (make-record-constructor-descriptor rtd #f #f)])
    (make-record-type '&term rtd rcd)))

(define (term? obj)
  (and (record? obj) (rtd-ancestor? (record-type-rtd &term) (record-rtd obj))))

(define term-src (record-accessor (record-type-rtd &term) 0))

(define &lref
  (let* ([rtd (make-record-type-descriptor '&lref (record-type-rtd &term) #f #f #f '#((immutable variable) (immutable sym)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&lref rtd rcd)))

(define lref? (record-predicate (record-type-rtd &lref)))
(define lref-name (record-accessor (record-type-rtd &lref) 0))
(define lref-sym (record-accessor (record-type-rtd &lref) 1))
(define make-lref (record-constructor (record-type-rcd &lref)))

(define &lset
  (let* ([rtd (make-record-type-descriptor '&lset (record-type-rtd &term) #f #f #f '#((immutable variable) (immutable sym) (immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&lset rtd rcd)))

(define (lset? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &lset) (record-rtd obj))))
(define lset-name (record-accessor (record-type-rtd &lset) 0))
(define lset-value (record-accessor (record-type-rtd &lset) 2))
(define lset-sym (record-accessor (record-type-rtd &lset) 1))
(define make-lset (record-constructor (record-type-rcd &lset)))

(define &if
  (let* ([rtd (make-record-type-descriptor '&if (record-type-rtd &term) #f #f #f '#((immutable test) (immutable then) (immutable else)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&if rtd rcd)))

(define (if? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &if) (record-rtd obj))))
(define if-test (record-accessor (record-type-rtd &if) 0))
(define if-then (record-accessor (record-type-rtd &if) 1))
(define if-else (record-accessor (record-type-rtd &if) 2))
(define make-if (record-constructor (record-type-rcd &if)))

(define &let
  (let* ([rtd (make-record-type-descriptor '&let (record-type-rtd &term) #f #f #f '#((immutable style) (immutable ids) (immutable lhs) (immutable rhs) (immutable body)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&let rtd rcd)))

(define (let? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &let) (record-rtd obj))))
(define let-style (record-accessor (record-type-rtd &let) 0))
(define let-ids (record-accessor (record-type-rtd &let) 1))
(define let-lhs (record-accessor (record-type-rtd &let) 2))
(define let-rhs (record-accessor (record-type-rtd &let) 3))
(define let-body (record-accessor (record-type-rtd &let) 4))
(define make-let (record-constructor (record-type-rcd &let)))

(define &fix
  (let* ([rtd (make-record-type-descriptor '&fix (record-type-rtd &term) #f #f #f '#((immutable ids) (immutable lhs) (immutable rhs) (immutable body)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&fix rtd rcd)))

(define (fix? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &fix) (record-rtd obj))))

(define fix-ids (record-accessor (record-type-rtd &fix) 0))
(define fix-lhs (record-accessor (record-type-rtd &fix) 1))
(define fix-rhs (record-accessor (record-type-rtd &fix) 2))
(define fix-body (record-accessor (record-type-rtd &fix) 3))

(define make-fix (record-constructor (record-type-rcd &fix)))

(define &receive
  (let* ([rtd (make-record-type-descriptor '&receive (record-type-rtd &term) #f #f #f '#((immutable ids) (immutable vars) (immutable producer) (immutable consumer)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&receive rtd rcd)))

(define (receive? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &receive) (record-rtd obj))))
(define receive-ids (record-accessor (record-type-rtd &receive) 0))
(define receive-vars (record-accessor (record-type-rtd &receive) 1))
(define receive-producer (record-accessor (record-type-rtd &receive) 2))
(define receive-consumer (record-accessor (record-type-rtd &receive) 3))
(define make-receive (record-constructor (record-type-rcd &receive)))

(define &application
  (let* ([rtd (make-record-type-descriptor '&application (record-type-rtd &term) #f #f #f '#((immutable operator) (immutable operands)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&application rtd rcd)))

(define (application? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &application) (record-rtd obj))))
(define application-operator (record-accessor (record-type-rtd &application) 0))
(define application-operands (record-accessor (record-type-rtd &application) 1))
(define make-application (record-constructor (record-type-rcd &application)))

(define &primcall
  (let* ([rtd (make-record-type-descriptor '&primcall (record-type-rtd &term) #f #f #f '#((immutable prim) (immutable args)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&primcall rtd rcd)))

(define (primcall? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &primcall) (record-rtd obj))))
(define primcall-prim (record-accessor (record-type-rtd &primcall) 0))
(define primcall-args (record-accessor (record-type-rtd &primcall) 1))
(define make-primcall (record-constructor (record-type-rcd &primcall)))

(define &primref
  (let* ([rtd (make-record-type-descriptor '&primref (record-type-rtd &term) #f #f #f '#((immutable prim)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&primref rtd rcd)))

(define (primref? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &primref) (record-rtd obj))))
(define primref-prim (record-accessor (record-type-rtd &primref) 0))
(define make-primref (record-constructor (record-type-rcd &primref)))

(define &constant
  (let* ([rtd (make-record-type-descriptor '&constant (record-type-rtd &term) #f #f #f '#((immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&constant rtd rcd)))
(define (constant? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &constant) (record-rtd obj))))
(define constant-value (record-accessor (record-type-rtd &constant) 0))
(define make-constant (record-constructor (record-type-rcd &constant)))

(define &void
  (let* ([rtd (make-record-type-descriptor '&void (record-type-rtd &term) #f #f #f '#())]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&void rtd rcd)))
(define void? (record-predicate (record-type-rtd &void)))
(define make-void (record-constructor (record-type-rcd &void)))

(define &proc
  (let* ([rtd (make-record-type-descriptor '&proc (record-type-rtd &term) #f #f #f '#((immutable args) (immutable body) (immutable meta) (immutable ids)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&proc rtd rcd)))

(define proc? (record-predicate (record-type-rtd &proc)))
(define proc-args (record-accessor (record-type-rtd &proc) 0))
(define proc-body (record-accessor (record-type-rtd &proc) 1))
(define proc-meta (record-accessor (record-type-rtd &proc) 2))
(define proc-ids (record-accessor (record-type-rtd &proc) 3))
(define make-proc (record-constructor (record-type-rcd &proc)))

(define &values
  (let* ([rtd (make-record-type-descriptor '&values (record-type-rtd &term) #f #f #f '#((immutable values)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&values rtd rcd)))

(define (values? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &values) (record-rtd obj))))
(define values-values (record-accessor (record-type-rtd &values) 0))
(define make-values (record-constructor (record-type-rcd &values)))

(define &sequence
  (let* ([rtd (make-record-type-descriptor '&sequence (record-type-rtd &term) #f #f #f '#((immutable head) (immutable tail)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&sequence rtd rcd)))

(define (sequence? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &sequence) (record-rtd obj))))
(define sequence-head (record-accessor (record-type-rtd &sequence) 0))
(define sequence-tail (record-accessor (record-type-rtd &sequence) 1))
(define make-sequence (record-constructor (record-type-rcd &sequence)))

(define (let*? expr)
  (and (let? expr) (eq? (let-style expr) 'let*)))

(define (letrec? expr)
  (and (let? expr) (eq? (let-style expr) 'letrec)))

(define (letrec*? expr)
  (and (let? expr) (eq? (let-style expr) 'letrec*)))

(define &wcm
  (let* ([rtd (make-record-type-descriptor '&wcm (record-type-rtd &term) #f #f #f '#((immutable key) (immutable mark) (immutable result)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&wcm rtd rcd)))
(define (wcm? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &wcm) (record-rtd obj))))
(define wcm-key (record-accessor (record-type-rtd &wcm) 0))
(define wcm-mark (record-accessor (record-type-rtd &wcm) 1))
(define wcm-result (record-accessor (record-type-rtd &wcm) 2))
(define make-wcm (record-constructor (record-type-rcd &wcm)))

(define &global-ref
  (let* ([rtd (make-record-type-descriptor '&global-ref (record-type-rtd &term) #f #f #f '#((immutable key) (immutable name) (immutable slot)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&global-ref rtd rcd)))
(define global-ref? (record-predicate (record-type-rtd &global-ref)))
(define global-ref-key (record-accessor (record-type-rtd &global-ref) 0))
(define global-ref-name (record-accessor (record-type-rtd &global-ref) 1))
(define global-ref-slot (record-accessor (record-type-rtd &global-ref) 2))
(define make-global-ref (record-constructor (record-type-rcd &global-ref)))

(define &global-set
  (let* ([rtd (make-record-type-descriptor '&global-set (record-type-rtd &term) #f #f #f '#((immutable key) (immutable name) (immutable slot) (immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&global-set rtd rcd)))
(define global-set? (record-predicate (record-type-rtd &global-set)))
(define global-set-key (record-accessor (record-type-rtd &global-set) 0))
(define global-set-name (record-accessor (record-type-rtd &global-set) 1))
(define global-set-slot (record-accessor (record-type-rtd &global-set) 2))
(define global-set-value (record-accessor (record-type-rtd &global-set) 3))
(define make-global-set (record-constructor (record-type-rcd &global-set)))

(define &linklet
  (let* ([rtd (make-record-type-descriptor '&linklet (record-type-rtd &term) #f #f #f '#((immutable name) (immutable imports) (immutable exports) (immutable slots) (immutable body) (immutable meta)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&linklet rtd rcd)))
(define linklet-term? (record-predicate (record-type-rtd &linklet)))
(define linklet-term-name (record-accessor (record-type-rtd &linklet) 0))
(define linklet-term-imports (record-accessor (record-type-rtd &linklet) 1))
(define linklet-term-exports (record-accessor (record-type-rtd &linklet) 2))
(define linklet-term-slots (record-accessor (record-type-rtd &linklet) 3))
(define linklet-term-body (record-accessor (record-type-rtd &linklet) 4))
(define linklet-term-meta (record-accessor (record-type-rtd &linklet) 5))
(define make-linklet-term (record-constructor (record-type-rcd &linklet)))

;; Runtime hooks for Rust, see `install-runtime-hook!`.
(install-expander-hook! '&term (record-type-rtd &term))
(install-expander-hook! '&lref (record-type-rtd &lref))
(install-expander-hook! '&lset (record-type-rtd &lset))
(install-expander-hook! '&if (record-type-rtd &if))
(install-expander-hook! '&let (record-type-rtd &let))
(install-expander-hook! '&fix (record-type-rtd &fix))
(install-expander-hook! '&receive (record-type-rtd &receive))
(install-expander-hook! '&application (record-type-rtd &application))
(install-expander-hook! '&primcall (record-type-rtd &primcall))
(install-expander-hook! '&primref (record-type-rtd &primref))
(install-expander-hook! '&constant (record-type-rtd &constant))
(install-expander-hook! '&void (record-type-rtd &void))
(install-expander-hook! '&values (record-type-rtd &values))
(install-expander-hook! '&sequence (record-type-rtd &sequence))
(install-expander-hook! '&proc (record-type-rtd &proc))
(install-expander-hook! '&wcm (record-type-rtd &wcm))
(install-expander-hook! '&global-ref (record-type-rtd &global-ref))
(install-expander-hook! '&global-set (record-type-rtd &global-set))
(install-expander-hook! '&linklet (record-type-rtd &linklet))
