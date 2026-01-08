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

(define &module-ref
  (let* ([rtd (make-record-type-descriptor '&module-ref (record-type-rtd &term) #f #f #f '#((immutable module) (immutable name) (immutable public?)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&module-ref rtd rcd)))

(define (module-ref? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &module-ref) (record-rtd obj))))
(define module-ref-module (record-accessor (record-type-rtd &module-ref) 0))
(define module-ref-name (record-accessor (record-type-rtd &module-ref) 1))
(define module-ref-public? (record-accessor (record-type-rtd &module-ref) 2))
(define make-module-ref (record-constructor (record-type-rcd &module-ref)))

(define &module-set
  (let* ([rtd (make-record-type-descriptor '&module-set (record-type-rtd &term) #f #f #f '#((immutable module) (immutable name) (immutable public?) (immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&module-set rtd rcd)))

(define (module-set? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &module-set) (record-rtd obj))))
(define module-set-module (record-accessor (record-type-rtd &module-set) 0))
(define module-set-name (record-accessor (record-type-rtd &module-set) 1))
(define module-set-public? (record-accessor (record-type-rtd &module-set) 2))
(define module-set-value (record-accessor (record-type-rtd &module-set) 3))
(define make-module-set (record-constructor (record-type-rcd &module-set)))

(define &toplevel-ref
  (let* ([rtd (make-record-type-descriptor '&toplevel-ref (record-type-rtd &term) #f #f #f '#((immutable mod) (immutable name)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&toplevel-ref rtd rcd)))
(define toplevel-ref? (record-predicate (record-type-rtd &toplevel-ref)))
(define toplevel-ref-mod (record-accessor (record-type-rtd &toplevel-ref) 0))
(define toplevel-ref-name (record-accessor (record-type-rtd &toplevel-ref) 1))
(define make-toplevel-ref (record-constructor (record-type-rcd &toplevel-ref)))

(define &toplevel-set
  (let* ([rtd (make-record-type-descriptor '&toplevel-set (record-type-rtd &term) #f #f #f '#((immutable mod) (immutable name) (immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&toplevel-set rtd rcd)))

(define (toplevel-set? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &toplevel-set) (record-rtd obj))))
(define toplevel-set-mod (record-accessor (record-type-rtd &toplevel-set) 0))
(define toplevel-set-name (record-accessor (record-type-rtd &toplevel-set) 1))
(define toplevel-set-value (record-accessor (record-type-rtd &toplevel-set) 2))
(define make-toplevel-set (record-constructor (record-type-rcd &toplevel-set)))

(define &toplevel-define
  (let* ([rtd (make-record-type-descriptor '&toplevel-define (record-type-rtd &term) #f #f #f '#((immutable mod) (immutable name) (immutable value)))]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&toplevel-define rtd rcd)))
(define (toplevel-define? obj)
  (and (term? obj) (rtd-ancestor? (record-type-rtd &toplevel-define) (record-rtd obj))))
(define toplevel-define-mod (record-accessor (record-type-rtd &toplevel-define) 0))
(define toplevel-define-name (record-accessor (record-type-rtd &toplevel-define) 1))
(define toplevel-define-value (record-accessor (record-type-rtd &toplevel-define) 2))
(define make-toplevel-define (record-constructor (record-type-rcd &toplevel-define)))

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

(define &void
  (let* ([rtd (make-record-type-descriptor '&void (record-type-rtd &term) #f #f #f '#())]
         [rcd (make-record-constructor-descriptor rtd (record-type-rcd &term) #f)])
    (make-record-type '&void rtd rcd)))

(define void? (record-predicate (record-type-rtd &void)))
(define make-void (record-constructor (record-type-rcd &void)))

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
