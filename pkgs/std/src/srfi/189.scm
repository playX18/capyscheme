;;; SRFI 189: Maybe and Either: optional container types.
;;;
;;; A Just, Left or Right holds a list of payload values. A payload of one value is the common
;;; case; it is still kept as a one-element list so that every procedure has one shape.
;;; Nothing is a single object.
;;;
;;; Notes on the specification:
;;;
;;; - `maybe-unfold`/`either-unfold` check that one step of SUCCESSOR reaches STOP?, as the
;;;   specification describes.
;;; - The trivalent procedures follow Kleene's logic: `tri-and` is Just #f as soon as one
;;;   argument is false, even if another is Nothing; `tri=?` is Nothing when any argument is.
(library (srfi 189)
  (export
    ;; constructors
    just nothing left right list->just list->left list->right
    maybe->either either->maybe either-swap
    ;; predicates
    just? nothing? left? right? maybe? either? maybe= either=
    ;; accessors
    maybe-ref maybe-ref/default either-ref either-ref/default
    ;; join and bind
    maybe-join either-join maybe-compose either-compose maybe-bind either-bind
    ;; sequence operations
    maybe-length either-length maybe-filter maybe-remove either-filter either-remove
    maybe-sequence either-sequence
    ;; protocol conversion
    maybe->list either->list list->maybe list->either
    maybe->truth either->truth truth->maybe truth->either
    maybe->list-truth either->list-truth list-truth->maybe list-truth->either
    maybe->generation either->generation generation->maybe generation->either
    maybe->values either->values values->maybe values->either
    maybe->two-values two-values->maybe exception->either
    ;; map, fold and unfold
    maybe-map either-map maybe-for-each either-for-each maybe-fold either-fold
    maybe-unfold either-unfold
    ;; syntax
    maybe-if maybe-and maybe-or maybe-let* maybe-let*-values
    either-and either-or either-let* either-let*-values either-guard
    ;; trivalent logic
    tri-not tri=? tri-and tri-or tri-merge)
  (import (rnrs))

  (define-record-type (<just> raw-just just?)
    (fields (immutable objs just-objs))
    (sealed #t))

  (define-record-type (<nothing> make-nothing nothing?)
    (sealed #t))

  (define-record-type (<left> raw-left left?)
    (fields (immutable objs left-objs))
    (sealed #t))

  (define-record-type (<right> raw-right right?)
    (fields (immutable objs right-objs))
    (sealed #t))

  (define the-nothing (make-nothing))

  (define (nothing) the-nothing)
  (define (just . objs) (raw-just objs))
  (define (left . objs) (raw-left objs))
  (define (right . objs) (raw-right objs))
  (define (list->just lst) (raw-just lst))
  (define (list->left lst) (raw-left lst))
  (define (list->right lst) (raw-right lst))

  (define (maybe? obj) (or (just? obj) (nothing? obj)))
  (define (either? obj) (or (left? obj) (right? obj)))

  (define (check-maybe who obj)
    (unless (maybe? obj) (assertion-violation who "not a Maybe" obj)))

  (define (check-either who obj)
    (unless (either? obj) (assertion-violation who "not an Either" obj)))

  ;; The payload of a Left or a Right.
  (define (either-objs e)
    (if (right? e) (right-objs e) (left-objs e)))

  (define (maybe->either maybe . objs)
    (check-maybe 'maybe->either maybe)
    (if (just? maybe) (raw-right (just-objs maybe)) (raw-left objs)))

  (define (either->maybe either)
    (check-either 'either->maybe either)
    (if (right? either) (raw-just (right-objs either)) the-nothing))

  (define (either-swap either)
    (check-either 'either-swap either)
    (if (right? either) (raw-left (right-objs either)) (raw-right (left-objs either))))

  (define (payloads=? equal a b)
    (let loop ([a a] [b b])
      (cond
        [(null? a) (null? b)]
        [(null? b) #f]
        [else (and (equal (car a) (car b)) (loop (cdr a) (cdr b)))])))

  (define (maybe= equal maybe . maybes)
    (let loop ([m maybe] [ms maybes])
      (check-maybe 'maybe= m)
      (or (null? ms)
          (let ([n (car ms)])
            (check-maybe 'maybe= n)
            (and (cond
                   [(nothing? m) (nothing? n)]
                   [(nothing? n) #f]
                   [else (payloads=? equal (just-objs m) (just-objs n))])
                 (loop n (cdr ms)))))))

  (define (either= equal either . eithers)
    (let loop ([e either] [es eithers])
      (check-either 'either= e)
      (or (null? es)
          (let ([f (car es)])
            (check-either 'either= f)
            (and (cond
                   [(right? e) (and (right? f) (payloads=? equal (right-objs e) (right-objs f)))]
                   [else (and (left? f) (payloads=? equal (left-objs e) (left-objs f)))])
                 (loop f (cdr es)))))))

  ;; Accessors

  (define maybe-ref
    (case-lambda
      [(maybe failure) (maybe-ref maybe failure values)]
      [(maybe failure success)
       (check-maybe 'maybe-ref maybe)
       (if (just? maybe) (apply success (just-objs maybe)) (failure))]))

  (define (maybe-ref/default maybe . defaults)
    (check-maybe 'maybe-ref/default maybe)
    (if (just? maybe) (apply values (just-objs maybe)) (apply values defaults)))

  (define either-ref
    (case-lambda
      [(either failure) (either-ref either failure values)]
      [(either failure success)
       (check-either 'either-ref either)
       (if (right? either)
         (apply success (right-objs either))
         (apply failure (left-objs either)))]))

  (define (either-ref/default either . defaults)
    (check-either 'either-ref/default either)
    (if (right? either) (apply values (right-objs either)) (apply values defaults)))

  ;; Join and bind

  (define (maybe-join maybe)
    (check-maybe 'maybe-join maybe)
    (if (just? maybe)
      (let ([objs (just-objs maybe)])
        (unless (and (pair? objs) (null? (cdr objs)) (maybe? (car objs)))
          (assertion-violation 'maybe-join "the payload is not one Maybe" maybe))
        (car objs))
      maybe))

  (define (either-join either)
    (check-either 'either-join either)
    (if (right? either)
      (let ([objs (right-objs either)])
        (unless (and (pair? objs) (null? (cdr objs)) (either? (car objs)))
          (assertion-violation 'either-join "the payload is not one Either" either))
        (car objs))
      either))

  (define (maybe-bind maybe . mprocs)
    (let loop ([m maybe] [ps mprocs])
      (check-maybe 'maybe-bind m)
      (if (or (null? ps) (nothing? m))
        m
        (loop (apply (car ps) (just-objs m)) (cdr ps)))))

  (define (either-bind either . mprocs)
    (let loop ([e either] [ps mprocs])
      (check-either 'either-bind e)
      (if (or (null? ps) (left? e))
        e
        (loop (apply (car ps) (right-objs e)) (cdr ps)))))

  (define (maybe-compose . mprocs)
    (lambda args
      (if (null? mprocs)
        (raw-just args)
        (apply maybe-bind (apply (car mprocs) args) (cdr mprocs)))))

  (define (either-compose . mprocs)
    (lambda args
      (if (null? mprocs)
        (raw-right args)
        (apply either-bind (apply (car mprocs) args) (cdr mprocs)))))

  ;; Sequence operations

  (define (maybe-length maybe)
    (check-maybe 'maybe-length maybe)
    (if (just? maybe) 1 0))

  (define (either-length either)
    (check-either 'either-length either)
    (if (right? either) 1 0))

  (define (maybe-filter pred maybe)
    (check-maybe 'maybe-filter maybe)
    (if (and (just? maybe) (apply pred (just-objs maybe))) maybe the-nothing))

  (define (maybe-remove pred maybe)
    (check-maybe 'maybe-remove maybe)
    (if (and (just? maybe) (not (apply pred (just-objs maybe)))) maybe the-nothing))

  (define (either-filter pred either . objs)
    (check-either 'either-filter either)
    (if (and (right? either) (apply pred (right-objs either))) either (raw-left objs)))

  (define (either-remove pred either . objs)
    (check-either 'either-remove either)
    (if (and (right? either) (not (apply pred (right-objs either)))) either (raw-left objs)))

  ;; CONTAINER is mapped with CMAP (default `map`) to a container of the payloads; the first
  ;; Nothing/Left is returned instead when there is one.
  (define (sequence who container cmap aggregator good? good-objs wrap)
    (call/cc
      (lambda (return)
        (wrap
          (list
            (cmap (lambda (x)
                    (if (good? x)
                      (apply aggregator (good-objs x))
                      (return x)))
                  container))))))

  (define maybe-sequence
    (case-lambda
      [(container cmap) (maybe-sequence container cmap list)]
      [(container cmap aggregator)
       (sequence 'maybe-sequence container cmap aggregator just? just-objs raw-just)]))

  (define either-sequence
    (case-lambda
      [(container cmap) (either-sequence container cmap list)]
      [(container cmap aggregator)
       (sequence 'either-sequence container cmap aggregator right? right-objs raw-right)]))

  ;; Protocol conversion

  (define (maybe->list maybe)
    (check-maybe 'maybe->list maybe)
    (if (just? maybe) (just-objs maybe) '()))

  (define (either->list either)
    (check-either 'either->list either)
    (either-objs either))

  (define (list->maybe lst)
    (if (null? lst) the-nothing (raw-just lst)))

  (define (list->either lst . objs)
    (if (null? lst) (raw-left objs) (raw-right lst)))

  (define (single who objs)
    (unless (and (pair? objs) (null? (cdr objs)))
      (assertion-violation who "the payload is not one value" objs))
    (car objs))

  (define (maybe->truth maybe)
    (check-maybe 'maybe->truth maybe)
    (and (just? maybe) (single 'maybe->truth (just-objs maybe))))

  (define (either->truth either)
    (check-either 'either->truth either)
    (and (right? either) (single 'either->truth (right-objs either))))

  (define (truth->maybe obj)
    (if obj (raw-just (list obj)) the-nothing))

  (define (truth->either obj . fail-objs)
    (if obj (raw-right (list obj)) (raw-left fail-objs)))

  (define (maybe->list-truth maybe)
    (check-maybe 'maybe->list-truth maybe)
    (and (just? maybe) (just-objs maybe)))

  (define (either->list-truth either)
    (check-either 'either->list-truth either)
    (and (right? either) (right-objs either)))

  (define (list-truth->maybe lst)
    (if lst (raw-just lst) the-nothing))

  (define (list-truth->either lst . fail-objs)
    (if lst (raw-right lst) (raw-left fail-objs)))

  (define (maybe->generation maybe)
    (check-maybe 'maybe->generation maybe)
    (if (just? maybe) (single 'maybe->generation (just-objs maybe)) (eof-object)))

  (define (either->generation either)
    (check-either 'either->generation either)
    (if (right? either) (single 'either->generation (right-objs either)) (eof-object)))

  (define (generation->maybe obj)
    (if (eof-object? obj) the-nothing (raw-just (list obj))))

  (define (generation->either obj . fail-objs)
    (if (eof-object? obj) (raw-left fail-objs) (raw-right (list obj))))

  (define (maybe->values maybe)
    (check-maybe 'maybe->values maybe)
    (if (just? maybe) (apply values (just-objs maybe)) (values)))

  (define (either->values either)
    (check-either 'either->values either)
    (if (right? either) (apply values (right-objs either)) (values)))

  (define (values->maybe producer)
    (call-with-values producer
      (lambda objs (if (null? objs) the-nothing (raw-just objs)))))

  (define (values->either producer . fail-objs)
    (call-with-values producer
      (lambda objs (if (null? objs) (raw-left fail-objs) (raw-right objs)))))

  (define (maybe->two-values maybe)
    (check-maybe 'maybe->two-values maybe)
    (if (just? maybe)
      (values (single 'maybe->two-values (just-objs maybe)) #t)
      (values #f #f)))

  (define (two-values->maybe producer)
    (call-with-values producer
      (lambda (value ok?) (if ok? (raw-just (list value)) the-nothing))))

  (define (exception->either pred thunk)
    (guard (obj [(pred obj) (raw-left (list obj))])
      (call-with-values thunk (lambda objs (raw-right objs)))))

  ;; Map, fold and unfold

  (define (maybe-map proc maybe)
    (check-maybe 'maybe-map maybe)
    (if (just? maybe)
      (call-with-values (lambda () (apply proc (just-objs maybe))) just)
      maybe))

  (define (either-map proc either)
    (check-either 'either-map either)
    (if (right? either)
      (call-with-values (lambda () (apply proc (right-objs either))) right)
      either))

  (define (maybe-for-each proc maybe)
    (check-maybe 'maybe-for-each maybe)
    (when (just? maybe) (apply proc (just-objs maybe))))

  (define (either-for-each proc either)
    (check-either 'either-for-each either)
    (when (right? either) (apply proc (right-objs either))))

  (define (maybe-fold kons nil maybe)
    (check-maybe 'maybe-fold maybe)
    (if (just? maybe) (apply kons (append (just-objs maybe) (list nil))) nil))

  (define (either-fold kons nil either)
    (check-either 'either-fold either)
    (if (right? either) (apply kons (append (right-objs either) (list nil))) nil))

  (define (unfold who stop? mapper successor seeds wrap-stop wrap)
    (if (apply stop? seeds)
      (wrap-stop seeds)
      (call-with-values (lambda () (apply successor seeds))
        (lambda next
          (unless (apply stop? next)
            (assertion-violation who "the successor of the seeds does not stop" seeds))
          (call-with-values (lambda () (apply mapper seeds))
            (lambda objs (wrap objs)))))))

  (define (maybe-unfold stop? mapper successor . seeds)
    (unfold 'maybe-unfold stop? mapper successor seeds (lambda (s) the-nothing) raw-just))

  (define (either-unfold stop? mapper successor . seeds)
    (unfold 'either-unfold stop? mapper successor seeds raw-left raw-right))

  ;; Syntax

  (define-syntax maybe-if
    (syntax-rules ()
      [(_ maybe-expr just-expr nothing-expr)
       (let ([m maybe-expr])
         (check-maybe 'maybe-if m)
         (if (just? m) just-expr nothing-expr))]))

  (define-syntax maybe-and
    (syntax-rules ()
      [(_) (just)]
      [(_ e) (let ([m e]) (check-maybe 'maybe-and m) m)]
      [(_ e rest ...)
       (let ([m e])
         (check-maybe 'maybe-and m)
         (if (just? m) (maybe-and rest ...) m))]))

  (define-syntax maybe-or
    (syntax-rules ()
      [(_) the-nothing]
      [(_ e) (let ([m e]) (check-maybe 'maybe-or m) m)]
      [(_ e rest ...)
       (let ([m e])
         (check-maybe 'maybe-or m)
         (if (just? m) m (maybe-or rest ...)))]))

  (define-syntax either-and
    (syntax-rules ()
      [(_) (right)]
      [(_ e) (let ([x e]) (check-either 'either-and x) x)]
      [(_ e rest ...)
       (let ([x e])
         (check-either 'either-and x)
         (if (right? x) (either-and rest ...) x))]))

  (define-syntax either-or
    (syntax-rules ()
      [(_) (left)]
      [(_ e) (let ([x e]) (check-either 'either-or x) x)]
      [(_ e rest ...)
       (let ([x e])
         (check-either 'either-or x)
         (if (right? x) x (either-or rest ...)))]))

  ;; A claw is (ID EXPR), (EXPR) or ID. EXPR must give a Maybe (an Either); a Nothing (a
  ;; Left) ends the form. ID is bound to the payload, which must be one value. Without a body,
  ;; the form gives the last claw's value.
  (define-syntax maybe-let*
    (syntax-rules ()
      [(_ ()) (just)]
      [(_ () body1 body ...) (let () body1 body ...)]
      [(_ ((id expr)) ) (let ([m expr]) (check-maybe 'maybe-let* m) m)]
      [(_ ((expr)) ) (let ([m expr]) (check-maybe 'maybe-let* m) m)]
      [(_ (id)) (let ([m id]) (check-maybe 'maybe-let* m) m)]
      [(_ ((id expr) claw ...) body ...)
       (let ([m expr])
         (check-maybe 'maybe-let* m)
         (if (just? m)
           (let ([id (single 'maybe-let* (just-objs m))])
             (maybe-let* (claw ...) body ...))
           m))]
      [(_ ((expr) claw ...) body ...)
       (let ([m expr])
         (check-maybe 'maybe-let* m)
         (if (just? m) (maybe-let* (claw ...) body ...) m))]
      [(_ (id claw ...) body ...)
       (let ([m id])
         (check-maybe 'maybe-let* m)
         (if (just? m) (maybe-let* (claw ...) body ...) m))]))

  (define-syntax maybe-let*-values
    (syntax-rules ()
      [(_ ()) (just)]
      [(_ () body1 body ...) (let () body1 body ...)]
      [(_ ((formals expr))) (let ([m expr]) (check-maybe 'maybe-let*-values m) m)]
      [(_ ((expr))) (let ([m expr]) (check-maybe 'maybe-let*-values m) m)]
      [(_ (id)) (let ([m id]) (check-maybe 'maybe-let*-values m) m)]
      [(_ ((formals expr) claw ...) body ...)
       (let ([m expr])
         (check-maybe 'maybe-let*-values m)
         (if (just? m)
           (apply (lambda formals (maybe-let*-values (claw ...) body ...)) (just-objs m))
           m))]
      [(_ ((expr) claw ...) body ...)
       (let ([m expr])
         (check-maybe 'maybe-let*-values m)
         (if (just? m) (maybe-let*-values (claw ...) body ...) m))]
      [(_ (id claw ...) body ...)
       (let ([m id])
         (check-maybe 'maybe-let*-values m)
         (if (just? m) (maybe-let*-values (claw ...) body ...) m))]))

  (define-syntax either-let*
    (syntax-rules ()
      [(_ ()) (right)]
      [(_ () body1 body ...) (let () body1 body ...)]
      [(_ ((id expr))) (let ([x expr]) (check-either 'either-let* x) x)]
      [(_ ((expr))) (let ([x expr]) (check-either 'either-let* x) x)]
      [(_ (id)) (let ([x id]) (check-either 'either-let* x) x)]
      [(_ ((id expr) claw ...) body ...)
       (let ([x expr])
         (check-either 'either-let* x)
         (if (right? x)
           (let ([id (single 'either-let* (right-objs x))])
             (either-let* (claw ...) body ...))
           x))]
      [(_ ((expr) claw ...) body ...)
       (let ([x expr])
         (check-either 'either-let* x)
         (if (right? x) (either-let* (claw ...) body ...) x))]
      [(_ (id claw ...) body ...)
       (let ([x id])
         (check-either 'either-let* x)
         (if (right? x) (either-let* (claw ...) body ...) x))]))

  (define-syntax either-let*-values
    (syntax-rules ()
      [(_ ()) (right)]
      [(_ () body1 body ...) (let () body1 body ...)]
      [(_ ((formals expr))) (let ([x expr]) (check-either 'either-let*-values x) x)]
      [(_ ((expr))) (let ([x expr]) (check-either 'either-let*-values x) x)]
      [(_ (id)) (let ([x id]) (check-either 'either-let*-values x) x)]
      [(_ ((formals expr) claw ...) body ...)
       (let ([x expr])
         (check-either 'either-let*-values x)
         (if (right? x)
           (apply (lambda formals (either-let*-values (claw ...) body ...)) (right-objs x))
           x))]
      [(_ ((expr) claw ...) body ...)
       (let ([x expr])
         (check-either 'either-let*-values x)
         (if (right? x) (either-let*-values (claw ...) body ...) x))]
      [(_ (id claw ...) body ...)
       (let ([x id])
         (check-either 'either-let*-values x)
         (if (right? x) (either-let*-values (claw ...) body ...) x))]))

  (define-syntax either-guard
    (syntax-rules ()
      [(_ pred-expr body ...)
       (exception->either pred-expr (lambda () body ...))]))

  ;; Trivalent logic: Just #t is true, Just #f false, Nothing unknown. Any payload other than
  ;; #f counts as true.

  (define (tri-value who m)
    (check-maybe who m)
    (if (just? m) (if (single who (just-objs m)) 'true 'false) 'unknown))

  (define (tri-not maybe)
    (case (tri-value 'tri-not maybe)
      [(true) (just #f)]
      [(false) (just #t)]
      [else the-nothing]))

  ;; Just #t when every argument is Just and they all have the same truth value; Just #f when
  ;; they differ; Nothing when any argument is Nothing.
  (define (tri=? maybe . maybes)
    (let ([v (tri-value 'tri=? maybe)])
      (let loop ([ms maybes] [same? #t])
        (cond
          [(eq? v 'unknown) the-nothing]
          [(null? ms) (just same?)]
          [else
           (let ([w (tri-value 'tri=? (car ms))])
             (if (eq? w 'unknown)
               the-nothing
               (loop (cdr ms) (and same? (eq? v w)))))]))))

  (define (tri-and . maybes)
    (let loop ([ms maybes] [unknown? #f])
      (if (null? ms)
        (if unknown? the-nothing (just #t))
        (case (tri-value 'tri-and (car ms))
          [(false) (just #f)]
          [(unknown) (loop (cdr ms) #t)]
          [else (loop (cdr ms) unknown?)]))))

  (define (tri-or . maybes)
    (let loop ([ms maybes] [unknown? #f])
      (if (null? ms)
        (if unknown? the-nothing (just #f))
        (case (tri-value 'tri-or (car ms))
          [(true) (just #t)]
          [(unknown) (loop (cdr ms) #t)]
          [else (loop (cdr ms) unknown?)]))))

  (define (tri-merge . maybes)
    (let loop ([ms maybes])
      (cond
        [(null? ms) the-nothing]
        [(just? (car ms)) (car ms)]
        [else (check-maybe 'tri-merge (car ms)) (loop (cdr ms))]))))
