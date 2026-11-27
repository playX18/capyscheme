;;; SRFI 158: Generators and accumulators.
;;;
;;; A generator is a thunk that returns the eof object when it is exhausted. The generators
;;; made here are closures over a few mutable variables, so a call is a direct closure call
;;; with no allocation. `make-coroutine-generator` (and `make-for-each-generator`, built on it)
;;; uses the continuation-based generators of (capy generator).
;;;
;;; Deviations from the specification: none known.
(library (srfi 158)
  (export
    ;; generator constructors
    generator circular-generator make-iota-generator make-range-generator
    make-coroutine-generator list->generator vector->generator reverse-vector->generator
    string->generator bytevector->generator make-for-each-generator make-unfold-generator
    ;; generator operations
    gcons* gappend gflatten ggroup gmerge gmap gcombine gfilter gremove gstate-filter
    gtake gdrop gtake-while gdrop-while gdelete gdelete-neighbor-dups gindex gselect
    ;; consuming generated values
    generator->list generator->reverse-list generator->vector generator->vector!
    generator->string generator-fold generator-for-each generator-map->list generator-find
    generator-count generator-any generator-every generator-unfold
    ;; accumulator constructors
    make-accumulator count-accumulator list-accumulator reverse-list-accumulator
    vector-accumulator reverse-vector-accumulator vector-accumulator! string-accumulator
    bytevector-accumulator bytevector-accumulator! sum-accumulator product-accumulator)
  (import
    (rnrs)
    (prefix (only (capy generator) make-generator) capy:))

  (define eof (eof-object))

  ;;; Constructors

  (define (generator . args)
    (lambda ()
      (if (null? args)
        eof
        (let ([x (car args)]) (set! args (cdr args)) x))))

  (define (circular-generator . args)
    (when (null? args)
      (assertion-violation 'circular-generator "no values"))
    (let ([l args])
      (lambda ()
        (when (null? l) (set! l args))
        (let ([x (car l)]) (set! l (cdr l)) x))))

  ;; Each value is START + i*STEP rather than a running sum, so inexact steps do not drift.
  (define make-iota-generator
    (case-lambda
      [(count) (make-iota-generator count 0 1)]
      [(count start) (make-iota-generator count start 1)]
      [(count start step)
       (let ([i 0])
         (lambda ()
           (if (< i count)
             (let ([x (+ start (* i step))]) (set! i (+ i 1)) x)
             eof)))]))

  (define make-range-generator
    (case-lambda
      [(start) (make-range-generator start +inf.0 1)]
      [(start end) (make-range-generator start end 1)]
      [(start end step)
       (let ([i 0])
         (lambda ()
           (let ([x (+ start (* i step))])
             (if (if (negative? step) (> x end) (< x end))
               (begin (set! i (+ i 1)) x)
               eof))))]))

  (define (make-coroutine-generator proc)
    (capy:make-generator
      (lambda (yield return)
        (proc (lambda (v) (yield v)))
        eof)))

  (define (list->generator lst)
    (lambda ()
      (if (null? lst)
        eof
        (let ([x (car lst)]) (set! lst (cdr lst)) x))))

  (define vector->generator
    (case-lambda
      [(v) (vector->generator v 0 (vector-length v))]
      [(v start) (vector->generator v start (vector-length v))]
      [(v start end)
       (let ([i start])
         (lambda ()
           (if (fx<? i end)
             (let ([x (vector-ref v i)]) (set! i (fx+ i 1)) x)
             eof)))]))

  (define reverse-vector->generator
    (case-lambda
      [(v) (reverse-vector->generator v 0 (vector-length v))]
      [(v start) (reverse-vector->generator v start (vector-length v))]
      [(v start end)
       (let ([i end])
         (lambda ()
           (if (fx>? i start)
             (begin (set! i (fx- i 1)) (vector-ref v i))
             eof)))]))

  (define string->generator
    (case-lambda
      [(s) (string->generator s 0 (string-length s))]
      [(s start) (string->generator s start (string-length s))]
      [(s start end)
       (let ([i start])
         (lambda ()
           (if (fx<? i end)
             (let ([x (string-ref s i)]) (set! i (fx+ i 1)) x)
             eof)))]))

  (define bytevector->generator
    (case-lambda
      [(bv) (bytevector->generator bv 0 (bytevector-length bv))]
      [(bv start) (bytevector->generator bv start (bytevector-length bv))]
      [(bv start end)
       (let ([i start])
         (lambda ()
           (if (fx<? i end)
             (let ([x (bytevector-u8-ref bv i)]) (set! i (fx+ i 1)) x)
             eof)))]))

  (define (make-for-each-generator for-each obj)
    (make-coroutine-generator (lambda (yield) (for-each yield obj))))

  (define (make-unfold-generator stop? mapper successor seed)
    (let ([done #f])
      (lambda ()
        (cond
          [done eof]
          [(stop? seed) (set! done #t) eof]
          [else
           (let ([x (mapper seed)])
             (set! seed (successor seed))
             x)]))))

  ;;; Operations

  ;; ITEMS then the values of the generator, which is the last argument.
  (define (gcons* . args)
    (when (null? args) (assertion-violation 'gcons* "no generator"))
    (lambda ()
      (if (null? (cdr args))
        ((car args))
        (let ([x (car args)]) (set! args (cdr args)) x))))

  (define (gappend . gens)
    (lambda ()
      (let loop ()
        (if (null? gens)
          eof
          (let ([x ((car gens))])
            (if (eof-object? x)
              (begin (set! gens (cdr gens)) (loop))
              x))))))

  (define (gflatten gen)
    (let ([pending '()])
      (lambda ()
        (let loop ()
          (if (pair? pending)
            (let ([x (car pending)]) (set! pending (cdr pending)) x)
            (let ([l (gen)])
              (if (eof-object? l)
                eof
                (begin (set! pending l) (loop)))))))))

  (define ggroup
    (case-lambda
      [(gen k)
       (lambda ()
         (let ([group (%take gen k)])
           (if (null? group) eof group)))]
      [(gen k padding)
       (lambda ()
         (let ([group (%take gen k)])
           (if (null? group)
             eof
             (let ([n (length group)])
               (if (< n k)
                 (append group (make-list (- k n) padding))
                 group)))))]))

  ;; Up to K values of GEN, as a list.
  (define (%take gen k)
    (let loop ([i 0] [acc '()])
      (if (fx<? i k)
        (let ([x (gen)])
          (if (eof-object? x) (reverse acc) (loop (fx+ i 1) (cons x acc))))
        (reverse acc))))

  (define (make-list n x)
    (let loop ([n n] [acc '()])
      (if (fx=? n 0) acc (loop (fx- n 1) (cons x acc)))))

  ;; Both heads are fetched on the first call, not when the merge is made.
  (define (merge2 < a b)
    (let ([started #f] [x #f] [y #f])
      (lambda ()
        (unless started (set! started #t) (set! x (a)) (set! y (b)))
        (cond
          [(and (eof-object? x) (eof-object? y)) eof]
          [(eof-object? y) (let ([r x]) (set! x (a)) r)]
          [(or (eof-object? x) (< y x)) (let ([r y]) (set! y (b)) r)]
          [else (let ([r x]) (set! x (a)) r)]))))

  (define (gmerge < . gens)
    (cond
      [(null? gens) (generator)]
      [(null? (cdr gens)) (car gens)]
      [else
       ;; A balanced tree of two-way merges.
       (let loop ([gens gens])
         (if (null? (cdr gens))
           (car gens)
           (loop (let pair ([gs gens])
                   (cond
                     [(null? gs) '()]
                     [(null? (cdr gs)) gs]
                     [else (cons (merge2 < (car gs) (cadr gs)) (pair (cddr gs)))])))))]))

  (define (any-eof? xs)
    (and (pair? xs) (or (eof-object? (car xs)) (any-eof? (cdr xs)))))

  (define (call-all gens)
    (if (null? gens) '() (let ([x ((car gens))]) (cons x (call-all (cdr gens))))))

  (define gmap
    (case-lambda
      [(proc gen)
       (lambda ()
         (let ([x (gen)])
           (if (eof-object? x) x (proc x))))]
      [(proc . gens)
       (lambda ()
         (let ([xs (call-all gens)])
           (if (any-eof? xs) eof (apply proc xs))))]))

  ;; PROC takes the values and the seed, and returns the value and the new seed.
  (define (gcombine proc seed . gens)
    (lambda ()
      (let ([xs (call-all gens)])
        (if (any-eof? xs)
          eof
          (let-values ([(v s) (apply proc (append xs (list seed)))])
            (set! seed s)
            v)))))

  (define (gfilter pred gen)
    (lambda ()
      (let loop ()
        (let ([x (gen)])
          (if (or (eof-object? x) (pred x)) x (loop))))))

  (define (gremove pred gen)
    (gfilter (lambda (x) (not (pred x))) gen))

  ;; PROC takes a value and the state, and returns whether to keep it and the new state.
  (define (gstate-filter proc seed gen)
    (lambda ()
      (let loop ()
        (let ([x (gen)])
          (if (eof-object? x)
            x
            (let-values ([(keep? s) (proc x seed)])
              (set! seed s)
              (if keep? x (loop))))))))

  (define gtake
    (case-lambda
      [(gen k)
       (let ([i 0])
         (lambda ()
           (if (< i k)
             (begin (set! i (+ i 1)) (gen))
             eof)))]
      [(gen k padding)
       (let ([i 0] [done #f])
         (lambda ()
           (if (< i k)
             (begin
               (set! i (+ i 1))
               (if done
                 padding
                 (let ([x (gen)])
                   (if (eof-object? x) (begin (set! done #t) padding) x))))
             eof)))]))

  (define (gdrop gen k)
    (lambda ()
      (let loop ()
        (if (> k 0)
          (let ([x (gen)])
            (set! k (- k 1))
            (if (eof-object? x) (begin (set! k 0) x) (loop)))
          (gen)))))

  (define (gtake-while pred gen)
    (let ([done #f])
      (lambda ()
        (if done
          eof
          (let ([x (gen)])
            (if (or (eof-object? x) (not (pred x)))
              (begin (set! done #t) eof)
              x))))))

  (define (gdrop-while pred gen)
    (let ([dropping #t])
      (lambda ()
        (if dropping
          (let loop ()
            (let ([x (gen)])
              (if (and (not (eof-object? x)) (pred x))
                (loop)
                (begin (set! dropping #f) x))))
          (gen)))))

  (define gdelete
    (case-lambda
      [(item gen) (gdelete item gen equal?)]
      [(item gen =) (gfilter (lambda (x) (not (= item x))) gen)]))

  (define gdelete-neighbor-dups
    (case-lambda
      [(gen) (gdelete-neighbor-dups gen equal?)]
      [(gen =)
       (let ([first #t] [prev #f])
         (lambda ()
           (let loop ()
             (let ([x (gen)])
               (cond
                 [(eof-object? x) x]
                 [first (set! first #f) (set! prev x) x]
                 [(= prev x) (loop)]
                 [else (set! prev x) x])))))]))

  ;; The values of VALUE-GEN at the increasing indices that INDEX-GEN gives.
  (define (gindex value-gen index-gen)
    (let ([pos 0])
      (lambda ()
        (let ([k (index-gen)])
          (if (eof-object? k)
            k
            (let loop ()
              (let ([x (value-gen)])
                (cond
                  [(eof-object? x) x]
                  [(= pos k) (set! pos (+ pos 1)) x]
                  [(< pos k) (set! pos (+ pos 1)) (loop)]
                  [else (assertion-violation 'gindex "indices are not increasing" k)]))))))))

  (define (gselect value-gen truth-gen)
    (lambda ()
      (let loop ()
        (let ([x (value-gen)] [t (truth-gen)])
          (cond
            [(or (eof-object? x) (eof-object? t)) eof]
            [t x]
            [else (loop)])))))

  ;;; Consumers

  (define generator->reverse-list
    (case-lambda
      [(gen)
       (let loop ([acc '()])
         (let ([x (gen)])
           (if (eof-object? x) acc (loop (cons x acc)))))]
      [(gen n)
       (let loop ([i 0] [acc '()])
         (if (< i n)
           (let ([x (gen)])
             (if (eof-object? x) acc (loop (+ i 1) (cons x acc))))
           acc))]))

  (define generator->list
    (case-lambda
      [(gen) (reverse (generator->reverse-list gen))]
      [(gen n) (reverse (generator->reverse-list gen n))]))

  (define generator->vector
    (case-lambda
      [(gen) (list->vector (generator->list gen))]
      [(gen n) (list->vector (generator->list gen n))]))

  ;; Fills VEC from AT; returns the number of values written.
  (define (generator->vector! vec at gen)
    (let ([n (vector-length vec)])
      (let loop ([i at])
        (if (fx<? i n)
          (let ([x (gen)])
            (if (eof-object? x)
              (fx- i at)
              (begin (vector-set! vec i x) (loop (fx+ i 1)))))
          (fx- i at)))))

  (define generator->string
    (case-lambda
      [(gen) (list->string (generator->list gen))]
      [(gen n) (list->string (generator->list gen n))]))

  (define generator-fold
    (case-lambda
      [(proc seed gen)
       (let loop ([acc seed])
         (let ([x (gen)])
           (if (eof-object? x) acc (loop (proc x acc)))))]
      [(proc seed . gens)
       (let loop ([acc seed])
         (let ([xs (call-all gens)])
           (if (any-eof? xs) acc (loop (apply proc (append xs (list acc)))))))]))

  (define generator-for-each
    (case-lambda
      [(proc gen)
       (let loop ()
         (let ([x (gen)])
           (unless (eof-object? x) (proc x) (loop))))]
      [(proc . gens)
       (let loop ()
         (let ([xs (call-all gens)])
           (unless (any-eof? xs) (apply proc xs) (loop))))]))

  (define generator-map->list
    (case-lambda
      [(proc gen)
       (let loop ([acc '()])
         (let ([x (gen)])
           (if (eof-object? x) (reverse acc) (loop (cons (proc x) acc)))))]
      [(proc . gens)
       (let loop ([acc '()])
         (let ([xs (call-all gens)])
           (if (any-eof? xs) (reverse acc) (loop (cons (apply proc xs) acc)))))]))

  (define (generator-find pred gen)
    (let loop ()
      (let ([x (gen)])
        (cond
          [(eof-object? x) #f]
          [(pred x) x]
          [else (loop)]))))

  (define (generator-count pred gen)
    (let loop ([c 0])
      (let ([x (gen)])
        (if (eof-object? x) c (loop (if (pred x) (+ c 1) c))))))

  (define (generator-any pred gen)
    (let loop ()
      (let ([x (gen)])
        (and (not (eof-object? x))
             (or (pred x) (loop))))))

  (define (generator-every pred gen)
    (let loop ([last #t])
      (let ([x (gen)])
        (if (eof-object? x)
          last
          (let ([r (pred x)])
            (and r (loop r)))))))

  ;; (unfold eof-object? (lambda (x) x) (lambda (x) (gen)) (gen) arg ...)
  (define (generator-unfold gen unfold . args)
    (apply unfold eof-object? (lambda (x) x) (lambda (x) (gen)) (gen) args))

  ;;; Accumulators

  (define (make-accumulator kons knil finalizer)
    (let ([state knil])
      (lambda (x)
        (if (eof-object? x)
          (finalizer state)
          (set! state (kons x state))))))

  (define (count-accumulator)
    (make-accumulator (lambda (x n) (+ n 1)) 0 (lambda (n) n)))

  (define (list-accumulator)
    (make-accumulator cons '() reverse))

  (define (reverse-list-accumulator)
    (make-accumulator cons '() (lambda (l) l)))

  (define (vector-accumulator)
    (make-accumulator cons '() (lambda (l) (list->vector (reverse l)))))

  (define (reverse-vector-accumulator)
    (make-accumulator cons '() list->vector))

  (define (vector-accumulator! vec at)
    (lambda (x)
      (if (eof-object? x)
        vec
        (begin (vector-set! vec at x) (set! at (+ at 1))))))

  (define (string-accumulator)
    (make-accumulator cons '() (lambda (l) (list->string (reverse l)))))

  (define (bytevector-accumulator)
    (make-accumulator cons '() (lambda (l) (u8-list->bytevector (reverse l)))))

  (define (bytevector-accumulator! bv at)
    (lambda (x)
      (if (eof-object? x)
        bv
        (begin (bytevector-u8-set! bv at x) (set! at (+ at 1))))))

  (define (sum-accumulator)
    (make-accumulator + 0 (lambda (x) x)))

  (define (product-accumulator)
    (make-accumulator * 1 (lambda (x) x))))
