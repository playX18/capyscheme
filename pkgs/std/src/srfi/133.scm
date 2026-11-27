;;; SRFI 133: Vector library (R7RS-compatible).
;;;
;;; Written for the JIT: each procedure has a one-vector fast case whose loop is a named `let`
;;; over a fixnum index, and the n-ary case goes through `apply` only once per element.
;;; Procedures that take several vectors stop at the shortest one, as the SRFI says (R6RS
;;; `vector-map` and `vector-for-each` require equal lengths; these do not).
;;;
;;; Deviations from the specification: none known.
(library (srfi 133)
  (export
    ;; constructors
    make-vector vector vector-unfold vector-unfold-right vector-copy vector-reverse-copy
    vector-append vector-concatenate vector-append-subvectors
    ;; predicates
    vector? vector-empty? vector=
    ;; selectors
    vector-ref vector-length
    ;; iteration
    vector-fold vector-fold-right vector-map vector-map! vector-for-each vector-count
    vector-cumulate
    ;; searching
    vector-index vector-index-right vector-skip vector-skip-right vector-binary-search
    vector-any vector-every vector-partition
    ;; mutators
    vector-set! vector-swap! vector-fill! vector-reverse! vector-copy! vector-reverse-copy!
    vector-unfold! vector-unfold-right!
    ;; conversion
    vector->list reverse-vector->list list->vector reverse-list->vector string->vector
    vector->string)
  (import
    (except (rnrs) vector-fill! vector->list list->vector vector-map vector-for-each)
    (prefix (only (rnrs) vector->list list->vector vector-fill!) r6rs:))

  (define (check-range who v start end)
    (unless (and (fixnum? start) (fixnum? end) (fx<=? 0 start) (fx<=? start end)
                 (fx<=? end (vector-length v)))
      (assertion-violation who "invalid range" v start end)))

  ;; The length of the shortest vector of VS.
  (define (min-length vs)
    (let loop ([vs vs] [n (greatest-fixnum)])
      (if (null? vs) n (loop (cdr vs) (fxmin n (vector-length (car vs)))))))

  ;; The elements of VS at index I.
  (define (refs vs i)
    (if (null? vs) '() (cons (vector-ref (car vs) i) (refs (cdr vs) i))))

  ;;; Constructors

  ;; F is called on the index and the seeds; it returns the element and the new seeds.
  (define (vector-unfold f n . seeds)
    (let ([v (make-vector n)])
      (apply vector-unfold! f v 0 n seeds)
      v))

  (define (vector-unfold-right f n . seeds)
    (let ([v (make-vector n)])
      (apply vector-unfold-right! f v 0 n seeds)
      v))

  (define vector-unfold!
    (case-lambda
      [(f v start end)
       (let loop ([i start])
         (when (fx<? i end)
           (vector-set! v i (f i))
           (loop (fx+ i 1))))]
      [(f v start end seed)
       (let loop ([i start] [seed seed])
         (when (fx<? i end)
           (let-values ([(x seed) (f i seed)])
             (vector-set! v i x)
             (loop (fx+ i 1) seed))))]
      [(f v start end . seeds)
       (let loop ([i start] [seeds seeds])
         (when (fx<? i end)
           (call-with-values (lambda () (apply f i seeds))
             (lambda (x . seeds)
               (vector-set! v i x)
               (loop (fx+ i 1) seeds)))))]))

  (define vector-unfold-right!
    (case-lambda
      [(f v start end)
       (let loop ([i (fx- end 1)])
         (when (fx>=? i start)
           (vector-set! v i (f i))
           (loop (fx- i 1))))]
      [(f v start end seed)
       (let loop ([i (fx- end 1)] [seed seed])
         (when (fx>=? i start)
           (let-values ([(x seed) (f i seed)])
             (vector-set! v i x)
             (loop (fx- i 1) seed))))]
      [(f v start end . seeds)
       (let loop ([i (fx- end 1)] [seeds seeds])
         (when (fx>=? i start)
           (call-with-values (lambda () (apply f i seeds))
             (lambda (x . seeds)
               (vector-set! v i x)
               (loop (fx- i 1) seeds)))))]))

  (define (%copy v start end)
    (let ([r (make-vector (fx- end start))])
      (let loop ([i start] [j 0])
        (when (fx<? i end)
          (vector-set! r j (vector-ref v i))
          (loop (fx+ i 1) (fx+ j 1))))
      r))

  (define vector-copy
    (case-lambda
      [(v) (%copy v 0 (vector-length v))]
      [(v start) (vector-copy v start (vector-length v))]
      [(v start end) (check-range 'vector-copy v start end) (%copy v start end)]))

  (define vector-reverse-copy
    (case-lambda
      [(v) (vector-reverse-copy v 0 (vector-length v))]
      [(v start) (vector-reverse-copy v start (vector-length v))]
      [(v start end)
       (check-range 'vector-reverse-copy v start end)
       (let ([r (make-vector (fx- end start))])
         (let loop ([i (fx- end 1)] [j 0])
           (when (fx>=? i start)
             (vector-set! r j (vector-ref v i))
             (loop (fx- i 1) (fx+ j 1))))
         r)]))

  ;; Copies V[start, end) into R at AT.
  (define (blit! r at v start end)
    (let loop ([i start] [j at])
      (when (fx<? i end)
        (vector-set! r j (vector-ref v i))
        (loop (fx+ i 1) (fx+ j 1)))))

  (define (vector-concatenate vs)
    (let* ([n (fold-left (lambda (n v) (fx+ n (vector-length v))) 0 vs)]
           [r (make-vector n)])
      (let loop ([vs vs] [at 0])
        (unless (null? vs)
          (let ([len (vector-length (car vs))])
            (blit! r at (car vs) 0 len)
            (loop (cdr vs) (fx+ at len)))))
      r))

  (define vector-append
    (case-lambda
      [() (vector)]
      [(a) (%copy a 0 (vector-length a))]
      [(a b)
       (let* ([na (vector-length a)] [r (make-vector (fx+ na (vector-length b)))])
         (blit! r 0 a 0 na)
         (blit! r na b 0 (vector-length b))
         r)]
      [vs (vector-concatenate vs)]))

  ;; Arguments are VEC START END triples.
  (define (vector-append-subvectors . args)
    (let count ([as args] [n 0])
      (if (null? as)
        (let ([r (make-vector n)])
          (let fill ([as args] [at 0])
            (if (null? as)
              r
              (let ([v (car as)] [s (cadr as)] [e (caddr as)])
                (blit! r at v s e)
                (fill (cdddr as) (fx+ at (fx- e s)))))))
        (let ([v (car as)] [s (cadr as)] [e (caddr as)])
          (check-range 'vector-append-subvectors v s e)
          (count (cdddr as) (fx+ n (fx- e s)))))))

  ;;; Predicates

  (define (vector-empty? v) (fx=? (vector-length v) 0))

  (define (vector= elt=? . vs)
    (or (null? vs)
        (null? (cdr vs))
        (let ([a (car vs)] [b (cadr vs)])
          (let ([n (vector-length a)])
            (and (fx=? n (vector-length b))
                 (let loop ([i 0])
                   (or (fx=? i n)
                       (and (elt=? (vector-ref a i) (vector-ref b i))
                            (loop (fx+ i 1)))))
                 (apply vector= elt=? (cdr vs)))))))

  ;;; Iteration

  (define vector-fold
    (case-lambda
      [(kons knil v)
       (let ([n (vector-length v)])
         (let loop ([i 0] [acc knil])
           (if (fx<? i n) (loop (fx+ i 1) (kons acc (vector-ref v i))) acc)))]
      [(kons knil v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0] [acc knil])
           (if (fx<? i n) (loop (fx+ i 1) (apply kons acc (refs vs i))) acc)))]))

  (define vector-fold-right
    (case-lambda
      [(kons knil v)
       (let loop ([i (fx- (vector-length v) 1)] [acc knil])
         (if (fx>=? i 0) (loop (fx- i 1) (kons acc (vector-ref v i))) acc))]
      [(kons knil v . vs)
       (let ([vs (cons v vs)])
         (let loop ([i (fx- (min-length vs) 1)] [acc knil])
           (if (fx>=? i 0) (loop (fx- i 1) (apply kons acc (refs vs i))) acc)))]))

  (define vector-map
    (case-lambda
      [(f v)
       (let* ([n (vector-length v)] [r (make-vector n)])
         (let loop ([i 0])
           (when (fx<? i n)
             (vector-set! r i (f (vector-ref v i)))
             (loop (fx+ i 1))))
         r)]
      [(f v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)] [r (make-vector n)])
         (let loop ([i 0])
           (when (fx<? i n)
             (vector-set! r i (apply f (refs vs i)))
             (loop (fx+ i 1))))
         r)]))

  (define vector-map!
    (case-lambda
      [(f v)
       (let ([n (vector-length v)])
         (let loop ([i 0])
           (when (fx<? i n)
             (vector-set! v i (f (vector-ref v i)))
             (loop (fx+ i 1)))))]
      [(f v . vs)
       (let* ([all (cons v vs)] [n (min-length all)])
         (let loop ([i 0])
           (when (fx<? i n)
             (vector-set! v i (apply f (refs all i)))
             (loop (fx+ i 1)))))]))

  (define vector-for-each
    (case-lambda
      [(f v)
       (let ([n (vector-length v)])
         (let loop ([i 0])
           (when (fx<? i n)
             (f (vector-ref v i))
             (loop (fx+ i 1)))))]
      [(f v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0])
           (when (fx<? i n)
             (apply f (refs vs i))
             (loop (fx+ i 1)))))]))

  (define vector-count
    (case-lambda
      [(pred v)
       (let ([n (vector-length v)])
         (let loop ([i 0] [c 0])
           (if (fx<? i n)
             (loop (fx+ i 1) (if (pred (vector-ref v i)) (fx+ c 1) c))
             c)))]
      [(pred v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0] [c 0])
           (if (fx<? i n)
             (loop (fx+ i 1) (if (apply pred (refs vs i)) (fx+ c 1) c))
             c)))]))

  (define (vector-cumulate f knil v)
    (let* ([n (vector-length v)] [r (make-vector n)])
      (let loop ([i 0] [acc knil])
        (if (fx<? i n)
          (let ([acc (f acc (vector-ref v i))])
            (vector-set! r i acc)
            (loop (fx+ i 1) acc))
          r))))

  ;;; Searching

  (define vector-index
    (case-lambda
      [(pred v)
       (let ([n (vector-length v)])
         (let loop ([i 0])
           (cond
             [(fx=? i n) #f]
             [(pred (vector-ref v i)) i]
             [else (loop (fx+ i 1))])))]
      [(pred v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0])
           (cond
             [(fx=? i n) #f]
             [(apply pred (refs vs i)) i]
             [else (loop (fx+ i 1))])))]))

  (define vector-index-right
    (case-lambda
      [(pred v)
       (let loop ([i (fx- (vector-length v) 1)])
         (cond
           [(fx<? i 0) #f]
           [(pred (vector-ref v i)) i]
           [else (loop (fx- i 1))]))]
      [(pred v . vs)
       (let ([vs (cons v vs)])
         (let loop ([i (fx- (min-length vs) 1)])
           (cond
             [(fx<? i 0) #f]
             [(apply pred (refs vs i)) i]
             [else (loop (fx- i 1))])))]))

  (define vector-skip
    (case-lambda
      [(pred v) (vector-index (lambda (x) (not (pred x))) v)]
      [(pred v . vs) (apply vector-index (lambda xs (not (apply pred xs))) v vs)]))

  (define vector-skip-right
    (case-lambda
      [(pred v) (vector-index-right (lambda (x) (not (pred x))) v)]
      [(pred v . vs) (apply vector-index-right (lambda xs (not (apply pred xs))) v vs)]))

  ;; CMP is called as (cmp element value) and returns a negative, zero or positive integer.
  (define vector-binary-search
    (case-lambda
      [(v value cmp) (vector-binary-search v value cmp 0 (vector-length v))]
      [(v value cmp start) (vector-binary-search v value cmp start (vector-length v))]
      [(v value cmp start end)
       (check-range 'vector-binary-search v start end)
       (let loop ([lo start] [hi end])
         (and (fx<? lo hi)
              (let* ([mid (fx+ lo (fxarithmetic-shift-right (fx- hi lo) 1))]
                     [c (cmp (vector-ref v mid) value)])
                (cond
                  [(zero? c) mid]
                  [(negative? c) (loop (fx+ mid 1) hi)]
                  [else (loop lo mid)]))))]))

  (define vector-any
    (case-lambda
      [(pred v)
       (let ([n (vector-length v)])
         (let loop ([i 0])
           (and (fx<? i n)
                (or (pred (vector-ref v i)) (loop (fx+ i 1))))))]
      [(pred v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0])
           (and (fx<? i n)
                (or (apply pred (refs vs i)) (loop (fx+ i 1))))))]))

  (define vector-every
    (case-lambda
      [(pred v)
       (let ([n (vector-length v)])
         (let loop ([i 0] [last #t])
           (if (fx<? i n)
             (let ([r (pred (vector-ref v i))])
               (and r (loop (fx+ i 1) r)))
             last)))]
      [(pred v . vs)
       (let* ([vs (cons v vs)] [n (min-length vs)])
         (let loop ([i 0] [last #t])
           (if (fx<? i n)
             (let ([r (apply pred (refs vs i))])
               (and r (loop (fx+ i 1) r)))
             last)))]))

  ;; A new vector with the elements satisfying PRED first, then the others, each in order;
  ;; and the number of the former.
  (define (vector-partition pred v)
    (let* ([n (vector-length v)]
           [flags (make-vector n #f)]
           [yes (let loop ([i 0] [c 0])
                  (if (fx<? i n)
                    (if (pred (vector-ref v i))
                      (begin (vector-set! flags i #t) (loop (fx+ i 1) (fx+ c 1)))
                      (loop (fx+ i 1) c))
                    c))]
           [r (make-vector n)])
      (let loop ([i 0] [y 0] [o yes])
        (when (fx<? i n)
          (if (vector-ref flags i)
            (begin (vector-set! r y (vector-ref v i)) (loop (fx+ i 1) (fx+ y 1) o))
            (begin (vector-set! r o (vector-ref v i)) (loop (fx+ i 1) y (fx+ o 1))))))
      (values r yes)))

  ;;; Mutators

  (define (vector-swap! v i j)
    (let ([x (vector-ref v i)])
      (vector-set! v i (vector-ref v j))
      (vector-set! v j x)))

  (define vector-fill!
    (case-lambda
      [(v x) (r6rs:vector-fill! v x)]
      [(v x start) (vector-fill! v x start (vector-length v))]
      [(v x start end)
       (check-range 'vector-fill! v start end)
       (let loop ([i start])
         (when (fx<? i end)
           (vector-set! v i x)
           (loop (fx+ i 1))))]))

  (define vector-reverse!
    (case-lambda
      [(v) (vector-reverse! v 0 (vector-length v))]
      [(v start) (vector-reverse! v start (vector-length v))]
      [(v start end)
       (check-range 'vector-reverse! v start end)
       (let loop ([i start] [j (fx- end 1)])
         (when (fx<? i j)
           (vector-swap! v i j)
           (loop (fx+ i 1) (fx- j 1))))]))

  (define vector-copy!
    (case-lambda
      [(to at from) (vector-copy! to at from 0 (vector-length from))]
      [(to at from start) (vector-copy! to at from start (vector-length from))]
      [(to at from start end)
       (check-range 'vector-copy! from start end)
       (unless (and (fixnum? at) (fx<=? 0 at) (fx<=? (fx+ at (fx- end start)) (vector-length to)))
         (assertion-violation 'vector-copy! "the target is too short" to at))
       (if (and (eq? to from) (fx<? start at))
         ;; Overlapping, moving right: copy from the end.
         (let loop ([i (fx- end 1)] [j (fx+ at (fx- end start 1))])
           (when (fx>=? i start)
             (vector-set! to j (vector-ref from i))
             (loop (fx- i 1) (fx- j 1))))
         (blit! to at from start end))]))

  (define vector-reverse-copy!
    (case-lambda
      [(to at from) (vector-reverse-copy! to at from 0 (vector-length from))]
      [(to at from start) (vector-reverse-copy! to at from start (vector-length from))]
      [(to at from start end)
       (check-range 'vector-reverse-copy! from start end)
       (unless (and (fixnum? at) (fx<=? 0 at) (fx<=? (fx+ at (fx- end start)) (vector-length to)))
         (assertion-violation 'vector-reverse-copy! "the target is too short" to at))
       (if (eq? to from)
         (begin
           (vector-copy! to at from start end)
           (vector-reverse! to at (fx+ at (fx- end start))))
         (let loop ([i (fx- end 1)] [j at])
           (when (fx>=? i start)
             (vector-set! to j (vector-ref from i))
             (loop (fx- i 1) (fx+ j 1)))))]))

  ;;; Conversion

  (define vector->list
    (case-lambda
      [(v) (r6rs:vector->list v)]
      [(v start) (vector->list v start (vector-length v))]
      [(v start end)
       (check-range 'vector->list v start end)
       (let loop ([i (fx- end 1)] [acc '()])
         (if (fx<? i start) acc (loop (fx- i 1) (cons (vector-ref v i) acc))))]))

  (define reverse-vector->list
    (case-lambda
      [(v) (reverse-vector->list v 0 (vector-length v))]
      [(v start) (reverse-vector->list v start (vector-length v))]
      [(v start end)
       (check-range 'reverse-vector->list v start end)
       (let loop ([i start] [acc '()])
         (if (fx<? i end) (loop (fx+ i 1) (cons (vector-ref v i) acc)) acc))]))

  (define list->vector
    (case-lambda
      [(lst) (r6rs:list->vector lst)]
      [(lst start) (r6rs:list->vector (list-tail lst start))]
      [(lst start end)
       (let ([r (make-vector (fx- end start))])
         (let loop ([l (list-tail lst start)] [i 0])
           (when (fx<? i (fx- end start))
             (vector-set! r i (car l))
             (loop (cdr l) (fx+ i 1))))
         r)]))

  (define reverse-list->vector
    (case-lambda
      [(lst) (reverse-list->vector lst 0 (length lst))]
      [(lst start) (reverse-list->vector lst start (length lst))]
      [(lst start end)
       (let* ([n (fx- end start)] [r (make-vector n)])
         (let loop ([l (list-tail lst start)] [i (fx- n 1)])
           (when (fx>=? i 0)
             (vector-set! r i (car l))
             (loop (cdr l) (fx- i 1))))
         r)]))

  (define string->vector
    (case-lambda
      [(s) (string->vector s 0 (string-length s))]
      [(s start) (string->vector s start (string-length s))]
      [(s start end)
       (let ([r (make-vector (fx- end start))])
         (let loop ([i start] [j 0])
           (when (fx<? i end)
             (vector-set! r j (string-ref s i))
             (loop (fx+ i 1) (fx+ j 1))))
         r)]))

  (define vector->string
    (case-lambda
      [(v) (vector->string v 0 (vector-length v))]
      [(v start) (vector->string v start (vector-length v))]
      [(v start end)
       (check-range 'vector->string v start end)
       (let ([r (make-string (fx- end start))])
         (let loop ([i start] [j 0])
           (when (fx<? i end)
             (string-set! r j (vector-ref v i))
             (loop (fx+ i 1) (fx+ j 1))))
         r)])))
