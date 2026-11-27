;;; SRFI 152: String library (reduced).
;;;
;;; Index-based, R7RS-compatible. The loops run over fixnum indices with `string-ref` and
;;; build results in a preallocated string, so they stay in the JIT's fast paths; nothing goes
;;; through an intermediate list except where the result is a list.
;;;
;;; Deviations from the specification:
;;;
;;; - `string-unfold` and `string-unfold-right` accept a character or a string from MAPPER.
;;; - `string-index`, `string-index-right`, `string-skip` and `string-skip-right` return #f when
;;;   no character qualifies.
(library (srfi 152)
  (export
    ;; predicates
    string? string-null? string-every string-any
    ;; constructors
    make-string string string-tabulate string-unfold string-unfold-right
    ;; conversion
    string->vector string->list vector->string list->string reverse-list->string
    ;; selection
    string-length string-ref substring string-copy
    string-take string-take-right string-drop string-drop-right
    string-pad string-pad-right string-trim string-trim-right string-trim-both
    ;; replacement
    string-replace
    ;; comparison
    string=? string<? string>? string<=? string>=?
    string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
    ;; prefixes and suffixes
    string-prefix-length string-suffix-length string-prefix? string-suffix?
    ;; searching
    string-index string-index-right string-skip string-skip-right
    string-contains string-contains-right
    ;; the whole string
    string-concatenate string-concatenate-reverse string-join
    string-fold string-fold-right string-map string-for-each string-count
    string-filter string-remove
    ;; replication and splitting
    string-replicate string-segment string-split
    ;; input and output
    read-string write-string
    ;; mutation
    string-set! string-fill! string-copy!)
  (import
    (except (rnrs) string->list string-copy string-for-each)
    (only (rnrs mutable-strings) string-set!)
    (only (scheme base) read-string write-string))

  (define (check-range who s start end)
    (unless (and (fixnum? start) (fixnum? end) (fx<=? 0 start) (fx<=? start end)
                 (fx<=? end (string-length s)))
      (assertion-violation who "invalid range" s start end)))

  ;; The optional START END of S, checked.
  (define-syntax with-range
    (syntax-rules ()
      [(_ who s opts (start end) body ...)
       (let* ([o opts]
              [start (if (pair? o) (car o) 0)]
              [end (if (and (pair? o) (pair? (cdr o))) (cadr o) (string-length s))])
         (check-range who s start end)
         body ...)]))

  ;; Copies FROM[start, end) into TO at AT; the ranges do not overlap or AT <= START.
  (define (blit! to at from start end)
    (let loop ([i start] [j at])
      (when (fx<? i end)
        (string-set! to j (string-ref from i))
        (loop (fx+ i 1) (fx+ j 1)))))

  (define (%substring s start end)
    (if (and (fx=? start 0) (fx=? end (string-length s)))
      (let ([r (make-string end)]) (blit! r 0 s 0 end) r)
      (substring s start end)))

  ;;; Predicates

  (define (string-null? s) (fx=? (string-length s) 0))

  (define (string-every pred s . opts)
    (with-range 'string-every s opts (start end)
      (let loop ([i start] [last #t])
        (if (fx<? i end)
          (let ([r (pred (string-ref s i))])
            (and r (loop (fx+ i 1) r)))
          last))))

  (define (string-any pred s . opts)
    (with-range 'string-any s opts (start end)
      (let loop ([i start])
        (and (fx<? i end)
             (or (pred (string-ref s i)) (loop (fx+ i 1)))))))

  ;;; Constructors

  (define (string-tabulate proc len)
    (let ([r (make-string len)])
      (let loop ([i 0])
        (when (fx<? i len)
          (string-set! r i (proc i))
          (loop (fx+ i 1))))
      r))

  ;; MAPPER's results, collected in a list of strings and characters.
  (define (piece->string x)
    (if (char? x) (string x) x))

  (define (string-unfold stop? mapper successor seed . opts)
    (let ([base (if (pair? opts) (car opts) "")]
          [make-final (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) (lambda (x) ""))])
      (let loop ([seed seed] [acc (list base)])
        (if (stop? seed)
          (string-concatenate-reverse (cons (piece->string (make-final seed)) acc))
          (loop (successor seed) (cons (piece->string (mapper seed)) acc))))))

  (define (string-unfold-right stop? mapper successor seed . opts)
    (let ([base (if (pair? opts) (car opts) "")]
          [make-final (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) (lambda (x) ""))])
      (let loop ([seed seed] [acc (list base)])
        (if (stop? seed)
          (string-concatenate (cons (piece->string (make-final seed)) acc))
          (loop (successor seed) (cons (piece->string (mapper seed)) acc))))))

  ;;; Conversion

  (define (string->list s . opts)
    (with-range 'string->list s opts (start end)
      (let loop ([i (fx- end 1)] [acc '()])
        (if (fx<? i start) acc (loop (fx- i 1) (cons (string-ref s i) acc))))))

  (define (string->vector s . opts)
    (with-range 'string->vector s opts (start end)
      (let ([v (make-vector (fx- end start))])
        (let loop ([i start] [j 0])
          (when (fx<? i end)
            (vector-set! v j (string-ref s i))
            (loop (fx+ i 1) (fx+ j 1))))
        v)))

  (define (vector->string v . opts)
    (let* ([start (if (pair? opts) (car opts) 0)]
           [end (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) (vector-length v))]
           [r (make-string (fx- end start))])
      (let loop ([i start] [j 0])
        (when (fx<? i end)
          (string-set! r j (vector-ref v i))
          (loop (fx+ i 1) (fx+ j 1))))
      r))

  (define (reverse-list->string chars)
    (let* ([n (length chars)] [r (make-string n)])
      (let loop ([l chars] [i (fx- n 1)])
        (if (null? l)
          r
          (begin (string-set! r i (car l)) (loop (cdr l) (fx- i 1)))))))

  ;;; Selection

  (define (string-copy s . opts)
    (with-range 'string-copy s opts (start end)
      (let ([r (make-string (fx- end start))])
        (blit! r 0 s start end)
        r)))

  (define (string-take s n)
    (check-range 'string-take s 0 n)
    (%substring s 0 n))

  (define (string-drop s n)
    (check-range 'string-drop s n (string-length s))
    (%substring s n (string-length s)))

  (define (string-take-right s n)
    (let ([len (string-length s)])
      (check-range 'string-take-right s (fx- len n) len)
      (%substring s (fx- len n) len)))

  (define (string-drop-right s n)
    (let ([len (string-length s)])
      (check-range 'string-drop-right s 0 (fx- len n))
      (%substring s 0 (fx- len n))))

  ;; Pads on the left with CHAR, or drops characters on the left, to make LEN characters.
  (define (string-pad s len . opts)
    (let ([char (if (pair? opts) (car opts) #\space)]
          [opts (if (pair? opts) (cdr opts) '())])
      (with-range 'string-pad s opts (start end)
        (let ([n (fx- end start)])
          (if (fx>=? n len)
            (%substring s (fx- end len) end)
            (let ([r (make-string len char)])
              (blit! r (fx- len n) s start end)
              r))))))

  (define (string-pad-right s len . opts)
    (let ([char (if (pair? opts) (car opts) #\space)]
          [opts (if (pair? opts) (cdr opts) '())])
      (with-range 'string-pad-right s opts (start end)
        (let ([n (fx- end start)])
          (if (fx>=? n len)
            (%substring s start (fx+ start len))
            (let ([r (make-string len char)])
              (blit! r 0 s start end)
              r))))))

  (define (skip-left s pred start end)
    (let loop ([i start])
      (if (and (fx<? i end) (pred (string-ref s i))) (loop (fx+ i 1)) i)))

  (define (skip-right s pred start end)
    (let loop ([i end])
      (if (and (fx>? i start) (pred (string-ref s (fx- i 1)))) (loop (fx- i 1)) i)))

  (define (string-trim s . opts)
    (let ([pred (if (pair? opts) (car opts) char-whitespace?)]
          [opts (if (pair? opts) (cdr opts) '())])
      (with-range 'string-trim s opts (start end)
        (%substring s (skip-left s pred start end) end))))

  (define (string-trim-right s . opts)
    (let ([pred (if (pair? opts) (car opts) char-whitespace?)]
          [opts (if (pair? opts) (cdr opts) '())])
      (with-range 'string-trim-right s opts (start end)
        (%substring s start (skip-right s pred start end)))))

  (define (string-trim-both s . opts)
    (let ([pred (if (pair? opts) (car opts) char-whitespace?)]
          [opts (if (pair? opts) (cdr opts) '())])
      (with-range 'string-trim-both s opts (start end)
        (let ([i (skip-left s pred start end)])
          (%substring s i (skip-right s pred i end))))))

  ;;; Replacement

  ;; S1 with [start1, end1) replaced by S2[start2, end2).
  (define (string-replace s1 s2 start1 end1 . opts)
    (check-range 'string-replace s1 start1 end1)
    (with-range 'string-replace s2 opts (start2 end2)
      (let* ([n1 (string-length s1)]
             [n2 (fx- end2 start2)]
             [r (make-string (fx+ (fx- n1 (fx- end1 start1)) n2))])
        (blit! r 0 s1 0 start1)
        (blit! r start1 s2 start2 end2)
        (blit! r (fx+ start1 n2) s1 end1 n1)
        r)))

  ;;; Prefixes and suffixes

  ;; The optional START1 END1 START2 END2 of two strings.
  (define-syntax with-ranges
    (syntax-rules ()
      [(_ who s1 s2 opts (start1 end1 start2 end2) body ...)
       (let* ([o opts]
              [start1 (if (pair? o) (car o) 0)]
              [o (if (pair? o) (cdr o) '())]
              [end1 (if (pair? o) (car o) (string-length s1))]
              [o (if (pair? o) (cdr o) '())]
              [start2 (if (pair? o) (car o) 0)]
              [o (if (pair? o) (cdr o) '())]
              [end2 (if (pair? o) (car o) (string-length s2))])
         (check-range who s1 start1 end1)
         (check-range who s2 start2 end2)
         body ...)]))

  (define (%prefix-length s1 start1 end1 s2 start2 end2)
    (let ([n (fxmin (fx- end1 start1) (fx- end2 start2))])
      (let loop ([k 0])
        (if (and (fx<? k n)
                 (char=? (string-ref s1 (fx+ start1 k)) (string-ref s2 (fx+ start2 k))))
          (loop (fx+ k 1))
          k))))

  (define (%suffix-length s1 start1 end1 s2 start2 end2)
    (let ([n (fxmin (fx- end1 start1) (fx- end2 start2))])
      (let loop ([k 0])
        (if (and (fx<? k n)
                 (char=? (string-ref s1 (fx- end1 (fx+ k 1))) (string-ref s2 (fx- end2 (fx+ k 1)))))
          (loop (fx+ k 1))
          k))))

  (define (string-prefix-length s1 s2 . opts)
    (with-ranges 'string-prefix-length s1 s2 opts (start1 end1 start2 end2)
      (%prefix-length s1 start1 end1 s2 start2 end2)))

  (define (string-suffix-length s1 s2 . opts)
    (with-ranges 'string-suffix-length s1 s2 opts (start1 end1 start2 end2)
      (%suffix-length s1 start1 end1 s2 start2 end2)))

  (define (string-prefix? s1 s2 . opts)
    (with-ranges 'string-prefix? s1 s2 opts (start1 end1 start2 end2)
      (let ([n1 (fx- end1 start1)])
        (and (fx<=? n1 (fx- end2 start2))
             (fx=? n1 (%prefix-length s1 start1 end1 s2 start2 end2))))))

  (define (string-suffix? s1 s2 . opts)
    (with-ranges 'string-suffix? s1 s2 opts (start1 end1 start2 end2)
      (let ([n1 (fx- end1 start1)])
        (and (fx<=? n1 (fx- end2 start2))
             (fx=? n1 (%suffix-length s1 start1 end1 s2 start2 end2))))))

  ;;; Searching

  (define (string-index s pred . opts)
    (with-range 'string-index s opts (start end)
      (let loop ([i start])
        (cond
          [(fx=? i end) #f]
          [(pred (string-ref s i)) i]
          [else (loop (fx+ i 1))]))))

  (define (string-index-right s pred . opts)
    (with-range 'string-index-right s opts (start end)
      (let loop ([i (fx- end 1)])
        (cond
          [(fx<? i start) #f]
          [(pred (string-ref s i)) i]
          [else (loop (fx- i 1))]))))

  (define (string-skip s pred . opts)
    (with-range 'string-skip s opts (start end)
      (let loop ([i start])
        (cond
          [(fx=? i end) #f]
          [(pred (string-ref s i)) (loop (fx+ i 1))]
          [else i]))))

  (define (string-skip-right s pred . opts)
    (with-range 'string-skip-right s opts (start end)
      (let loop ([i (fx- end 1)])
        (cond
          [(fx<? i start) #f]
          [(pred (string-ref s i)) (loop (fx- i 1))]
          [else i]))))

  ;; Does S2[start2, end2) occur in S1 at I?
  (define (match-at? s1 i s2 start2 end2)
    (let loop ([j start2] [i i])
      (or (fx=? j end2)
          (and (char=? (string-ref s1 i) (string-ref s2 j))
               (loop (fx+ j 1) (fx+ i 1))))))

  (define (%contains s1 start1 end1 s2 start2 end2)
    (let ([m (fx- end2 start2)])
      (if (fx=? m 0)
        start1
        (let ([first (string-ref s2 start2)] [last (fx- end1 m)])
          (let loop ([i start1])
            (cond
              [(fx>? i last) #f]
              [(and (char=? (string-ref s1 i) first)
                    (match-at? s1 (fx+ i 1) s2 (fx+ start2 1) end2))
               i]
              [else (loop (fx+ i 1))]))))))

  (define (string-contains s1 s2 . opts)
    (with-ranges 'string-contains s1 s2 opts (start1 end1 start2 end2)
      (%contains s1 start1 end1 s2 start2 end2)))

  (define (string-contains-right s1 s2 . opts)
    (with-ranges 'string-contains-right s1 s2 opts (start1 end1 start2 end2)
      (let ([m (fx- end2 start2)])
        (let loop ([i (fx- end1 m)])
          (cond
            [(fx<? i start1) #f]
            [(match-at? s1 i s2 start2 end2) i]
            [else (loop (fx- i 1))])))))

  ;;; The whole string

  (define (string-concatenate strings)
    (let* ([n (let loop ([l strings] [n 0])
                (if (null? l) n (loop (cdr l) (fx+ n (string-length (car l))))))]
           [r (make-string n)])
      (let loop ([l strings] [at 0])
        (if (null? l)
          r
          (let ([len (string-length (car l))])
            (blit! r at (car l) 0 len)
            (loop (cdr l) (fx+ at len)))))))

  ;; (string-concatenate (reverse strings)) followed by FINAL[0, end).
  (define (string-concatenate-reverse strings . opts)
    (let* ([final (if (pair? opts) (car opts) "")]
           [end (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) (string-length final))]
           [n (let loop ([l strings] [n end])
                (if (null? l) n (loop (cdr l) (fx+ n (string-length (car l))))))]
           [r (make-string n)])
      (check-range 'string-concatenate-reverse final 0 end)
      (blit! r (fx- n end) final 0 end)
      (let loop ([l strings] [at (fx- n end)])
        (if (null? l)
          r
          (let* ([len (string-length (car l))] [at (fx- at len)])
            (blit! r at (car l) 0 len)
            (loop (cdr l) at))))))

  (define (string-join strings . opts)
    (let ([delim (if (pair? opts) (car opts) " ")]
          [grammar (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) 'infix)])
      (define (interleave l)
        (if (null? l) '() (cons delim (cons (car l) (interleave (cdr l))))))
      (case grammar
        [(infix)
         (if (null? strings) "" (string-concatenate (cons (car strings) (interleave (cdr strings)))))]
        [(strict-infix)
         (when (null? strings)
           (assertion-violation 'string-join "empty list with strict-infix grammar"))
         (string-concatenate (cons (car strings) (interleave (cdr strings))))]
        [(prefix) (string-concatenate (interleave strings))]
        [(suffix)
         (string-concatenate
           (let loop ([l strings])
             (if (null? l) '() (cons (car l) (cons delim (loop (cdr l)))))))]
        [else (assertion-violation 'string-join "invalid grammar" grammar)])))

  (define (string-fold kons knil s . opts)
    (with-range 'string-fold s opts (start end)
      (let loop ([i start] [acc knil])
        (if (fx<? i end) (loop (fx+ i 1) (kons (string-ref s i) acc)) acc))))

  (define (string-fold-right kons knil s . opts)
    (with-range 'string-fold-right s opts (start end)
      (let loop ([i (fx- end 1)] [acc knil])
        (if (fx>=? i start) (loop (fx- i 1) (kons (string-ref s i) acc)) acc))))

  (define (min-string-length strings)
    (let loop ([l strings] [n (greatest-fixnum)])
      (if (null? l) n (loop (cdr l) (fxmin n (string-length (car l)))))))

  (define (refs strings i)
    (if (null? strings) '() (cons (string-ref (car strings) i) (refs (cdr strings) i))))

  (define string-map
    (case-lambda
      [(proc s)
       (let* ([n (string-length s)] [r (make-string n)])
         (let loop ([i 0])
           (when (fx<? i n)
             (string-set! r i (proc (string-ref s i)))
             (loop (fx+ i 1))))
         r)]
      [(proc s . ss)
       (let* ([all (cons s ss)] [n (min-string-length all)] [r (make-string n)])
         (let loop ([i 0])
           (when (fx<? i n)
             (string-set! r i (apply proc (refs all i)))
             (loop (fx+ i 1))))
         r)]))

  (define string-for-each
    (case-lambda
      [(proc s)
       (let ([n (string-length s)])
         (let loop ([i 0])
           (when (fx<? i n)
             (proc (string-ref s i))
             (loop (fx+ i 1)))))]
      [(proc s . ss)
       (let* ([all (cons s ss)] [n (min-string-length all)])
         (let loop ([i 0])
           (when (fx<? i n)
             (apply proc (refs all i))
             (loop (fx+ i 1)))))]))

  (define (string-count s pred . opts)
    (with-range 'string-count s opts (start end)
      (let loop ([i start] [c 0])
        (if (fx<? i end)
          (loop (fx+ i 1) (if (pred (string-ref s i)) (fx+ c 1) c))
          c))))

  ;; The characters of S[start, end) for which (pred c) is KEEP?.
  (define (%filter who pred s opts keep?)
    (with-range who s opts (start end)
      (let ([buf (make-string (fx- end start))])
        (let loop ([i start] [j 0])
          (if (fx<? i end)
            (let ([c (string-ref s i)])
              (if (eq? keep? (not (not (pred c))))
                (begin (string-set! buf j c) (loop (fx+ i 1) (fx+ j 1)))
                (loop (fx+ i 1) j)))
            (if (fx=? j (string-length buf)) buf (substring buf 0 j)))))))

  (define (string-filter pred s . opts) (%filter 'string-filter pred s opts #t))
  (define (string-remove pred s . opts) (%filter 'string-remove pred s opts #f))

  ;;; Replication and splitting

  ;; Characters FROM to TO of S[start, end) repeated infinitely in both directions.
  (define (string-replicate s from to . opts)
    (with-range 'string-replicate s opts (start end)
      (let ([n (fx- end start)] [len (- to from)])
        (when (< len 0)
          (assertion-violation 'string-replicate "TO is before FROM" from to))
        (when (and (fx=? n 0) (> len 0))
          (assertion-violation 'string-replicate "empty string" s))
        (let ([r (make-string len)])
          (let loop ([k 0] [i (fx+ start (mod from (max n 1)))])
            (when (fx<? k len)
              (string-set! r k (string-ref s i))
              (let ([i (fx+ i 1)])
                (loop (fx+ k 1) (if (fx=? i end) start i)))))
          r))))

  (define (string-segment s k)
    (unless (and (fixnum? k) (fx>? k 0))
      (assertion-violation 'string-segment "the segment length must be positive" k))
    (let ([n (string-length s)])
      (let loop ([i 0] [acc '()])
        (if (fx>=? i n)
          (reverse acc)
          (let ([j (fxmin n (fx+ i k))])
            (loop j (cons (substring s i j) acc)))))))

  (define (string-split s delimiter . opts)
    (let* ([grammar (if (pair? opts) (car opts) 'infix)]
           [opts (if (pair? opts) (cdr opts) '())]
           [limit (if (pair? opts) (car opts) #f)]
           [opts (if (pair? opts) (cdr opts) '())])
      (unless (memq grammar '(infix strict-infix prefix suffix))
        (assertion-violation 'string-split "invalid grammar" grammar))
      (unless (or (not limit) (and (fixnum? limit) (fx>=? limit 0)))
        (assertion-violation 'string-split "invalid limit" limit))
      (with-range 'string-split s opts (start end)
        (let ([m (string-length delimiter)])
          (cond
            [(fx=? start end)
             (if (eq? grammar 'strict-infix)
               (assertion-violation 'string-split "empty string with strict-infix grammar" s)
               '())]
            [else
             (let* ([parts
                     (let loop ([i start] [splits 0] [acc '()])
                       (let ([j (cond
                                  [(and limit (fx>=? splits limit)) #f]
                                  [(fx=? m 0) (and (fx<? (fx+ i 1) end) (fx+ i 1))]
                                  [else (%contains s i end delimiter 0 m)])])
                         (if j
                           (loop (fx+ j m) (fx+ splits 1) (cons (substring s i j) acc))
                           (reverse (cons (substring s i end) acc)))))]
                    [parts (if (and (eq? grammar 'prefix) (pair? parts) (string-null? (car parts)))
                             (cdr parts)
                             parts)])
               (if (and (eq? grammar 'suffix) (pair? parts))
                 (let ([r (reverse parts)])
                   (if (string-null? (car r)) (reverse (cdr r)) parts))
                 parts))])))))

  ;;; Mutation

  (define string-fill!
    (case-lambda
      [(s c) (string-fill! s c 0 (string-length s))]
      [(s c start) (string-fill! s c start (string-length s))]
      [(s c start end)
       (check-range 'string-fill! s start end)
       (let loop ([i start])
         (when (fx<? i end)
           (string-set! s i c)
           (loop (fx+ i 1))))]))

  (define string-copy!
    (case-lambda
      [(to at from) (string-copy! to at from 0 (string-length from))]
      [(to at from start) (string-copy! to at from start (string-length from))]
      [(to at from start end)
       (check-range 'string-copy! from start end)
       (check-range 'string-copy! to at (fx+ at (fx- end start)))
       (if (and (eq? to from) (fx<? start at))
         (let loop ([i (fx- end 1)] [j (fx+ at (fx- end start 1))])
           (when (fx>=? i start)
             (string-set! to j (string-ref from i))
             (loop (fx- i 1) (fx- j 1))))
         (blit! to at from start end))])))
