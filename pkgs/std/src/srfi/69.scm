;;; SRFI 69: Basic hash tables.
;;;
;;; The tables are R6RS hashtables, so a SRFI 69 table can be passed to (rnrs hashtables),
;;; SRFI 125 and SRFI 126 procedures and back.
;;;
;;; Deviations from the specification:
;;;
;;; - `hash-by-identity` is `equal-hash` reduced to the bound. It agrees with `eq?` as long as
;;;   the object is not mutated between calls. `(make-hash-table eq? ...)` and
;;;   `(make-hash-table eqv? ...)` ignore the hash function and make an `eq?`/`eqv?` table,
;;;   which hashes by identity correctly.
;;; - `hash-table-copy` takes an optional MUTABLE? argument (default #t, so the copy is mutable
;;;   as SRFI 69 expects); with #f it makes an immutable R6RS copy.
;;; - `hash-table-hash-function` of a table made with a custom hash function returns the
;;;   one-argument wrapper the R6RS table holds, not the procedure that was passed.
(library (srfi 69)
  (export
    make-hash-table
    hash-table?
    alist->hash-table
    hash-table-equivalence-function
    hash-table-hash-function
    hash-table-ref
    hash-table-ref/default
    hash-table-set!
    hash-table-delete!
    hash-table-exists?
    hash-table-update!
    hash-table-update!/default
    hash-table-size
    hash-table-keys
    hash-table-values
    hash-table-walk
    hash-table-fold
    hash-table->alist
    hash-table-copy
    hash-table-merge!
    hash
    string-hash
    string-ci-hash
    hash-by-identity)
  (import
    (except (rnrs) string-hash string-ci-hash)
    (prefix (only (rnrs hashtables) string-hash string-ci-hash) r6rs:))

  ;; The bound of a hash value when the caller gives none.
  (define default-bound (greatest-fixnum))

  (define (reduce h bound)
    (mod (abs h) bound))

  (define hash
    (case-lambda
      [(obj) (reduce (equal-hash obj) default-bound)]
      [(obj bound) (reduce (equal-hash obj) bound)]))

  (define string-hash
    (case-lambda
      [(s) (reduce (r6rs:string-hash s) default-bound)]
      [(s bound) (reduce (r6rs:string-hash s) bound)]))

  (define string-ci-hash
    (case-lambda
      [(s) (reduce (r6rs:string-ci-hash s) default-bound)]
      [(s bound) (reduce (r6rs:string-ci-hash s) bound)]))

  (define hash-by-identity
    (case-lambda
      [(obj) (hash obj)]
      [(obj bound) (hash obj bound)]))

  ;; The hash function SRFI 69 associates with a well-known equivalence predicate.
  (define (default-hash-for equiv)
    (cond
      [(eq? equiv string=?) string-hash]
      [(eq? equiv string-ci=?) string-ci-hash]
      [else hash]))

  ;; A one-argument hash function for R6RS `make-hashtable`. SRFI 69 hash functions take an
  ;; optional bound; R6RS ones take one argument and may return any exact integer.
  (define (unary-hash h)
    (cond
      [(eq? h hash) equal-hash]
      [(eq? h string-hash) r6rs:string-hash]
      [(eq? h string-ci-hash) r6rs:string-ci-hash]
      [else (lambda (key) (h key))]))

  (define (%make equiv h)
    (cond
      [(eq? equiv eq?) (make-eq-hashtable)]
      [(eq? equiv eqv?) (make-eqv-hashtable)]
      [else (make-hashtable (unary-hash (or h (default-hash-for equiv))) equiv)]))

  (define make-hash-table
    (case-lambda
      [() (%make equal? #f)]
      [(equiv) (%make equiv #f)]
      [(equiv h . _) (%make equiv h)]))

  (define (hash-table? obj) (hashtable? obj))

  (define (alist->hash-table alist . rest)
    (let ([table (apply make-hash-table rest)])
      ;; The first association of a key wins, as in an alist lookup.
      (for-each
        (lambda (entry)
          (unless (hashtable-contains? table (car entry))
            (hashtable-set! table (car entry) (cdr entry))))
        alist)
      table))

  (define (hash-table-equivalence-function table)
    (hashtable-equivalence-function table))

  (define (hash-table-hash-function table)
    (or (hashtable-hash-function table)
        (let ([equiv (hashtable-equivalence-function table)])
          (if (eq? equiv eq?) hash-by-identity hash))))

  ;; A unique object that no table holds as a value.
  (define missing (list 'missing))

  (define (missing-key who table key)
    (assertion-violation who "no value associated with key" table key))

  (define hash-table-ref
    (case-lambda
      [(table key)
       (let ([v (hashtable-ref table key missing)])
         (if (eq? v missing) (missing-key 'hash-table-ref table key) v))]
      [(table key fail)
       (let ([v (hashtable-ref table key missing)])
         (if (eq? v missing) (fail) v))]
      [(table key fail succeed)
       (let ([v (hashtable-ref table key missing)])
         (if (eq? v missing) (fail) (succeed v)))]))

  (define (hash-table-ref/default table key default)
    (hashtable-ref table key default))

  (define (hash-table-set! table key value)
    (hashtable-set! table key value))

  (define (hash-table-delete! table key)
    (hashtable-delete! table key))

  (define (hash-table-exists? table key)
    (hashtable-contains? table key))

  (define hash-table-update!
    (case-lambda
      [(table key proc)
       (let ([v (hashtable-ref table key missing)])
         (if (eq? v missing)
           (missing-key 'hash-table-update! table key)
           (hashtable-set! table key (proc v))))]
      [(table key proc fail)
       (let ([v (hashtable-ref table key missing)])
         (hashtable-set! table key (proc (if (eq? v missing) (fail) v))))]))

  (define (hash-table-update!/default table key proc default)
    (hashtable-set! table key (proc (hashtable-ref table key default))))

  (define (hash-table-size table)
    (hashtable-size table))

  (define (hash-table-keys table)
    (vector->list (hashtable-keys table)))

  (define (hash-table-values table)
    (let-values ([(keys vals) (hashtable-entries table)])
      (vector->list vals)))

  (define (hash-table-walk table proc)
    (let-values ([(keys vals) (hashtable-entries table)])
      (let ([n (vector-length keys)])
        (let loop ([i 0])
          (when (fx<? i n)
            (proc (vector-ref keys i) (vector-ref vals i))
            (loop (fx+ i 1)))))))

  (define (hash-table-fold table kons knil)
    (let-values ([(keys vals) (hashtable-entries table)])
      (let ([n (vector-length keys)])
        (let loop ([i 0] [acc knil])
          (if (fx<? i n)
            (loop (fx+ i 1) (kons (vector-ref keys i) (vector-ref vals i) acc))
            acc)))))

  (define (hash-table->alist table)
    (hash-table-fold table (lambda (k v acc) (cons (cons k v) acc)) '()))

  (define hash-table-copy
    (case-lambda
      [(table) (hashtable-copy table #t)]
      [(table mutable?) (hashtable-copy table mutable?)]))

  (define (hash-table-merge! table1 table2)
    (hash-table-walk table2 (lambda (k v) (hashtable-set! table1 k v)))
    table1))
