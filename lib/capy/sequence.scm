#!r6rs
;; Racket-compatible sequences and comprehensions for CapyScheme.
;;
;; New sequence types implement the single sequence->iterator generic.
;; Iterators add deterministic close! to Racket's generation model. A
;; continuation cannot re-enter an iterator scope after it has been closed.

(library (capy sequence)
  (export
    ;; sequence protocol
    sequence? sequence->iterator
    make-iterator iterator? iterator-more? iterator-next! iterator-close!
    make-iterator-sequence
    sequence-generate sequence-generate*

    ;; sequence constructors
    empty-sequence
    make-range-sequence range-sequence?
    make-naturals-sequence naturals-sequence?
    in-range in-inclusive-range in-naturals
    in-list in-vector in-string in-bytes
    in-value in-producer in-indexed in-sequences in-cycle in-parallel
    in-hash in-hash-keys in-hash-values in-hash-pairs
    in-hash-pairs/key+value
    in-values-sequence in-values*-sequence stop-before stop-after
    in-slice

    ;; sequence operations
    sequence->list sequence-length sequence-ref sequence-tail
    sequence-append sequence-map sequence-filter sequence-add-between
    sequence-andmap sequence-ormap sequence-for-each sequence-fold
    sequence-count

    ;; comprehensions
    for for*
    for/fold for*/fold
    for/list for*/list
    for/lists for*/lists
    for/vector for*/vector
    for/and for*/and
    for/or for*/or
    for/sum for*/sum
    for/product for*/product
    for/first for*/first
    for/last for*/last)

  (import (rnrs)
          (capy))

  ;; ------------------------------------------------------------------
  ;; Runtime iterator protocol
  ;; ------------------------------------------------------------------

  ;; NEXT! returns all values belonging to one sequence element.
  (define-record-type (iterator %make-iterator iterator?)
    (fields
      (immutable more? %iterator-more?)
      (immutable next! %iterator-next!)
      (immutable close! %iterator-close!)))

  (define (make-iterator more? next! close!)
    (unless (and (procedure? more?) (procedure? next!) (procedure? close!))
      (assertion-violation 'make-iterator
        "expected more?, next!, and close! procedures"
        more? next! close!))
    (let ((closed? #f))
      (%make-iterator
        (lambda () (and (not closed?) (more?)))
        (lambda ()
          (when closed?
            (assertion-violation 'iterator-next! "iterator is closed"))
          (next!))
        (lambda ()
          (unless closed?
            (set! closed? #t)
            (close!))))))

  (define (iterator-more? iterator)
    ((%iterator-more? iterator)))

  (define (iterator-next! iterator)
    ((%iterator-next! iterator)))

  (define (iterator-close! iterator)
    ((%iterator-close! iterator)))

  (define-generic sequence->iterator)

  (define-class iterator-sequence ()
    ((open #:accessor iterator-sequence-open)))

  (define-class range-sequence ()
    ((start #:accessor range-start)
     (end #:accessor range-end)
     (step #:accessor range-step)))

  (define-class naturals-sequence ()
    ((start #:accessor naturals-start)
     (step #:accessor naturals-step)))

  (define (range-sequence? object)
    (is-a? object range-sequence))

  (define (naturals-sequence? object)
    (is-a? object naturals-sequence))

  (define (make-iterator-sequence open)
    (unless (procedure? open)
      (assertion-violation 'make-iterator-sequence
        "expected an iterator-producing procedure" open))
    (make iterator-sequence #:open open))

  (define-method (sequence->iterator (sequence iterator-sequence))
    (let ((iterator ((iterator-sequence-open sequence))))
      (unless (iterator? iterator)
        (assertion-violation 'sequence->iterator
          "sequence opener did not return an iterator" iterator))
      iterator))

  (define (sequence? object)
    (cond
      ((number? object)
       (and (exact? object) (integer? object) (not (negative? object))))
      ((pair? object) (list? object))
      (else
        (not (null? (compute-applicable-methods
                      sequence->iterator
                      (list object)))))))

  (define (make-range-sequence start end step)
    (unless (and (real? start) (real? end) (real? step))
      (assertion-violation 'in-range
        "expected real start, end, and step"
        start end step))
    (make range-sequence #:start start #:end end #:step step))

  (define (make-naturals-sequence start)
    (unless (and (exact? start) (integer? start) (not (negative? start)))
      (assertion-violation 'in-naturals
        "expected an exact nonnegative integer" start))
    (make naturals-sequence #:start start #:step 1))

  (define in-range
    (case-lambda
      ((end)         (make-range-sequence 0 end 1))
      ((start end)   (make-range-sequence start end 1))
      ((start end step) (make-range-sequence start end step))))

  (define in-naturals
    (case-lambda
      (()      (make-naturals-sequence 0))
      ((start) (make-naturals-sequence start))))

  (define in-inclusive-range
    (case-lambda
      ((start end)
       (in-inclusive-range start end 1))
      ((start end step)
       (unless (and (real? start) (real? end) (real? step))
         (assertion-violation 'in-inclusive-range
           "expected real start, end, and step"
           start end step))
       (make-iterator-sequence
         (lambda ()
           (let ((current start))
             (make-iterator
               (if (negative? step)
                   (lambda () (>= current end))
                   (lambda () (<= current end)))
               (lambda ()
                 (let ((value current))
                   (set! current (+ current step))
                   value))
               (lambda () (values)))))))))

  (define (in-list xs)
    (unless (list? xs)
      (assertion-violation 'in-list "expected a list" xs))
    xs)

  (define (%in-indexed-container who object size-of ref args)
    (let* ((size (size-of object))
           (start (if (null? args) 0 (car args)))
           (stop (if (or (null? args) (null? (cdr args)))
                     size
                     (cadr args)))
           (step (if (or (null? args)
                         (null? (cdr args))
                         (null? (cddr args)))
                     1
                     (caddr args))))
      (unless (and (<= (length args) 3)
                   (exact? start) (integer? start)
                   (exact? stop) (integer? stop)
                   (exact? step) (integer? step) (not (zero? step))
                   (<= 0 start size) (<= 0 stop size))
        (assertion-violation who
          "expected valid start, stop, and non-zero step"
          object start stop step))
      (make-iterator-sequence
        (lambda ()
          (let ((index start))
            (make-iterator
              (if (positive? step)
                  (lambda () (< index stop))
                  (lambda () (> index stop)))
              (lambda ()
                (let ((value (ref object index)))
                  (set! index (+ index step))
                  value))
              (lambda () (set! index stop))))))))

  (define (in-vector vector . args)
    (%in-indexed-container
      'in-vector vector vector-length vector-ref args))

  (define (in-string string . args)
    (%in-indexed-container
      'in-string string string-length string-ref args))

  (define (in-bytes bytes . args)
    (%in-indexed-container
      'in-bytes bytes bytevector-length bytevector-u8-ref args))

  (define-method (sequence->iterator (xs <pair>))
    (unless (list? xs)
      (assertion-violation 'sequence->iterator
        "pair sequence must be a proper list" xs))
    (let ((state xs))
      (make-iterator
        (lambda () (pair? state))
        (lambda ()
          (let ((x (car state)))
            (set! state (cdr state))
            x))
        (lambda () (set! state '())))))

  (define-method (sequence->iterator (xs <null>))
    (make-iterator
      (lambda () #f)
      (lambda () (assertion-violation 'sequence->iterator "empty iterator"))
      (lambda () (values))))

  (define-method (sequence->iterator (v <vector>))
    (let ((i 0) (n (vector-length v)))
      (make-iterator
        (lambda () (< i n))
        (lambda ()
          (let ((x (vector-ref v i)))
            (set! i (+ i 1))
            x))
        (lambda () (set! i n)))))

  (define-method (sequence->iterator (v <bytevector>))
    (sequence->iterator (in-bytes v)))

  (define-method (sequence->iterator (s <string>))
    (let ((i 0) (n (string-length s)))
      (make-iterator
        (lambda () (< i n))
        (lambda ()
          (let ((x (string-ref s i)))
            (set! i (+ i 1))
            x))
        (lambda () (set! i n)))))

  (define-method (sequence->iterator (table <hash-table>))
    (let-values (((keys values*) (hashtable-entries table)))
      (let ((index 0)
            (size (vector-length keys)))
        (make-iterator
          (lambda () (< index size))
          (lambda ()
            (let ((current index))
              (set! index (+ index 1))
              (values
                (vector-ref keys current)
                (vector-ref values* current))))
          (lambda () (set! index size))))))

  ;; Racket treats an exact nonnegative integer as 0 .. n-1.
  (define-method (sequence->iterator (n <number>))
    (unless (and (exact? n) (integer? n) (not (negative? n)))
      (assertion-violation 'sequence->iterator
        "number sequence must be an exact nonnegative integer" n))
    (let ((i 0))
      (make-iterator
        (lambda () (< i n))
        (lambda ()
          (let ((x i))
            (set! i (+ i 1))
            x))
        (lambda () (set! i n)))))

  (define-method (sequence->iterator (r range-sequence))
    (let* ((i (range-start r))
           (end (range-end r))
           (step (range-step r))
           (more? (if (negative? step)
                      (lambda () (> i end))
                      (lambda () (< i end)))))
      (make-iterator
        more?
        (lambda ()
          (let ((x i))
            (set! i (+ i step))
            x))
        (lambda () (set! i end)))))

  (define-method (sequence->iterator (r naturals-sequence))
    (let ((i (naturals-start r))
          (step (naturals-step r)))
      (make-iterator
        (lambda () #t)
        (lambda ()
          (let ((x i))
            (set! i (+ i step))
            x))
        (lambda () (values)))))

  (define empty-sequence '())

  (define (sequence-generate sequence)
    (let ((iterator (sequence->iterator sequence)))
      (let ((more?
              (lambda ()
                (let ((more? (iterator-more? iterator)))
                  (unless more? (iterator-close! iterator))
                  more?))))
        (values
          more?
          (lambda ()
            (unless (more?)
              (assertion-violation 'sequence-generate
                "sequence has no more elements"))
            (iterator-next! iterator))))))

  (define (sequence-generate* sequence)
    (let ((iterator (sequence->iterator sequence))
          (finished? #f))
      (letrec ((next
                 (lambda ()
                   (when finished?
                     (assertion-violation 'sequence-generate*
                       "sequence has no more elements"))
                   (if (iterator-more? iterator)
                       (values
                         (call-with-values
                           (lambda () (iterator-next! iterator))
                           list)
                         next)
                       (begin
                         (set! finished? #t)
                         (iterator-close! iterator)
                         (values #f next))))))
        (next))))

  (define (in-value value)
    (list value))

  (define (in-producer producer . stop+args)
    (unless (procedure? producer)
      (assertion-violation 'in-producer "expected a procedure" producer))
    (make-iterator-sequence
      (lambda ()
        (let ((done? #f)
              (ready? #f)
              (values* '()))
          (define (fill!)
            (unless (or done? ready?)
              (set! values*
                (call-with-values
                  (lambda ()
                    (apply producer
                      (if (null? stop+args) '() (cdr stop+args))))
                  list))
              (if (and
                    (pair? stop+args)
                    (let ((stop (car stop+args)))
                      (if (procedure? stop)
                          (apply stop values*)
                          (and (= (length values*) 1)
                               (eq? (car values*) stop)))))
                  (set! done? #t)
                  (set! ready? #t))))
          (make-iterator
            (lambda () (fill!) (not done?))
            (lambda ()
              (fill!)
              (set! ready? #f)
              (apply values values*))
            (lambda () (set! done? #t)))))))

  (define (in-indexed sequence)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence))
              (index 0))
          (make-iterator
            (lambda () (iterator-more? iterator))
            (lambda ()
              (let ((value (iterator-next! iterator))
                    (current index))
                (set! index (+ index 1))
                (values value current)))
            (lambda () (iterator-close! iterator)))))))

  (define (in-sequences . sequences)
    (if (= (length sequences) 1)
        (car sequences)
        (make-iterator-sequence
          (lambda ()
            (let ((remaining sequences)
                  (current #f))
              (define (advance!)
                (let loop ()
                  (when (and (or (not current)
                                 (not (iterator-more? current)))
                             (pair? remaining))
                    (when current (iterator-close! current))
                    (set! current
                      (sequence->iterator (car remaining)))
                    (set! remaining (cdr remaining))
                    (loop))))
              (make-iterator
                (lambda ()
                  (advance!)
                  (and current (iterator-more? current)))
                (lambda ()
                  (advance!)
                  (iterator-next! current))
                (lambda ()
                  (when current (iterator-close! current))
                  (set! remaining '()))))))))

  (define (in-cycle . sequences)
    (if (null? sequences)
        empty-sequence
        (make-iterator-sequence
          (lambda ()
            (let ((remaining sequences)
                  (current #f)
                  (produced? #f)
                  (done? #f))
              (define (advance!)
                (let loop ()
                  (when (and (not done?)
                             (or (not current)
                                 (not (iterator-more? current))))
                    (when current (iterator-close! current))
                    (when (null? remaining)
                      (if produced?
                          (begin
                            (set! remaining sequences)
                            (set! produced? #f))
                          (set! done? #t)))
                    (unless done?
                      (set! current
                        (sequence->iterator (car remaining)))
                      (set! remaining (cdr remaining))
                      (loop)))))
              (make-iterator
                (lambda () (advance!) (not done?))
                (lambda ()
                  (advance!)
                  (set! produced? #t)
                  (iterator-next! current))
                (lambda ()
                  (when current (iterator-close! current))
                  (set! done? #t))))))))

  (define (in-parallel . sequences)
    (make-iterator-sequence
      (lambda ()
        (let ((iterators (map sequence->iterator sequences)))
          (make-iterator
            (lambda ()
              (and (pair? iterators)
                   (for-all iterator-more? iterators)))
            (lambda ()
              (apply values (map iterator-next! iterators)))
            (lambda ()
              (for-each iterator-close! iterators)))))))

  (define (in-hash table)
    (unless (hashtable? table)
      (assertion-violation 'in-hash "expected a hash table" table))
    table)

  (define (in-hash-keys table)
    (sequence-map (lambda (key value) key) (in-hash table)))

  (define (in-hash-values table)
    (sequence-map (lambda (key value) value) (in-hash table)))

  (define (in-hash-pairs table)
    (sequence-map cons (in-hash table)))

  (define (in-hash-pairs/key+value table)
    (sequence-map
      (lambda (key value)
        (values (cons key value) key value))
      (in-hash table)))

  (define (in-values-sequence sequence)
    (sequence-map
      (lambda values* values*)
      sequence))

  (define (in-values*-sequence sequence)
    (sequence-map
      (lambda values*
        (if (= (length values*) 1)
            (car values*)
            values*))
      sequence))

  (define (stop-before sequence predicate)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence))
              (ready? #f)
              (done? #f)
              (values* '()))
          (define (fill!)
            (when (and (not ready?) (not done?))
              (if (iterator-more? iterator)
                  (begin
                    (set! values*
                      (call-with-values
                        (lambda () (iterator-next! iterator))
                        list))
                    (if (apply predicate values*)
                        (set! done? #t)
                        (set! ready? #t)))
                  (set! done? #t))))
          (make-iterator
            (lambda () (fill!) ready?)
            (lambda ()
              (fill!)
              (set! ready? #f)
              (apply values values*))
            (lambda () (iterator-close! iterator)))))))

  (define (stop-after sequence predicate)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence))
              (done? #f))
          (make-iterator
            (lambda () (and (not done?) (iterator-more? iterator)))
            (lambda ()
              (call-with-values
                (lambda () (iterator-next! iterator))
                (lambda values*
                  (when (apply predicate values*) (set! done? #t))
                  (apply values values*))))
            (lambda () (iterator-close! iterator)))))))

  (define (in-slice size sequence)
    (unless (and (exact? size) (integer? size) (positive? size))
      (assertion-violation 'in-slice
        "expected an exact positive slice length" size))
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence)))
          (make-iterator
            (lambda () (iterator-more? iterator))
            (lambda ()
              (let loop ((remaining size) (result '()))
                (if (or (zero? remaining)
                        (not (iterator-more? iterator)))
                    (reverse result)
                    (loop
                      (- remaining 1)
                      (cons (iterator-next! iterator) result)))))
            (lambda () (iterator-close! iterator)))))))

  (define (sequence->list sequence)
    (reverse
      (sequence-fold
        (lambda (result value) (cons value result))
        '()
        sequence)))

  (define (sequence-length sequence)
    (sequence-fold (lambda (count . ignored) (+ count 1)) 0 sequence))

  (define (sequence-ref sequence index)
    (unless (and (exact? index) (integer? index) (not (negative? index)))
      (assertion-violation 'sequence-ref
        "expected an exact nonnegative index" index))
    (call-with-current-continuation
      (lambda (return)
        (let ((current 0)
              (iterator (sequence->iterator sequence)))
          (dynamic-wind
            (lambda () (values))
            (lambda ()
              (let loop ()
                (unless (iterator-more? iterator)
                  (assertion-violation 'sequence-ref
                    "index is out of range" sequence index))
                (if (= current index)
                    (call-with-values
                      (lambda () (iterator-next! iterator))
                      return)
                    (begin
                      (iterator-next! iterator)
                      (set! current (+ current 1))
                      (loop)))))
            (lambda () (iterator-close! iterator)))))))

  (define (sequence-tail sequence count)
    (unless (and (exact? count) (integer? count) (not (negative? count)))
      (assertion-violation 'sequence-tail
        "expected an exact nonnegative count" count))
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence)))
          (let loop ((remaining count))
            (when (positive? remaining)
              (unless (iterator-more? iterator)
                (iterator-close! iterator)
                (assertion-violation 'sequence-tail
                  "sequence has fewer elements than count"
                  sequence count))
              (iterator-next! iterator)
              (loop (- remaining 1))))
          iterator))))

  (define (sequence-append . sequences)
    (apply in-sequences sequences))

  (define (sequence-map procedure sequence)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence)))
          (make-iterator
            (lambda () (iterator-more? iterator))
            (lambda ()
              (call-with-values
                (lambda () (iterator-next! iterator))
                procedure))
            (lambda () (iterator-close! iterator)))))))

  (define (sequence-filter predicate sequence)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence))
              (ready? #f)
              (values* '()))
          (define (fill!)
            (let loop ()
              (when (and (not ready?) (iterator-more? iterator))
                (set! values*
                  (call-with-values
                    (lambda () (iterator-next! iterator))
                    list))
                (if (apply predicate values*)
                    (set! ready? #t)
                    (loop)))))
          (make-iterator
            (lambda () (fill!) ready?)
            (lambda ()
              (fill!)
              (set! ready? #f)
              (apply values values*))
            (lambda () (iterator-close! iterator)))))))

  (define (sequence-add-between sequence element)
    (make-iterator-sequence
      (lambda ()
        (let ((iterator (sequence->iterator sequence))
              (between? #f))
          (make-iterator
            (lambda () (or between? (iterator-more? iterator)))
            (lambda ()
              (if between?
                  (begin (set! between? #f) element)
                  (call-with-values
                    (lambda () (iterator-next! iterator))
                    (lambda values*
                      (when (iterator-more? iterator)
                        (set! between? #t))
                      (apply values values*)))))
            (lambda () (iterator-close! iterator)))))))

  (define (sequence-for-each procedure sequence)
    (let ((iterator (sequence->iterator sequence)))
      (dynamic-wind
        (lambda () (values))
        (lambda ()
          (let loop ()
            (when (iterator-more? iterator)
              (call-with-values
                (lambda () (iterator-next! iterator))
                procedure)
              (loop))))
        (lambda () (iterator-close! iterator)))))

  (define (sequence-fold procedure initial sequence)
    (let ((result initial))
      (sequence-for-each
        (lambda values*
          (set! result (apply procedure (cons result values*))))
        sequence)
      result))

  (define (sequence-count predicate sequence)
    (sequence-fold
      (lambda (count . values*)
        (if (apply predicate values*) (+ count 1) count))
      0
      sequence))

  (define (sequence-andmap predicate sequence)
    (call-with-current-continuation
      (lambda (return)
        (let ((result #t))
          (sequence-for-each
            (lambda values*
              (set! result (apply predicate values*))
              (unless result (return #f)))
            sequence)
          result))))

  (define (sequence-ormap predicate sequence)
    (call-with-current-continuation
      (lambda (return)
        (sequence-for-each
          (lambda values*
            (let ((result (apply predicate values*)))
              (when result (return result))))
          sequence)
        #f)))

  ;; ------------------------------------------------------------------
  ;; Expansion engine
  ;; ------------------------------------------------------------------
  ;;
  ;; EXPAND-CLAUSES emits code which calls EMIT for each accepted tuple.
  ;;
  ;; In ordinary `for`, consecutive binding clauses form one parallel
  ;; (zip/shortest) iteration group. A guard/do/break/final boundary ends
  ;; that group. `for*` inserts an internal nesting boundary after every
  ;; binding, making every sequence loop nested.
  ;;
  ;; Each sequence expression is evaluated once per enclosing iteration.
  ;; Iterators are closed with dynamic-wind, including non-local exits.

  (define-syntax %for/run
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))

      (define (all-identifiers? xs)
        (or (null? xs)
            (and (identifier? (car xs))
                 (all-identifiers? (cdr xs)))))

      (define (id-list lhs who)
        (syntax-case lhs ()
          (id
           (identifier? #'id)
           (list #'id))
          ((id ...)
           (let ((ids (stx->list #'(id ...))))
             (if (all-identifiers? ids)
                 ids
                 (syntax-violation
                   who "invalid sequence identifiers" stx lhs))))
          (_
           (syntax-violation who "invalid sequence binding" stx lhs))))

      (define (binding-clause? c)
        (syntax-case c ()
          ((lhs rhs) #t)
          (_ #f)))

      (define (take-bindings cs)
        (let loop ((rest cs) (rev '()))
          (if (and (pair? rest) (binding-clause? (car rest)))
              (loop (cdr rest) (cons (car rest) rev))
              (values (reverse rev) rest))))

      (define (make-temp ctx prefix)
        ;; PREFIX is documentation only; R6RS hygienic temporaries do not
        ;; require printable names.
        (car (generate-temporaries (list ctx))))

      ;; Bind VALUES returned by NEXT! to one or several ids.
      (define (bind-next ids iter body)
        (with-syntax (((id ...) ids)
                      (it iter)
                      (body body))
          #'(call-with-values
              (lambda () (iterator-next! it))
              (lambda (id ...) body))))

      (define (expand-parallel group rest emit abort final?)
        (let* ((iters (map (lambda (x) (make-temp x "iter-")) group))
               (loops (make-temp stx "loop-"))
               (bindings
                 (map (lambda (cl it)
                        (syntax-case cl ()
                          ((lhs rhs)
                           #`(#,it (sequence->iterator rhs)))))
                      group iters))
               (mores
                 (map (lambda (it) #`(iterator-more? #,it)) iters))
               (close-forms
                 (map (lambda (it) #`(iterator-close! #,it)) iters))
               (inner (expand-clauses rest emit abort final?)))
          ;; Wrap NEXT calls from right to left so all produced ids are in
          ;; scope in INNER. More? checks happen before any NEXT! call.
          (let ((step
                  (let loop ((gs (reverse group))
                             (is (reverse iters))
                             (body inner))
                    (if (null? gs)
                        body
                        (syntax-case (car gs) ()
                          ((lhs rhs)
                           (loop (cdr gs) (cdr is)
                             (bind-next
                               (id-list #'lhs '%for/run)
                               (car is)
                               body))))))))
            (with-syntax ((((it init) ...) bindings)
                          ((more ...) mores)
                          ((close ...) close-forms)
                          (loop-id loops)
                          (step step))
              #'(let* ((it init) ...)
                  (dynamic-wind
                    (lambda () (values))
                    (lambda ()
                      (let loop-id ()
                        (when (and more ...)
                          step
                          (loop-id))))
                    (lambda () close ...)))))))

      (define (expand-clauses cs emit abort final?)
        (if (null? cs)
            emit
            (with-syntax (((clause ...) cs))
              (syntax-case #'(clause ...) ()
                ((#:when test rest ...)
                 #`(when test
                     #,(expand-clauses
                         (stx->list #'(rest ...))
                         emit abort final?)))
                ((#:unless test rest ...)
                 #`(unless test
                     #,(expand-clauses
                         (stx->list #'(rest ...))
                         emit abort final?)))
                ((#:break test rest ...)
                 #`(if test
                       (#,abort)
                       #,(expand-clauses
                           (stx->list #'(rest ...))
                           emit abort final?)))
                ((#:final test rest ...)
                 ;; FINAL permits this body, but no subsequent outer tuple.
                 #`(let ((#,final? (or #,final? test)))
                     #,(expand-clauses
                         (stx->list #'(rest ...))
                         emit abort final?)
                     ;; Later empty sequences and guards can prevent EMIT.
                     (when #,final? (#,abort))))
                ((#:do (form ...) rest ...)
                 #`(let ()
                     form ...
                     #,(expand-clauses
                         (stx->list #'(rest ...))
                         emit abort final?)))
                (_
                 (call-with-values
                   (lambda () (take-bindings cs))
                   (lambda (group rest)
                     (if (null? group)
                         (syntax-violation
                           '%for/run "invalid for clause" stx (car cs))
                         (expand-parallel
                           group rest emit abort final?)))))))))

      (syntax-case stx ()
        ((_ (clause ...) emit-expr)
         (let ((abort (make-temp stx "abort-"))
               (final? (make-temp stx "final?-")))
           (with-syntax ((abort abort)
                         (final? final?)
                         (run
                           (expand-clauses
                             (stx->list #'(clause ...))
                             #`(begin emit-expr
                                     (when #,final? (#,abort)))
                             abort
                             final?)))
             #'(call-with-current-continuation
                 (lambda (abort)
                   (let ((final? #f))
                     run)))))))))

  ;; Transform each binding into a singleton group by inserting `(when #t)`.
  (define-syntax %for*/run
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))
      (define (binding? c)
        (syntax-case c ()
          ((lhs rhs) #t)
          (_ #f)))
      (syntax-case stx ()
        ((_ (clause ...) emit)
         (let loop ((cs (stx->list #'(clause ...))) (out '()))
           (if (null? cs)
               (with-syntax (((x ...) (reverse out)))
                 #'(%for/run (x ...) emit))
               (let ((c (car cs)))
                 (if (binding? c)
                     (loop
                       (cdr cs)
                       (cons #'#t (cons #'#:when (cons c out))))
                     (loop (cdr cs) (cons c out))))))))))

  ;; ------------------------------------------------------------------
  ;; Public forms
  ;; ------------------------------------------------------------------

  (define-syntax for
    (syntax-rules ()
      ((_ (clause ...) body1 body2 ...)
       (%for/run (clause ...) (begin body1 body2 ... (values))))))

  (define-syntax for*
    (syntax-rules ()
      ((_ (clause ...) body1 body2 ...)
       (%for*/run (clause ...) (begin body1 body2 ... (values))))))

  ;; Handwritten because each accumulator needs a generated temporary.
  (define-syntax for/fold
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))
      (define (expand accs inits result clauses bodies runner)
        (let ((news (generate-temporaries accs)))
          (with-syntax (((acc ...) accs)
                        ((init ...) inits)
                        ((new ...) news)
                        ((clause ...) clauses)
                        ((body ...) bodies)
                        (result result)
                        (runner runner))
            #'(let ((acc init) ...)
                (runner (clause ...)
                  (call-with-values
                    (lambda () body ...)
                    (lambda (new ...)
                      (set! acc new) ...)))
                result))))
      (syntax-case stx ()
        ((_ ([acc init] ... #:result result-expr)
            (clause ...) body ...)
         (expand (stx->list #'(acc ...))
                 (stx->list #'(init ...))
                 #'result-expr
                 (stx->list #'(clause ...))
                 (stx->list #'(body ...))
                 #'%for/run))
        ((_ ([acc init] ...) (clause ...) body ...)
         (expand (stx->list #'(acc ...))
                 (stx->list #'(init ...))
                 #'(values acc ...)
                 (stx->list #'(clause ...))
                 (stx->list #'(body ...))
                 #'%for/run)))))

  (define-syntax for*/fold
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))
      (define (expand accs inits result clauses bodies)
        (let ((news (generate-temporaries accs)))
          (with-syntax (((acc ...) accs)
                        ((init ...) inits)
                        ((new ...) news)
                        ((clause ...) clauses)
                        ((body ...) bodies)
                        (result result))
            #'(let ((acc init) ...)
                (%for*/run (clause ...)
                  (call-with-values
                    (lambda () body ...)
                    (lambda (new ...)
                      (set! acc new) ...)))
                result))))
      (syntax-case stx ()
        ((_ ([acc init] ... #:result result-expr)
            (clause ...) body ...)
         (expand (stx->list #'(acc ...))
                 (stx->list #'(init ...))
                 #'result-expr
                 (stx->list #'(clause ...))
                 (stx->list #'(body ...))))
        ((_ ([acc init] ...) (clause ...) body ...)
         (expand (stx->list #'(acc ...))
                 (stx->list #'(init ...))
                 #'(values acc ...)
                 (stx->list #'(clause ...))
                 (stx->list #'(body ...)))))))

  (define-syntax for/list
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (reverse
         (for/fold ((out '())) (clause ...)
           (cons (begin body ...) out))))))

  (define-syntax for*/list
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (reverse
         (for*/fold ((out '())) (clause ...)
           (cons (begin body ...) out))))))

  (define-syntax for/lists
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))
      (define (expand ids result clauses bodies runner)
        (let ((values* (generate-temporaries ids)))
          (with-syntax (((id ...) ids)
                        ((value ...) values*)
                        ((clause ...) clauses)
                        ((body ...) bodies)
                        (result result)
                        (runner runner))
            #'(for/fold ((id '()) ...
                         #:result
                         (let ((id (reverse id)) ...)
                           result))
                        (clause ...)
                (call-with-values
                  (lambda () body ...)
                  (lambda (value ...)
                    (values (cons value id) ...)))))))
      (syntax-case stx ()
        ((_ (id ... #:result result) (clause ...) body ...)
         (expand
           (stx->list #'(id ...))
           #'result
           (stx->list #'(clause ...))
           (stx->list #'(body ...))
           #'%for/run))
        ((_ (id ...) (clause ...) body ...)
         (expand
           (stx->list #'(id ...))
           #'(values id ...)
           (stx->list #'(clause ...))
           (stx->list #'(body ...))
           #'%for/run)))))

  (define-syntax for*/lists
    (lambda (stx)
      (define (stx->list x)
        (syntax-case x ()
          (() '())
          ((a . d) (cons #'a (stx->list #'d)))
          (_ #f)))
      (define (expand ids result clauses bodies)
        (let ((values* (generate-temporaries ids)))
          (with-syntax (((id ...) ids)
                        ((value ...) values*)
                        ((clause ...) clauses)
                        ((body ...) bodies)
                        (result result))
            #'(for*/fold ((id '()) ...
                          #:result
                          (let ((id (reverse id)) ...)
                            result))
                         (clause ...)
                (call-with-values
                  (lambda () body ...)
                  (lambda (value ...)
                    (values (cons value id) ...)))))))
      (syntax-case stx ()
        ((_ (id ... #:result result) (clause ...) body ...)
         (expand
           (stx->list #'(id ...))
           #'result
           (stx->list #'(clause ...))
           (stx->list #'(body ...))))
        ((_ (id ...) (clause ...) body ...)
         (expand
           (stx->list #'(id ...))
           #'(values id ...)
           (stx->list #'(clause ...))
           (stx->list #'(body ...)))))))

  (define-syntax %for/vector
    (syntax-rules ()
      ((_ runner length fill (clause ...) body ...)
       (let ((size length))
         (unless (and (exact? size) (integer? size) (not (negative? size)))
           (assertion-violation 'for/vector
             "expected an exact nonnegative length" size))
         (let ((result (make-vector size fill))
               (index 0))
           (when (positive? size)
             (call-with-current-continuation
               (lambda (done)
                 (runner (clause ...)
                   (begin
                     (vector-set! result index (begin body ...))
                     (set! index (+ index 1))
                     (when (= index size) (done)))))))
           result)))))

  (define-syntax for/vector
    (syntax-rules ()
      ((_ #:length length #:fill fill clauses body ...)
       (%for/vector %for/run length fill clauses body ...))
      ((_ #:length length clauses body ...)
       (%for/vector %for/run length 0 clauses body ...))
      ((_ clauses body ...)
       (list->vector (for/list clauses body ...)))))

  (define-syntax for*/vector
    (syntax-rules ()
      ((_ #:length length #:fill fill clauses body ...)
       (%for/vector %for*/run length fill clauses body ...))
      ((_ #:length length clauses body ...)
       (%for/vector %for*/run length 0 clauses body ...))
      ((_ clauses body ...)
       (list->vector (for*/list clauses body ...)))))

  (define-syntax for/sum
    (syntax-rules ()
      ((_ clauses body ...)
       (for/fold ((sum 0)) clauses
         (+ sum (begin body ...))))))

  (define-syntax for*/sum
    (syntax-rules ()
      ((_ clauses body ...)
       (for*/fold ((sum 0)) clauses
         (+ sum (begin body ...))))))

  (define-syntax for/product
    (syntax-rules ()
      ((_ clauses body ...)
       (for/fold ((product 1)) clauses
         (* product (begin body ...))))))

  (define-syntax for*/product
    (syntax-rules ()
      ((_ clauses body ...)
       (for*/fold ((product 1)) clauses
         (* product (begin body ...))))))

  (define-syntax for/and
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (let ((answer #t))
             (%for/run (clause ...)
               (let ((x (begin body ...)))
                 (set! answer x)
                 (unless x (done #f))))
             answer))))))

  (define-syntax for*/and
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (let ((answer #t))
             (%for*/run (clause ...)
               (let ((x (begin body ...)))
                 (set! answer x)
                 (unless x (done #f))))
             answer))))))

  (define-syntax for/or
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (%for/run (clause ...)
             (let ((x (begin body ...)))
               (when x (done x))))
           #f)))))

  (define-syntax for*/or
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (%for*/run (clause ...)
             (let ((x (begin body ...)))
               (when x (done x))))
           #f)))))

  (define-syntax for/first
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (%for/run (clause ...) (done (begin body ...)))
           #f)))))

  (define-syntax for*/first
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (call-with-current-continuation
         (lambda (done)
           (%for*/run (clause ...) (done (begin body ...)))
           #f)))))

  (define-syntax for/last
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (let ((answer #f))
         (%for/run (clause ...) (set! answer (begin body ...)))
         answer))))

  (define-syntax for*/last
    (syntax-rules ()
      ((_ (clause ...) body ...)
       (let ((answer #f))
         (%for*/run (clause ...) (set! answer (begin body ...)))
         answer))))

)
