;; Program linking and object writing.

(library (core compiler object)
  (export program->datum write-compiled-program compiled-program->bytevector
          load-compiled-program directory-header strip-spans? set-strip-spans!
          procedure-metadata)
  (import (core) (capy)
    (core compiler bytecode types)
    (core compiler tree-il terms))

  (define strip-override 'default)
  (define (set-strip-spans! v) (set! strip-override v))
  (define (strip-spans?)
    (if (eq? strip-override 'default)
      (let ([v (getenv "CAPY_STRIP_SPANS")])
        (and v (member v '("1" "true" "yes" "on" "all")) #t))
      strip-override))

  (define (alist? x)
    (let loop ([x x])
      (cond [(null? x) #t]
            [(pair? x) (and (pair? (car x)) (loop (cdr x)))]
            [else #f])))

  ;; Procedure metadata as stored in a code block: the procedure's own metadata
  ;; alist with its source location added.
  (define (procedure-metadata metadata source)
    (cond
      [(eq? source #f) metadata]
      [(and (pair? metadata) (assq 'source metadata)) metadata]
      [else
       (let ([metadata (if (eq? metadata #f) '() metadata)])
         (if (alist? metadata)
           (cons (cons 'source source) metadata)
           metadata))]))

  (define program-tag (string->symbol "#%program"))

  (define (pool->bytevector pool)
    (let* ([n (vector-length pool)] [bv (make-bytevector (* 4 n) 0)])
      (do ([i 0 (+ i 1)]) ((= i n) bv)
        (bytevector-u32-set! bv (* 4 i) (vector-ref pool i) 'little))))

  (define (list->constants-vector constants index-of cells counter)
    ;; counter: a box-like mutable cell holding the next cell id
    (list->vector
      (map (lambda (c)
             (case (car c)
               [(value) (cons 'value (cdr c))]
               [(code) (cons 'code (index-of (cdr c)))]
               [(cell)
                (let ([id (hashtable-ref cells (cdr c) #f)])
                  (cons 'cell
                    (or id
                      (let ([next (car counter)])
                        (hashtable-set! cells (cdr c) next)
                        (set-car! counter (+ next 1))
                        next))))]
               [else (assertion-violation 'program->datum "unknown constant kind" c)]))
        constants)))

  ;; Code ids numbered by position, cache cells numbered across the whole
  ;; program in first-seen order, spans stripped when requested.
  (define (program->datum program)
    (let* ([procedures (compiled-program-procedures program)]
           [indices (make-hashtable equal-hash equal?)]   ; code-id -> index (lookup only)
           [cells (make-eq-hashtable)]                    ; cell key -> id (lookup only)
           [counter (list 0)]
           [stripped (strip-spans?)])
      (define (index-of id)
        (or (hashtable-ref indices id #f)
          (assertion-violation 'program->datum "closure of a code id that is not in the program" id)))
      (let loop ([ps procedures] [i 0])
        (unless (null? ps)
          (let ([id (compiled-procedure-code-id (car ps))])
            (unless (hashtable-ref indices id #f)
              (hashtable-set! indices id i)))
          (loop (cdr ps) (+ i 1))))
      (vector program-tag
        (compiled-program-entry program)
        (list->vector
          (map
            (lambda (cp)
              ;; constants first: cell numbering follows procedure order
              (let ([constants (list->constants-vector (compiled-procedure-constants cp)
                                index-of cells counter)])
                (vector
                  (compiled-procedure-arity cp)
                  (compiled-procedure-frame-size cp)
                  (procedure-metadata (compiled-procedure-meta cp) (compiled-procedure-source cp))
                  (compiled-procedure-code cp)
                  (pool->bytevector (compiled-procedure-pool cp))
                  (compiled-procedure-metadata-counts cp)
                  constants
                  (if stripped '#() (list->vector (compiled-procedure-spans cp)))
                  (if stripped
                    '#()
                    (list->vector
                      (map (lambda (r)
                             (vector (src-run-start r) (src-run-len r) (src-run-flags r) (src-run-span r)))
                        (compiled-procedure-srcmap cp)))))))
            procedures)))))

  ;; The directory header of a top-level term: one `(name imports exports meta)`
  ;; entry per linklet, for a linklet or a `make-linklet-directory` over linklets.
  ;; Other terms have none (#f).
  (define (directory-header il)
    (define (entry l)
      (list (linklet-term-name l) (linklet-term-imports l)
        (linklet-term-exports l) (linklet-term-meta l)))
    (define (entries args)
      (let loop ([args args] [acc '()])
        (cond [(null? args) (reverse acc)]
              [(linklet-term? (car args)) (loop (cdr args) (cons (entry (car args)) acc))]
              [else #f])))
    (cond
      [(linklet-term? il) (list (entry il))]
      [(and (application? il)
            (primref? (application-operator il))
            (eq? (primref-prim (application-operator il)) 'make-linklet-directory))
       (entries (application-operands il))]
      [(and (primcall? il) (eq? (primcall-prim il) 'make-linklet-directory))
       (entries (primcall-args il))]
      [else #f]))

  (define (write-compiled-program program original-il output-file)
    (fasl-write (program->datum program) output-file (directory-header original-il) 'gzip))

  (define (compiled-program->bytevector program original-il)
    (fasl-write->bytevector (program->datum program) (directory-header original-il) 'gzip))

  (define (load-compiled-program program original-il)
    (%load-program-bytevector (compiled-program->bytevector program original-il)))
)
