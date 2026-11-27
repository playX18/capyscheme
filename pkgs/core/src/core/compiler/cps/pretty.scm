;; Rendering graph CPS as text
(library (core compiler cps pretty)
  (export render-graph render-value)
  (import (core) (capy) (core compiler cps graph))

  (define (->string-with proc)
    (call-with-string-output-port proc))

  (define (display->string x)
    (->string-with (lambda (p) (display x p))))

  (define (write->string x)
    (->string-with (lambda (p) (write x p))))

  ;; pretty.rs render_value.
  (define (render-value value)
    (cond
      [(eq? value #f) "#f"]
      [(eq? value #t) "#t"]
      [(symbol? value) (symbol->string value)]
      [(number? value) (number->string value)]
      [(string? value) value]
      [else (format #f "~a" value)]))

  (define (join strs sep)
    (cond
      [(null? strs) ""]
      [else
        (let loop ([rest (cdr strs)] [acc (car strs)])
          (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc sep (car rest)))))]))

  (define (nstr n) (number->string n))

  ;; Every maximal run of ASCII digits in S as a number, in order
  ;; (successor_blocks: runs that overflow usize are skipped).
  (define (successor-blocks s)
    (let ([n (string-length s)])
      (let loop ([i 0] [acc '()])
        (cond
          [(>= i n) (reverse acc)]
          [(char<=? #\0 (string-ref s i) #\9)
            (let scan ([j i])
              (if (and (< j n) (char<=? #\0 (string-ref s j) #\9))
                (scan (+ j 1))
                (loop j
                  (if (> (- j i) 19)
                    acc
                    (cons (string->number (substring s i j)) acc)))))]
          [else (loop (+ i 1) acc)]))))

  (define-record-type (render-block make-render-block render-block?)
    (fields id params lines successors))

  (define-record-type (renderer make-renderer renderer?)
    (fields
      graph
      values                       ; eqv-hashtable BoundVar -> value number (lookup only)
      (mutable next-value renderer-next-value renderer-next-value-set!)
      (mutable blocks renderer-blocks renderer-blocks-set!)   ; reversed push order
      local-blocks                 ; eqv-hashtable BoundVar -> block id (lookup only)
      (mutable next-block renderer-next-block renderer-next-block-set!)
      (mutable is-toplevel renderer-is-toplevel renderer-is-toplevel-set!)))

  (define (new-renderer g)
    (make-renderer g (make-eqv-hashtable) 0 '() (make-eqv-hashtable) 0 #f))

  (define (value-id r var)
    (let ([id (hashtable-ref (renderer-values r) var #f)])
      (or id
        (let ([id (renderer-next-value r)])
          (renderer-next-value-set! r (+ id 1))
          (hashtable-set! (renderer-values r) var id)
          id))))

  (define (value r var) (string-append "v@" (nstr (value-id r var))))

  ;; Left to right, like the Rust iterator.
  (define (values* r vars)
    (let loop ([vars vars] [acc '()])
      (if (null? vars)
        (reverse acc)
        (loop (cdr vars) (cons (value r (car vars)) acc)))))

  (define (param-ids r vars variadic)
    (let ([params (values* r vars)])
      (if variadic
        (append params (list (string-append "..." (value r variadic))))
        params)))

  (define (free-var r occ) (value r (free-binder (renderer-graph r) occ)))

  (define (free-vars r vars)
    (let loop ([vars vars] [acc '()])
      (if (null? vars)
        (reverse acc)
        (loop (cdr vars) (cons (free-var r (car vars)) acc)))))

  (define (alloc-block r)
    (let ([id (renderer-next-block r)])
      (renderer-next-block-set! r (+ id 1))
      id))

  (define (push-block! r block)
    (renderer-blocks-set! r (cons block (renderer-blocks r))))

  ;; Rust threads `lines: &mut Vec<String>`; here LINES is a box holding the
  ;; reversed list.
  (define (lines-push! lines line) (set-box! lines (cons line (unbox lines))))

  (define (convert-block r id params variadic link)
    (let* ([g (renderer-graph r)]
           [term (read-term-link g link)])
      (when term
        (let* ([lines (box '())]
               [successors (convert-term r term lines)]
               [block-params
                 (if variadic
                   (append params (list (string-append "...v@" (nstr variadic))))
                   params)])
          (push-block! r (make-render-block id block-params (reverse (unbox lines)) successors))))))

  (define dead-successors "Successors: <dead>")

  (define (convert-term-link r link lines)
    (let ([term (read-term-link (renderer-graph r) link)])
      (if term (convert-term r term lines) dead-successors)))

  (define (convert-term r term lines)
    (let* ([g (renderer-graph r)]
           [kind (term-kind (graph-term g term))])
      (cond
        [(k-let-val? kind)
          (let ([expr (read-expr-link g (k-let-val-expr kind))])
            (if (not expr)
              dead-successors
              (let* ([rendered-expr (render-expr r expr)]
                     [line (string-append (value r (k-let-val-binder kind)) " = " rendered-expr)])
                (lines-push! lines line)
                (convert-term-link r (k-let-val-body kind) lines))))]
        [(k-fix? kind)
          (when (renderer-is-toplevel r)
            (for-each
              (lambda (function)
                (let ([binder (function-var (graph-function g function))])
                  (lines-push! lines
                    (string-append (value r binder) " = Fix f" (nstr function)))))
              (live-functions g (k-fix-functions kind))))
          (convert-term-link r (k-fix-body kind) lines)]
        [(k-letk? kind)
          (let* ([continuations (live-functions g (k-letk-functions kind))]
                 [reified (filter (lambda (c) (function-is-reified (graph-function g c))) continuations)]
                 [local (filter (lambda (c) (not (function-is-reified (graph-function g c)))) continuations)])
            (when (renderer-is-toplevel r)
              (for-each
                (lambda (continuation)
                  (let ([binder (function-var (graph-function g continuation))])
                    (lines-push! lines
                      (string-append (value r binder) " = Letk k" (nstr continuation)))))
                reified))
            (for-each
              (lambda (continuation)
                (let ([id (alloc-block r)])
                  (hashtable-set! (renderer-local-blocks r)
                    (function-var (graph-function g continuation)) id)))
              local)
            (for-each
              (lambda (continuation)
                (let* ([data (graph-function g continuation)]
                       [id (hashtable-ref (renderer-local-blocks r) (function-var data) #f)]
                       [params (values* r (bound-vars-slice g (function-vars data)))]
                       [variadic (and (function-variadic data)
                                   (value-id r (function-variadic data)))])
                  (convert-block r id params variadic (function-body data))))
              local)
            (convert-term-link r (k-letk-body kind) lines))]
        [(k-continue? kind)
          (let* ([target (free-binder g (k-continue-cont kind))]
                 [args (join (free-vars r (k-continue-vars kind)) ", ")]
                 [block (hashtable-ref (renderer-local-blocks r) target #f)])
            (if block
              (string-append "Successors: BB" (nstr block) "(" args ")")
              (string-append "Successors: TailCall v@" (nstr (value-id r target)) "(" args ")")))]
        [(k-app? kind)
          (let* ([callee (free-var r (k-app-func kind))]
                 [retk (free-var r (k-app-cont kind))]
                 [args (join (free-vars r (k-app-vars kind)) ", ")])
            (string-append "Successors: Call " callee " " retk "(" args ")"))]
        [(k-raise? kind)
          (let ([args (join (free-vars r (k-raise-vars kind)) ", ")])
            (string-append "Successors: Raise " (symbol->string (k-raise-kind kind)) "(" args ")"))]
        [(k-branch-prim? kind)
          (let ([op (k-branch-prim-op kind)])
            (let-values ([(args true-args false-args)
                          (branch-operands g op (k-branch-prim-vars kind))])
              (let* ([args (join (free-vars r args) ", ")]
                     [true-args (join (free-vars r true-args) ", ")]
                     [false-args (join (free-vars r false-args) ", ")]
                     [consequent (free-var r (k-branch-prim-consequent kind))]
                     [alternative (free-var r (k-branch-prim-alternative kind))]
                     [hints (branch-op-hints op)])
                (string-append "Successors: BranchPrim " (symbol->string (branch-op-prim op))
                  "(" args ") Then: " consequent "(" true-args "), Else: "
                  alternative "(" false-args ") ["
                  (symbol->string (car hints)) ", " (symbol->string (cadr hints)) "]"))))]
        [(k-if? kind)
          (let* ([test (free-var r (k-if-test kind))]
                 [then-target (branch-target r (k-if-then kind))]
                 [else-target (branch-target r (k-if-else kind))]
                 [hints (k-if-hints kind)])
            (string-append "Successors: If " test " Then: " then-target ", Else: "
              else-target " [" (symbol->string (car hints)) ", "
              (symbol->string (cadr hints)) "]"))]
        [else (assertion-violation 'convert-term "bad term kind" kind)])))

  (define (branch-target r link)
    (let ([dc (direct-continue (renderer-graph r) link)])
      (if dc
        (continuation-target r (car dc) (cdr dc))
        (let ([block (alloc-block r)])
          (convert-block r block '() #f link)
          (string-append "BB" (nstr block) "()")))))

  (define (continuation-target r continuation args)
    (let* ([g (renderer-graph r)]
           [continuation (free-binder g continuation)]
           [args (join (free-vars r args) ", ")]
           [block (hashtable-ref (renderer-local-blocks r) continuation #f)])
      (if block
        (string-append "BB" (nstr block) "(" args ")")
        (string-append "Reified v@" (nstr (value-id r continuation)) "(" args ")"))))

  (define (render-expr r expr)
    (let* ([g (renderer-graph r)]
           [kind (expr-kind (graph-expr g expr))])
      (cond
        [(k-literal? kind) (render-value (k-literal-value kind))]
        [else
          (let* ([prim (display->string (k-prim-call-prim kind))]
                 [args (join (free-vars r (k-prim-call-vars kind)) ", ")])
            (string-append prim "(" args ")"))])))

  ;; Insert-sorted unique list of numbers.
  (define (sort-unique ids)
    (let loop ([sorted (list-sort < ids)] [acc '()])
      (cond
        [(null? sorted) (reverse acc)]
        [(and (pair? acc) (= (car acc) (car sorted))) (loop (cdr sorted) acc)]
        [else (loop (cdr sorted) (cons (car sorted) acc))])))

  (define (finish r p)
    (let ([blocks (reverse (renderer-blocks r))]
          [predecessors (make-eqv-hashtable)])
      (for-each
        (lambda (block)
          (for-each
            (lambda (pred)
              (hashtable-set! predecessors pred
                (cons (render-block-id block) (hashtable-ref predecessors pred '()))))
            (successor-blocks (render-block-successors block))))
        blocks)
      (for-each
        (lambda (block)
          (display "BB" p)
          (display (nstr (render-block-id block)) p)
          (display ": (" p)
          (display (join (render-block-params block) ", ") p)
          (display ")\n" p)
          (let ([preds (hashtable-ref predecessors (render-block-id block) #f)])
            (when preds
              (display "  Predecessors: " p)
              (display
                (join (map (lambda (id) (string-append "BB" (nstr id))) (sort-unique preds)) ", ")
                p)
              (display "\n" p)))
          (for-each
            (lambda (line) (display "  " p) (display line p) (display "\n" p))
            (render-block-lines block))
          (display "  " p)
          (display (render-block-successors block) p)
          (display "\n\n" p))
        (list-sort (lambda (a b) (< (render-block-id a) (render-block-id b))) blocks))))


  (define (collect-functions g term seen order)
    ;; SEEN: eqv-hashtable (lookup only); ORDER: box holding the reversed list.
    (let ([kind (term-kind (graph-term g term))])
      (cond
        [(k-let-val? kind)
          (let ([body (read-term-link g (k-let-val-body kind))])
            (when body (collect-functions g body seen order)))]
        [(or (k-fix? kind) (k-letk? kind))
          (let ([functions (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind))]
                [body (if (k-fix? kind) (k-fix-body kind) (k-letk-body kind))])
            (for-each
              (lambda (function)
                (unless (hashtable-ref seen function #f)
                  (hashtable-set! seen function #t)
                  (set-box! order (cons function (unbox order)))
                  (let ([fbody (read-term-link g (function-body (graph-function g function)))])
                    (when fbody (collect-functions g fbody seen order)))))
              (live-functions g functions))
            (let ([b (read-term-link g body)])
              (when b (collect-functions g b seen order))))]
        [else (if #f #f)])))

  (define (render-function p g function)
    (let* ([data (graph-function g function)]
           [kind (if (function-cont data) "function" "continuation")]
           [code-id (string-append (if (function-cont data) "f" "k") (nstr function))]
           [r (new-renderer g)]
           [params (values* r (bound-vars-slice g (function-vars data)))]
           [retk (if (function-cont data) (value r (function-cont data)) "#f")]
           [name (render-value (function-name data))])
      (display (string-append "procedure " kind " " code-id " " name " ("
                 (join params ", ") ") retk " retk ":\n") p)
      (let* ([entry (alloc-block r)]
             [params (param-ids r (bound-vars-slice g (function-vars data)) (function-variadic data))]
             [variadic (and (function-variadic data) (value-id r (function-variadic data)))])
        (convert-block r entry params variadic (function-body data))
        (finish r p))))

  (define (render-toplevel p g root)
    (let* ([r (new-renderer g)])
      (renderer-is-toplevel-set! r #t)
      (let* ([entry (alloc-block r)]
             [lines (box '())]
             [successors (convert-term r root lines)])
        (unless (and (string=? successors dead-successors) (null? (unbox lines)))
          (push-block! r (make-render-block entry '() (reverse (unbox lines)) successors))
          (display "toplevel:\n" p)
          (finish r p)))))

  (define (render-graph g root)
    (->string-with
      (lambda (p)
        (render-toplevel p g root)
        (let ([seen (make-eqv-hashtable)] [order (box '())])
          (collect-functions g root seen order)
          (for-each
            (lambda (function)
              (render-function p g function)
              (display "\n" p))
            (reverse (unbox order))))))))
