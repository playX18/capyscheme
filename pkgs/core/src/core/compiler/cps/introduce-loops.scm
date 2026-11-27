;; Loop recognition: turn a function's self-calls into a real loop.
;;
;; Port of crates/capy/src/compiler/cps/introduce_loops.rs.
;;
;; A fixed-arity function whose self-calls are tail calls gets a local
;; continuation holding the loop body; the function prologue initializes the
;; loop and every self tail call becomes a `Continue' back to it.  Non-tail
;; list-building self calls (`cons') and exact associative primitives get an
;; accumulator parameter (see the Rust module doc for the exact conditions).
;;
;; API:
;;   (introduce-loops graph entry) => loop-stats
;;       `ctx' is dropped.  loop-stats accessors: loop-stats-loops
;;       loop-stats-self-calls loop-stats-loops-accumulated
;;       loop-stats-accumulated-self-calls; (make-loop-stats l s la as)
;;       (zero-loop-stats).
;;
;; Flags (utils/flags.rs, read from the environment with `getenv'; the CLI
;; spellings --gcps-loops etc. are not handled here):
;;   (flag-bool name default)   default is a boolean
;;   (flag-usize name default min)
;;   (flag-enum name default words)  => the lower-case word as a string
;;   (flag-list name default words)  => list of words (strings)
;; They parse exactly like Flags::parse_value (invalid input falls back to the
;; default).  `gcps-loops?' / `gcps-accumulate?' are the two flags this module
;; and the optimizer read (CAPY_GCPS_LOOPS, CAPY_GCPS_ACCUMULATE, both "1").
;;
;; Determinism: `functions_by_binder' and `literals' are HashMaps that are only
;; looked up in the Rust; here they are eqv hashtables used the same way.
;; Rust's Value `==' (bit equality) is `eqv?' here.
(library (core compiler cps introduce-loops)
  (export
    introduce-loops
    make-loop-stats loop-stats? loop-stats-loops loop-stats-self-calls
    loop-stats-loops-accumulated loop-stats-accumulated-self-calls zero-loop-stats
    flag-bool flag-usize flag-enum flag-list gcps-loops? gcps-accumulate?)
  (import (core) (capy) (core compiler cps graph))

  (define (string-trim-both s)
    (let* ([n (string-length s)]
           [start (let loop ([i 0]) (if (and (< i n) (char-whitespace? (string-ref s i))) (loop (+ i 1)) i))]
           [end (let loop ([i n]) (if (and (> i start) (char-whitespace? (string-ref s (- i 1)))) (loop (- i 1)) i))])
      (substring s start end)))

  (define (flag-raw name)
    (let ([v (getenv name)])
      (and v (string-trim-both v))))

  (define (flag-bool name default)
    (let ([raw (flag-raw name)])
      (if (not raw)
        default
        (let ([w (string-downcase raw)])
          (cond
            [(member w '("1" "true" "yes" "on" "all")) #t]
            [(member w '("" "0" "false" "no" "off" "none")) #f]
            [else default])))))

  ;; usize::MAX stand-in for "max" / "unlimited".
  (define usize-max (- (expt 2 64) 1))

  (define (flag-usize name default min)
    (let ([raw (flag-raw name)])
      (if (not raw)
        default
        (let* ([w (string-downcase raw)]
               [n (cond
                    [(member w '("max" "unlimited")) usize-max]
                    [(and (> (string-length w) 0)
                          (let loop ([i 0])
                            (or (= i (string-length w))
                                (and (char<=? #\0 (string-ref w i) #\9) (loop (+ i 1))))))
                      (string->number w)]
                    [else #f])])
          (if n (max min (min* n usize-max)) default)))))

  (define (min* a b) (if (< a b) a b))

  (define (flag-enum name default words)
    (let ([raw (flag-raw name)])
      (if (not raw)
        default
        (let ([w (string-downcase raw)])
          (cond
            [(string=? w "") ""]
            [(member w words) w]
            [else default])))))

  (define (flag-list name default words)
    (let ([raw (flag-raw name)])
      (if (not raw)
        default
        (let loop ([parts (split-commas raw)] [acc '()])
          (if (null? parts)
            (reverse acc)
            (let ([w (string-downcase (string-trim-both (car parts)))])
              (cond
                [(string=? w "") (loop (cdr parts) acc)]
                [(member w words) (loop (cdr parts) (cons w acc))]
                [else (loop (cdr parts) acc)])))))))

  (define (split-commas s)
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i (string-length s)) (reverse (cons (substring s start i) acc))]
        [(char=? (string-ref s i) #\,) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
        [else (loop (+ i 1) start acc)])))

  (define (gcps-loops?) (flag-bool "CAPY_GCPS_LOOPS" #t))
  (define (gcps-accumulate?) (flag-bool "CAPY_GCPS_ACCUMULATE" #t))

  (define-record-type (loop-stats make-loop-stats loop-stats?)
    (fields loops self-calls loops-accumulated accumulated-self-calls))

  (define (zero-loop-stats) (make-loop-stats 0 0 0 0))

  ;; mode: 'ConsReverse | 'LeftFold; base: literal every exit returns, also the
  ;; accumulator's initial value.
  (define-record-type (acc-plan make-acc-plan acc-plan?)
    (fields mode prim base))

  ;; cont: the continuation the call returns to; equal to the function's return
  ;; continuation for a tail self call.
  (define-record-type (self-call make-self-call self-call?)
    (fields link cont tail))

  (define-record-type (candidate make-candidate candidate?)
    (fields calls exits plan))

  (define no-value (list 'none))

  ;; map with a guaranteed left-to-right order (ids are allocated by the callback).
  (define (map-seq f lst)
    (let loop ([lst lst] [acc '()])
      (if (null? lst) (reverse acc) (loop (cdr lst) (cons (f (car lst)) acc)))))
  ;; Any value as name text: syntax objects by their datum, everything else by `display'.
  ;; Ids are often #f (fresh-variable) and never guaranteed to be symbols.
  (define (display-id id)
    (format "~a" (if (syntax? id) (syntax-expression id) id)))

  ;; LVar::derived: NAME ID : SUFFIX, uninterned.
  (define (fresh-lvar-derived base suffix)
    (make-lvar
      (string->uninterned-symbol
        (string-append
          (display-id (lvar-name base))
          (display-id (lvar-id base))
          ":" suffix))
      (lvar-id base)))

  (define (bound-var-lvar g binder) (bound-var-data-var (graph-bound-var g binder)))

  ;; Recognize loops in every reachable function of GRAPH.
  (define (introduce-loops g entry)
    (let* ([seen (make-entity-set)]
           [functions (collect-functions g (function-body (graph-function g entry)) seen)]
           [by-binder (make-eqv-hashtable)])
      (for-each
        (lambda (function)
          (hashtable-set! by-binder (function-var (graph-function g function)) function))
        functions)
      (let ([loops 0] [self-calls 0] [loops-acc 0] [acc-self-calls 0])
        (for-each
          (lambda (function)
            (unless (eqv? function entry)
              (let ([cand (self-calls-of g function by-binder)])
                (when (and cand
                           (pair? (candidate-calls cand))
                           ;; Without the accumulator transform only tail calls can
                           ;; become jumps; a function with no tail self call would
                           ;; get a useless loop block.
                           (or (candidate-plan cand)
                               (exists self-call-tail (candidate-calls cand))))
                  (let ([accumulated (and (candidate-plan cand) #t)])
                    (when (introduce-loop g function cand by-binder)
                      (set! loops (+ loops 1))
                      (set! self-calls (+ self-calls (length (candidate-calls cand))))
                      (when accumulated
                        (set! loops-acc (+ loops-acc 1))
                        (set! acc-self-calls
                          (+ acc-self-calls
                            (length (filter (lambda (c) (not (self-call-tail c)))
                                      (candidate-calls cand))))))))))))
          functions)
        (make-loop-stats loops self-calls loops-acc acc-self-calls))))

  ;; Enumerate every function reachable from LINK, including nested groups.
  (define (collect-functions g link seen)
    (let ([out '()])
      (let walk ([link link])
        (let ([term (read-term-link g link)])
          (when term
            (let ([kind (term-kind (graph-term g term))])
              (cond
                [(k-let-val? kind) (walk (k-let-val-body kind))]
                [(or (k-letk? kind) (k-fix? kind))
                  (let ([functions (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))]
                        [body (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind))])
                    (for-each
                      (lambda (function)
                        (when (entity-set-insert! seen function)
                          (set! out (cons function out))
                          (walk (function-body (graph-function g function)))))
                      (live-functions g functions))
                    (walk body))]
                [(k-if? kind)
                  (walk (k-if-then kind))
                  (walk (k-if-else kind))]
                [else (if #f #f)])))))
      (reverse out)))

  ;; Collect literal bindings in a procedure body.  Binders are unique, so a
  ;; single map suffices for the exit checks.
  (define (collect-literals! g link out)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind)
              (let ([expr (read-expr-link g (k-let-val-expr kind))])
                (when expr
                  (let ([ek (expr-kind (graph-expr g expr))])
                    (when (k-literal? ek)
                      (hashtable-set! out (k-let-val-binder kind) (k-literal-value ek))))))
              (collect-literals! g (k-let-val-body kind) out)]
            [(or (k-letk? kind) (k-fix? kind))
              (for-each
                (lambda (function)
                  (collect-literals! g (function-body (graph-function g function)) out))
                (live-functions g (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))))
              (collect-literals! g (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind)) out)]
            [(k-if? kind)
              (collect-literals! g (k-if-then kind) out)
              (collect-literals! g (k-if-else kind) out)]
            [else (if #f #f)])))))

  ;; Collect the self calls of FUNCTION, or #f when it is not a loop candidate.
  (define (self-calls-of g function by-binder)
    (let ([data (graph-function g function)])
      (and (function-cont data)
           (not (function-case-group data))
           (not (function-is-variadic data))
           (not (function-variadic data))
           (let* ([self-binder (function-binder-rooted g (function-var data))]
                  [self-cont (function-binder-rooted g (function-cont data))]
                  [arity (length (bound-vars-slice g (function-vars data)))]
                  [literals (make-eqv-hashtable)]
                  [calls '()] [exits '()] [accumulate-ok #t])
             (collect-literals! g (function-body data) literals)
             (and (collect-self-calls g (function-body data) self-binder self-cont arity
                    (lambda (c) (set! calls (cons c calls)))
                    (lambda (e) (set! exits (cons e exits)))
                    (lambda () (set! accumulate-ok #f)))
                  (let ([calls (reverse calls)] [exits (reverse exits)])
                    (let ([plan #f] [exits exits])
                      (when (and accumulate-ok
                                 (gcps-accumulate?)
                                 (exists (lambda (c) (not (self-call-tail c))) calls)
                                 (pair? exits))
                        (let ([result (accumulate-plan g self-cont calls exits literals
                                        by-binder (function-body data))])
                          (when result
                            (set! plan (car result))
                            (set! exits (cdr result)))))
                      (make-candidate calls exits plan))))))))

  ;; Walk a procedure body looking for self calls and exits.  Returns #f when
  ;; the function must not be loopified at all (a wrong arity self call).
  ;; ADD-CALL! / ADD-EXIT! collect; CLEAR-ACC! clears `accumulate_ok'.
  (define (collect-self-calls g link self-binder self-cont arity add-call! add-exit! clear-acc!)
    (let walk ([link link])
      (let ([term (read-term-link g link)])
        (if (not term)
          #t
          (let ([kind (term-kind (graph-term g term))])
            (cond
              [(k-let-val? kind) (walk (k-let-val-body kind))]
              [(k-letk? kind)
                (and (let loop ([fs (live-functions g (k-letk-functions kind))])
                       (or (null? fs)
                           (and (walk (function-body (graph-function g (car fs))))
                                (loop (cdr fs)))))
                     (walk (k-letk-body kind)))]
              [(k-fix? kind) (walk (k-fix-body kind))]
              [(k-if? kind) (and (walk (k-if-then kind)) (walk (k-if-else kind)))]
              [(k-app? kind)
                (let ([cont-binder (free-binder-rooted g (k-app-cont kind))])
                  (cond
                    [(eqv? (free-binder-rooted g (k-app-func kind)) self-binder)
                      ;; One wrong-arity self call bails the whole function.
                      (if (not (= (length (free-vars-slice g (k-app-vars kind))) arity))
                        #f
                        (begin
                          (add-call! (make-self-call link cont-binder (eqv? cont-binder self-cont)))
                          #t))]
                    [(eqv? cont-binder self-cont)
                      ;; Returning another call's result through the loop's return
                      ;; continuation bypasses the loop's accumulator.
                      (clear-acc!) #t]
                    [else #t]))]
              [(k-continue? kind)
                (when (eqv? (free-binder-rooted g (k-continue-cont kind)) self-cont)
                  (add-exit! link))
                #t]
              [(k-branch-prim? kind)
                (when (or (eqv? (free-binder-rooted g (k-branch-prim-consequent kind)) self-cont)
                          (eqv? (free-binder-rooted g (k-branch-prim-alternative kind)) self-cont))
                  (clear-acc!))
                #t]
              [else #t]))))))

  ;; Build the accumulator plan: (plan . remaining-exits), or #f when some
  ;; non-tail call or exit does not fit.
  (define (accumulate-plan g self-cont calls exits literals by-binder body)
    (let ([consumed (make-entity-set)])
      (let loop ([cs calls] [mode #f] [prim #f])
        (cond
          [(null? cs)
            (let ([remaining (filter (lambda (e) (not (entity-set-contains? consumed e))) exits)])
              (and (pair? remaining)
                (let ([base (exit-base g self-cont remaining literals)])
                  (and (not (eq? base no-value))
                    mode
                    (not (and (eq? mode 'ConsReverse) (not (null? base))))
                    (not (and (eq? mode 'ConsReverse) (body-uses-division? g body)))
                    (cons (make-acc-plan mode prim base) remaining)))))]
          [(self-call-tail (car cs)) (loop (cdr cs) mode prim)]
          [else
            (let ([m (match-accumulate-cont g (self-call-cont (car cs)) self-cont by-binder)])
              (and m
                (let ([m-prim (vector-ref m 1)] [m-mode (vector-ref m 2)] [m-exit (vector-ref m 3)])
                  (entity-set-insert! consumed m-exit)
                  (cond
                    [(not mode) (loop (cdr cs) m-mode m-prim)]
                    [(and (eq? mode m-mode) (eq? prim m-prim)) (loop (cdr cs) mode prim)]
                    [else #f]))))]))))

  (define division-prims
    '("quotient" "remainder" "modulo" "div" "mod" "div0" "mod0" "/"))

  (define (symbol-name-in? sym names)
    (and (symbol? sym) (member (symbol->string sym) names) #t))

  ;; Whether the loop body performs an integer division.
  (define (body-uses-division? g link)
    (let ([term (read-term-link g link)])
      (and term
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind)
              (or (let ([expr (read-expr-link g (k-let-val-expr kind))])
                    (and expr
                      (let ([ek (expr-kind (graph-expr g expr))])
                        (and (k-prim-call? ek)
                          (symbol-name-in? (k-prim-call-prim ek) division-prims)))))
                  (body-uses-division? g (k-let-val-body kind)))]
            [(or (k-letk? kind) (k-fix? kind))
              (or (exists
                    (lambda (function)
                      (body-uses-division? g (function-body (graph-function g function))))
                    (live-functions g (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))))
                  (body-uses-division? g (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind))))]
            [(k-if? kind)
              (or (body-uses-division? g (k-if-then kind))
                  (body-uses-division? g (k-if-else kind)))]
            [else #f])))))

  ;; The literal returned by every exit (the accumulator's initial value), or
  ;; `no-value'.  All exits must agree.
  (define (exit-base g self-cont exits literals)
    (let loop ([exits exits] [base no-value])
      (if (null? exits)
        base
        (let ([term (read-term-link g (car exits))])
          (if (not term)
            no-value
            (let ([kind (term-kind (graph-term g term))])
              (if (not (k-continue? kind))
                no-value
                (if (not (eqv? (free-binder-rooted g (k-continue-cont kind)) self-cont))
                  no-value
                  (let ([args (free-vars-slice g (k-continue-vars kind))])
                    (if (not (= (length args) 1))
                      no-value
                      (let ([value (hashtable-ref literals (free-binder-rooted g (car args)) no-value)])
                        (cond
                          [(eq? value no-value) no-value]
                          [(eq? base no-value) (loop (cdr exits) value)]
                          [(eqv? base value) (loop (cdr exits) base)]
                          [else no-value]))))))))))))

  (define (primitive-mode prim)
    (cond
      [(not (symbol? prim)) #f]
      [(string=? (symbol->string prim) "cons") 'ConsReverse]
      [(member (symbol->string prim)
         '("logand" "logior" "fxand" "fxior" "fxxor" "fxlogand" "fxlogior" "fxlogxor"
           "fxmin" "fxmax"))
        'LeftFold]
      [else #f]))

  ;; Recognize `(cons head v)' / `(f head v)' continuations and return the
  ;; accumulator update as #(head prim mode body-link), or #f.
  (define (match-accumulate-cont g cont self-cont by-binder)
    (let ([function (hashtable-ref by-binder cont #f)])
      (and function
        (let ([data (graph-function g function)])
          ;; Only second-class continuations (Letk members) can be bypassed.
          (and (not (function-cont data))
               ;; The continuation must be used only by the self call being rewritten.
               (binder-is-singleton-or-dead? g cont)
               (let ([vars (bound-vars-slice g (function-vars data))])
                 (and (= (length vars) 1)
                      (let ([value-arg (car vars)] [rest (function-variadic data)])
                        (and (or (not rest) (null? (collect-occurrences g rest)))
                             (let ([term (read-term-link g (function-body data))])
                               (and term
                                    (let ([kind (term-kind (graph-term g term))])
                                      (and (k-let-val? kind)
                                           (match-accumulate-body g kind value-arg self-cont))))))))))))))

  (define (match-accumulate-body g kind value-arg self-cont)
    (let* ([result (k-let-val-binder kind)]
           [expr (read-expr-link g (k-let-val-expr kind))])
      (and expr
        (let ([ek (expr-kind (graph-expr g expr))])
          (and (k-prim-call? ek)
            (let* ([prim (k-prim-call-prim ek)]
                   [mode (primitive-mode prim)])
              (and mode
                (let ([args (free-vars-slice g (k-prim-call-vars ek))])
                  (and (= (length args) 2)
                    (let* ([first (free-binder-rooted g (car args))]
                           [second (free-binder-rooted g (cadr args))]
                           [head
                            (if (eq? mode 'ConsReverse)
                              ;; cons is not associative: only `(cons head value)' fits.
                              (and (eqv? second value-arg) first)
                              ;; Exact associative primitives are commutative in the
                              ;; supported set.
                              (cond
                                [(eqv? first value-arg) second]
                                [(eqv? second value-arg) first]
                                [else #f]))])
                      (and head
                        (not (eqv? head value-arg))
                        (not (eqv? head result))
                        (let* ([body-link (k-let-val-body kind)]
                               [body (read-term-link g body-link)])
                          (and body
                            (let ([bk (term-kind (graph-term g body))])
                              (and (k-continue? bk)
                                (eqv? (free-binder-rooted g (k-continue-cont bk)) self-cont)
                                (let ([cargs (free-vars-slice g (k-continue-vars bk))])
                                  (and (= (length cargs) 1)
                                    (eqv? (free-binder-rooted g (car cargs)) result)
                                    (vector head prim mode body-link))))))))))))))))))

  (define (introduce-loop g function cand by-binder)
    (let* ([data (graph-function g function)]
           [old-params (function-vars data)]
           [old-body (function-body data)]
           [self-cont (function-binder-rooted g (function-cont data))]
           [source (function-source data)]
           [function-lvar (bound-var-lvar g (function-var data))]
           [plan (candidate-plan cand)]
           [old-param-lvars (map (lambda (p) (bound-var-lvar g p)) (bound-vars-slice g old-params))]
           [new-params (map-seq (lambda (lvar) (new-bound-var g (fresh-lvar-derived lvar "loop")))
                        old-param-lvars)]
           [new-args (new-bound-vars g new-params)]
           [accumulator (and plan (new-bound-var g (fresh-lvar-derived function-lvar "loop-acc")))]
           [loop-binder (new-bound-var g (fresh-lvar-derived function-lvar "loop-body"))]
           [loop-formals (new-bound-vars g
                          (append (bound-vars-slice g old-params)
                            (if accumulator (list accumulator) '())))]
           [loop-function (new-function g
                           (make-function (function-name data) source loop-binder loop-formals
                             #f #f #f old-body #t #f #f #f (function-meta data) #f))])
      (backpatch-function! g loop-function)
      ;; The function's new body is
      ;;   letk loop(params [acc]) = <old body> in continue loop(new params [base])
      (let* ([body-link (new-term-link g #f)]
             [letk-link (new-function-link g #f)])
        (set-function-link! g letk-link loop-function)
        (let* ([letk-functions (new-function-links g (list letk-link))]
               [continue-link (new-term-link g #f)]
               [letk-body-link (if plan (new-term-link g #f) continue-link)]
               [letk-parent (new-parent-link g #f)]
               [letk-term (new-term g letk-parent (make-k-letk letk-functions letk-body-link) source)])
          (set-parent-link! g letk-parent (parent-func function))
          (set-term-link! g body-link letk-term)
          (let* ([loop-occ (new-free-occ-for-binder g loop-binder continue-link)]
                 [arg-occs (map-seq (lambda (param) (new-free-occ-for-binder g param continue-link))
                            new-params)])
            (if plan
              (let* ([base-binder (new-bound-var g (fresh-lvar-derived function-lvar "loop-base"))]
                     [base-occ (new-free-occ-for-binder g base-binder continue-link)]
                     [arg-occs (append arg-occs (list base-occ))]
                     [base-expr-link (new-expr-link g #f)]
                     [base-parent (new-parent-link g #f)]
                     [base-term (new-term g base-parent
                                  (make-k-let-val base-binder base-expr-link continue-link) source)])
                (set-parent-link! g base-parent (parent-term letk-term))
                (set-term-link! g letk-body-link base-term)
                (let* ([base-expr-parent (new-parent-link g #f)]
                       [base-expr (new-expr g base-expr-parent (make-k-literal (acc-plan-base plan)) source)])
                  (set-expr-link! g base-expr-link base-expr)
                  (set-parent-link! g base-expr-parent (parent-term base-term))
                  (let* ([continue-args (new-free-vars g arg-occs)]
                         [continue-parent (new-parent-link g #f)]
                         [continue-term (new-term g continue-parent
                                          (make-k-continue loop-occ continue-args) source)])
                    (set-term-link! g continue-link continue-term)
                    (set-parent-link! g continue-parent (parent-term base-term)))))
              (let* ([args (new-free-vars g arg-occs)]
                     [continue-parent (new-parent-link g #f)]
                     [continue-term (new-term g continue-parent (make-k-continue loop-occ args) source)])
                (set-term-link! g continue-link continue-term)
                (set-parent-link! g continue-parent (parent-term letk-term)))))
          (function-vars-set! (graph-function g function) new-args)
          (function-body-set! (graph-function g function) body-link)
          (backpatch-function! g function)
          (for-each
            (lambda (call)
              (if (self-call-tail call)
                (rewrite-tail-call g (self-call-link call) loop-binder accumulator)
                (when plan
                  (rewrite-accumulate-call g call self-cont loop-binder accumulator
                    (acc-plan-prim plan) (acc-plan-mode plan) by-binder))))
            (candidate-calls cand))
          (when plan
            (for-each (lambda (exit) (rewrite-exit g exit accumulator plan))
              (candidate-exits cand)))
          ;; The function no longer recurses unless it escapes as a value.
          (when (null? (collect-occurrences g
                         (function-binder-rooted g (function-var (graph-function g function)))))
            (function-is-rec-set! (graph-function g function) #f))
          #t))))

  (define (rewrite-tail-call g link loop-binder accumulator)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (when (k-app? kind)
            ;; Drop the closure and return-continuation operands, then jump to
            ;; the loop block.  `args' occurrences stay owned by this term.
            (remove-free-occ! g (k-app-func kind))
            (remove-free-occ! g (k-app-cont kind))
            (let ([vars (new-free-vars g (free-vars-slice g (k-app-vars kind)))])
              (let* ([vars (if accumulator
                             (let ([occ (new-free-occ-for-binder g accumulator link)])
                               (free-vars-push g vars occ))
                             vars)]
                     [loop-occ (new-free-occ-for-binder g loop-binder link)])
                (term-kind-set! (graph-term g term) (make-k-continue loop-occ vars)))))))))

  (define (rewrite-accumulate-call g call self-cont loop-binder accumulator prim mode by-binder)
    (let ([term (read-term-link g (self-call-link call))])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (when (k-app? kind)
            (let ([m (match-accumulate-cont g (self-call-cont call) self-cont by-binder)])
              (when m
                (let ([head (vector-ref m 0)]
                      [source (term-source (graph-term g term))]
                      [args (k-app-vars kind)]
                      [link (self-call-link call)])
                  (remove-free-occ! g (k-app-func kind))
                  (remove-free-occ! g (k-app-cont kind))
                  (let* ([next-acc (new-bound-var g (fresh-lvar-derived (bound-var-lvar g head) "loop-acc"))]
                         ;; The jump back to the loop becomes the body of the
                         ;; accumulator update.
                         [continue-link (new-term-link g #f)]
                         [arg-occs (free-vars-slice g args)])
                    (for-each (lambda (occ) (set-free-owner! g occ continue-link)) arg-occs)
                    (let* ([next-occ (new-free-occ-for-binder g next-acc continue-link)]
                           [arg-occs (append arg-occs (list next-occ))]
                           [loop-occ (new-free-occ-for-binder g loop-binder continue-link)]
                           [continue-args (new-free-vars g arg-occs)]
                           [continue-parent (new-parent-link g #f)]
                           [continue-term (new-term g continue-parent
                                            (make-k-continue loop-occ continue-args) source)])
                      (set-term-link! g continue-link continue-term)
                      (set-parent-link! g continue-parent (parent-term term))
                      ;; acc' = (f ... head acc)
                      (let* ([head-occ (new-free-occ-for-binder g head link)]
                             [acc-occ (new-free-occ-for-binder g accumulator link)]
                             [operands (new-free-vars g
                                        (if (eq? mode 'ConsReverse)
                                          (list head-occ acc-occ)
                                          (list acc-occ head-occ)))]
                             [expr-link (new-expr-link g #f)]
                             [expr-parent (new-parent-link g #f)]
                             [expr (new-expr g expr-parent (make-k-prim-call prim operands) source)])
                        (set-expr-link! g expr-link expr)
                        (set-parent-link! g expr-parent (parent-term term))
                        (term-kind-set! (graph-term g term)
                          (make-k-let-val next-acc expr-link continue-link)))))))))))))

  (define (rewrite-exit g link accumulator plan)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (when (k-continue? kind)
            (let ([cont (k-continue-cont kind)]
                  [source (term-source (graph-term g term))]
                  [args (free-vars-slice g (k-continue-vars kind))])
              (when (= (length args) 1)
                (remove-free-occ! g (car args))
                (if (eq? (acc-plan-mode plan) 'LeftFold)
                  ;; The accumulator already holds the exit's base value.
                  (let* ([acc-occ (new-free-occ-for-binder g accumulator link)]
                         [args (new-free-vars g (list acc-occ))])
                    (term-kind-set! (graph-term g term) (make-k-continue cont args)))
                  ;; (reverse acc)
                  (let* ([result (new-bound-var g (fresh-lvar-derived (bound-var-lvar g accumulator) "loop-result"))]
                         [continue-link (new-term-link g #f)])
                    (set-free-owner! g cont continue-link)
                    (let* ([result-occ (new-free-occ-for-binder g result continue-link)]
                           [continue-args (new-free-vars g (list result-occ))]
                           [continue-parent (new-parent-link g #f)]
                           [continue-term (new-term g continue-parent
                                            (make-k-continue cont continue-args) source)])
                      (set-term-link! g continue-link continue-term)
                      (set-parent-link! g continue-parent (parent-term term))
                      (let* ([acc-occ (new-free-occ-for-binder g accumulator link)]
                             [reverse-args (new-free-vars g (list acc-occ))]
                             [expr-link (new-expr-link g #f)]
                             [expr-parent (new-parent-link g #f)]
                             [expr (new-expr g expr-parent (make-k-prim-call 'reverse reverse-args) source)])
                        (set-expr-link! g expr-link expr)
                        (set-parent-link! g expr-parent (parent-term term))
                        (term-kind-set! (graph-term g term)
                          (make-k-let-val result expr-link continue-link)))))))))))))
)
