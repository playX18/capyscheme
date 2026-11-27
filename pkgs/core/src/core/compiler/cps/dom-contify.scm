;; Dominator-based contification candidates.
;;
;; Port of crates/capy/src/compiler/cps/dom_contify.rs.
;;
;; API:
;;   (find-candidate choose-insertion graph active-link term live body)
;;       Same convention as (core compiler cps scc-contify): CHOOSE-INSERTION is
;;       (lambda (graph site return-cont binders functions) ...) => insertion or #f,
;;       LIVE a list of (function-link . function-id) pairs.
;;       => contify-candidate (source 'Dominator) or #f
;;
;; Determinism notes (relative to the Rust):
;;   * the `nodes' / `reverse' HashMaps and the `destinations' HashMap are only
;;     looked up; the one iteration (`nodes.iter()' to add the root -> cont
;;     edges) only adds edges, and the immediate-dominator tree does not depend
;;     on edge order, so any order gives the same result.  Here the dominators
;;     are computed with the Cooper-Harvey-Kennedy algorithm (petgraph's
;;     `simple_fast'), whose result is unique.
;;   * `groups' follows `live' order; the final sort by group size (descending)
;;     is stable, as Vec::sort_by_key is.
(library (core compiler cps dom-contify)
  (export find-candidate)
  (import (core) (capy) (core compiler cps graph) (core compiler cps scc-contify))

  ;; DomLoc / Destination: (Root), (Func . id), (Cont . id); compared with equal?.
  (define loc-root (cons 'Root 0))
  (define (loc-func f) (cons 'Func f))
  (define (loc-cont b) (cons 'Cont b))
  ;; fixnum key of a location for hashtable lookup (never iterated)
  (define (loc-key loc)
    (case (car loc)
      [(Root) 0]
      [(Func) (+ 1 (* 4 (cdr loc)))]
      [else (+ 2 (* 4 (cdr loc)))]))

  (define-record-type (call-fact make-call-fact call-fact?)
    (fields caller proc callee cont is-tail arg-count))

  (define-record-type (dom-facts make-dom-facts dom-facts?)
    (fields
      functions-by-binder               ; eqv-hashtable binder -> function
      live-binders escaped-binders ineligible   ; entity-sets of BoundVar
      (mutable calls dom-facts-calls dom-facts-calls-set!)))

  ;; With OWNER-OF, a procedure from a function to the term link of the Fix or Letk
  ;; that binds it, the facts come from the uses of the binders (see
  ;; `for-each-use-of-live-binders' in scc-contify) instead of a walk of the scope.
  ;; Nothing below depends on the order of the calls: the immediate dominators are
  ;; unique, and the groups follow LIVE.
  (define find-candidate
    (case-lambda
      [(choose-insertion g active-link term live body)
        (find-candidate* choose-insertion g active-link live body (collect-facts g live body))]
      [(choose-insertion g active-link term live body owner-of)
        (find-candidate* choose-insertion g active-link live body
          (or (collect-facts/uses g term live owner-of) (collect-facts g live body)))]))

  (define (find-candidate* choose-insertion g active-link live body facts)
    (let* ([reachable (reachable-functions facts)]
           [groups (analyze-dominators g live facts reachable)]
           [destinations (make-eqv-hashtable)]
           [destination-count 0])
      (for-each
        (lambda (group)
          (for-each
            (lambda (function)
              (unless (hashtable-ref destinations function #f)
                (set! destination-count (+ destination-count 1)))
              (hashtable-set! destinations function (car group)))
            (cdr group)))
        groups)
      (choose-candidate choose-insertion g active-link live body facts
        destinations destination-count groups)))

  (define (new-facts g live)
    (let ([facts (make-dom-facts (make-eqv-hashtable) (make-entity-set)
                   (make-entity-set) (make-entity-set) '())])
      (for-each
        (lambda (entry)
          (let* ([function (cdr entry)]
                 [data (graph-function g function)]
                 [binder (function-var data)])
            (hashtable-set! (dom-facts-functions-by-binder facts) binder function)
            (entity-set-insert! (dom-facts-live-binders facts) binder)
            (when (or (not (function-cont data))
                      (function-is-variadic data)
                      (function-variadic data)
                      (function-case-group data))
              (entity-set-insert! (dom-facts-ineligible facts) binder))))
        live)
      facts))

  ;; `collect-facts' from the uses of the binders, or #f when they cannot be followed.
  (define (collect-facts/uses g term live owner-of)
    (let ([facts (new-facts g live)])
      (and
        (for-each-use-of-live-binders g term live owner-of
          (lambda (ctx binder kind)
            (cond
              [(eq? ctx 'body) (add-call! g facts kind #f #f #f)]
              [(eq? (car ctx) 'live)
                (let ([f (cdr ctx)])
                  (add-call! g facts kind f (function-cont (graph-function g f)) f))]
              [else (add-call! g facts kind #f #f (cdr ctx))]))
          (lambda (binder) (entity-set-insert! (dom-facts-escaped-binders facts) binder)))
        (begin
          (dom-facts-calls-set! facts (reverse (dom-facts-calls facts)))
          facts))))

  ;; Records the App KIND made from CALLER (#f outside the functions of the Fix) in
  ;; PROC, when its operator is one of the functions.
  (define (add-call! g facts kind caller current-return-cont proc)
    (let* ([args (k-app-vars kind)]
           [cont (k-app-cont kind)]
           [callee-binder (free-binder g (k-app-func kind))]
           [callee (hashtable-ref (dom-facts-functions-by-binder facts) callee-binder #f)])
      (when callee
        (let* ([arg-count (length (free-vars-slice g args))]
               [data (graph-function g callee)])
          (when (or (function-is-variadic data)
                    (function-variadic data)
                    (not (= arg-count (length (bound-vars-slice g (function-vars data))))))
            (entity-set-insert! (dom-facts-ineligible facts) callee-binder))
          (let ([cont-binder (free-binder g cont)])
            (dom-facts-calls-set! facts
              (cons (make-call-fact caller proc callee cont-binder
                      (and current-return-cont (eqv? current-return-cont cont-binder))
                      arg-count)
                (dom-facts-calls facts))))))))

  (define (collect-facts g live body)
    (let ([facts (new-facts g live)])
      (collect-calls g facts body #f #f #f)
      (for-each
        (lambda (entry)
          (let* ([function (cdr entry)]
                 [data (graph-function g function)])
            (collect-calls g facts (function-body data) function (function-cont data) function)))
        live)
      (dom-facts-calls-set! facts (reverse (dom-facts-calls facts)))
      facts))

  (define (mark-if-live-binder-escapes! g facts var)
    (let ([binder (free-binder g var)])
      (when (entity-set-contains? (dom-facts-live-binders facts) binder)
        (entity-set-insert! (dom-facts-escaped-binders facts) binder))))

  (define (mark-free-vars-escape! g facts vars)
    (for-each (lambda (var) (mark-if-live-binder-escapes! g facts var)) vars))

  (define (collect-calls g facts link caller current-return-cont proc)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind)
              (let ([expr (read-expr-link g (k-let-val-expr kind))])
                (when expr
                  (for-each-free-var-of-expr g expr
                    (lambda (var) (mark-if-live-binder-escapes! g facts var)))))
              (collect-calls g facts (k-let-val-body kind) caller current-return-cont proc)]
            [(k-fix? kind)
              (collect-calls g facts (k-fix-body kind) caller current-return-cont proc)
              (for-each
                (lambda (flink)
                  (let ([function (read-function-link g flink)])
                    (when function
                      (collect-calls g facts (function-body (graph-function g function))
                        #f #f function))))
                (function-links-slice g (k-fix-functions kind)))]
            [(k-letk? kind)
              (for-each
                (lambda (flink)
                  (let ([function (read-function-link g flink)])
                    (when function
                      (collect-calls g facts (function-body (graph-function g function))
                        caller current-return-cont proc))))
                (function-links-slice g (k-letk-functions kind)))
              (collect-calls g facts (k-letk-body kind) caller current-return-cont proc)]
            [(k-if? kind)
              (mark-if-live-binder-escapes! g facts (k-if-test kind))
              (collect-calls g facts (k-if-then kind) caller current-return-cont proc)
              (collect-calls g facts (k-if-else kind) caller current-return-cont proc)]
            [(k-continue? kind)
              (mark-if-live-binder-escapes! g facts (k-continue-cont kind))
              (mark-free-vars-escape! g facts (free-vars-slice g (k-continue-vars kind)))]
            [(k-raise? kind)
              (mark-free-vars-escape! g facts (free-vars-slice g (k-raise-vars kind)))]
            [(k-branch-prim? kind)
              (mark-if-live-binder-escapes! g facts (k-branch-prim-consequent kind))
              (mark-if-live-binder-escapes! g facts (k-branch-prim-alternative kind))
              (mark-free-vars-escape! g facts (free-vars-slice g (k-branch-prim-vars kind)))]
            [(k-app? kind)
              (let ([args (k-app-vars kind)] [cont (k-app-cont kind)])
                (mark-free-vars-escape! g facts (free-vars-slice g args))
                (mark-if-live-binder-escapes! g facts cont)
                (add-call! g facts kind caller current-return-cont proc))]
            [else (if #f #f)])))))

  (define (call-reachable? call reachable)
    (let ([caller (call-fact-caller call)])
      (or (not caller) (entity-set-contains? reachable caller))))

  (define (reachable-functions facts)
    (let ([reachable (make-entity-set)])
      (let loop ()
        (let ([changed #f])
          (for-each
            (lambda (call)
              (when (and (call-reachable? call reachable)
                         (not (entity-set-contains? reachable (call-fact-callee call))))
                (entity-set-insert! reachable (call-fact-callee call))
                (set! changed #t)))
            (dom-facts-calls facts))
          (when changed (loop))))
      reachable))

  ;; Immediate dominators of every node reachable from ROOT (Cooper, Harvey,
  ;; Kennedy).  Returns a vector; unreachable nodes and ROOT hold #f.
  (define (immediate-dominators n root succs preds)
    (let ([po (make-vector n #f)]            ; postorder number
          [order (make-vector n #f)]         ; postorder number -> node
          [visited (make-vector n #f)]
          [counter 0])
      ;; iterative DFS postorder
      (let ([stack (list (cons root (vector-ref succs root)))])
        (vector-set! visited root #t)
        (let loop ()
          (unless (null? stack)
            (let* ([frame (car stack)] [node (car frame)] [rest (cdr frame)])
              (cond
                [(null? rest)
                  (vector-set! po node counter)
                  (vector-set! order counter node)
                  (set! counter (+ counter 1))
                  (set! stack (cdr stack))]
                [else
                  (set-cdr! frame (cdr rest))
                  (let ([next (car rest)])
                    (unless (vector-ref visited next)
                      (vector-set! visited next #t)
                      (set! stack (cons (cons next (vector-ref succs next)) stack))))])
              (loop)))))
      (let ([idom (make-vector n #f)])
        (vector-set! idom root root)
        (define (intersect a b)
          (let loop ([a a] [b b])
            (cond
              [(= a b) a]
              [(< (vector-ref po a) (vector-ref po b)) (loop (vector-ref idom a) b)]
              [(< (vector-ref po b) (vector-ref po a)) (loop a (vector-ref idom b))]
              [else a])))
        (let loop ()
          (let ([changed #f])
            ;; reverse postorder, skipping the root (which has the highest number)
            (let node-loop ([k (- counter 2)])
              (when (>= k 0)
                (let* ([node (vector-ref order k)]
                       [new-idom
                        (let pred-loop ([ps (vector-ref preds node)] [acc #f])
                          (cond
                            [(null? ps) acc]
                            [(not (vector-ref idom (car ps))) (pred-loop (cdr ps) acc)]
                            [(not acc) (pred-loop (cdr ps) (car ps))]
                            [else (pred-loop (cdr ps) (intersect (car ps) acc))]))])
                  (when (and new-idom (not (eqv? (vector-ref idom node) new-idom)))
                    (vector-set! idom node new-idom)
                    (set! changed #t)))
                (node-loop (- k 1))))
            (when changed (loop))))
        (vector-set! idom root #f)
        idom)))

  (define (analyze-dominators g live facts reachable)
    (let ([nodes (make-eqv-hashtable)]       ; loc-key -> node index
          [locs '()]                         ; reversed list of locs, by node index
          [count 0]
          [edges '()])                       ; reversed (from . to)
      (define (add-node! loc)
        (let ([key (loc-key loc)])
          (or (hashtable-ref nodes key #f)
              (let ([node count])
                (hashtable-set! nodes key node)
                (set! locs (cons loc locs))
                (set! count (+ count 1))
                node))))
      (define (node-of loc) (hashtable-ref nodes (loc-key loc) #f))
      (define (add-edge! from to) (set! edges (cons (cons from to) edges)))
      (let ([root (add-node! loc-root)]
            [conts '()])
        (for-each (lambda (entry) (add-node! (loc-func (cdr entry)))) live)
        (for-each
          (lambda (call)
            (let ([before count])
              (add-node! (loc-cont (call-fact-cont call)))
              (when (> count before) (set! conts (cons (- count 1) conts)))))
          (dom-facts-calls facts))
        (for-each (lambda (cont) (add-edge! root cont)) (reverse conts))
        (for-each
          (lambda (entry)
            (let ([function (cdr entry)])
              (unless (entity-set-contains? reachable function)
                (add-edge! root (node-of (loc-func function))))))
          live)
        (for-each
          (lambda (call)
            (when (call-reachable? call reachable)
              (let ([callee (node-of (loc-func (call-fact-callee call)))])
                (if (and (call-fact-is-tail call) (call-fact-caller call))
                  (add-edge! (node-of (loc-func (call-fact-caller call))) callee)
                  (add-edge! (node-of (loc-cont (call-fact-cont call))) callee)))))
          (dom-facts-calls facts))
        (let* ([n count]
               [loc-vector (list->vector (reverse locs))]
               [succs (make-vector n '())]
               [preds (make-vector n '())])
          (for-each
            (lambda (edge)
              (vector-set! succs (car edge) (cons (cdr edge) (vector-ref succs (car edge))))
              (vector-set! preds (cdr edge) (cons (car edge) (vector-ref preds (cdr edge)))))
            (reverse edges))
          (let ([idom (immediate-dominators n root succs preds)]
                [groups '()])
            (define (topmost-non-root-dominator node)
              (let loop ([cursor node] [topmost #f])
                (let ([parent (vector-ref idom cursor)])
                  (if (or (not parent) (= parent root))
                    topmost
                    (loop parent parent)))))
            (for-each
              (lambda (entry)
                (let* ([function (cdr entry)]
                       [binder (function-var (graph-function g function))])
                  (unless (or (entity-set-contains? (dom-facts-escaped-binders facts) binder)
                              (entity-set-contains? (dom-facts-ineligible facts) binder)
                              (not (entity-set-contains? reachable function)))
                    (let* ([node (node-of (loc-func function))]
                           [top (topmost-non-root-dominator node)])
                      (when top
                        (let ([loc (vector-ref loc-vector top)])
                          ;; destination_from_loc: Root has none
                          (unless (eq? (car loc) 'Root)
                            (unless (and (eq? (car loc) 'Func) (= (cdr loc) function))
                              (set! groups (push-group groups loc function))))))))))
              live)
            (stable-sort-by-lt groups
              (lambda (a b) (> (length (cdr a)) (length (cdr b))))))))))

  ;; GROUPS: list of (destination . functions); adds FUNCTION to the group of
  ;; DESTINATION (creating it at the end when new).
  (define (push-group groups destination function)
    (cond
      [(null? groups) (list (list destination function))]
      [(equal? (caar groups) destination)
        (cons (append (car groups) (list function)) (cdr groups))]
      [else (cons (car groups) (push-group (cdr groups) destination function))]))

  (define (choose-candidate choose-insertion g active-link live body facts
            destinations destination-count groups)
    (let loop ([groups groups])
      (if (null? groups)
        #f
        (let* ([destination (caar groups)]
               [functions (cdar groups)]
               [binders (make-entity-set)])
          (for-each (lambda (f) (entity-set-insert! binders (function-var (graph-function g f))))
            functions)
          (let ([return-cont
                 (if (eq? (car destination) 'Cont)
                   (cdr destination)
                   (let ([function (cdr destination)])
                     (if (entity-set-contains? binders (function-var (graph-function g function)))
                       #f
                       (resolve-destination-cont g destinations destination-count function))))])
            (cond
              [(not return-cont) (loop (cdr groups))]
              [(not (calls-return-to? g facts functions return-cont)) (loop (cdr groups))]
              [(not (calls-share-proc? facts functions)) (loop (cdr groups))]
              [else
                (let ([site (contification-site g active-link live body binders)])
                  (and site
                    (let ([insertion (choose-insertion g site return-cont binders functions)])
                      (if insertion
                        (make-contify-candidate binders return-cont insertion 'Dominator)
                        (loop (cdr groups))))))]))))))

  ;; A contified `Letk' is a block of one procedure; calls from two different
  ;; procedures cannot reach it.  A call made from inside the group (a loop's own
  ;; back edge, or `inner' calling `loop') becomes a jump within the block the
  ;; group turns into, so only the calls from outside decide which procedure
  ;; the group lands in.
  (define (calls-share-proc? facts functions)
    (let loop ([calls (dom-facts-calls facts)] [proc #f] [first #t])
      (cond
        [(null? calls) #t]
        [else
          (let ([call (car calls)])
            (cond
              [(not (memv (call-fact-callee call) functions)) (loop (cdr calls) proc first)]
              [(and (call-fact-proc call) (memv (call-fact-proc call) functions))
                (loop (cdr calls) proc first)]
              [first (loop (cdr calls) (call-fact-proc call) #f)]
              [(not (eqv? (call-fact-proc call) proc)) #f]
              [else (loop (cdr calls) proc first)]))])))

  ;; Every call to a group member must return to the group's common
  ;; continuation.  Calls whose continuation is one of the members' own tail
  ;; labels are covered because the transform substitutes those binders too.
  (define (calls-return-to? g facts functions return-cont)
    (let ([tails (make-entity-set)])
      (for-each
        (lambda (function)
          (let ([cont (function-cont (graph-function g function))])
            (when cont (entity-set-insert! tails cont))))
        functions)
      (let loop ([calls (dom-facts-calls facts)])
        (cond
          [(null? calls) #t]
          [(not (memv (call-fact-callee (car calls)) functions)) (loop (cdr calls))]
          [(or (eqv? (call-fact-cont (car calls)) return-cont)
               (entity-set-contains? tails (call-fact-cont (car calls))))
            (loop (cdr calls))]
          [else #f]))))

  ;; Follow `Func' destinations until a continuation.
  (define (resolve-destination-cont g destinations destination-count function)
    (let loop ([i 0] [current function])
      (if (> i destination-count)
        #f
        (let ([dest (hashtable-ref destinations current #f)])
          (cond
            [(and dest (eq? (car dest) 'Cont)) (cdr dest)]
            [(and dest (eq? (car dest) 'Func) (not (= (cdr dest) current)))
              (loop (+ i 1) (cdr dest))]
            [else (function-cont (graph-function g current))])))))
)
