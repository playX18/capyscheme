;; Contification via SCCs over the call graph.
(library (core compiler cps scc-contify)
  (export
    find-candidate contification-site
    make-contify-candidate contify-candidate? contify-candidate-binders
    contify-candidate-return-cont contify-candidate-insertion
    contify-candidate-source
    make-contify-insertion contify-insertion? contify-insertion-kind
    contify-insertion-site contify-insertion-with-site
    kosaraju-scc stable-sort-by-lt
    for-each-use-of-live-binders)
  (import (core) (capy) (core compiler cps graph))

  ;;; Facts from the uses of the binders
  ;;
  ;; Both contifiers need the calls of, and the other uses of, the functions LIVE of
  ;; the Fix TERM. Walking the body of the Fix and the bodies of its functions finds
  ;; them, but that costs the size of the whole scope, once for every visit of every
  ;; Fix. A program merged into one `letrec*' (the static linker) is a chain of Fixes,
  ;; each the body of the one before, so the walks cost the square of its size.
  ;;
  ;; The occurrence lists of the binders reach the same uses directly. For every
  ;; occurrence, the walk up the parent links from the term that holds it to TERM says
  ;; which procedure the use is in:
  ;;
  ;;   body          in the body of TERM, outside every function of a Fix
  ;;   (live . F)    in the body of F, a function of TERM
  ;;   (nested . F)  in the body of F, a function of a Fix inside the scope
  ;;
  ;; A Letk is transparent: its functions are continuations of the procedure they are
  ;; in. Every step is checked (the link is active, the function is a live member of
  ;; its group); when a check fails, or a use is not below TERM, the result is #f and
  ;; the caller walks the scope as before. OWNER-OF maps a function to the term link
  ;; of the Fix or Letk that binds it.
  ;;
  ;; ON-CALL is called with the context, the binder and the kind of the App for a use
  ;; as the operator of an App, ON-ESCAPE with the binder for any other use. Returns
  ;; #t when every use was classified.
  (define (for-each-use-of-live-binders g term live owner-of on-call on-escape)
    (let ([memo (make-eqv-hashtable)]       ; term -> context, for terms proven below TERM
          [live-set (make-entity-set)])
      (define (member-of-group? kind f)
        (and (memv f (live-functions g (if (k-fix? kind) (k-fix-functions kind) (k-letk-functions kind))))
          #t))
      (define (commit! assigned)
        (for-each (lambda (entry) (hashtable-set! memo (car entry) (cdr entry))) assigned))
      (define (assign terms context assigned)
        (fold-left (lambda (acc t) (cons (cons t context) acc)) assigned terms))
      ;; PENDING: the terms passed since the last function boundary, which take the
      ;; context of the term the walk is at. ASSIGNED: terms whose context is known,
      ;; committed once the walk reaches TERM.
      (define (context start)
        (let loop ([cur start] [pending '()] [assigned '()])
          (cond
            [(hashtable-ref memo cur #f)
              => (lambda (ctx)
                   (commit! (assign pending ctx assigned))
                   (hashtable-ref memo start #f))]
            [(eqv? cur term)
              ;; only reached from below, as the parent of a term in the body
              (and (pair? pending)
                (begin
                  (commit! (assign pending 'body assigned))
                  (hashtable-ref memo start #f)))]
            [else
              (let ([parent (read-parent-link g (term-uplink (graph-term g cur)))])
                (cond
                  [(not parent) #f]
                  [(not (get-active-link-for g cur)) #f]
                  [(parent-term? parent) (loop (parent-id parent) (cons cur pending) assigned)]
                  [else
                    (let* ([f (parent-id parent)]
                           [owner-link (owner-of f)]
                           [owner (and owner-link (read-term-link g owner-link))]
                           [kind (and owner (term-kind (graph-term g owner)))])
                      (cond
                        [(not (and kind (or (k-fix? kind) (k-letk? kind)) (member-of-group? kind f))) #f]
                        [(k-letk? kind) (loop owner (cons cur pending) assigned)]
                        [(eqv? owner term)
                          (and (entity-set-contains? live-set f)
                            (begin
                              (commit! (assign (cons cur pending) (cons 'live f) assigned))
                              (hashtable-ref memo start #f)))]
                        [else
                          (loop owner '() (assign (cons cur pending) (cons 'nested f) assigned))]))]))])))
      ;; The free variables a term holds itself (a LetVal: those of its expression).
      (define (held-vars t kind)
        (if (k-let-val? kind)
          (let ([expr (read-expr-link g (k-let-val-expr kind))])
            (if expr (free-vars-of-expr g expr) '()))
          (direct-free-vars-of-term g t)))
      (for-each (lambda (entry) (entity-set-insert! live-set (cdr entry))) live)
      (let live-loop ([entries live])
        (or (null? entries)
          (let ([binder (function-var (graph-function g (cdr (car entries))))])
            (and
              (let use-loop ([occs (collect-occurrences g binder)])
                (or (null? occs)
                  (let* ([occ (car occs)]
                         [t (and (eqv? (free-binder g occ) binder)
                                 (read-term-link g (free-owner g occ)))]
                         [kind (and t (term-kind (graph-term g t)))]
                         [ctx (and kind (memv occ (held-vars t kind)) (context t))])
                    (and ctx
                      (begin
                        (if (and (k-app? kind) (eqv? (k-app-func kind) occ))
                          (on-call ctx binder kind)
                          (on-escape binder))
                        (use-loop (cdr occs)))))))
              (live-loop (cdr entries))))))))

  (define-record-type (contify-candidate make-contify-candidate contify-candidate?)
    (fields binders return-cont insertion source))

  (define-record-type (contify-insertion make-contify-insertion contify-insertion?)
    (fields kind site))

  (define (contify-insertion-with-site insertion site)
    (make-contify-insertion (contify-insertion-kind insertion) site))

  ;; Stable merge sort.
  (define (stable-sort-by-lt lst less?)
    (define (merge a b)
      (cond
        [(null? a) b]
        [(null? b) a]
        ;; take from B only when strictly less, so equal keys keep A first
        [(less? (car b) (car a)) (cons (car b) (merge a (cdr b)))]
        [else (cons (car a) (merge (cdr a) b))]))
    (define (split lst)
      (if (or (null? lst) (null? (cdr lst)))
        (values lst '())
        (let loop ([slow lst] [fast (cdr lst)] [left '()])
          (if (or (null? fast) (null? (cdr fast)))
            (values (reverse (cons (car slow) left)) (cdr slow))
            (loop (cdr slow) (cddr fast) (cons (car slow) left))))))
    (if (or (null? lst) (null? (cdr lst)))
      lst
      (let-values ([(a b) (split lst)])
        (merge (stable-sort-by-lt a less?) (stable-sort-by-lt b less?)))))

  ;; Iterative DFS 
  (define (kosaraju-scc n out-adj in-adj)
    (let ([discovered (make-vector n #f)]
          [finished (make-vector n #f)]
          [finish-order '()])
      ;; first phase: DfsPostOrder over the reversed graph
      (let start-loop ([i 0])
        (when (< i n)
          (unless (vector-ref discovered i)
            (let ([stack (list i)])
              (let step ()
                (unless (null? stack)
                  (let ([nx (car stack)])
                    (if (not (vector-ref discovered nx))
                      (begin
                        (vector-set! discovered nx #t)
                        (for-each
                          (lambda (succ)
                            (unless (vector-ref discovered succ)
                              (set! stack (cons succ stack))))
                          (vector-ref in-adj nx)))
                      (begin
                        (set! stack (cdr stack))
                        (unless (vector-ref finished nx)
                          (vector-set! finished nx #t)
                          (set! finish-order (cons nx finish-order))))))
                  (step)))))
          (start-loop (+ i 1))))
      ;; second phase: Dfs over the graph in decreasing finishing time
      ;; (finish-order is already reversed: latest finished first)
      (let ([seen (make-vector n #f)])
        (let phase2 ([order finish-order] [sccs '()])
          (cond
            [(null? order) (reverse sccs)]
            [(vector-ref seen (car order)) (phase2 (cdr order) sccs)]
            [else
              (let ([stack (list (car order))] [scc '()])
                (let step ()
                  (unless (null? stack)
                    (let ([node (car stack)])
                      (set! stack (cdr stack))
                      (unless (vector-ref seen node)
                        (vector-set! seen node #t)
                        (for-each
                          (lambda (succ)
                            (unless (vector-ref seen succ)
                              (set! stack (cons succ stack))))
                          (vector-ref out-adj node))
                        (set! scc (cons node scc))))
                    (step)))
                (phase2 (cdr order) (cons (reverse scc) sccs)))])))))

  (define-record-type (call-fact make-call-fact call-fact?)
    (fields caller proc callee ret arg-count))

  (define-record-type (facts make-facts facts?)
    (fields
      by-binder                          ; eqv-hashtable binder -> function
      candidates                         ; entity-set of function ids
      (mutable calls facts-calls facts-calls-set!)   ; reversed while collecting
      returns                            ; eqv-hashtable function -> entity-set
      tails))                            ; eqv-hashtable function -> bound var

  (define (live-function live-entry) (cdr live-entry))

  ;; With OWNER-OF (see `for-each-use-of-live-binders'), the facts of the uses decide
  ;; first whether a group can pass the checks of `attempt' before its site is looked
  ;; for. Those checks do not depend on the order of the calls, so when no group
  ;; passes there is no candidate. Otherwise the search runs as without OWNER-OF:
  ;; which group is tried first depends on the order the walk finds the calls in.
  (define find-candidate
    (case-lambda
      [(choose-insertion g active-link term live body)
        (find-candidate/walk choose-insertion g active-link term live body)]
      [(choose-insertion g active-link term live body owner-of)
        (let ([facts (collect-facts/uses g term live owner-of)])
          (and (or (not facts) (possible-group? g facts))
            (find-candidate/walk choose-insertion g active-link term live body)))]))

  (define (possible-group? g facts)
    (and (not (null? (entity-set->list (facts-candidates facts))))
      (exists
        (lambda (group)
          (or (group-passes? facts group)
            (and (> (length group) 1)
              (exists (lambda (f) (group-passes? facts (list f))) group))))
        (call-sccs facts))))

  ;; `attempt' up to the site: every member is a candidate, the calls of the group
  ;; return to one continuation, and all are made from one procedure.
  (define (group-passes? facts group)
    (and (for-all (lambda (f) (entity-set-contains? (facts-candidates facts) f)) group)
      (let ([returns (make-entity-set)])
        (for-each
          (lambda (f)
            (for-each (lambda (r) (entity-set-insert! returns r))
              (entity-set->list (hashtable-ref (facts-returns facts) f #f))))
          group)
        (for-each (lambda (f) (entity-set-remove! returns (hashtable-ref (facts-tails facts) f #f)))
          group)
        (let ([rs (entity-set->list returns)])
          (and (pair? rs) (null? (cdr rs))
            (let loop ([calls (facts-calls facts)] [proc #f] [first #t])
              (cond
                [(null? calls) #t]
                [(not (memv (call-fact-callee (car calls)) group)) (loop (cdr calls) proc first)]
                [first (loop (cdr calls) (call-fact-proc (car calls)) #f)]
                [(eqv? (call-fact-proc (car calls)) proc) (loop (cdr calls) proc first)]
                [else #f])))))))

  ;; `collect-facts' from the uses of the binders, or #f when they cannot be followed.
  (define (collect-facts/uses g term live owner-of)
    (let ([facts (make-facts (make-eqv-hashtable) (make-entity-set) '()
                   (make-eqv-hashtable) (make-eqv-hashtable))])
      (init-facts! g facts live)
      (cond
        ;; no function can be contified: the calls do not matter
        [(null? (entity-set->list (facts-candidates facts))) facts]
        [(for-each-use-of-live-binders g term live owner-of
           (lambda (ctx binder kind)
             (let ([caller (if (eq? ctx 'body) #f (cdr ctx))])
               (add-call! g facts caller caller
                 (hashtable-ref (facts-by-binder facts) binder #f) kind)))
           (lambda (binder) (if #f #f)))
          (finish-facts! g facts)
          facts]
        [else #f])))

  (define (find-candidate/walk choose-insertion g active-link term live body)
    (let* ([facts (collect-facts g live body)]
           [groups (call-sccs facts)])
      (let loop ([groups groups])
        (if (null? groups)
          #f
          (let ([group (car groups)])
            (or (attempt choose-insertion g active-link live body facts group)
                (and (> (length group) 1)
                  (let inner ([fs group])
                    (and (pair? fs)
                      (or (attempt choose-insertion g active-link live body facts
                            (list (car fs)))
                        (inner (cdr fs))))))
                (loop (cdr groups))))))))

  (define (all-uses-are-calls? g binder)
    (let ([ok #t])
      (for-each-occurrence g binder
        (lambda (occ)
          (when ok
            (let ([term (read-term-link g (free-owner g occ))])
              (if (not term)
                (set! ok #f)
                (let ([kind (term-kind (graph-term g term))])
                  (unless (and (k-app? kind) (eqv? (k-app-func kind) occ))
                    (set! ok #f))))))))
      ok))

  (define (init-facts! g facts live)
    (for-each
      (lambda (entry)
        (let ([function (live-function entry)])
          (hashtable-set! (facts-by-binder facts)
            (function-var (graph-function g function)) function)))
      live)
    (for-each
      (lambda (entry)
        (let* ([function (live-function entry)]
               [data (graph-function g function)])
          (unless (or (not (function-cont data))
                      (function-is-variadic data)
                      (function-variadic data)
                      (function-case-group data))
            (when (all-uses-are-calls? g (function-var data))
              (entity-set-insert! (facts-candidates facts) function)
              (hashtable-set! (facts-tails facts) function (function-cont data))
              (hashtable-set! (facts-returns facts) function (make-entity-set))))))
      live))

  ;; Records the call KIND (an App) of CALLEE made from CALLER in PROC.
  (define (add-call! g facts caller proc callee kind)
    (let ([ret (free-binder g (k-app-cont kind))])
      (facts-calls-set! facts
        (cons (make-call-fact caller proc callee ret
                (length (free-vars-slice g (k-app-vars kind))))
          (facts-calls facts)))
      (when (entity-set-contains? (facts-candidates facts) callee)
        (entity-set-insert! (hashtable-ref (facts-returns facts) callee #f) ret))))

  (define (finish-facts! g facts)
    (facts-calls-set! facts (reverse (facts-calls facts)))
    ;; Drop candidates with a call whose argument count does not match.
    (let ([bad (make-entity-set)])
      (for-each
        (lambda (call)
          (when (entity-set-contains? (facts-candidates facts) (call-fact-callee call))
            (let ([arity (length (bound-vars-slice g
                                   (function-vars (graph-function g (call-fact-callee call)))))])
              (unless (= (call-fact-arg-count call) arity)
                (entity-set-insert! bad (call-fact-callee call))))))
        (facts-calls facts))
      (for-each (lambda (f) (entity-set-remove! (facts-candidates facts) f))
        (entity-set->list bad))))

  (define (collect-facts g live body)
    (let ([facts (make-facts (make-eqv-hashtable) (make-entity-set) '()
                   (make-eqv-hashtable) (make-eqv-hashtable))])
      (init-facts! g facts live)
      (collect-calls g facts body #f #f)
      (for-each
        (lambda (entry)
          (let ([function (live-function entry)])
            (collect-calls g facts (function-body (graph-function g function))
              function function)))
        live)
      (finish-facts! g facts)
      facts))

  (define (collect-calls g facts link caller proc)
    (let ([term (read-term-link g link)])
      (when term
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind) (collect-calls g facts (k-let-val-body kind) caller proc)]
            [(k-letk? kind)
              (for-each
                (lambda (function)
                  (collect-calls g facts (function-body (graph-function g function)) caller proc))
                (live-functions g (k-letk-functions kind)))
              (collect-calls g facts (k-letk-body kind) caller proc)]
            [(k-fix? kind)
              (for-each
                (lambda (function)
                  (collect-calls g facts (function-body (graph-function g function))
                    function function))
                (live-functions g (k-fix-functions kind)))
              (collect-calls g facts (k-fix-body kind) caller proc)]
            [(k-if? kind)
              (collect-calls g facts (k-if-then kind) caller proc)
              (collect-calls g facts (k-if-else kind) caller proc)]
            [(k-app? kind)
              (let* ([callee-binder (free-binder g (k-app-func kind))]
                     [callee (hashtable-ref (facts-by-binder facts) callee-binder #f)])
                (when callee (add-call! g facts caller proc callee kind)))]
            [else (if #f #f)])))))

  ;; Strongly connected components of the candidate call graph, largest first,
  ;; ties broken by the first function id.
  (define (call-sccs facts)
    (let* ([candidates (entity-set->list (facts-candidates facts))]
           [n (length candidates)]
           [index (make-eqv-hashtable)]
           [out-adj (make-vector n '())]
           [in-adj (make-vector n '())]
           [node-function (list->vector candidates)])
      (let loop ([cs candidates] [i 0])
        (unless (null? cs)
          (hashtable-set! index (car cs) i)
          (loop (cdr cs) (+ i 1))))
      (for-each
        (lambda (call)
          (let ([caller (call-fact-caller call)] [callee (call-fact-callee call)])
            (when (and caller
                       (entity-set-contains? (facts-candidates facts) caller)
                       (entity-set-contains? (facts-candidates facts) callee))
              (let ([from (hashtable-ref index caller #f)]
                    [to (hashtable-ref index callee #f)])
                ;; petgraph lists the newest edge first
                (vector-set! out-adj from (cons to (vector-ref out-adj from)))
                (vector-set! in-adj to (cons from (vector-ref in-adj to)))))))
        (facts-calls facts))
      (let ([groups (map (lambda (component)
                          (map (lambda (node) (vector-ref node-function node)) component))
                     (kosaraju-scc n out-adj in-adj))])
        (stable-sort-by-lt groups
          (lambda (a b)
            (let ([la (length a)] [lb (length b)])
              (or (> la lb)
                  (and (= la lb) (< (car a) (car b))))))))))

  (define (attempt choose-insertion g active-link live body facts group)
    (let ([binders (make-entity-set)]
          [returns (make-entity-set)]
          [tails (make-entity-set)])
      (let ([ok
             (let loop ([fs group])
               (cond
                 [(null? fs) #t]
                 [(not (entity-set-contains? (facts-candidates facts) (car fs))) #f]
                 [else
                   (let ([function (car fs)])
                     (entity-set-insert! binders (function-var (graph-function g function)))
                     (for-each (lambda (r) (entity-set-insert! returns r))
                       (entity-set->list (hashtable-ref (facts-returns facts) function #f)))
                     (entity-set-insert! tails (hashtable-ref (facts-tails facts) function #f))
                     (loop (cdr fs)))]))])
        (and ok
          (begin
            (for-each (lambda (t) (entity-set-remove! returns t)) (entity-set->list tails))
            (let ([rs (entity-set->list returns)])
              (and (pair? rs) (null? (cdr rs))
                (let ([return-cont (car rs)]
                      [proc #f] [first #t] [shared #t])
                  (for-each
                    (lambda (call)
                      (when (and shared (memv (call-fact-callee call) group))
                        (if first
                          (begin (set! proc (call-fact-proc call)) (set! first #f))
                          (unless (eqv? (call-fact-proc call) proc)
                            (set! shared #f)))))
                    (facts-calls facts))
                  (and shared
                    (let ([site (contification-site g active-link live body binders)])
                      (and site
                        (let ([insertion (choose-insertion g site return-cont binders group)])
                          (and insertion
                            (make-contify-candidate binders return-cont insertion 'Scc))))))))))))))

  ;; PushSearch is (count . site).

  (define push-none (cons 0 #f))

  (define (combine-push-sites current children)
    (let ([active (filter (lambda (child) (not (= (car child) 0))) children)])
      (cond
        [(null? active) push-none]
        [(null? (cdr active)) (car active)]
        [else
          (cons (fold-left + 0 (map car active)) current)])))

  ;; The term where the contified group must be inserted: the nearest common
  ;; position of every call to a group member.
  (define (contification-site g active-link live body binders)
    (let ([children
           (let loop ([live live] [acc '()])
             (if (null? live)
               (reverse acc)
               (let ([function (live-function (car live))])
                 (if (entity-set-contains? binders (function-var (graph-function g function)))
                   (loop (cdr live) acc)
                   (loop (cdr live)
                     (cons (find-push-site g (function-body (graph-function g function)) binders)
                       acc))))))])
      (cdr (combine-push-sites active-link
             (append children (list (find-push-site g body binders)))))))

  (define (find-push-site g link binders)
    (let ([term (read-term-link g link)])
      (if (not term)
        push-none
        (let ([kind (term-kind (graph-term g term))])
          (cond
            [(k-let-val? kind) (find-push-site g (k-let-val-body kind) binders)]
            [(or (k-letk? kind) (k-fix? kind))
              (let* ([functions (if (k-letk? kind) (k-letk-functions kind) (k-fix-functions kind))]
                     [body (if (k-letk? kind) (k-letk-body kind) (k-fix-body kind))]
                     [children
                      (append
                        (map (lambda (function)
                              (find-push-site g (function-body (graph-function g function)) binders))
                          (live-functions g functions))
                        (list (find-push-site g body binders)))])
                (combine-push-sites link children))]
            [(k-if? kind)
              (combine-push-sites link
                (list (find-push-site g (k-if-then kind) binders)
                  (find-push-site g (k-if-else kind) binders)))]
            [(and (k-app? kind) (entity-set-contains? binders (free-binder g (k-app-func kind))))
              (cons 1 link)]
            [else push-none]))))))
